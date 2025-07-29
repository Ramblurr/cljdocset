;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: EUPL-1.2
(ns cljdocset.download
  (:require
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [cljdocset.util :as util]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn zip-filename [lib-info]
  (format "%s-%s.zip"
          (:artifact-id lib-info)
          (:version lib-info)))

(defn bundle-dir [lib-info]
  (format "%s-%s"
          (:artifact-id lib-info)
          (:version lib-info)))

(defn resolve-paths [{:keys [lib-info paths] :as ctx}]
  (assert lib-info "Library info must be provided in context")
  (let [build-dir (:build-dir paths)]
    (assoc ctx :paths (-> paths
                          (assoc :zip-path (str (fs/path build-dir (zip-filename lib-info))))
                          (assoc :bundle-dir (str (fs/path build-dir (bundle-dir lib-info))))))))

(defn get-latest-version [{:keys [lib-info]}]
  (let [uri (:uri (http/get (format "https://cljdoc.org/d/%s/%s" (:group-id lib-info) (:artifact-id lib-info))))]
    (->
     (re-seq #"/d/[^/]+/[^/]+/([^/]+)" (str uri))
     first
     second)))

(defn build-download-url
  "Constructs the cljdoc download URL from library identifier and version.
  Expects a map with :group-id, :artifact-id, and :version."
  [{:keys [group-id artifact-id version]}]
  (format "https://cljdoc.org/download/%s/%s/%s"
          group-id artifact-id version))

(defn- resolve-version [{:keys [lib-info] :as ctx}]
  (if (= "latest" (:version lib-info))
    (if-let [latest (get-latest-version ctx)]
      (do
        (util/info "Resolving latest version for" (:artifact-id lib-info))
        (assoc-in ctx [:lib-info :version] latest))
      (throw (ex-info "Failed to resolve latest version"
                      {:lib-info lib-info})))
    ctx))

(defn- download-to-file
  "Downloads from URL to file-path."
  [url file-path]
  (let [response (http/get url {:as :stream})]
    (when (not= 200 (:status response))
      (throw (ex-info (str "Download failed with status " (:status response))
                      {:url url :status (:status response)})))
    (with-open [in (:body response)
                out (io/output-stream file-path)]
      (io/copy in out))
    file-path))

(defn download-bundle
  "Downloads the cljdoc bundle for a given library and version.
  Expects a context map with :lib-info containing :group-id, :artifact-id, and :version.
  Returns the context map updated with :zip-path."
  [ctx]
  (let [zip-path (-> ctx :paths :zip-path)]
    (if (and (fs/exists? zip-path) (not (-> ctx :cli-opts :force)))
      (do
        (util/info "Bundle already exists at" zip-path)
        ctx)
      (let [url (build-download-url (:lib-info ctx))]
        (util/info "Downloading from" url)
        (try
          (download-to-file url (str zip-path))
          (util/debug "Download complete:" (str zip-path))
          ctx
          (catch Exception e
            (throw (ex-info "Failed to download bundle"
                            {:url url
                             :error (.getMessage e)}
                            e))))))))

(defn create-build-directory
  "Creates the build directory based on context options.
  If :build-dir is specified in :cli-opts, uses that directory.
  Otherwise creates a temporary directory.
  Returns ctx with updated :paths and :temp-dir? flag."
  [ctx]
  (let [build-dir-opt (get-in ctx [:cli-opts :build-dir])
        temp? (str/blank? build-dir-opt)
        build-dir (if temp?
                    (fs/create-temp-dir {:prefix "cljdocset-"})
                    (let [build-dir (-> build-dir-opt fs/canonicalize fs/absolutize)]
                      (fs/create-dirs build-dir)
                      build-dir))]

    (util/debug "Using build directory:" (str build-dir))
    (-> ctx
        (assoc :temp-dir? temp?)
        (assoc-in [:paths :build-dir] (str build-dir)))))

(defn extract-bundle
  "Extracts the downloaded zip bundle to the build directory.
  Expects ctx with :paths containing :zip-file and :build-dir.
  Returns ctx with :bundle-dir added to :paths."
  [ctx]

  (let [zip-path (get-in ctx [:paths :zip-path])
        build-dir (get-in ctx [:paths :build-dir])]
    (assert zip-path)
    (assert build-dir)
    (util/info "Extracting bundle..." zip-path build-dir)
    (fs/unzip zip-path build-dir {:replace-existing true})
    (let [extracted-dirs (->> (fs/list-dir build-dir)
                              (filter fs/directory?))
          bundle-dir (first extracted-dirs)]
      (when-not bundle-dir
        (throw (ex-info "No directory found after extraction"
                        {:build-dir build-dir})))
      (let [bundle-path (fs/path build-dir (fs/file-name bundle-dir))]
        (when-not (fs/exists? (fs/path bundle-path "index.html"))
          (throw (ex-info "Invalid bundle: missing index.html"
                          {:bundle-dir (str bundle-path)})))
        (util/debug "Bundle extracted to:" (str bundle-path))
        (assoc-in ctx [:paths :bundle-dir] (str bundle-path))))))

(defn setup-cleanup
  "Sets up cleanup for temporary directories.
  Only registers cleanup when :temp-dir? is true."
  [ctx]
  (when (:temp-dir? ctx)
    (fs/delete-on-exit (get-in ctx [:paths :build-dir])))
  ctx)

(defn prepare-build-environment
  "Pipeline function that prepares the complete build environment.
  Creates directories, downloads bundle, extracts it, and sets up cleanup."
  [ctx]
  (util/info "Preparing build environment...")
  (-> ctx
      create-build-directory
      resolve-version
      resolve-paths
      download-bundle
      extract-bundle))