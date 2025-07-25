(ns cljdocset.docset
  (:require
   [hiccup2.core :as hiccup]
   [babashka.fs :as fs]
   [babashka.process :as process]))

(defn resolve-docset-paths
  "Resolves and adds all docset-related paths to the context map.
  Expects :cli-opts with :output-dir and :lib-info with :docset-name.
  Returns ctx with :paths containing all resolved paths."
  [{:keys [cli-opts lib-info paths] :as ctx}]
  (let [output-dir      (fs/path (:build-dir paths) "out")
        docset-name     (:docset-name lib-info)
        docset-dir      (fs/path output-dir (str docset-name ".docset"))
        contents-dir    (fs/path docset-dir "Contents")
        resources-dir   (fs/path contents-dir "Resources")
        documents-dir   (fs/path resources-dir "Documents")
        db-file         (fs/path resources-dir "docSet.dsidx")
        plist-file      (fs/path contents-dir "Info.plist")
        archive-file    (fs/path output-dir (str docset-name ".tgz"))
        icon-path       (fs/path docset-dir "icon@2x.png")
        input-icon-path (when-let [icon-opt (:icon-path cli-opts)]
                          (when (fs/exists? icon-opt)
                            icon-opt))]
    (update ctx :paths merge
            {:output-dir      (str output-dir)
             :docset-dir      (str docset-dir)
             :contents-dir    (str contents-dir)
             :resources-dir   (str resources-dir)
             :documents-dir   (str documents-dir)
             :input-icon-path input-icon-path
             :icon-path       (str icon-path)
             :db-file         (str db-file)
             :plist-file      (str plist-file)
             :archive-file    (str archive-file)})))

(defn create-docset-structure
  "Creates the necessary directory structure for the docset.
  Expects ctx with :paths containing :documents-dir.
  Returns ctx unchanged after creating directories."
  [{:keys [paths] :as ctx}]
  (fs/create-dirs (:documents-dir paths))
  ctx)

(defn copy-assets
  "Copies the HTML documentation and assets from the unzipped bundle into the docset's Documents directory.
  Expects ctx with :bundle-dir and :paths containing :documents-dir.
  Returns ctx unchanged after copying files."
  [{:keys [bundle-dir paths]
    :as ctx}]
  (let [source-dir bundle-dir
        target-dir (:documents-dir paths)]
    (fs/copy-tree source-dir target-dir)
    ctx))

(defn- info-plist-hiccup
  "Returns the Hiccup structure for the Info.plist file."
  [{:keys [bundle-id docset-name artifact-id version]}]
  [:plist {:version "1.0"}
   [:dict
    [:key "CFBundleIdentifier"]
    [:string bundle-id]
    [:key "CFBundleName"]
    [:string (str artifact-id " " version)]
    [:key "isDashDocset"]
    [:true]
    [:key "DocSetPlatformFamily"]
    [:string "cljlib"]
    [:key "dashIndexFilePath"]
    [:string (str docset-name "/index.html")]
    [:key "isJavaScriptEnabled"]
    [:true]
    [:key "DashDocSetFamily"]
    [:string "dashtoc"]
    [:key "DashDocSetKeyword"]
    [:string "cljdoc"]]])

(defn create-info-plist
  "Generates the Info.plist file for the docset.
  Expects ctx with :lib-info and :paths containing :plist-file.
  Returns ctx unchanged after writing plist file."
  [{:keys [lib-info paths]
    :as ctx}]
  (let [plist-hiccup (info-plist-hiccup lib-info)
        plist-xml (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                       "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" "
                       "\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
                       (hiccup/html {:mode :xml} plist-hiccup))
        plist-file (:plist-file paths)]
    (spit (str plist-file) plist-xml)
    ctx))

(defn add-icon
  "Copies a user-specified icon into the root of the .docset directory.
  Expects ctx with :cli-opts containing optional :icon-path and :paths.
  Returns ctx unchanged. No-op if :icon-path is nil."
  [{:keys [paths] :as ctx}]
  (when-let [input-icon-path (:input-icon-path paths)]
    (fs/copy input-icon-path (:icon-path paths)))

  ctx)

(defn archive-docset
  "Creates a compressed tarball (.tgz) of the generated .docset directory.
  Expects ctx with :paths containing :docset-dir and :archive-file.
  Returns ctx with :archive-path added."
  [{:keys [paths] :as ctx}]
  (let [docset-dir (:docset-dir paths)
        archive-file (:archive-file paths)
        parent-dir (fs/parent docset-dir)
        docset-name (fs/file-name docset-dir)]
    (process/shell {:dir (str parent-dir)}
                   "tar" "--exclude=.DS_Store" "-czf"
                   (str archive-file)
                   docset-name)
    (assoc ctx :archive-path (str archive-file))))
