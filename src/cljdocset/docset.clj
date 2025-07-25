(ns cljdocset.docset
  (:require
   [babashka.fs :as fs]
   [babashka.process :as process]
   [cljdocset.util :as util]
   [hiccup2.core :as hiccup]))

(defn resolve-docset-paths
  "Resolves and adds all docset-related paths to the context map.
  Always uses build-dir/out for the output directory during build.
  The user's output-dir from cli-opts is only used at the final move step.
  Returns ctx with :paths containing all resolved paths."
  [{:keys [cli-opts lib-info paths] :as ctx}]
  (let [;; Always use build-dir/out for the working output directory
        output-dir (fs/path (:build-dir paths) "out")
        docset-name (:docset-name lib-info)
        docset-dir (fs/path output-dir (str docset-name ".docset"))
        contents-dir (fs/path docset-dir "Contents")
        resources-dir (fs/path contents-dir "Resources")
        documents-dir (fs/path resources-dir "Documents")
        db-file (fs/path resources-dir "docSet.dsidx")
        plist-file (fs/path contents-dir "Info.plist")
        archive-file (fs/path output-dir (str docset-name ".tgz"))
        icon-path (fs/path docset-dir "icon@2x.png")
        ;; Only set input-icon-path if the file exists
        input-icon-path (when-let [icon-opt (:icon-path cli-opts)]
                          (let [icon-path-obj (fs/path icon-opt)]
                            (when (fs/exists? icon-path-obj)
                              (str icon-path-obj))))]
    (update ctx :paths merge
            {:output-dir (str output-dir)
             :docset-dir (str docset-dir)
             :contents-dir (str contents-dir)
             :resources-dir (str resources-dir)
             :documents-dir (str documents-dir)
             :input-icon-path input-icon-path
             :icon-path (str icon-path)
             :db-file (str db-file)
             :plist-file (str plist-file)
             :archive-file (str archive-file)})))

(defn create-docset-structure
  "Creates the necessary directory structure for the docset.
  Expects ctx with :paths containing :documents-dir.
  Returns ctx unchanged after creating directories."
  [{:keys [paths] :as ctx}]
  (util/info "Creating docset structure...")
  (fs/create-dirs (:documents-dir paths))
  ctx)

(defn copy-assets
  "Copies the HTML documentation and assets from the unzipped bundle into the docset's Documents directory.
  Expects ctx with :bundle-dir and :paths containing :documents-dir.
  Returns ctx unchanged after copying files."
  [{:keys [paths] :as ctx}]
  (util/info "Copying documentation files...")
  (let [source-dir (get-in ctx [:paths :bundle-dir])
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
  [{:keys [lib-info paths] :as ctx}]
  (util/info "Creating Info.plist...")
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
  (util/info "Creating archive...")
  (let [docset-dir (:docset-dir paths)
        archive-file (:archive-file paths)
        parent-dir (fs/parent docset-dir)
        docset-name (fs/file-name docset-dir)]
    (process/shell {:dir (str parent-dir)}
                   "tar" "--exclude=.DS_Store" "-czf"
                   (str archive-file)
                   docset-name)
    (assoc ctx :archive-path (str archive-file))))

(defn prepare-output-directory
  "Creates the final output directory if it doesn't exist.
  Expects ctx with :paths containing :output-dir.
  Returns ctx unchanged after creating directory."
  [{:keys [paths] :as ctx}]
  (let [output-dir (:output-dir paths)]
    (when-not (fs/exists? output-dir)
      (fs/create-dirs output-dir)))
  ctx)

(defn move-artifacts
  "Moves the generated docset and archive to the final output directory.
  Uses the user's specified output-dir from cli-opts, or current directory if not specified.
  Returns ctx with updated :final-artifacts paths."
  [{:keys [paths cli-opts] :as ctx}]
  (util/info "Moving artifacts to output directory...")
  (let [{:keys [docset-dir archive-file]} paths
        ;; Use user's output-dir or default to current directory
        final-output-dir (or (:output-dir cli-opts) ".")
        final-output-path (fs/absolutize final-output-dir)]

    ;; Create the final output directory if it doesn't exist
    (when-not (fs/exists? final-output-path)
      (fs/create-dirs final-output-path))

    ;; Always move files to the user's specified location
    (let [docset-name (fs/file-name docset-dir)
          archive-name (fs/file-name archive-file)
          final-docset (fs/path final-output-path docset-name)
          final-archive (fs/path final-output-path archive-name)]

      ;; Move docset directory
      (when (fs/exists? final-docset)
        (fs/delete-tree final-docset))
      (fs/move docset-dir final-docset)

      ;; Move archive file
      (when (fs/exists? final-archive)
        (fs/delete final-archive))
      (fs/move archive-file final-archive)

      ;; Return updated context with final locations
      (assoc ctx :final-artifacts
             {:docset-path (str final-docset)
              :archive-path (str final-archive)}))))
