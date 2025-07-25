(ns cljdocset.cli
  (:require
   [babashka.cli :as cli]
   [cljdocset.db :as db]
   [cljdocset.docset :as docset]
   [cljdocset.download :as download]
   [cljdocset.images :as images]
   [cljdocset.parse :as parse]
   [cljdocset.util :as util]
   [clojure.string :as str]))

(def global-spec
  {:help {:desc "Show help"
          :alias :h
          :coerce :boolean}
   :verbose {:desc "Enable verbose output"
             :alias :v
             :coerce :boolean}})

(def build-spec
  {:output-dir {:desc "Directory to save the final docset"
                :alias :o
                :ref "<dir>"
                :default "."
                :default-desc "current directory"}
   :build-dir {:desc "Build directory for extraction (default: temp dir, deleted on exit)"
               :alias :b
               :ref "<dir>"}
   :icon-path {:desc "Path to a 16x16 or 32x32 PNG icon to include in the docset"
               :alias :i
               :ref "<file>"}
   :with-javascript {:desc "Enable JavaScript in the docset (default: false)"
                     :alias :j
                     :coerce :boolean
                     :default false}
   :enable-fts {:desc "Enable full-text search by default in the docset"
                :alias :f
                :coerce :boolean
                :default false}
   :verbose {:desc "Enable verbose output"
             :alias :v
             :coerce :boolean}
   :help {:desc "Show help"
          :alias :h
          :coerce :boolean}})

(def version "0.1.0-SNAPSHOT")

(defn- format-help
  "Format help text for display"
  [{:keys [spec order]}]
  (cli/format-opts {:spec spec
                    :order (or order (vec (keys spec)))}))

(defn- print-main-help
  "Print main help text"
  [_]
  (println "cljdocset" version "- Convert cljdoc bundles to Dash/Zeal docsets")
  (println)
  (println "Usage: cljdocset <subcommand> [options]")
  (println)
  (println "Available subcommands:")
  (println "  build    Generate a docset from a cljdoc bundle")
  (println)
  (println "Options:")
  (println (format-help {:spec global-spec}))
  (println)
  (println "Run 'cljdocset <subcommand> --help' for more information on a subcommand."))

(defn build-docset
  "Main pipeline function that orchestrates the complete docset generation.
  Takes a context map and threads it through all transformation steps."
  [ctx]
  (util/info "Starting docset generation pipeline...")
  (-> ctx
      download/prepare-build-environment
      download/setup-cleanup
      parse/parse-all-entries
      docset/resolve-docset-paths
      docset/create-docset-structure
      docset/copy-assets
      images/download-all-images
      docset/create-info-plist
      docset/add-icon
      db/initialize-db
      db/index-symbols
      docset/archive-docset
      docset/move-artifacts))

(defn- build-command
  "Execute the build command with parsed options"
  [{:keys [args opts]}]
  (binding [util/*verbose* (:verbose opts)]
    (when (> (count args) 2)
      (util/error "Too many arguments provided. Expected: <library-name> [version]")
      (System/exit 1))
    (let [library-name (first args)
          version (or (second args) "latest")]
      (when-not library-name
        (util/error "Missing required argument. Expected: <library-name> [version]")
        (System/exit 1))

      (try
        (let [[group-id artifact-id] (str/split library-name #"/")
              docset-name (str artifact-id "-" version)
              ctx {:cli-opts opts
                   :lib-info {:group-id group-id
                              :artifact-id artifact-id
                              :version version
                              :docset-name docset-name}
                   :paths {}}]
          (when (or (not group-id) (not artifact-id))
            (throw (ex-info "Invalid library name format. Expected: group-id/artifact-id"
                            {:library-name library-name})))

          ;; Run the complete pipeline
          (let [result-ctx (build-docset ctx)
                {:keys [docset-path archive-path]} (:final-artifacts result-ctx)]

            ;; Report success
            (util/info "")
            (util/info "✅ Docset generation completed successfully!")
            (util/info "")
            (util/info "Generated artifacts:")
            (util/info "  Docset:" docset-path)
            (util/info "  Archive:" archive-path)
            (util/info "")
            (util/info "You can now:")
            (util/info "  - Import the .docset directory into Dash/Zeal")
            (util/info "  - Share the .tgz archive for distribution")))

        (catch Exception e
          (util/error "Docset generation failed:")
          (util/error (.getMessage e))
          (when-let [data (ex-data e)]
            (doseq [[k v] data]
              (util/error (str "  " (name k) ":") v)))
          (when util/*verbose*
            (util/error "Stack trace:")
            (.printStackTrace e))
          (System/exit 1))))))

(def dispatch-table
  [{:cmds ["build"] :fn build-command :spec build-spec}
   {:cmds [] :fn print-main-help}])

(defn -main
  [& args]
  (try
    (cli/dispatch dispatch-table args)
    (catch Exception e
      (util/error "Exception: ")
      (util/error e)
      (System/exit 1))))
