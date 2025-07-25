(ns cljdocset.cli
  (:require
   [babashka.cli :as cli]
   [cljdocset.download :as download]
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
              ctx {:cli-opts opts
                   :lib-info {:group-id group-id
                              :artifact-id artifact-id
                              :version version}
                   :paths {}}]
          (when (or (not group-id) (not artifact-id))
            (throw (ex-info "Invalid library name format. Expected: group-id/artifact-id"
                            {:library-name library-name})))

          (util/info "Building docset for" library-name "version" version)
          (util/debug "Output directory:" (:output-dir opts))
          (when (:icon-path opts)
            (util/debug "Icon path:" (:icon-path opts)))

          (let [result-ctx (download/prepare-build-environment ctx)]
            (util/info "Build environment prepared successfully")
            (util/debug "Bundle directory:" (get-in result-ctx [:paths :bundle-dir]))))

        (catch Exception e
          (util/error (.getMessage e))
          (when-let [data (ex-data e)]
            (doseq [[k v] data]
              (util/error (str "  " (name k) ":") v)))
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
