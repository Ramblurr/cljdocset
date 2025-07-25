(ns cljdocset.cli
  (:require [babashka.cli :as cli]))

(def global-spec
  {:help {:desc "Show help"
          :alias :h
          :coerce :boolean}})

(def build-spec
  {:output-dir {:desc "Directory to save the final docset"
                :alias :o
                :ref "<dir>"
                :default "."
                :default-desc "current directory"}
   :icon-path {:desc "Path to a 16x16 or 32x32 PNG icon to include in the docset"
               :alias :i
               :ref "<file>"}
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

(defn- print-build-help
  "Print build command help text"
  [_]
  (println "cljdocset build - Generate a docset from a cljdoc bundle")
  (println)
  (println "Usage: cljdocset build <library-name> [version] [options]")
  (println)
  (println "Arguments:")
  (println "  <library-name>  The fully qualified library name (e.g., metosin/reitit)")
  (println "  [version]       The specific version (e.g., 0.9.1), defaults to \"latest\"")
  (println)
  (println "Options:")
  (println (format-help {:spec build-spec
                         :order [:output-dir :icon-path :help]}))
  (println)
  (println "Examples:")
  (println "  # Generate docset for latest version")
  (println "  cljdocset build metosin/reitit")
  (println)
  (println "  # Generate docset for specific version")
  (println "  cljdocset build metosin/reitit 0.9.1")
  (println)
  (println "  # Generate docset with custom output directory and icon")
  (println "  cljdocset build clj-commons/hickory 0.7.3 \\")
  (println "    --output-dir ~/Documents/Docsets \\")
  (println "    --icon-path ./assets/hickory-icon.png"))

(defn- build-command
  "Execute the build command with parsed options"
  [{:keys [args opts]}]
  (when (:help opts)
    (print-build-help nil)
    (System/exit 0))

  (let [library-name (first args)
        version (or (second args) "latest")]
    (when-not library-name
      (println "Error: Missing required argument <library-name>")
      (println)
      (print-build-help nil)
      (System/exit 1))
    ;; TODO: Implement actual build logic
    (println "Building docset for" library-name "version" version)
    (println "Output directory:" (:output-dir opts))
    (when (:icon-path opts)
      (println "Icon path:" (:icon-path opts)))))

(def dispatch-table
  [{:cmds ["build"]
    :fn build-command
    :spec build-spec}
   {:cmds []
    :fn print-main-help}])

(defn -main
  "Main entry point for the cljdocset CLI tool"
  [& args]
  (try
    (let [parsed (cli/dispatch dispatch-table args {:spec global-spec})]
      (when (and (empty? (:dispatch parsed))
                 (:help (:opts parsed)))
        (print-main-help nil)
        (System/exit 0)))
    (catch Exception e
      (if (= :org.babashka/cli (:type (ex-data e)))
        (do
          (println "Error:" (ex-message e))
          (println)
          (print-main-help nil)
          (System/exit 1))
        (throw e)))))
