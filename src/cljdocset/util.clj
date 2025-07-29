;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: EUPL-1.2
(ns cljdocset.util
  "Common utility functions for logging and output formatting"
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]))

;; ANSI color codes
(def ^:private colors
  {:red "\033[0;31m"
   :green "\033[0;32m"
   :yellow "\033[1;33m"
   :purple "\033[0;35m"
   :reset "\033[0m"})

(defn colorize
  "Apply ANSI color to text"
  [color text]
  (str (get colors color "") text (:reset colors)))

(defn timestamp
  "Get current timestamp in HH:MM:SS format"
  []
  (.format (java.time.LocalTime/now)
           (java.time.format.DateTimeFormatter/ofPattern "HH:mm:ss")))

(def ^:dynamic *verbose* false)

(defn debug
  "Log a message with timestamp if verbose is enabled"
  [& args]
  (when *verbose*
    (binding [*out* *err*]
      (apply println (str (colorize :green (str "[" (timestamp) "]")) " ") args))))

(defn unsafe-shell-escape
  "This function is for debugging and troubleshooting purposes, and should not be used to escape input for execution"
  [s]
  (cond
    (empty? s) "\"\""
    ;; no special characters -> no quoting needed
    (re-matches #"[a-zA-Z0-9._/=-]+" s) s
    :else
    (str "\""
         (-> s
             (str/replace #"\\" "\\\\") ; Escape backslashes first
             (str/replace #"\"" "\\\"") ; Escape double quotes
             (str/replace #"\$" "\\$") ; Escape dollar signs
             (str/replace #"`" "\\`")) ; Escape backticks
         "\"")))

(defn debug-cmd
  [cmd]
  (when *verbose*
    (let [cmd (if (string? cmd)
                cmd
                (->> cmd
                     (map str)
                     (map unsafe-shell-escape)
                     (str/join " ")))]
      (binding [*out* *err*]
        (apply println (str (colorize :purple "[shell]") " ") [cmd])))))

(defn info
  "Print informational message to stdout"
  [& args]
  (apply println (str (colorize :green "[INFO]") " ") args))

(defn error
  "Print error message to stderr"
  [& args]
  (binding [*out* *err*]
    (apply println (str (colorize :red "[ERROR]") " ") args)))

(defn warn
  "Print warning message to stderr"
  [& args]
  (binding [*out* *err*]
    (apply println (str (colorize :yellow "[WARN]") " ") args)))

(defn exit-msg [& msg]
  (apply error msg)
  (System/exit 1))

(defn xxx [v]
  (clojure.pprint/pprint v)
  v)