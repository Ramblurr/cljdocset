(ns cljdocset.parse
  (:require
   [babashka.fs :as fs]
   [cljdocset.util :as util]
   [clojure.string :as str]
   [hickory.core :as hickory]
   [hickory.select :as s]))

(def dash-type-map
  "Maps Clojure symbol types to Dash entry types"
  {:var "Variable"
   :macro "Macro"
   :protocol "Protocol"
   :multimethod "Method"})

(def default-dash-type
  "Default Dash type for unmapped Clojure types"
  "Function")

(defn find-all-headers
  "Find all h2 and h3 header elements in the hickory DOM, preserving document order"
  [hickory-dom]
  (s/select (s/or (s/tag :h2) (s/tag :h3)) hickory-dom))

(defn extract-header-text
  "Extract clean text content from a header element, handling nested anchor tags"
  [header-elem]
  (let [extract-text (fn extract-text [elem]
                       (cond
                         (string? elem) elem
                         (and (map? elem) (= :a (:tag elem)))
                         (apply str (map extract-text (:content elem)))
                         (map? elem)
                         (apply str (map extract-text (:content elem)))
                         :else ""))]
    (-> (extract-text header-elem)
        str/trim)))

(defn headers->section-entries
  "Convert header elements to section entries with h2 context tracking"
  [headers relative-path]
  (loop [headers headers
         current-h2 nil
         entries []]
    (if-let [header (first headers)]
      (let [tag (:tag header)
            text (extract-header-text header)
            ;; Try to get anchor from header attrs first, then from first child if it's an anchor
            anchor (or (get-in header [:attrs :id])
                       (when-let [first-child (first (:content header))]
                         (when (and (map? first-child) (= :a (:tag first-child)))
                           (get-in first-child [:attrs :id]))))
            entry-name (if (and (= :h3 tag)
                                current-h2
                                (not (str/blank? current-h2)))
                         (format "%s - %s" text current-h2)
                         text)
            new-h2 (if (= :h2 tag) text current-h2)]
        (if (and (not (str/blank? text)) anchor)
          (recur (rest headers)
                 new-h2
                 (conj entries {:name entry-name
                                :type "Section"
                                :path (str relative-path "#" anchor)}))
          (recur (rest headers) new-h2 entries)))
      entries)))

(defn extract-sections
  "Extract all section entries from a hickory DOM for a given file"
  [hickory-dom relative-path]
  (let [headers (find-all-headers hickory-dom)]
    (headers->section-entries headers relative-path)))

(defn parse-html-file
  "Parse an HTML file into a hickory DOM structure"
  [file-path]
  (-> file-path
      slurp
      hickory/parse
      hickory/as-hickory))

(defn find-api-symbols
  "Find all symbol definition blocks in the hickory DOM"
  [hickory-dom]
  (s/select (s/class "def-block") hickory-dom))

(defn extract-symbol-name
  "Extract the symbol name from a def-block, removing metadata like 'clj/s'"
  [def-block]
  (let [title-elem (first (s/select (s/class "def-block-title") def-block))
        text-content (when title-elem
                       (-> title-elem
                           :content
                           first
                           str/trim))]
    (when text-content
      ;; Remove trailing metadata like "clj/s" or spaces
      (-> text-content
          (str/replace #"\s*(clj|cljs|clj/s)?\s*$" "")
          str/trim))))

(defn extract-symbol-type
  "Detect the type of symbol from the def-block content"
  [def-block]
  (let [title-elem (first (s/select (s/class "def-block-title") def-block))
        content (when title-elem (:content title-elem))
        ;; Helper to check if content has a span with specific text
        has-type-span? (fn [type-name]
                         (some #(and (map? %)
                                     (= :span (:tag %))
                                     (str/includes? (str (:content %)) type-name))
                               content))]
    (cond
      (has-type-span? "protocol") :protocol
      (has-type-span? "multimethod") :multimethod
      (has-type-span? "macro") :macro
      ;; Add more type detection logic as needed
      :else nil)))

(defn extract-symbol-anchor
  "Extract the anchor (id attribute) from the def-block title"
  [def-block]
  (let [title-elem (first (s/select (s/class "def-block-title") def-block))]
    (get-in title-elem [:attrs :id])))

(defn symbol->entry
  "Convert a hickory def-block node to a docset entry map"
  [def-block relative-path]
  (when-let [name (extract-symbol-name def-block)]
    (when-let [anchor (extract-symbol-anchor def-block)]
      (let [clj-type (extract-symbol-type def-block)
            dash-type (get dash-type-map clj-type default-dash-type)]
        {:name name
         :type dash-type
         :path (str relative-path "#" anchor)}))))

(defn extract-symbols
  "Extract all symbols from a hickory DOM tree for a given file"
  [hickory-dom relative-path]
  (->> (find-api-symbols hickory-dom)
       (map #(symbol->entry % relative-path))
       (remove nil?)))

(defn list-api-pages
  "Find all API documentation HTML files in the bundle directory"
  [bundle-dir]
  (let [api-dir (fs/path bundle-dir "api")
        html-files (when (fs/exists? api-dir)
                     (fs/glob api-dir "*.html"))]
    (->> html-files
         (map #(str (fs/relativize bundle-dir %)))
         sort)))

(defn list-guide-pages
  "Find all guide documentation HTML files in the bundle directory"
  [bundle-dir]
  (let [doc-dir (fs/path bundle-dir "doc")
        html-files (when (fs/exists? doc-dir)
                     (fs/glob doc-dir "*.html"))]
    (->> html-files
         (map #(str (fs/relativize bundle-dir %)))
         sort)))

(defn extract-guide-title
  "Extract the title from a guide HTML page using the first <h1>"
  [hickory-dom]
  (when-let [h1-elem (first (s/select (s/tag :h1) hickory-dom))]
    (let [h1-content (:content h1-elem)]
      (str/trim
       (apply str (map #(if (string? %) %
                            (when (and (map? %) (= :a (:tag %)))
                              (apply str (filter string? (:content %)))))
                       h1-content))))))

(defn parse-guide-entry
  "Parse a guide HTML file and return a docset entry"
  [bundle-dir relative-path]
  (let [full-path (str (fs/path bundle-dir relative-path))
        dom (parse-html-file full-path)
        guide-title (extract-guide-title dom)]
    (when guide-title
      {:name guide-title
       :type "Guide"
       :path relative-path})))

(defn extract-namespace-name
  "Extract namespace name from API HTML page using the first <h1>"
  [hickory-dom]
  (when-let [h1-elem (first (s/select (s/tag :h1) hickory-dom))]
    (str/trim (apply str (filter string? (:content h1-elem))))))

(defn parse-namespace-entry
  "Parse an API HTML file and return a namespace docset entry"
  [bundle-dir relative-path]
  (let [full-path (str (fs/path bundle-dir relative-path))
        dom (parse-html-file full-path)
        namespace-name (extract-namespace-name dom)]
    (when namespace-name
      {:name namespace-name
       :type "Namespace"
       :path relative-path})))

(defn parse-all-entries
  "Parse all documentation files and extract entries (symbols, namespaces, guides, and sections).
  Expects context with :paths :bundle-dir.
  Returns context with entries added at :docset-data :symbols"
  [ctx]
  (util/info "Parsing documentation...")
  (let [bundle-dir (get-in ctx [:paths :bundle-dir])
        ;; Parse API pages for symbols and namespace entries
        api-pages (list-api-pages bundle-dir)
        namespace-entries (keep #(parse-namespace-entry bundle-dir %) api-pages)
        all-symbols (mapcat (fn [relative-path]
                              (let [full-path (str (fs/path bundle-dir relative-path))
                                    dom (parse-html-file full-path)]
                                (extract-symbols dom relative-path)))
                            api-pages)
        ;; Parse guide pages
        guide-pages (list-guide-pages bundle-dir)
        guide-entries (keep #(parse-guide-entry bundle-dir %) guide-pages)
        ;; Parse sections from both API and guide pages
        all-pages (concat api-pages guide-pages)
        section-entries (mapcat (fn [relative-path]
                                  (let [full-path (str (fs/path bundle-dir relative-path))
                                        dom (parse-html-file full-path)]
                                    (extract-sections dom relative-path)))
                                all-pages)
        ;; Combine all entries
        all-entries (concat all-symbols namespace-entries guide-entries section-entries)]
    (util/debug (format "Parsed %d entries: %d symbols, %d namespaces, %d guides, %d sections"
                        (count all-entries)
                        (count all-symbols)
                        (count namespace-entries)
                        (count guide-entries)
                        (count section-entries)))
    (assoc-in ctx [:docset-data :symbols] all-entries)))
