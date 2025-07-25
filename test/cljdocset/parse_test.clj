(ns cljdocset.parse-test
  (:require
   [babashka.fs :as fs]
   [cljdocset.parse :as parse]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn prepare-fixtures []
  (let [fixtures ["reitit-0.9.1.zip" "hiccup-2.0.0.zip" "clj-poly-0.2.22.zip" "tempel-1.0.0-RC1.zip"]]
    (doseq [f fixtures]
      (when-not (fs/exists? (str "test/fixtures/" f))
        (fs/copy (str "test/fixtures/" f) "test/fixtures/")))))

(prepare-fixtures)

(def fixture-dir "test/fixtures/reitit-0.9.1")
(def core-html-path (str fixture-dir "/api/reitit.core.html"))

(deftest parse-html-file-test
  (testing "Can parse HTML file into hickory DOM"
    (let [dom (parse/parse-html-file core-html-path)]
      (is (map? dom))
      (is (= :document (:type dom))))))

(deftest find-api-symbols-test
  (testing "Can find all def-block elements"
    (let [dom (parse/parse-html-file core-html-path)
          symbols (parse/find-api-symbols dom)]
      (is (seq symbols))
      (is (> (count symbols) 10)))))

(deftest extract-symbol-name-test
  (testing "Can extract symbol names correctly"
    (let [dom (parse/parse-html-file core-html-path)
          symbols (parse/find-api-symbols dom)
          first-symbol (first symbols)
          names (map parse/extract-symbol-name (take 5 symbols))]
      (is (= "Expand" (parse/extract-symbol-name first-symbol)))
      (is (every? string? names))
      (is (every? #(not (str/includes? % "clj/s")) names)))))

(deftest extract-symbol-type-test
  (testing "Can detect protocol types"
    (let [dom (parse/parse-html-file core-html-path)
          symbols (parse/find-api-symbols dom)
          first-symbol (first symbols)
          router-symbol (first (filter #(= "Router" (parse/extract-symbol-name %)) symbols))]
      (is (= :protocol (parse/extract-symbol-type first-symbol)))
      (is (= :protocol (parse/extract-symbol-type router-symbol))))))

(deftest symbol->entry-test
  (testing "Can convert symbol to docset entry"
    (let [dom (parse/parse-html-file core-html-path)
          symbols (parse/find-api-symbols dom)
          first-symbol (first symbols)
          entry (parse/symbol->entry first-symbol "api/reitit.core.html")]
      (is (= {:name "Expand"
              :type "Protocol"
              :path "api/reitit.core.html#Expand"}
             entry)))))

(deftest extract-symbols-test
  (testing "Can extract all symbols from a file"
    (let [dom (parse/parse-html-file core-html-path)
          symbols (parse/extract-symbols dom "api/reitit.core.html")]
      (is (seq symbols))
      (is (every? #(contains? % :name) symbols))
      (is (every? #(contains? % :type) symbols))
      (is (every? #(contains? % :path) symbols))
      (is (some #(= "router" (:name %)) symbols))
      (is (some #(= "match->path" (:name %)) symbols))
      (is (some #(= "Protocol" (:type %)) symbols)))))

(deftest list-api-pages-test
  (testing "Can find all API HTML files"
    (let [api-pages (parse/list-api-pages fixture-dir)]
      (is (seq api-pages))
      (is (every? #(str/starts-with? % "api/") api-pages))
      (is (every? #(str/ends-with? % ".html") api-pages))
      (is (some #(= "api/reitit.core.html" %) api-pages)))))

(deftest list-guide-pages-test
  (testing "Can find all guide HTML files"
    (let [guide-pages (parse/list-guide-pages fixture-dir)]
      (is (seq guide-pages))
      (is (every? #(str/starts-with? % "doc/") guide-pages))
      (is (every? #(str/ends-with? % ".html") guide-pages))
      (is (= 49 (count guide-pages)))
      (is (some #(= "doc/introduction.html" %) guide-pages)))))

(deftest extract-guide-title-test
  (testing "Can extract title from guide HTML"
    (let [intro-path (str fixture-dir "/doc/introduction.html")
          dom (parse/parse-html-file intro-path)
          title (parse/extract-guide-title dom)]
      (is (= "Introduction" title))))

  (testing "Can extract title from various guide formats"
    (let [guides ["doc/basics-route-syntax.html"
                  "doc/ring-ring-router.html"
                  "doc/misc-faq.html"]
          titles (map (fn [guide]
                        (let [dom (parse/parse-html-file (str fixture-dir "/" guide))]
                          (parse/extract-guide-title dom)))
                      guides)]
      (is (= ["Route Syntax" "Ring Router" "Frequently Asked Questions"] titles)))))

(deftest extract-namespace-name-test
  (testing "Can extract namespace name from API HTML"
    (let [core-dom (parse/parse-html-file core-html-path)
          ring-dom (parse/parse-html-file (str fixture-dir "/api/reitit.ring.html"))]
      (is (= "reitit.core" (parse/extract-namespace-name core-dom)))
      (is (= "reitit.ring" (parse/extract-namespace-name ring-dom))))))

(deftest parse-all-entries-test
  (testing "Full integration test - parse all entries from reitit bundle"
    (let [ctx {:paths {:bundle-dir fixture-dir}}
          result (parse/parse-all-entries ctx)
          entries (get-in result [:docset-data :symbols])
          symbols (filter #(not (contains? #{"Namespace" "Guide"} (:type %))) entries)
          namespaces (filter #(= "Namespace" (:type %)) entries)
          guides (filter #(= "Guide" (:type %)) entries)]

      ;; Check symbols (same as before)
      (is (> (count symbols) 200))
      (is (every? #(contains? % :name) symbols))
      (is (every? #(contains? % :type) symbols))
      (is (every? #(str/includes? (:path %) "#") symbols))

      (testing "Path format is correct for symbols"
        (is (every? #(re-matches #"api/[^/]+\.html#.+" (:path %)) symbols)))

      (testing "Contains expected protocols"
        (let [protocols (filter #(= "Protocol" (:type %)) symbols)]
          (is (= 10 (count protocols)))
          (is (some #(= "Coercion" (:name %)) protocols))
          (is (some #(= "Router" (:name %)) protocols))
          (is (some #(= "Executor" (:name %)) protocols))
          (is (some #(= "IntoInterceptor" (:name %)) protocols))))

      (testing "Contains expected multimethods"
        (let [multimethods (filter #(= "Method" (:type %)) symbols)]
          (is (= 4 (count multimethods)))
          (is (= 2 (count (filter #(= "format-exception" (:name %)) multimethods))))
          ;; format-exception exists in both reitit.dev.pretty and reitit.exception
          (is (some #(and (= "format-exception" (:name %))
                          (str/includes? (:path %) "reitit.exception.html"))
                    multimethods))
          (is (some #(and (= "format-exception" (:name %))
                          (str/includes? (:path %) "reitit.dev.pretty.html"))
                    multimethods))))

      (testing "Default type is Function"
        (let [functions (filter #(= "Function" (:type %)) symbols)]
          (is (> (count functions) 200))
          ;; Check some specific functions
          (is (some #(= "router" (:name %)) functions))
          (is (some #(= "match->path" (:name %)) functions))
          (is (some #(= "*max-compile-depth*" (:name %)) functions))))

      ;; Check namespace entries
      (testing "Contains namespace entries"
        (is (= 36 (count namespaces)))
        (is (every? #(= "Namespace" (:type %)) namespaces))
        (is (every? #(str/starts-with? (:path %) "api/") namespaces))
        (is (every? #(str/ends-with? (:path %) ".html") namespaces))
        (is (some #(= "reitit.core" (:name %)) namespaces))
        (is (some #(= "reitit.ring" (:name %)) namespaces)))

      ;; Check guide entries
      (testing "Contains guide entries"
        (is (= 49 (count guides)))
        (is (every? #(= "Guide" (:type %)) guides))
        (is (every? #(str/starts-with? (:path %) "doc/") guides))
        (is (every? #(str/ends-with? (:path %) ".html") guides))
        (is (some #(= "Introduction" (:name %)) guides))
        (is (some #(= "Route Syntax" (:name %)) guides)))))

  (testing "Full integration test - parse all entries from hiccup bundle"
    (let [ctx {:paths {:bundle-dir "test/fixtures/hiccup-2.0.0"}}
          result (parse/parse-all-entries ctx)
          entries (get-in result [:docset-data :symbols])
          symbols (filter #(not (contains? #{"Namespace" "Guide"} (:type %))) entries)
          namespaces (filter #(= "Namespace" (:type %)) entries)
          guides (filter #(= "Guide" (:type %)) entries)]

      (is (> (count symbols) 50))

      (testing "Contains expected macros"
        (let [macros (filter #(= "Macro" (:type %)) symbols)]
          (is (>= (count macros) 4))
          (is (some #(= "html" (:name %)) macros))
          (is (some #(= "defelem" (:name %)) macros))
          (is (some #(= "defhtml" (:name %)) macros))
          (is (some #(= "build-string" (:name %)) macros))))

      (testing "Contains expected protocols"
        (let [protocols (filter #(= "Protocol" (:type %)) symbols)]
          (is (some #(= "HtmlRenderer" (:name %)) protocols))))

      (testing "Contains expected form helper functions"
        (let [functions (filter #(= "Function" (:type %)) symbols)]
          (is (some #(= "form-to" (:name %)) functions))
          (is (some #(= "check-box" (:name %)) functions))
          (is (some #(= "drop-down" (:name %)) functions))))

      ;; Check namespace entries
      (testing "Contains namespace entries"
        (is (= 9 (count namespaces)))
        (is (every? #(= "Namespace" (:type %)) namespaces))
        (is (some #(= "hiccup.core" (:name %)) namespaces))
        (is (some #(= "hiccup2.core" (:name %)) namespaces)))

      ;; Check guide entries
      (testing "Contains guide entries"
        (is (= 2 (count guides)))
        (is (every? #(= "Guide" (:type %)) guides))
        (is (some #(= "Hiccup" (:name %)) guides))
        (is (some #(= "Syntax" (:name %)) guides))))))

(deftest type-mapping-test
  (testing "Clojure types map correctly to Dash types"
    (is (= "Variable" (get parse/dash-type-map :var)))
    (is (= "Macro" (get parse/dash-type-map :macro)))
    (is (= "Protocol" (get parse/dash-type-map :protocol)))
    (is (= "Method" (get parse/dash-type-map :multimethod)))
    (is (= "Function" parse/default-dash-type))))

(deftest protocol-test
  (testing "Can detect protocols in reitit.interceptor namespace"
    (let [interceptor-path (str fixture-dir "/api/reitit.interceptor.html")
          dom (parse/parse-html-file interceptor-path)
          symbols (parse/extract-symbols dom "api/reitit.interceptor.html")
          protocols (filter #(= "Protocol" (:type %)) symbols)]
      (is (seq protocols))
      (is (some #(= "Executor" (:name %)) protocols))
      (is (some #(= "IntoInterceptor" (:name %)) protocols))
      (testing "Protocol entries have correct format"
        (let [executor (first (filter #(= "Executor" (:name %)) protocols))]
          (is (= {:name "Executor"
                  :type "Protocol"
                  :path "api/reitit.interceptor.html#Executor"}
                 executor)))))))

(deftest multimethod-test
  (testing "Can detect multimethods in reitit.exception namespace"
    (let [exception-path (str fixture-dir "/api/reitit.exception.html")
          dom (parse/parse-html-file exception-path)
          symbols (parse/extract-symbols dom "api/reitit.exception.html")
          multimethods (filter #(= "Method" (:type %)) symbols)]
      (is (seq multimethods))
      (is (some #(= "format-exception" (:name %)) multimethods))
      (testing "Multimethod entries have correct format"
        (let [format-exception (first (filter #(= "format-exception" (:name %)) multimethods))]
          (is (= {:name "format-exception"
                  :type "Method"
                  :path "api/reitit.exception.html#format-exception"}
                 format-exception)))))))

(deftest macro-test
  (testing "Can detect macros in hiccup2.core namespace"
    (let [hiccup-path "test/fixtures/hiccup-2.0.0/api/hiccup2.core.html"
          dom (parse/parse-html-file hiccup-path)
          symbols (parse/extract-symbols dom "api/hiccup2.core.html")
          macros (filter #(= "Macro" (:type %)) symbols)]
      (is (seq macros))
      (is (some #(= "html" (:name %)) macros))
      (testing "Macro entries have correct format"
        (let [html-macro (first (filter #(= "html" (:name %)) macros))]
          (is (= {:name "html"
                  :type "Macro"
                  :path "api/hiccup2.core.html#html"}
                 html-macro)))))))

(comment
  (require '[clojure.pprint :as pp])

  (let [fixture-dir "test/fixtures/reitit-0.9.1"]
    (spit "test/fixtures/reitit.edn"
          (with-out-str
            (-> {:paths {:bundle-dir fixture-dir}}
                (parse/parse-all-entries)
                (pp/pprint)))))

  (let [fixture-dir "test/fixtures/hiccup-2.0.0"]
    (spit "test/fixtures/hiccup.edn"
          (with-out-str
            (-> {:paths {:bundle-dir fixture-dir}}
                (parse/parse-all-entries)
                (pp/pprint)))))

;;
  )
