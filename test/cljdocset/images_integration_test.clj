;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: EUPL-1.2
(ns cljdocset.images-integration-test
  (:require
   [babashka.fs :as fs]
   [cljdocset.images :as images]
   [cljdocset.parse-test] ;; prepare fixtures
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [hickory.core :as hickory]))

(def ^:dynamic *test-server* nil)

(defn mock-server-fixture [f]
  ;; For real integration tests, we'll test with actual remote URLs
  ;; No mock server needed for now
  (f))

(use-fixtures :once mock-server-fixture)

(defn- process-html-inline [html-content images-dir]
  (let [temp-file (fs/create-temp-file {:suffix ".html"})
        temp-dir (fs/parent temp-file)
        _ (spit (str temp-file) html-content)
        result (images/process-html-file
                {:file-path (str temp-file)
                 :images-dir images-dir
                 :documents-dir temp-dir})]
    (fs/delete temp-file)
    result))

(deftest test-process-html-with-tempel-fixture
  (testing "processes Tempel fixture HTML with remote images"
    (fs/with-temp-dir [temp-dir {}]
      (let [images-dir (fs/path temp-dir "images")
            ;; Read the actual Tempel readme.html
            tempel-readme "test/fixtures/tempel-1.0.0-RC1/doc/readme.html"
            html-content (slurp tempel-readme)]

        ;; Check that the HTML contains remote images
        (is (str/includes? html-content "https://github.com/taoensso/tempel"))
        (is (str/includes? html-content "https://img.youtube.com"))
        (is (str/includes? html-content "https://microicon-clone.vercel.app"))

        ;; Process the HTML file
        (let [result (images/process-html-file
                      {:file-path tempel-readme
                       :images-dir (str images-dir)
                       :documents-dir "test/fixtures/tempel-1.0.0-RC1"})
              processed-html (:content result)
              processed-images (:images result)]

          ;; Verify images were found and processed
          (is (pos? (count processed-images)))

          ;; Check specific known images
          (let [microicon-img (some #(when (str/includes? (:url %) "microicon") %) processed-images)
                youtube-img (some #(when (str/includes? (:url %) "youtube") %) processed-images)]
            (is microicon-img "Should find microicon image")
            (is youtube-img "Should find YouTube thumbnail"))

          ;; Verify HTML was rewritten
          (is (not= html-content processed-html))
          (is (str/includes? processed-html "../images/"))

          ;; Verify local images are preserved
          (is (not (str/includes? processed-html "images/images/"))))))))

(deftest test-process-html-with-polylith-fixture
  (testing "processes Polylith fixture HTML with GitHub images"
    (fs/with-temp-dir [temp-dir {}]
      (let [images-dir (fs/path temp-dir "images")
            ;; Read a Polylith doc file with images
            poly-component "test/fixtures/clj-poly-0.2.22/doc/component.html"
            html-content (slurp poly-component)]

        ;; Check that the HTML contains GitHub images
        (is (str/includes? html-content "https://github.com/polyfy/polylith/raw"))

        ;; Process the HTML file
        (let [result (images/process-html-file
                      {:file-path poly-component
                       :images-dir (str images-dir)
                       :documents-dir "test/fixtures/clj-poly-0.2.22"})
              processed-html (:content result)
              processed-images (:images result)]

          ;; Verify images were found
          (is (pos? (count processed-images)))

          ;; Check for GitHub raw images
          (let [github-images (filter #(str/includes? (:url %) "github.com/polyfy") processed-images)]
            (is (pos? (count github-images)) "Should find GitHub images"))

          ;; Verify HTML was rewritten (doc/component.html is 1 level deep, so ../images/)
          (is (not= html-content processed-html))
          (is (str/includes? processed-html "../images/")))))))

(deftest test-download-all-images-pipeline
  (testing "full pipeline processes multiple HTML files"
    (fs/with-temp-dir [temp-dir {}]
      (let [;; Set up test structure
            docs-dir (fs/path temp-dir "Documents")
            _ (fs/create-dirs docs-dir)

            ;; Copy a few test HTML files
            _ (fs/copy "test/fixtures/tempel-1.0.0-RC1/doc/readme.html"
                       (fs/path docs-dir "readme.html"))
            _ (fs/copy "test/fixtures/tempel-1.0.0-RC1/index.html"
                       (fs/path docs-dir "index.html"))

            ;; Create context
            ctx {:paths {:documents-dir (str docs-dir)}}

            ;; Run the pipeline
            result (images/download-all-images ctx)]

        ;; Check that images were processed
        (is (contains? result :images))
        (is (pos? (get-in result [:images :total])))

        ;; Verify images directory was created
        (is (fs/exists? (fs/path docs-dir "images")))

        ;; Check that HTML files were updated
        (let [readme-content (slurp (str (fs/path docs-dir "readme.html")))
              index-content (slurp (str (fs/path docs-dir "index.html")))]
          (is (str/includes? readme-content "images/"))
          (is (str/includes? index-content "images/")))))))

(deftest test-local-image-preservation
  (testing "local images are not downloaded or modified"
    (let [html-with-local "<html><body>
                           <img src=\"../assets/logo.png\">
                           <img src=\"images/existing.jpg\">
                           <img src=\"/absolute/path.gif\">
                           </body></html>"
          hickory-dom (-> html-with-local hickory/parse hickory/as-hickory)
          urls (images/find-image-references hickory-dom)
          classified (map images/classify-image-url urls)]

      ;; All should be classified as local
      (is (every? #(= :local (:type %)) classified))

      ;; Process with empty images dir
      (fs/with-temp-dir [temp-dir {}]
        (let [result (process-html-inline html-with-local (str temp-dir))]
          ;; No images should be downloaded
          (is (empty? (:images result)))
          ;; HTML should remain unchanged
          (is (= html-with-local (:content result))))))))

(deftest test-error-handling
  (testing "handles network failures gracefully"
    (fs/with-temp-dir [temp-dir {}]
      (let [html-with-bad-url "<html><body>
                               <img src=\"https://nonexistent.invalid/image.png\">
                               </body></html>"
            ;; Create a temp file for testing
            temp-file (fs/path temp-dir "test.html")
            _ (spit (str temp-file) html-with-bad-url)

            result (images/process-html-file
                    {:file-path (str temp-file)
                     :images-dir (str (fs/path temp-dir "images"))
                     :documents-dir temp-dir})]

        ;; Should have one failed image
        (is (= 1 (count (:images result))))
        (is (false? (-> result :images first :success?)))
        (is (-> result :images first :error))

        ;; HTML should remain unchanged since download failed
        (is (= html-with-bad-url (:content result)))))))