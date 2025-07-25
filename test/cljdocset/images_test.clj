(ns cljdocset.images-test
  (:require
   [cljdocset.images :as images]
   [clojure.test :refer [deftest is testing]]
   [hickory.core :as hickory]))

(deftest test-classify-image-url
  (testing "Remote URLs"
    (is (= {:type :remote :url "https://example.com/image.png"}
           (images/classify-image-url "https://example.com/image.png")))
    (is (= {:type :remote :url "http://example.com/image.png"}
           (images/classify-image-url "http://example.com/image.png"))))

  (testing "Data URIs"
    (is (= {:type :data-uri :url "data:image/png;base64,iVBORw0KGgoAAAANS"}
           (images/classify-image-url "data:image/png;base64,iVBORw0KGgoAAAANS"))))

  (testing "Local URLs"
    (is (= {:type :local :url "images/logo.png"}
           (images/classify-image-url "images/logo.png")))
    (is (= {:type :local :url "../assets/icon.svg"}
           (images/classify-image-url "../assets/icon.svg")))
    (is (= {:type :local :url "/absolute/path/image.jpg"}
           (images/classify-image-url "/absolute/path/image.jpg")))))

(deftest test-find-image-references
  (testing "Extracts image URLs from HTML"
    (let [html "<html><body>
                  <img src=\"https://example.com/1.png\">
                  <img src='local.jpg'>
                  <p>Some text</p>
                  <img src=\"data:image/gif;base64,R0lGOD\">
                </body></html>"
          hickory-dom (-> html hickory/parse hickory/as-hickory)
          urls (images/find-image-references hickory-dom)]
      (is (= ["https://example.com/1.png" "local.jpg" "data:image/gif;base64,R0lGOD"]
             urls))))

  (testing "Returns empty sequence for HTML without images"
    (let [html "<html><body><p>No images here</p></body></html>"
          hickory-dom (-> html hickory/parse hickory/as-hickory)
          urls (images/find-image-references hickory-dom)]
      (is (empty? urls)))))

(deftest test-content-hash
  (testing "Generates consistent SHA-256 hash"
    (let [content (.getBytes "test content")
          hash1 (images/content-hash content)
          hash2 (images/content-hash content)]
      (is (= hash1 hash2))
      (is (= 64 (count hash1))) ; SHA-256 produces 64 hex chars
      (is (re-matches #"[0-9a-f]+" hash1))))

  (testing "Different content produces different hashes"
    (let [content1 (.getBytes "content 1")
          content2 (.getBytes "content 2")
          hash1 (images/content-hash content1)
          hash2 (images/content-hash content2)]
      (is (not= hash1 hash2)))))

(deftest test-determine-image-extension
  (testing "Extension from content-type"
    (is (= ".png" (images/determine-image-extension "image/png" "whatever.txt")))
    (is (= ".jpg" (images/determine-image-extension "image/jpeg" "whatever.txt")))
    (is (= ".svg" (images/determine-image-extension "image/svg+xml" "whatever.txt")))
    (is (= ".webp" (images/determine-image-extension "image/webp" "whatever.txt"))))

  (testing "Extension from content-type with parameters"
    (is (= ".svg" (images/determine-image-extension "image/svg+xml;charset=utf-8" "whatever.txt")))
    (is (= ".png" (images/determine-image-extension "image/png; charset=utf-8" "whatever.txt"))))

  (testing "Extension from URL when content-type is nil"
    (is (= ".png" (images/determine-image-extension nil "image.png")))
    (is (= ".jpg" (images/determine-image-extension nil "photo.jpg")))
    (is (= ".jpg" (images/determine-image-extension nil "photo.JPEG"))) ; Case insensitive
    (is (= ".gif" (images/determine-image-extension nil "animation.gif"))))

  (testing "Returns nil for unknown extensions"
    (is (nil? (images/determine-image-extension nil "file.unknown")))
    (is (nil? (images/determine-image-extension "application/octet-stream" "file")))))

(deftest test-normalize-content-type
  (testing "Strips parameters from content-type"
    (is (= "image/svg+xml" (images/normalize-content-type "image/svg+xml;charset=utf-8")))
    (is (= "image/png" (images/normalize-content-type "image/png; charset=utf-8; boundary=something")))
    (is (= "image/jpeg" (images/normalize-content-type "image/jpeg"))))

  (testing "Handles nil and empty strings"
    (is (nil? (images/normalize-content-type nil)))
    (is (= "" (images/normalize-content-type ""))))

  (testing "Trims whitespace"
    (is (= "image/png" (images/normalize-content-type " image/png ; charset=utf-8")))))

(deftest test-calculate-relative-images-path
  (testing "Calculates correct relative paths for different depths"
    (let [documents-dir "/tmp/docs"]
      (is (= "images/" (images/calculate-relative-images-path "/tmp/docs/index.html" documents-dir)))
      (is (= "../images/" (images/calculate-relative-images-path "/tmp/docs/api/clojure.core.html" documents-dir)))
      (is (= "../../images/" (images/calculate-relative-images-path "/tmp/docs/doc/advanced/guide.html" documents-dir)))))

  (testing "Handles root level files correctly"
    (let [documents-dir "/tmp/docs"]
      (is (= "images/" (images/calculate-relative-images-path "/tmp/docs/readme.html" documents-dir))))))

(deftest test-extract-filename-from-url
  (testing "Extracts filename from various URL formats"
    (is (= "logo" (images/extract-filename-from-url "https://example.com/logo.png")))
    (is (= "my-image" (images/extract-filename-from-url "https://example.com/path/to/my-image.jpg")))
    (is (= "file" (images/extract-filename-from-url "http://example.com/file.svg?v=123")))
    (is (= "image.test" (images/extract-filename-from-url "https://example.com/image.test.png"))))

  (testing "Returns nil for invalid URLs"
    (is (nil? (images/extract-filename-from-url "not-a-url")))
    (is (nil? (images/extract-filename-from-url "")))
    (is (nil? (images/extract-filename-from-url "https://example.com/")))))

(deftest test-build-image-mapping
  (testing "Builds URL to local path mapping"
    (let [entries [{:url "https://example.com/1.png" :local-path "../images/abc123.png"}
                   {:url "https://example.com/2.jpg" :local-path "../images/def456.jpg"}]
          mapping (images/build-image-mapping entries)]
      (is (= {"https://example.com/1.png" "../images/abc123.png"
              "https://example.com/2.jpg" "../images/def456.jpg"}
             mapping))))

  (testing "Empty input produces empty mapping"
    (is (= {} (images/build-image-mapping [])))))

(deftest test-rewrite-image-urls
  (testing "Rewrites URLs with double quotes"
    (let [html "<img src=\"https://example.com/old.png\"> <img src=\"local.jpg\">"
          mapping {"https://example.com/old.png" "../images/new.png"}
          result (images/rewrite-image-urls html mapping)]
      (is (= "<img src=\"../images/new.png\"> <img src=\"local.jpg\">" result))))

  (testing "Rewrites URLs with single quotes"
    (let [html "<img src='https://example.com/old.png'>"
          mapping {"https://example.com/old.png" "../images/new.png"}
          result (images/rewrite-image-urls html mapping)]
      (is (= "<img src='../images/new.png'>" result))))

  (testing "Handles multiple replacements"
    (let [html "<img src=\"https://a.com/1.png\"> <img src=\"https://b.com/2.jpg\">"
          mapping {"https://a.com/1.png" "../images/aaa.png"
                   "https://b.com/2.jpg" "../images/bbb.jpg"}
          result (images/rewrite-image-urls html mapping)]
      (is (= "<img src=\"../images/aaa.png\"> <img src=\"../images/bbb.jpg\">" result))))

  (testing "Preserves URLs not in mapping"
    (let [html "<img src=\"https://example.com/old.png\"> <img src=\"other.jpg\">"
          mapping {"https://different.com/image.png" "../images/new.png"}
          result (images/rewrite-image-urls html mapping)]
      (is (= html result)))))