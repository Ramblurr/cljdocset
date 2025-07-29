(ns cljdocset.docset-test
  (:require
   [babashka.fs :as fs]
   [cljdocset.docset :as docset]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(deftest resolve-docset-paths-test
  (testing "resolves all docset paths with build dir"
    (fs/with-temp-dir [temp-dir {}]
      (let [ctx {:cli-opts {}
                 :lib-info {:docset-name "test-group-test-lib-1.0.0"}
                 :paths {:build-dir (str temp-dir)}}
            result (docset/resolve-docset-paths ctx)
            paths (:paths result)]
        (is (str/ends-with? (str (:output-dir paths)) "out"))
        (is (str/ends-with? (str (:docset-dir paths)) "test-group-test-lib-1.0.0.docset"))
        (is (str/ends-with? (str (:contents-dir paths)) "test-group-test-lib-1.0.0.docset/Contents"))
        (is (str/ends-with? (str (:resources-dir paths)) "Contents/Resources"))
        (is (str/ends-with? (str (:documents-dir paths)) "Resources/Documents"))
        (is (str/ends-with? (str (:db-file paths)) "Resources/docSet.dsidx"))
        (is (str/ends-with? (str (:plist-file paths)) "Contents/Info.plist"))
        (is (str/ends-with? (str (:archive-file paths)) "test-group-test-lib-1.0.0.tgz")))))

  (testing "resolves paths with different build dir"
    (fs/with-temp-dir [temp-dir {}]
      (let [ctx {:cli-opts {}
                 :lib-info {:docset-name "test-group-test-lib-1.0.0"}
                 :paths {:build-dir (str temp-dir)}}
            result (docset/resolve-docset-paths ctx)
            paths (:paths result)]
        (is (str/starts-with? (str (:output-dir paths)) (str temp-dir)))
        (is (str/ends-with? (str (:output-dir paths)) "/out"))))))

(deftest create-docset-structure-test
  (testing "creates directory structure"
    (fs/with-temp-dir [temp-dir {}]
      (let [ctx {:paths {:documents-dir (fs/path temp-dir "test.docset" "Contents" "Resources" "Documents")}}]
        (docset/create-docset-structure ctx)
        (is (fs/exists? (:documents-dir (:paths ctx))))
        (is (fs/directory? (:documents-dir (:paths ctx))))))))

(deftest copy-assets-test
  (testing "copies bundle contents to documents directory"
    (fs/with-temp-dir [temp-dir {}]
      (let [source-dir (fs/path temp-dir "source")
            docs-dir (fs/path temp-dir "docs")
            test-file (fs/path source-dir "test.html")]
        (fs/create-dirs source-dir)
        (fs/create-dirs docs-dir)
        (spit (str test-file) "<html>test</html>")

        (let [ctx {:paths {:bundle-dir (str source-dir)
                           :documents-dir (str docs-dir)}}]
          (docset/copy-assets ctx)
          (is (fs/exists? (fs/path docs-dir "test.html")))
          (is (= "<html>test</html>" (slurp (str (fs/path docs-dir "test.html"))))))))))

(deftest create-info-plist-test
  (testing "creates valid Info.plist XML"
    (fs/with-temp-dir [temp-dir {}]
      (let [plist-file (fs/path temp-dir "Info.plist")
            ctx {:lib-info {:bundle-id "test-bundle"
                            :docset-name "test-group-test-lib-1.0.0"
                            :group-id "test-group"
                            :artifact-id "test-lib"
                            :version "1.0.0"}
                 :paths {:plist-file (str plist-file)}}]
        (docset/create-info-plist ctx)
        (is (fs/exists? plist-file))
        (let [content (slurp (str plist-file))]
          (is (str/includes? content "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"))
          (is (str/includes? content "<key>CFBundleIdentifier</key>"))
          (is (str/includes? content "<string>test-bundle</string>"))
          (is (str/includes? content "<key>CFBundleName</key>"))
          (is (str/includes? content "<string>test-group/test-lib 1.0.0</string>"))
          (is (str/includes? content "<key>isDashDocset</key>"))
          (is (str/includes? content "<true />")))))))

(deftest add-icon-test
  (testing "copies user icon when provided"
    (fs/with-temp-dir [temp-dir {}]
      (let [icon-file (fs/path temp-dir "icon.png")
            docset-dir (fs/path temp-dir "test.docset")]
        (fs/create-dirs docset-dir)
        (spit (str icon-file) "fake-png-data")

        (let [ctx {:cli-opts {}
                   :paths {:docset-dir (str docset-dir)
                           :icon-paths [[icon-file (fs/path docset-dir "icon.png")]]}}]
          (docset/add-icon ctx)
          (is (fs/exists? (fs/path docset-dir "icon.png")))
          (is (= "fake-png-data" (slurp (str (fs/path docset-dir "icon.png")))))))))

  (testing "uses default icons when no user icon provided"
    (fs/with-temp-dir [temp-dir {}]
      (let [docset-dir (fs/path temp-dir "test.docset")]
        (fs/create-dirs docset-dir)
        (let [ctx {:cli-opts {}
                   :paths {:docset-dir (str docset-dir)
                           :icon-paths [[(io/resource "cljdoc.png") (fs/path docset-dir "icon.png")]
                                        [(io/resource "cljdoc@2x.png") (fs/path docset-dir "icon@2x.png")]]}}]
          (docset/add-icon ctx)
          ;; Check that default icons were copied
          (is (fs/exists? (fs/path docset-dir "icon.png")))
          (is (fs/exists? (fs/path docset-dir "icon@2x.png")))
          ;; Check that the files contain actual icon data (not empty)
          (is (> (fs/size (fs/path docset-dir "icon.png")) 0))
          (is (> (fs/size (fs/path docset-dir "icon@2x.png")) 0)))))))

(deftest archive-docset-test
  (testing "creates tar.gz archive"
    (fs/with-temp-dir [temp-dir {}]
      (let [docset-dir (fs/path temp-dir "test.docset")
            archive-file (fs/path temp-dir "test.tgz")
            test-file (fs/path docset-dir "test.txt")]
        (fs/create-dirs docset-dir)
        (spit (str test-file) "test content")

        (let [ctx {:paths {:docset-dir (str docset-dir)
                           :archive-file (str archive-file)}}
              result (docset/archive-docset ctx)]
          (is (fs/exists? archive-file))
          (is (pos? (fs/size archive-file)))
          (is (= (str archive-file) (:archive-path result))))))))