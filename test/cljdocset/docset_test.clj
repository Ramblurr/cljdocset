(ns cljdocset.docset-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [cljdocset.docset :as docset]
   [babashka.fs :as fs]
   [clojure.string :as str]))

(deftest resolve-docset-paths-test
  (testing "resolves all docset paths with build dir"
    (fs/with-temp-dir [temp-dir {}]
      (let [ctx {:cli-opts {}
                 :lib-info {:docset-name "test-lib-1.0.0"}
                 :paths {:build-dir (str temp-dir)}}
            result (docset/resolve-docset-paths ctx)
            paths (:paths result)]
        (is (str/ends-with? (str (:output-dir paths)) "out"))
        (is (str/ends-with? (str (:docset-dir paths)) "test-lib-1.0.0.docset"))
        (is (str/ends-with? (str (:contents-dir paths)) "test-lib-1.0.0.docset/Contents"))
        (is (str/ends-with? (str (:resources-dir paths)) "Contents/Resources"))
        (is (str/ends-with? (str (:documents-dir paths)) "Resources/Documents"))
        (is (str/ends-with? (str (:db-file paths)) "Resources/docSet.dsidx"))
        (is (str/ends-with? (str (:plist-file paths)) "Contents/Info.plist"))
        (is (str/ends-with? (str (:archive-file paths)) "test-lib-1.0.0.tgz")))))

  (testing "resolves paths with different build dir"
    (fs/with-temp-dir [temp-dir {}]
      (let [ctx {:cli-opts {}
                 :lib-info {:docset-name "test-lib-1.0.0"}
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
                            :docset-name "test-lib-1.0.0"
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
          (is (str/includes? content "<string>test-lib 1.0.0</string>"))
          (is (str/includes? content "<key>isDashDocset</key>"))
          (is (str/includes? content "<true />")))))))

(deftest add-icon-test
  (testing "copies icon when provided"
    (fs/with-temp-dir [temp-dir {}]
      (let [icon-file (fs/path temp-dir "icon.png")
            docset-dir (fs/path temp-dir "test.docset")]
        (fs/create-dirs docset-dir)
        (spit (str icon-file) "fake-png-data")

        (let [ctx {:cli-opts {}
                   :paths {:docset-dir (str docset-dir)
                           :input-icon-path (str icon-file)
                           :icon-path (str (fs/path docset-dir "icon@2x.png"))}}]
          (docset/add-icon ctx)
          (is (fs/exists? (fs/path docset-dir "icon@2x.png")))
          (is (= "fake-png-data" (slurp (str (fs/path docset-dir "icon@2x.png")))))))))

  (testing "no-op when input-icon-path is nil"
    (fs/with-temp-dir [temp-dir {}]
      (let [docset-dir (fs/path temp-dir "test.docset")]
        (fs/create-dirs docset-dir)
        (let [ctx {:cli-opts {}
                   :paths {:docset-dir (str docset-dir)}}]
          (docset/add-icon ctx)
          (is (empty? (fs/list-dir docset-dir))))))))

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