(ns cljdocset.download-test
  (:require
   [babashka.fs :as fs]
   [cljdocset.download :as download]
   [clojure.test :refer [deftest is testing]]))

(deftest test-build-download-url
  (testing "builds correct download URL"
    (is (= "https://cljdoc.org/download/metosin/reitit/0.9.1"
           (download/build-download-url {:group-id "metosin"
                                         :artifact-id "reitit"
                                         :version "0.9.1"})))
    (is (= "https://cljdoc.org/download/clj-commons/hickory/0.7.3"
           (download/build-download-url {:group-id "clj-commons"
                                         :artifact-id "hickory"
                                         :version "0.7.3"})))))

(deftest test-setup-cleanup
  (testing "registers cleanup for temp directories"
    (let [ctx {:temp-dir? true
               :paths {:build-dir "/tmp/test-cleanup"}}]
      (is (= ctx (download/setup-cleanup ctx))))))

(deftest test-extract-bundle-with-fixture
  (testing "extracts test fixture successfully"
    (fs/with-temp-dir [temp-dir {}]
      (let [fixture-path "test/fixtures/reitit-0.9.1.zip"
            ctx {:paths {:zip-path fixture-path
                         :build-dir (str temp-dir)}}]
        (when (fs/exists? fixture-path)
          (let [result (download/extract-bundle ctx)]
            (is (contains? (:paths result) :bundle-dir))
            (let [bundle-dir (get-in result [:paths :bundle-dir])]
              (is (fs/exists? bundle-dir))
              (is (fs/exists? (fs/path bundle-dir "index.html")))
              (is (fs/exists? (fs/path bundle-dir "api"))))))))))