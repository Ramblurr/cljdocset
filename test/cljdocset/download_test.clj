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
    (let [ctx {::download/temp-dir? true
               ::download/paths {:build-dir "/tmp/test-cleanup"}}]
      (is (= ctx (download/setup-cleanup ctx))))))

(deftest test-extract-bundle-with-fixture
  (testing "extracts test fixture successfully"
    (let [fixture-path "test/fixtures/reitit-0.9.1.zip"
          temp-dir (fs/create-temp-dir)
          ctx {::download/paths {:zip-file fixture-path
                                 :build-dir (str temp-dir)}}]
      (try
        (when (fs/exists? fixture-path)
          (let [result (download/extract-bundle ctx)]
            (is (contains? (::download/paths result) :bundle-dir))
            (let [bundle-dir (get-in result [::download/paths :bundle-dir])]
              (is (fs/exists? bundle-dir))
              (is (fs/exists? (fs/path bundle-dir "index.html")))
              (is (fs/exists? (fs/path bundle-dir "api"))))))
        (finally
          (fs/delete-tree temp-dir))))))
