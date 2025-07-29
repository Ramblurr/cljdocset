;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: EUPL-1.2
(ns cljdocset.db-test
  "Tests for SQLite database operations"
  (:require
   [babashka.fs :as fs]
   [cljdocset.db :as db]
   [cljdocset.util]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [next.jdbc :as jdbc]))

(def ^:dynamic *temp-db-file* nil)

(defn temp-db-fixture
  "Creates a temporary database for testing and cleans up after."
  [f]
  (fs/with-temp-dir [temp-dir {}]
    (let [db-file (fs/path temp-dir "test.dsidx")]
      (binding [*temp-db-file* (str db-file)
                cljdocset.util/*verbose* false]
        (f)))))

(use-fixtures :each temp-db-fixture)

(deftest test-db-spec
  (testing "db-spec creates correct JDBC connection spec"
    (let [spec (db/db-spec "/path/to/db.sqlite")]
      (is (= {:dbtype "sqlite"
              :dbname "/path/to/db.sqlite"}
             spec)))))

(deftest test-create-database
  (testing "create-database creates SQLite file and parent directories"
    (let [temp-dir (fs/create-temp-dir)
          test-dir (fs/path temp-dir "nested" "dir")
          test-db (str (fs/path test-dir "test.db"))]
      (is (not (fs/exists? test-dir)))
      (db/create-database test-db)
      (is (fs/exists? test-db))
      (is (fs/regular-file? test-db)))))

(deftest test-insert-symbols-batch
  (testing "insert-symbols! batch inserts multiple symbols"
    (db/create-database *temp-db-file*)
    (let [spec (db/db-spec *temp-db-file*)
          symbols [{:name "func1" :type "Function" :path "api/a.html#func1"}
                   {:name "func2" :type "Macro" :path "api/b.html#func2"}
                   {:name "func3" :type "Variable" :path "api/c.html#func3"}]]
      (db/with-connection [conn spec]
        (db/create-schema conn)
        (db/create-unique-index conn)
        (db/insert-symbols! conn symbols)
        (let [count (db/count-symbols conn)]
          (is (= 3 count))))))

  (testing "insert-symbols! handles empty sequence"
    (let [temp-db (str (fs/path (fs/create-temp-dir) "test-batch-empty.dsidx"))]
      (db/create-database temp-db)
      (let [spec (db/db-spec temp-db)]
        (db/with-connection [conn spec]
          (db/create-schema conn)
          (db/create-unique-index conn)
          (db/insert-symbols! conn [])
          (is (= 0 (db/count-symbols conn))))))))

(deftest test-duplicate-handling
  (testing "unique constraint prevents duplicate entries"
    (db/create-database *temp-db-file*)
    (let [spec (db/db-spec *temp-db-file*)]
      (db/with-connection [conn spec]
        (db/create-schema conn)
        (db/create-unique-index conn)
        (db/insert-symbol! conn "my-func" "Function" "api/core.html#my-func")
        (db/insert-symbols! conn [{:name "my-func" :type "Function" :path "api/core.html#my-func"}
                                  {:name "other-func" :type "Function" :path "api/core.html#other-func"}])
        (is (= 2 (db/count-symbols conn)))))))

(deftest test-symbol-exists
  (testing "symbol-exists? correctly identifies existing symbols"
    (db/create-database *temp-db-file*)
    (let [spec (db/db-spec *temp-db-file*)]
      (db/with-connection [conn spec]
        (db/create-schema conn)
        (db/create-unique-index conn)
        (is (false? (db/symbol-exists? conn "my-func" "Function" "api/core.html#my-func")))
        (db/insert-symbol! conn "my-func" "Function" "api/core.html#my-func")
        (is (true? (db/symbol-exists? conn "my-func" "Function" "api/core.html#my-func")))
        (is (false? (db/symbol-exists? conn "other-func" "Function" "api/core.html#other-func")))))))

(deftest test-count-symbols
  (testing "count-symbols returns correct count"
    (db/create-database *temp-db-file*)
    (let [spec (db/db-spec *temp-db-file*)]
      (db/with-connection [conn spec]
        (db/create-schema conn)
        (is (= 0 (db/count-symbols conn)))
        (db/insert-symbol! conn "func1" "Function" "api/a.html#func1")
        (is (= 1 (db/count-symbols conn)))
        (db/insert-symbols! conn [{:name "func2" :type "Function" :path "api/b.html#func2"}
                                  {:name "func3" :type "Macro" :path "api/c.html#func3"}])
        (is (= 3 (db/count-symbols conn)))))))

(deftest test-initialize-db
  (testing "initialize-db creates and sets up database from context"
    (let [ctx {:paths {:db-file *temp-db-file*}}]
      (is (not (fs/exists? *temp-db-file*)))
      (let [result (db/initialize-db ctx)]
        (is (= ctx result))
        (is (fs/exists? *temp-db-file*))
        (let [spec (db/db-spec *temp-db-file*)]
          (db/with-connection [conn spec]
            (let [tables (jdbc/execute! conn
                                        ["SELECT name FROM sqlite_master WHERE type='table' AND name='searchIndex'"])
                  indexes (jdbc/execute! conn
                                         ["SELECT name FROM sqlite_master WHERE type='index' AND name='anchor'"])]
              (is (= 1 (count tables)))
              (is (= 1 (count indexes))))))))))

(deftest test-index-symbols
  (testing "index-symbols adds symbols from context to database"
    (let [symbols [{:name "router" :type "Function" :path "api/reitit.core.html#router"}
                   {:name "routes" :type "Function" :path "api/reitit.core.html#routes"}
                   {:name "match-by-path" :type "Function" :path "api/reitit.core.html#match-by-path"}]
          ctx {:paths {:db-file *temp-db-file*}
               :docset-data {:symbols symbols}}]
      (db/initialize-db ctx)
      (let [result (db/index-symbols ctx)]
        (is (= ctx result))
        (let [spec (db/db-spec *temp-db-file*)]
          (db/with-connection [conn spec]
            (is (= 3 (db/count-symbols conn))))))))

  (testing "index-symbols handles nil symbols gracefully"
    (let [temp-db (str (fs/path (fs/create-temp-dir) "test-nil.dsidx"))
          ctx {:paths {:db-file temp-db}
               :docset-data {:symbols nil}}]
      (db/initialize-db ctx)
      (let [result (db/index-symbols ctx)]
        (is (= ctx result))
        (let [spec (db/db-spec temp-db)]
          (db/with-connection [conn spec]
            (is (= 0 (db/count-symbols conn))))))))

  (testing "index-symbols handles empty symbols list"
    (let [temp-db (str (fs/path (fs/create-temp-dir) "test-empty.dsidx"))
          ctx {:paths {:db-file temp-db}
               :docset-data {:symbols []}}]
      (db/initialize-db ctx)
      (let [result (db/index-symbols ctx)]
        (is (= ctx result))
        (let [spec (db/db-spec temp-db)]
          (db/with-connection [conn spec]
            (is (= 0 (db/count-symbols conn)))))))))