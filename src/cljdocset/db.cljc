;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: EUPL-1.2
(ns cljdocset.db
  "SQLite database operations for docset index creation"
  (:require
   [babashka.fs :as fs]
   [cljdocset.util :as util]
   [next.jdbc :as jdbc]
   #?@(:bb []
       :clj [[next.jdbc.sql :as sql]])))

(defn db-spec
  "Create JDBC connection spec for SQLite database."
  [db-file]
  {:dbtype "sqlite"
   :dbname (str db-file)})

(defn create-database
  "Create the SQLite database file if it doesn't exist.
  Takes the database file path as input.
  Returns the file path."
  [db-file]
  (when-not (fs/exists? db-file)
    (let [parent-dir (fs/parent db-file)]
      (when-not (fs/exists? parent-dir)
        (fs/create-dirs parent-dir))
      (fs/create-file db-file)))
  db-file)

(defn create-schema
  "Create the searchIndex table required by Dash/Zeal.
  Takes a database connection."
  [db-conn]
  (jdbc/execute! db-conn
                 ["CREATE TABLE IF NOT EXISTS searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT)"]))

(defn create-unique-index
  "Create unique index on (name, type, path) to prevent duplicates.
  Takes a database connection."
  [db-conn]
  (jdbc/execute! db-conn
                 ["CREATE UNIQUE INDEX IF NOT EXISTS anchor ON searchIndex (name, type, path)"]))

#?(:bb
   (defmacro with-connection
     [[conn-sym db-spec] & body]
     `(let [~conn-sym ~db-spec]
        ~@body))

   :clj
   (defmacro with-connection
     "Execute body with a database connection bound to conn-sym.
     Ensures connection is properly closed after use."
     [[conn-sym db-spec] & body]
     `(with-open [~conn-sym (jdbc/get-connection ~db-spec)]
        ~@body)))

(defn insert-symbol!
  "Insert a single symbol into the searchIndex table.
  Takes db-conn, name, type, and path."
  [db-conn symbol-name symbol-type symbol-path]
  #?(:bb
     (jdbc/execute! db-conn
                    ["INSERT OR IGNORE INTO searchIndex (name, type, path) VALUES (?, ?, ?)"
                     symbol-name symbol-type symbol-path])
     :clj
     (sql/insert! db-conn :searchIndex {:name symbol-name :type symbol-type :path symbol-path})))

(defn insert-symbols!
  "Batch insert multiple symbols into the searchIndex table.
  Takes db-conn and a sequence of symbol maps with :name, :type, and :path."
  [db-conn symbols]
  (when (seq symbols)
    #?(:bb
       ;; Use individual inserts for Babashka compatibility
       (do
         (doseq [{:keys [name type path]} symbols]
           (jdbc/execute! db-conn
                          ["INSERT OR IGNORE INTO searchIndex (name, type, path) VALUES (?, ?, ?)"
                           name type path]))
         nil)

       :clj
       ;; Use batch operations with transactions in Clojure
       (jdbc/with-transaction [tx db-conn]
         (let [sql "INSERT OR IGNORE INTO searchIndex (name, type, path) VALUES (?, ?, ?)"
               params (mapv (fn [{:keys [name type path]}]
                              [name type path])
                            symbols)]
           (jdbc/execute-batch! tx sql params {:batch-size 100}))))))

(defn symbol-exists?
  "Check if a symbol is already indexed.
  Takes db-conn, name, type, and path."
  [db-conn symbol-name symbol-type symbol-path]
  (let [result (jdbc/execute-one! db-conn
                                  ["SELECT 1 FROM searchIndex WHERE name = ? AND type = ? AND path = ?"
                                   symbol-name symbol-type symbol-path])]
    (boolean result)))

(defn count-symbols
  "Return total count of indexed symbols in the database."
  [db-conn]
  (let [result (jdbc/execute-one! db-conn ["SELECT COUNT(*) as count FROM searchIndex"])]
    (:count result 0)))

(defn initialize-db
  "Initialize the SQLite database for the docset.
  Creates the database file, schema, and indexes.
  Returns ctx unchanged."
  [{:keys [paths] :as ctx}]
  (let [db-file (:db-file paths)]
    (assert db-file "Database file path must be provided in context")
    (util/info "Initializing database at" db-file)
    (create-database db-file)
    (let [spec (db-spec db-file)]
      (with-connection [conn spec]
        (create-schema conn)
        (create-unique-index conn))
      (util/debug "Database initialized successfully"))
    ctx))

(defn index-symbols
  "Add symbols from context to the database index.
  Expects ctx with :docset-data containing :symbols and :paths containing :db-file.
  Returns updated ctx."
  [{:keys [paths docset-data] :as ctx}]
  (let [db-file (:db-file paths)
        symbols (:symbols docset-data)]
    (assert db-file "Database file path must be provided in context")
    (assert (or (nil? symbols) (sequential? symbols)) "Symbols must be a sequence")
    (if (seq symbols)
      (do
        (util/info "Indexing" (count symbols) "symbols")
        (let [spec (db-spec db-file)]
          (with-connection [conn spec]
            (insert-symbols! conn symbols)
            (let [total (count-symbols conn)]
              (util/debug "Total symbols in index:" total))))
        ctx)
      (do
        (util/warn "No symbols to index")
        ctx))))