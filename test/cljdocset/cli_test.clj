;; Copyright © 2025 Casey Link <casey@outskirtslabs.com>
;; SPDX-License-Identifier: EUPL-1.2
(ns cljdocset.cli-test
  (:require
   [cljdocset.cli :as cli]
   [clojure.test :refer [deftest is testing]]))

(deftest test-dispatch-table
  (testing "dispatch table contains build command"
    (let [build-entry (first (filter #(= ["build"] (:cmds %)) cli/dispatch-table))]
      (is (some? build-entry))
      (is (= cli/build-spec (:spec build-entry))))))

(deftest test-build-spec
  (testing "build spec contains required options"
    (is (contains? cli/build-spec :output-dir))
    (is (contains? cli/build-spec :build-dir))
    (is (contains? cli/build-spec :icon-path))
    (is (contains? cli/build-spec :help))))