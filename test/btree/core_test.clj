(ns btree.core-test
  (:require [clojure.test :refer :all]
            [btree.core :as btree]))

(deftest creation
  (let [tr (btree/->BTree 3)]
    (testing "starts out empty"
      (is (empty? tr)))))

(deftest insertion
  (let [tr (-> (btree/create 3) (btree/insert 1 "hi"))]
    (testing "changes empty status"
      (is (not (empty? tr))))
    (testing "inserts the element"
      (is (= (btree/value tr 1) "hi")))
    (testing "keeps the keys in order")
    (testing "creates new nodes to keep the tree balanced")))
