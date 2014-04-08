(ns btree.core-test
  (:require [clojure.test :refer :all]
            [btree.core :as btree]))

(defn testbt []
  (let [lch (btree/->Node 3
                          [{:val 1   :lch (atom nil) :rch (atom nil)}
                           {:val nil :lch (atom nil) :rch (atom nil)}]
                          (atom nil))
        rch (btree/->Node 3
                          [{:val 8   :lch (atom nil) :rch (atom nil)}
                           {:val nil :lch (atom nil) :rch (atom nil)}]
                          (atom nil))
        parent (btree/->Node 3
                             [{:val 4   :lch (atom lch) :rch (atom nil)}
                              {:val nil :lch (atom rch) :rch (atom nil)}]
                             (atom nil))]
    (reset! (.parent lch) parent)
    (reset! (.parent rch) parent)
    parent))

(comment
  (deftest height-virtual-property
    (is (= (btree/height (btree/btree 3)) 1))
    (is (= (btree/height (testbt)) 2))))

(deftest children-virtual-property
  (let [children (btree/children (testbt))]
    (is (= (map :val (.keys (first children)))  [1 nil]))
    (is (= (map :val (.keys (second children))) [8 nil]))
    (is (nil? (last children)))
    (is (= (count children) 3))))

(deftest creation
  (testing "creating without keys"
    (let [subject (btree/btree 3)]
      (is (= 3 (.order subject)))
      (is (= [{:val nil :lch nil :rch nil}
              {:val nil :lch nil :rch nil}]
             (.keys subject)))
      (is (= nil @(.parent subject)))))
  (testing "creating with keys"
    (let [subject (btree/btree 3 4 20)]
      (is (= 3 (.order subject)))
      (is (= [{:val 4  :lch nil :rch nil}
              {:val 20 :lch nil :rch nil}]
             (.keys subject)))
      (is (= nil @(.parent subject))))))

(comment
  (deftest insertion
    (let [bt1 (conj (testbt) 5)]
      (testing "inserts elements in the correct nodes"
        (is (= (.keys bt1) [4 nil]))
        (is (= (.keys @(first (.ch bt1))) [1 nil]))
        (is (= (.keys @(second (.ch bt1))) [5 8]))
        (is (nil? @(last (.ch bt1))))
        (let [bt2 (conj bt1 20)]
          (is (= (.keys bt2) [4 20]))
          (is (= (.keys @(first (.ch bt2))) [1 nil]))
          (is (= (.keys @(second (.ch bt2))) [5 8]))
          (is (nil? @(last (.ch bt2))))
          (let [bt3 (conj bt2 50)]
            (is (= (.keys bt3) [4 20]))
            (is (= (.keys @(first (.ch bt3))) [1 nil]))
            (is (= (.keys @(second (.ch bt3))) [5 8]))
            (is (= (.keys @(last (.ch bt3))) [50 nil]))
            (let [bt4 (conj bt3 10)]
              (is (= (.keys bt4) [4 20]))
              (is (= (.keys @(first (.ch bt4))) [1 5]))
              (is (= (.keys @(second (.ch bt4))) [8 10]))
              (is (= (.keys @(last (.ch bt4))) [50 nil])))))))))
