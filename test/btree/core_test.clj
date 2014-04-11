(ns btree.core-test
  (:require [clojure.test :refer :all]
            [btree.core :as btree]))

(defn testbt []
  (let [lch (btree/->Node 3
                          [1]
                          {1 [(atom nil) (atom nil)]}
                          {:node (atom nil) :left (atom nil) :right (atom nil)})
        rch (btree/->Node 3
                          [8]
                          {8 [(atom nil) (atom nil)]}
                          {:node (atom nil) :left (atom nil) :right (atom nil)})
        parent (btree/->Node 3
                             [4]
                             {4 [(atom lch) (atom rch)]}
                             {:node (atom nil) :left (atom nil) :right (atom nil)})]
    (reset! (:node  (.parent lch)) parent)
    (reset! (:left  (.parent lch)) 4)
    (reset! (:node  (.parent rch)) parent)
    (reset! (:right (.parent rch)) 4)
    parent))

(deftest height-virtual-property
  (is (= (btree/height (btree/btree 3)) 1))
  (is (= (btree/height (testbt)) 2)))

(deftest children-virtual-property
  (is (= [nil nil] (btree/children (btree/btree 3 1))))
  (let [children (btree/children (testbt))]
    (is (= (.keys (first children))  [1]))
    (is (= (.keys (second children)) [8]))
    (is (= (count children) 2))
    (let [bt (conj (testbt) 20)
          ch (btree/children bt)]
      (is (= (.keys (first ch))  [1]))
      (is (= (.keys (second ch)) [8]))
      (is (nil? (last ch)))
      (is (= (count ch) 3)))))

(deftest full-predicate
  (is (not (btree/full? (testbt))))
  (is (btree/full? (btree/btree 3 1 2))))

(deftest leaf-predicate
  (is (btree/leaf? (btree/btree 3)))
  (is (not (btree/leaf? (testbt)))))

(deftest creation
  (testing "creating without keys"
    (let [subject (btree/btree 3)]
      (is (= 3 (.order subject)))
      (is (empty? (.keys subject)))
      (is (empty? (btree/children subject)))
      (is (= [nil nil nil]
             (map deref (vals (.parent subject)))))))
  (testing "creating with keys"
    (let [subject (btree/btree 3 4 20)]
      (is (= 3 (.order subject)))
      (is (= [4 20] (.keys subject)))
      (is (= [nil nil nil] (btree/children subject)))
      (is (= [nil nil nil]
             (map deref (vals (.parent subject))))))))

(deftest insertion
  (let [bt1 (conj (testbt) 5)
        ch (btree/children bt1)]
    (is (= (.keys bt1) [4]))
    (is (= (.keys (first  ch)) [1]))
    (is (= (.keys (second ch)) [5 8]))
    (is (= (count ch) 2))
    (let [bt2 (conj bt1 20)
          ch (btree/children bt2)]
      (is (= (.keys bt2) [4 20]))
      (is (= (.keys (first ch)) [1]))
      (is (= (.keys (second ch)) [5 8]))
      (is (nil? (last ch)))
      (is (= (count ch) 3))
      (let [bt3 (conj bt2 50)
            ch (btree/children bt3)]
        (is (= (.keys bt3) [4 20]))
        (is (= (.keys (first ch)) [1]))
        (is (= (.keys (second ch)) [5 8]))
        (is (= (.keys (last ch)) [50]))
        (is (= (count ch) 3))
        (let [bt4 (conj bt3 10)
              ch (btree/children bt4)]
          (is (= (btree/height bt4) 3))
          (is (= (.keys bt4) [8]))
          (is (= (.keys (first ch)) [4]))
          (is (= (.keys (second ch)) [20]))
          ;; test second level
          )))))
