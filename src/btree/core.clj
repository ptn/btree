(ns btree.core)

(defn full? [node]
  (= -1 (.indexOf (.keys node) nil)))

(defn leaf? [node]
  (= (count (filter nil? (.ch node))) (.order node)))

(defn- child-idx
  "Return the index of the child of node where x should be tried to be inserted."
  ([node x] (child-idx node x 0))
  ([node x idx]
     (cond
      (< x (nth (.keys node) idx)) idx
      (= (inc idx) (count (remove nil? (.keys node)))) (inc idx)
      :else (recur node x (inc idx)))))

;; TODO handle repeated items
(defn- find-insertion-point
  "Returns a vector of two elements, where:

  * the first one indicates which node to insert x into
  * the second one, if it is -1, indicates that x should be inserted as one of
    the keys of the first element of the return vector, and if it is any other
    integer, it means a new node should be created and inserted as a child of
    the node at that index."
  [node x]
  (if (full? node)
    (let [idx (child-idx node x)]
      (if-let [child (nth (.ch node) idx)]
        (recur child x)
        [node idx]))
    ;; always insert at the end?
    (let [pos (.indexOf (.keys node) nil)
          leftch (nth (.ch node) pos)
          rightch (nth (.ch node) (+ pos 1))]
      (if (and (if leftch
                 (every? #(< % x)
                         (remove nil? (.keys leftch)))
                 1)
               (if rightch
                 (every? #(> % x)
                         (remove nil? (.keys rightch)))
                 1))
        [node -1]
        ;; TODO refactor, same code as after (full? node)
        (let [idx (child-idx node x)]
          (if-let [child (nth (.ch node) idx)]
            (recur child x)
            [node idx]))))))

(deftype Node [order keys ch]
  clojure.lang.IPersistentCollection
  ;; insert x into node, keeping it balanced
  (cons [node x]
    (let [[place child-idx] (find-insertion-point node x)]
      (println place child-idx)
      node)))

(defn btree [order]
  (->Node order
          (vec (repeat (- order 1) nil))
          (vec (repeat order nil))))

(defn testbt []
  (let [lch (->Node 3 [1 nil] [nil nil nil])
        rch (->Node 3 [8 nil] [nil nil nil])]
    (->Node 3 [2 nil] [lch rch nil])))
