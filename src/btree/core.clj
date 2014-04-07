(ns btree.core)

(declare btree-cons)

(deftype Node [order keys ch parent]
  clojure.lang.IPersistentCollection
  ;; insert x into node, keeping it balanced
  (cons [node x] (btree-cons node x))
  (equiv [self other]
    (if (and (instance? Node self)
             (instance? Node other))
      (and (= (.order self) (.order other))
           (= (.keys self) (.keys other)))
      false)))

(defn btree [order]
  (->Node order
          (vec (repeat (- order 1) nil))
          (vec (repeatedly order #(atom nil)))
          (atom nil)))

(defn full? [node]
  (= -1 (.indexOf (.keys node) nil)))

(defn leaf? [node]
  (let [derefed-ch (map deref (.ch node))]
    (= (count (keep nil? derefed-ch))
       (.order node))))

(defn height [node]
  (if (nil? node)
    0
    (let [node' (if (instance? Node node) node @node)
          child-heights (map height (map deref (.ch node')))]
      (inc (apply max child-heights)))))

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
      (if-let [child @(nth (.ch node) idx)]
        (recur child x)
        [node idx]))
    ;; always insert at the end?
    (let [pos (.indexOf (.keys node) nil)
          leftch @(nth (.ch node) pos)
          rightch @(nth (.ch node) (+ pos 1))]
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
          (if-let [child @(nth (.ch node) idx)]
            (recur child x)
            [node idx]))))))

(defn- bubble-up
  "Make sure node's parent contains it as one of its children."
  [node pos-in-parent]
  (if @(.parent node)
    (let [update-me (nth (.ch @(.parent node)) pos-in-parent)]
      (reset! update-me node)
      (when @(.parent node)
        (let [pos-in-parent' (.indexOf (mapv deref (.ch @(.parent node)))
                                       node)]
          (recur @(.parent node) pos-in-parent'))))
    node))

(defn- trickle-down
  "Make sure the children of node point to it as their parent."
  [node]
  (doseq [ch (.ch node)]
    (when @ch
      (reset! (.parent @ch) node)
      (trickle-down @ch))))

(defn- add-to-keys [node x]
  (let [node' (->Node (.order node)
                      ;; does the B-tree property hold after sorting?
                      (vec (sort (assoc (.keys node)
                                        (.indexOf (.keys node) nil)
                                        x)))
                      (.ch node)
                      (.parent node))]
    (trickle-down node')
    (if @(.parent node)
      (let [pos-in-parent (.indexOf (map deref (.ch @(.parent node)))
                                    node)]
        (bubble-up node' pos-in-parent))
      node')))

(defn- rebalance [node] node)

(defn- create-child
  "Insert a new child of node that contains key x at the pos-th position."
  [node key pos]
  (let [keys (vec (repeat (- (.order node) 1) nil))
        child (->Node (.order node)
                      (assoc keys 0 key)
                      (vec (repeatedly (.order node) #(atom nil)))
                      (atom node))]
    (reset! (nth (.ch node) pos) child)
    (rebalance node)))

(defn btree-cons [node x]
  (let [[place idx] (find-insertion-point node x)]
    (if (= -1 idx)
      (add-to-keys place x)
      (create-child place x idx))))
