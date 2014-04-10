(ns btree.core)

(declare btree-cons)

(deftype Node [order keys children parent]
  clojure.lang.IPersistentCollection
  ;; insert x into node, keeping it balanced
  (cons [node x] (btree-cons node x))
  (equiv [self other]
    (if (and (instance? Node self)
             (instance? Node other))
      (and (= (.order self) (.order other))
           (= (.keys self) (.keys other)))
      false)))

(defn btree
  ([order]
     (->Node order [] {} {:node (atom nil) :left (atom nil) :right (atom nil)}))
  ([order & keys]
     (->Node order
             (vec (sort keys))
             (apply hash-map
                    (interleave keys
                                (repeatedly (count keys)
                                            #(vector (atom nil) (atom nil)))))
             {:node (atom nil) :left (atom nil) :right (atom nil)})))

(defn- extract-children [nested]
  (remove nil?
          (map deref
               (into (mapv first (butlast nested))
                     (last nested)))))

(defn children [node]
  (extract-children (vals (.children node))))

(defn full? [node]
  (= (count (.keys node))
     (- (.order node) 1)))

(defn leaf? [node]
  (empty? (children node)))

;; should cache this
(defn height [node]
  (let [ch (children node)]
    (if (empty? ch)
      1
      (inc (apply max (map height ch))))))

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

(defn pos-in-parent [node]
  (when @(.parent node)
    (.indexOf (mapv deref (.ch @(.parent node)))
              node)))

(defn- bubble-up
  "Make sure node's parent contains it as one of its children."
  [node parent-pos]
  (if @(.parent node)
    (let [update-me (nth (.ch @(.parent node)) parent-pos)]
      (reset! update-me node)
      (when-let [parent-pos' (pos-in-parent node)]
        (recur @(.parent node) parent-pos')))
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
    (if-let [parent-pos (pos-in-parent node)]
      (bubble-up node' parent-pos)
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

(defn testbt []
  (let [lch (->Node 3
                          [1]
                          {1 [(atom nil) (atom nil)]}
                          {:node (atom nil) :left (atom nil) :right (atom nil)})
        rch (->Node 3
                          [8]
                          {8 [(atom nil) (atom nil)]}
                          {:node (atom nil) :left (atom nil) :right (atom nil)})
        parent (->Node 3
                             [4]
                             {4 [(atom lch) (atom rch)]}
                             {:node (atom nil) :left (atom nil) :right (atom nil)})]
    (reset! (:node  (.parent lch)) parent)
    (reset! (:left  (.parent lch)) 4)
    (reset! (:node  (.parent rch)) parent)
    (reset! (:right (.parent rch)) 4)
    parent))
