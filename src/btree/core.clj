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
  (map deref
       (into (mapv first (butlast nested))
             (last nested))))

(defn children [node]
  (extract-children (map (.children node) (.keys node))))

(defn full? [node]
  (= (count (.keys node))
     (- (.order node) 1)))

(defn leaf? [node]
  (empty? (remove nil? (children node))))

;; should cache this
(defn height [node]
  (if (nil? node)
    0
    (let [ch (children node)]
      (if (empty? ch)
        1
        (inc (apply max
                    (map height (children node))))))))

(defn- would-be-children
  "If x could be inserted into node, who would be its left and right children?"
  [node x]
  (let [conjed-keys (-> (conj (.keys node) x) sort vec)]
    (cond
     (= x (first conjed-keys))
     [nil (-> (get (.children node) (first (.keys node)))
              first
              deref)]

     (= x (last conjed-keys))
     [(-> (get (.children node) (last (.keys node)))
          second
          deref)
      nil]

     :else
     (let [idx (.indexOf conjed-keys x)]
       [(-> (get (.children node) (nth conjed-keys (dec idx)))
            second
            deref)
        (-> (get (.children node) (nth conjed-keys (inc idx)))
            first
            deref)]))))

(defn- next-child-to-search
  "x can't be inserted into node, find the child of node to keep looking."
  [node x]
  (let [children (remove nil? (would-be-children node x))]
    (if (empty? children)
      nil
      (first children))))

(defn- respects-property?
  "There's space for x in node, but figure out if inserting it would respect the B-tree balance."
  [node x]
  (let [[left right] (would-be-children node x)]
    (and (every? #(< x %) (if right (.keys right) []))
         (every? #(> x %) (if left  (.keys left)  [])))))

;; TODO handle repeated items
(defn- find-insertion-point [node x]
  ;; FIXME nope, bug. should insert as low as possible.
  (if (or (full? node)
          (not (respects-property? node x)))
    (if-let [child (next-child-to-search node x)]
      (recur child x)
      ;; need to split node, so return it
      node)
    node))

(defn- bubble-up!
  "Make sure node's parent contains it as one of its children."
  [node]
  (if-let [parent @(:node (.parent node))]
    (do
      (if @(:left (.parent node))
        (let [left (first (get (.children parent)
                               @(:left (.parent node))))]
          (reset! left node)))
      (if @(:right (.parent node))
        (let [right (second (get (.children parent)
                                 @(:right (.parent node))))]
          (reset! right node)))
      (recur parent))
    node))

(defn- trickle-down!
  "Make sure the children of node point to it as their parent."
  [node]
  (doseq [[k [left right]] (.children node)]
    (when @left
      (reset! (:node  (.parent @left)) node)
      (reset! (:left  (.parent @left)) k)
      (trickle-down! @left))
    (when @right
      (reset! (:node  (.parent @right)) node)
      (reset! (:right (.parent @right)) k)))
  (when-let [rightmost-ch @(second (get (.children node)
                                        (last (.keys node))))]
    (recur rightmost-ch)))

(defn- new-node-from-key [node x]
  ;; FIXME double calculation of the new keys vector, once in would-be-children
  ;; and another one directly in the Node constructor
  (let [[leftch rightch] (would-be-children node x)]
    (->Node (.order node)
            (-> (conj (.keys node) x) sort vec)
            (assoc (.children node) x [(atom leftch) (atom rightch)])
            (.parent node))))

(defn- add-to-keys [node x]
  (let [node' (new-node-from-key node x)]
    (trickle-down! node')
    (bubble-up! node')))

(defn- add-as-new-child! [where what]
  (let [new (->Node (.order where)
                    [what]
                    {what [(atom nil) (atom nil)]}
                    {:node (atom where) :left (atom nil) :right (atom nil)})
        keys (-> (conj (.keys where) what) sort vec)
        idx (.indexOf keys what)]
    ;; FIXME unfuck this
    (try
      (let [left-ch-of (nth keys (inc idx))]
        (reset! (first (get (.children where) left-ch-of))
                new)
        (reset! (:left (.parent new)) left-ch-of))
      (catch IndexOutOfBoundsException e))
    (try
      (let [right-ch-of (nth keys (dec idx))]
        (reset! (second (get (.children where) right-ch-of))
                new)
        (reset! (:right (.parent new)) right-ch-of))
      (catch IndexOutOfBoundsException e))
    new))

(defn- rebalance [where what] where)

(defn- add-with-overflow! [where what]
  (if (empty? (children where))
    (rebalance where (btree (.order where) what))
    (add-as-new-child! where what)))

(defn btree-cons [node x]
  (let [point (find-insertion-point node x)]
    (if (full? point)
      (do
        (add-with-overflow! point x)
        node)
      (add-to-keys point x))))
