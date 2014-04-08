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

(defn- neighbors
  "If x could be inserted into node, who would be its left and right neighbors
  in the keys array?"
  [node x]
  (let [conjed-keys (sort (conj (.keys node) x))]
    (cond
     (= x (first conjed-keys)) [nil (first (.keys node))]
     (= x (last conjed-keys))  [(last (.keys node)) nil]
     :else (let [idx (.indexOf conjed-keys x)]
             [(nth conjed-keys (dec idx)) (nth conjed-keys (inc idx))]))))

(defn- would-be-children
  "If x could be inserted into node, who would be its left and right children?"
  [node x]
  (let [[left right] (neighbors node x)
        leftch (if left
                 (-> (get (.children node) left) second deref)
                 nil)
        rightch (if right
                  (-> (get (.children node) right) first deref)
                  nil)]
    [leftch rightch]))

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

;; why does this bubble all the way up if it never creates new nodes? resetting
;; pointers in the parent should be enough
(defn- bubble-up!
  "Make sure node's parent contains it as one of its children."
  [node]
  (when-let [parent @(:node (.parent node))]
    (do
      (if @(:left (.parent node))
        (let [left (first (get (.children parent)
                               @(:left (.parent node))))]
          (reset! left node)))
      (if @(:right (.parent node))
        (let [right (second (get (.children parent)
                                 @(:right (.parent node))))]
          (reset! right node))))))

(defn- trickle-down!
  "Make sure the children of node point to it as their parent."
  [node]
  (doseq [[k [left right]] (.children node)]
    (when @left
      (reset! (:node (.parent @left)) node)
      (reset! (:left (.parent @left)) k))
    (when @right
      (reset! (:node  (.parent @right)) node)
      (reset! (:right (.parent @right)) k))))

(defn- new-node-from-key [node x]
  (let [[leftch rightch] (would-be-children node x)]
    (->Node (.order node)
            ;; FIXME double calculation of the new keys vector, once in
            ;; would-be-children and another one here
            (-> (conj (.keys node) x) sort vec)
            (assoc (.children node) x [(atom leftch) (atom rightch)])
            (.parent node))))

(defn- add-to-keys [node x]
  (let [node' (new-node-from-key node x)]
    (trickle-down! node')
    (bubble-up! node')
    node'))

(defn- add-as-new-child! [where what]
  (let [new (->Node (.order where)
                    [what]
                    {what [(atom nil) (atom nil)]}
                    {:node (atom where) :left (atom nil) :right (atom nil)})
        [left right] (neighbors where what)]
    (when left
      (reset! (second (get (.children where) left)) new)
      (reset! (:right (.parent new)) left))
    (when right
      (reset! (first (get (.children where) right)) new)
      (reset! (:left (.parent new)) right))
    new))

(defn- unlink-parent! [node]
  (when-let [parent @(:node (.parent node))]
    (when-let [left @(:left (.parent node))]
      (reset! (first (get (.children parent) left))
              nil))
    (when-let [right @(:right (.parent node))]
      (reset! (second (get (.children parent) right))
              nil))))

(defn median [xs]
  (nth xs (quot (count xs) 2)))

(defn- split-children [base ks]
  (let [vs (map (.children base) ks)]
    (if (empty? (remove nil? vs))
      (apply hash-map (interleave ks (repeatedly (count ks)
                                                 #(vector (atom nil) (atom nil)))))
      (apply hash-map (interleave ks vs)))))

(defn- split [where what]
  (let [conjed-keys (sort (into (.keys where) (.keys what)))
        m (median conjed-keys)
        lks (vec (filter #(< % m) conjed-keys))
        rks (vec (filter #(> % m) conjed-keys))
        lch (->Node (.order where)
                    lks
                    (split-children where lks)
                    {:node (atom nil) :left (atom m) :right (atom nil)})
        rch (->Node (.order where)
                    rks
                    (split-children where rks)
                    {:node (atom nil) :left (atom nil) :right (atom m)})
        parent (->Node (.order where)
                       [m]
                       {m [(atom lch) (atom rch)]}
                       {:node (atom nil) :left (atom nil) :right (atom nil)})]
    (reset! (:node (.parent lch)) parent)
    (reset! (:node (.parent rch)) parent)
    (trickle-down! lch)
    (trickle-down! rch)
    parent))

(defn- assign-temp-children! [parent trio-root]
  (let [median (first (.keys trio-root))
        [left right] (neighbors parent median)]
    (when left
      (reset! (second (get (.children parent) left))
              @(first (get (.children trio-root) median))))
    (when right
      (reset! (first (get (.children parent) right))
              @(second (get (.children trio-root) median))))))

(defn- rebalance [where what]
  (unlink-parent! where)
  (let [trio-root (split where what)]
    (if-let [parent @(:node (.parent where))]
      (do
        (assign-temp-children! parent trio-root)
        (recur parent trio-root))
      trio-root)))

(defn btree-cons
  "Add x to the tree that starts at node, return the new root."
  [node x]
  (let [point (find-insertion-point node x)]
    (if (full? point)
      (if (empty? (remove nil? (children point)))
        (rebalance point (btree (.order node) x))
        (do
          (add-as-new-child! point x)
          node))
      (let [new (add-to-keys point x)]
        (if (= node point) new node)))))
