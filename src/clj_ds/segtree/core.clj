(ns clj-ds.segtree.core)

;; These functions must be defined for each tree type
(defmulti join-vals (fn [_ _ type] type))
(defmulti identity-val (fn [tree] (:type tree)))

;; This one is only necessary to use `range-update`. All other operations can be
;; used without implementing it.
(defmulti join-range-val (fn [tree _] (:type tree)))

(defmethod join-range-val :default [_ _] nil)

(defn- join-trees [left right]
  (when (and left right)
    {:type      (:type left)
     :value     (join-vals (:value left) (:value right) (:type left))
     :left      left
     :right     right
     :idx-left  (:idx-left left)
     :idx-right (:idx-right right)}))

(defn- leaf [value idx tree-type]
  {:type      tree-type
   :value     value
   :idx-left  idx
   :idx-right idx})

(defn- imput-lazy-val [tree]
  (assoc tree :lazy-val (or (:lazy-val tree) (identity-val tree))))

(defn- reset-lazy-val [tree]
  (assoc tree :lazy-val (identity-val tree)))

(defn- relax-tree
  [{:keys [left right lazy-val type] :as tree}]
  (if lazy-val
    (when-let [tree' (some-> tree (join-range-val lazy-val) reset-lazy-val)]
      (if left
        (merge tree'
               {:left (-> (imput-lazy-val left)
                          (update :lazy-val join-vals lazy-val type))
                :right (-> (imput-lazy-val right)
                          (update :lazy-val join-vals lazy-val type))})
        tree'))
    tree))


;; API

(defn build
  ([arr tree-type]
   (build arr 0 (dec (count arr)) tree-type))
  ([arr i j tree-type]
   (if (= i j)
     (leaf (get arr i) i tree-type)
     (join-trees (build arr i (quot (+ i j) 2) tree-type)
                 (build arr (inc (quot (+ i j) 2)) j tree-type)))))

(defn query
  "Returns result of joining all values between `a` and `b`."
  [tree a b]
  (let [tree (or (relax-tree tree) tree)]
    (cond
      (or (< (:idx-right tree) a)
          (> (:idx-left tree) b))
      (identity-val tree)

      (and (>= (:idx-left tree) a)
           (<= (:idx-right tree) b))
      (:value tree)

      :true
      (join-vals (query (:left tree) a b)
                 (query (:right tree) a b)
                 (:type tree)))))

(defn point-update
  "Joins `v` with value in `idx`."
  [tree idx v]
  (let [tree (or (relax-tree tree) tree)]
    (cond
      (or (< (:idx-right tree) idx)
          (> (:idx-left tree) idx))
      tree

      (and (= (:idx-left tree) idx)
           (= (:idx-right tree) idx))
      (update tree :value join-vals v (:type tree))

      :true
      (join-trees (point-update (:left tree) idx v)
                  (point-update (:right tree) idx v)))))

(defn range-update
  "Joins `v` with all values between `a` and `b`. This function requires the
  `join-range-val` method to be defined for the type of tree."
  [tree a b v]
  (when-let [tree (relax-tree tree)]
    (cond
      (or (< (:idx-right tree) a)
          (> (:idx-left tree) b))
      tree

      (and (>= (:idx-left tree) a)
           (<= (:idx-right tree) b))
      (relax-tree (assoc tree :lazy-val v))

      :true
      (join-trees (range-update (:left tree) a b v)
                  (range-update (:right tree) a b v)))))
