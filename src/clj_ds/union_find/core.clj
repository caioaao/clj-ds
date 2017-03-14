(ns clj-ds.union-find.core)
;; with path compression
(defn update-state [ds f] (update ds :state f))
(defn update-res [ds f] (assoc ds :res (f (:state ds))))

(defn create-ds
  ([] {})
  ([s0] (update-state (create-ds) (constantly s0))))

(defn create-uf [n]
  (create-ds (zipmap (range 0 n) (range 0 n))))

(defn compress-path [ds x]
  (let [old-s (:state ds)]
    (if (= (old-s x) x)
      ds
      (let [new-ds (compress-path ds (old-s x))]
        (update-state new-ds (fn [s] (assoc s x (s (s x)))))))))

(defn union-sets [ds a b]
  (let [ds' (-> ds (compress-path a) (compress-path b))]
    (update-state ds' (fn [s] (assoc s (s a) (s b))))))

(defn get-set [ds x]
  (let [ds' (compress-path ds x)]
    (update-res ds' (fn [s] (s x)))))

(defn same-set? [ds a b]
  (let [ds' (get-set ds a)
        ds'' (get-set ds' b)]
    (update-res ds'' (fn [_] (= (:res ds')
                               (:res ds''))))))
