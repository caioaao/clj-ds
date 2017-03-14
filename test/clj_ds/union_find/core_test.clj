(ns clj-ds.union-find.core-test
  (:require [clj-ds.union-find.core :as union-find]
            [clojure.test :refer :all]))

(defn- generate-graph
  [collisions]
  (let [biggest-node (reduce (fn [r [a b]] (max r a b)) 0 collisions)]
    (reduce (fn [uf [a b]] (union-find/union-sets uf a b))
            (union-find/create-uf biggest-node)
            collisions)))

(deftest poorly-written-test
  (let [collisions [[1 2] [3 4] [1 4] [5 6]]
        graph (atom (generate-graph collisions))
        queries [[1 4 true] [2 1 true] [1 3 true] [1 6 false]]]
    (testing "Graph is updated with results"
      (doseq [[a b r] queries]
        (is (= (-> (swap! graph #(union-find/same-set? % a b))
                   :res)
               r))))))
