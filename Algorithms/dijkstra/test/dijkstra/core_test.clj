(ns dijkstra.core-test
  (:require [clojure.test    :refer :all]
            [dijkstra.core   :refer :all]
            [clojure.java.io :as io])
  (:import  [dijkstra.core DirectedGraph UndirectedGraph]))

(deftest a-test
  (testing "test itself."
    (is (= 1 1))))

(def ^:private a-graph
  (DirectedGraph. 
   {:s {:v 1, :w 4}
    :v {:w 2, :t 6}
    :w {:t 3 }
    :t {}}))

(def ^:private b-graph
  (DirectedGraph.
   {1 {2 1, 3 4}
    2 {3 2, 4 6}
    3 {4 3}
    4 {}}))

(deftest paths-test
  (is (=  (shortest-paths-linear a-graph :s)
          [[:s 0 [:s]]
           [:v 1 [:s :v]]
           [:w 3 [:s :v :w]]
           [:t 6 [:s :v :w :t]]]))

  (is (=  (shortest-paths-log-linear a-graph :s)
          [[:s 0 [:s]]
           [:v 1 [:s :v]]
           [:w 3 [:s :v :w]]
           [:t 6 [:s :v :w :t]]]))

  (is (= (shortest-path a-graph :s :t) [:s :v :w :t]))

  (is (= (shortest-path b-graph 1 4) [1 2 3 4]))
)

;;; Find BFG's here: http://snap.stanford.edu/data/

(def ^:private c-graph
  { :1 [:2 :3],
    :2 [:4],
    :3 [:4],
    :4 []
   })

(defn- kw-to-int [kw] (->> kw str rest (apply str) read-string))
(defn- square [x] (* x x))

(deftest traverse-test
  (is (= (seq-graph-dfs c-graph :1) [:1 :3 :4 :2]))
  (is (= (seq-graph-bfs c-graph :1) [:1 :2 :3 :4]))
  (is (= (seq-graph-dfs c-graph :1  (comp square kw-to-int)) [1 9 16 4]))
  (is (= (seq-graph-bfs c-graph :1  (comp square kw-to-int)) [1 4 9 16]))
  )

(deftest corner-cases-test 
  (-> (= [] (seq-graph-dfs {} :1)) is)
  (-> (= [] (seq-graph-bfs {} :1)) is)

  (-> (= [:1] (seq-graph-dfs {:1 []} :1)) is)
  (-> (= [:1] (seq-graph-bfs {:1 []} :1)) is)

  (-> (= [] (seq-graph-dfs {:2 []} :1)) is)
  (-> (= [] (seq-graph-bfs {:2 []} :1)) is)

  (-> (= [:1] (seq-graph-dfs {:1 [:1]} :1)) is)
  (-> (= [:1] (seq-graph-bfs {:1 [:1]} :1)) is)
  )

(defn- with-bfg-1 [op]
  (with-open [rdr (io/reader "./data/wiki-Vote.txt")]
    (op (line-seq rdr))))

(def ^:private bfg-1-pairs
  (with-open [rdr (io/reader "./data/wiki-Vote.txt")]
    (let [edges
          (doall (for [line (line-seq rdr)
                       :when (not (re-find #"^#" line))]
                   (map read-string (re-seq #"\d+" line))))]
      (map vec edges))))

(defn graph-from-pairs [pairs]
  (reduce
   (fn [hmap pair]
     (let [[v1 v2] pair
           tail (hmap v1)]
       (if tail
         (let [head (or (tail v2) 0)]
           (into hmap [[v1 (into tail [[v2 (inc head)]])]]))
         (into hmap [[v1 {v2 1}]]))
       ))
   {}
   pairs))

(deftest bfg-test
  (is (== 103693 (with-bfg-1 count)))
  (is (= [30 5534 2658 2014])) (shortest-path
      (DirectedGraph. (graph-from-pairs bfg-1-pairs))
      30 2014))



