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
    :w {:t 3 }}))

(def ^:private b-graph
  (DirectedGraph.
   {1 {2 1, 3 4}
    2 {3 2, 4 6}
    3 {4 3}}))

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

(deftest traverse-test
  (is (= (seq-graph-dfs c-graph :1) [:1 :3 :4 :2]))
  (is (= (seq-graph-bfs c-graph :1) [:1 :2 :3 :4])))

(defn ^:private with-bfg-1 [op]
  (with-open [rdr (io/reader "./data/wiki-Vote.txt")]
    (op (line-seq rdr))))


(deftest bfg-test
  (is (== 103693 (with-bfg-1 count)))

  )



