(ns dijkstra.core-test
  (:require [clojure.test :refer :all]
            [dijkstra.core :refer :all])
  (:import  [dijkstra.core DirectedGraph UndirectedGraph]))

(deftest a-test
  (testing "test itself."
    (is (= 1 1))))

(def ^:private a-graph
  (DirectedGraph. 
   {:s {:v 1, :w 4}
    :v {:w 2, :t 6}
    :w {:t 3 }}))

(deftest paths-test
  (is (=  (shortest-paths a-graph :s) [[:s 0 [:s]]
                                       [:v 1 [:s :v]]
                                       [:w 3 [:s :v :w]]
                                       [:t 6 [:s :v :w :t]]])))
