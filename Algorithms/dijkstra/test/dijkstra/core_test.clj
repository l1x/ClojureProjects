(ns dijkstra.core-test
  (:require [clojure.test :refer :all]
            [dijkstra.core :refer :all]))

(deftest a-test
  (testing "test itself."
    (is (= 1 1))))

(def ^:private a-graph
  (DirectedGraph. 
   { :s { :v 1, :w 4 }
    :v { :w 2, :t 6 }
    :w { :t 3 } }))
