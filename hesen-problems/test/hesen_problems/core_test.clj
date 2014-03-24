(ns hesen-problems.core-test
  (:require [clojure.test :refer :all]
            [hesen-problems.core :refer :all]))

(deftest a-test
  (testing "chained function application"
    (is (= {:accumulated-results [0 5], :vertical 79}
           (chain-all
            [(fn [a b] {:vertical (+ a b) :horizontal (- a b)})]
            [42]
            {:accumulated-results [0] :vertical 37}
            )))
    (is (= {:accumulated-results [0 30 -1547], :vertical 4285}
           (chain-all
            [(fn [a b] {:vertical (+ a b) :horizontal (- a b)})
             (fn [a b] {:vertical (+ (* a a) (* b b))
                       :horizontal (- (* a a) (* b b))})]
            [42 37]
            {:accumulated-results [0] :vertical 12}
            ))
        )))
