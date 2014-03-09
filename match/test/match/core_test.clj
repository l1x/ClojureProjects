(ns match.core-test
  (:require [clojure.test       :refer :all]
            [match.core         :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

  (let [x true
        y true
        z true]
    (match [x y z]
           [_ false true] 1
           [false true _ ] 2
           [_ _ false] 3
           [_ _ true] 4
           :else 5))
