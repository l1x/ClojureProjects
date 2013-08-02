(ns big-prime.core-test
  (:import java.util.Random)
  (:require [clojure.test :refer :all]
            [big-prime.core :refer :all]))

(deftest big-prime-test
  (testing "Correct importing of java.util.Random"
    (let [r (Random.)]
      (is (= 0 (.nextInt r 1)))
      (is (= 0 (rand-int 1)))
      )))
