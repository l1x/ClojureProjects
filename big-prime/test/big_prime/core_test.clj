(ns big-prime.core-test
  (:import java.util.Random)
  (:require [clojure.test :refer :all]
            [big-prime.core :refer :all]))

(deftest big-prime-test
  (testing ""
    (let [r (Random.)]
      (is (= 0 (.nextInt r 1))))))
