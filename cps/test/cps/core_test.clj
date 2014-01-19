(ns cps.core-test
  (:require [clojure.test :refer :all]
            [cps.core :refer :all]))

(deftest pyth-test
  (testing "pyth-cps"
    (is (= 61 (pyth-cps 5 6 identity)))))

(deftest fib-test
  (testing "fib-cps"
    (is (= 55 (fib-cps 10 identity)))))

(deftest trampoline-test
  (is (= 0 (trampoline funa 42)))
  (is (= 0 (trampoline funa 100000))))
