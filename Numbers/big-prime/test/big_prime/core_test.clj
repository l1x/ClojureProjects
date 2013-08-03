;; If running this from nrepl in emacs, be sure to C-c M-j in a buffer
;; visiting the core file and NOT in a buffer visiting THIS file. That
;; way, the keyboard shortcut C-c C-, in a buffer visiting this file
;; will run the tests. Due to an integration gap in nrepl and
;; leiningen, that keystroke combination will not work if you start
;; nrepl from a buffer visiting the test file.

(ns big-prime.core-test
  (:import java.util.Random)
  (:require [clojure.test    :refer :all]
            [big-prime.utils :refer :all]
            [big-prime.core  :refer :all]))

(deftest big-prime-tests

  (testing "java.util.Random"
    (let [r (Random.)]
      (is (= 0 (.nextInt r 1)))
      (is (= 0 (rand-int 1)))))

  (testing "big-rand"
    (is (= 1000 (count (repeatedly 1000 #(big-rand 1)))))
    (is (= 1000 (count (repeatedly 1000 #(big-rand 1000)))))
    )

  (-> (testing "BigInt number-theoretic square root"
        (let [i (pdump (big-rand 100))
              j (nt-sqrt i)
              q (square i)
              s (nt-sqrt q)]
          (is (= s i))
          (is (< (square j) i))
          (is (>= (square (inc j)) i))
          ))
      time
      pdump
      )

  (testing "Partitioning big range into P buckets"
    )

  )

(deftest integer-operation-tests
  (testing "sum"
    (are [x y] (= x y)
         0 (sum)
         1 (sum 1)
         2 (sum 1 1)
         9 (apply sum (repeat 3 3))
         10000000000000000000000000000000000000000000000000000000000000000 (sum 1
          9999999999999999999999999999999999999999999999999999999999999999))
    )
  
  (testing "abs"
    (are [x y] (= x y)
         0 (abs 0)
         1 (abs -1)
         1 (abs 1)
         9999999999999999999999999999999999999999999999999999999999999999
         (abs 9999999999999999999999999999999999999999999999999999999999999999)
         9999999999999999999999999999999999999999999999999999999999999999
         (abs -9999999999999999999999999999999999999999999999999999999999999999))
    (is (thrown? clojure.lang.ArityException (abs)))
    (is (thrown? ClassCastException (abs "0")))
    )
  
  (testing "square"
    (are [x y] (= x y)
         0 (square 0)
         1 (square 1)
         1 (square -1)
         99999999999999999999999999999999999999999999999999999999999999980000000000000000000000000000000000000000000000000000000000000001
         (square 9999999999999999999999999999999999999999999999999999999999999999))
    (is (thrown? clojure.lang.ArityException (square)))
    (is (thrown? ClassCastException (square "0")))
    )

  (testing "exceptions"
    (is (thrown? ArithmeticException (/ 1 0)))
    (is (thrown? ArithmeticException (/ 0 0))))
  )


