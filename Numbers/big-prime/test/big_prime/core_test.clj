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
      (is (== 0 (.nextInt r 1)))
      (is (== 0 (rand-int 1)))))

  (testing "big-rand"
    (is (== 1000 (count (repeatedly 1000 #(big-rand 1)))))
    (is (== 1000 (count (repeatedly 1000 #(big-rand 100)))))
    )

  (-> (testing "BigInt number-theoretic square root"
        (let [i (pdump (big-rand 100))
              j (pdump (nt-sqrt i))
              q (square i)
              s (nt-sqrt q)]
          (is (== s i))
          (is (< (square j) i))
          (is (>= (square (inc j)) i))
          ))
      time
      )

  (testing "Partitioning big range into P buckets"
    (are [x y] (= x y)
         (make-partition-book-ends 100 10)
         '([0 10] [10 20] [20 30] [30 40] [40 50]
             [50 60] [60 70] [70 80] [80 90] [90 100])

         (make-partition-book-ends 101 10)
         '([0 10] [10 20] [20 30] [30 40] [40 50]
             [50 60] [60 70] [70 80] [80 90] [90 101])

         (make-partition-book-ends 109 10)
         '([0 10] [10 20] [20 30] [30 40] [40 50]
             [50 60] [60 70] [70 80] [80 90] [90 109])

         (make-partition-book-ends 110 10)
         '([0 11] [11 22] [22 33] [33 44] [44 55]
             [55 66] [66 77] [77 88] [88 99] [99 110])))

  (testing "factors"
    #_(is (= (factors 10000  4) [10000N '([2N 4] [5N 4])]))
    #_(is (= (factors 82763  1) [82763N '([82763N 1])]))
    #_(is (= (factors 82763  4) [82763N '([82763N 1])]))
    #_(is (= (factors 82763 16) [82763N '([82763N 1])]))
    #_(is (= (factors 82763 64) [82763N '([82763N 1])]))
    #_(is (thrown? ArithmeticException "Divide by zero" (factors 82763 0)))

    (is (= (simple-factors (* 55511N 283N 59N))  [59N 283N 55511N]))
    (is (= (simple-factors 477841685N)           [5  1367  69911 ]))
    (is (= (try-divisors 477841685N 31 100000) [   1367N 69911N]))
    (is (= (try-divisors 477841685N  1 100000) [5N 1367N 69911N]))
    (is (= (try-divisors 477841685N  5 100000) [5N 1367N 69911N]))

    (is (= (divide-out 10000N 2N []) [625 [2 2 2 2]]))
    (is (= (divide-out 10001N 2N []) [10001 []]))

    ;; Fuzz-test:

    #_(is (every? (plucker 3)
                (repeatedly
                 10
                 (fn [] (check-factorization
                        (factors
                         (big-rand 5) (inc (rand-int 10))))))))

    #_(is (every? (plucker 3)
                (repeatedly
                 10
                 (fn [] (check-factorization
                        (factors-parallel
                         (big-rand 5) 4))))))

    )
  )

(deftest integer-operation-tests

  (testing "nt-power"
    (are [x y] (== x y)
         10 (nt-power 10 1)
         1  (nt-power 10 0)
         (reduce * (repeat 40 10N)) (nt-power 10 40)
         ))

  (testing "sum"
    (are [x y] (== x y)
         0 (sum)
         1 (sum 1)
         2 (sum 1 1)
         9 (apply sum (repeat 3 3))
         (nt-power 10 64) (sum 1 (dec (nt-power 10 64))))
    )
  
  (testing "abs"
    (are [x y] (== x y)
         0 (abs 0)
         1 (abs -1)
         1 (abs 1)
         (dec (nt-power 10 64)) (abs    (dec (nt-power 10 64)))
         (dec (nt-power 10 64)) (abs (- (dec (nt-power 10 64)))))
    (is (thrown? clojure.lang.ArityException (abs)))
    (is (thrown? ClassCastException (abs "0")))
    )
  
  (testing "square"
    (are [x y] (== x y)
         0 (square 0)
         1 (square 1)
         1 (square -1)
         99999999999999999999999999999999999999999999999999999999999999980000000000000000000000000000000000000000000000000000000000000001
         (square (dec (nt-power 10 64))))
    (is (thrown? clojure.lang.ArityException (square)))
    (is (thrown? ClassCastException (square "0")))
    )

  (testing "exceptions"
    (is (thrown? ArithmeticException (/ 1 0)))
    (is (thrown? ArithmeticException (/ 0 0))))
  )


