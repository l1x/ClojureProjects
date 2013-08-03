;; If running this from nrepl in emacs, be sure to C-c M-j in a
;; buffer visiting THIS file, the test file, and not in a buffer
;; visiting the core file. That way, the keyboard shortcut C-c C-,
;; will run the tests. Due to an integration gap in nrepl and
;; leiningen, that keystroke combination will not work if you start
;; nrepl from a buffer visiting the core file.

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

  (testing "Invertibility of big-sqrt and big-square; also tests big-average, big-le, and private functions."
    (let [i (pdump (big-rand 100))
          j (big-sqrt i)
          q (pdump (big-square i))
          s (pdump (big-sqrt q))]
      (is (.equals s i))
      (is (big-le (big-square j) i))
      (is (big-ge (big-square (big-inc j)) i))
      ))
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
    )
  
  (testing "exceptions"
    (is (thrown? ArithmeticException (/ 1 0))))
    (is (thrown? ArithmeticException (/ 0 0)))
    (is (thrown? clojure.lang.ArityException (abs)))
  )

(deftest big-integer-interop-tests
  
  (testing "Interop with java.math.BigInteger"
    (is (= (type BigInteger/ONE) (type (big-sum))))
    (is (= (type BigInteger/ONE) (type (big-sum BigInteger/ONE))))
    (is (= (type BigInteger/ONE) (type (big-sum BigInteger/ONE BigInteger/ONE)))))

  (testing "big-inc and big-dec"
    (is (.equals (big-inc BigInteger/ZERO) (BigInteger/ONE)))
    (is (.equals (big-dec BigInteger/ONE)  (BigInteger/ZERO)))
    (let [r (Random.)
          h (big-rand 100)]
      (is (.equals (big-inc (big-dec h)) h))
      (is (.equals (big-dec (big-inc h)) h)))
    )

  (testing "String representations"
    (is (= (str BigInteger/ZERO)          "0"))
    (is (= (str BigInteger/ONE)           "1"))
    (is (= (str (big-inc BigInteger/ONE)) "2"))
    (is (= (str (BigInteger. "999999999999999")) "999999999999999")))

  (testing "Coercions"
    (is (= (str (big-integer "9999")) "9999"))
    (is (= (str (big-integer 9999)) "9999"))
    (is (= (str (big-integer -9999)) "-9999"))
    (is (= (str (big-integer 9999N)) "9999"))
    (is (= (str (big-integer -9999N)) "-9999"))
    (is (= (str (big-integer 9999M)) "9999"))
    (is (= (str (big-integer -9999M)) "-9999"))
    (is (= (str (big-integer -9999999999999999)) "-9999999999999999"))
    (is (= (str (big-integer -9999999999999999N)) "-9999999999999999"))
    (is (= (str (big-integer -9999999999999999M)) "-9999999999999999")))

  (testing "Idempotency"
    (is (.equals (big-integer BigInteger/ZERO) BigInteger/ZERO))
    (is (.equals (big-integer BigInteger/ONE)  BigInteger/ONE))
    (let [r (Random.)
          h (big-rand 100)]
      (is (.equals (big-integer h) h))
      (is (.equals (big-integer h) h))))
  )


(deftest big-integer-operations

  (testing "big equivalence classes"
    (let [bm1 (big-dec BigInteger/ZERO)]
      (is      (big-pos? BigInteger/ONE))
      (is (not (big-pos? BigInteger/ZERO)))
      (is (not (big-pos? bm1)))

      (is (not (big-neg? BigInteger/ONE)))
      (is (not (big-neg? BigInteger/ZERO)))
      (is      (big-neg? bm1))

      (is      (big-non-neg? BigInteger/ONE))
      (is      (big-non-neg? BigInteger/ZERO))
      (is (not (big-non-neg? bm1)))

      (is (not (big-non-pos? BigInteger/ONE)))
      (is      (big-non-pos? BigInteger/ZERO))
      (is      (big-non-pos? bm1))

      (is (not (big-zero? BigInteger/ONE)))
      (is      (big-zero? BigInteger/ZERO))
      (is (not (big-zero? bm1)))

      (is (=  1 (big-sign BigInteger/ONE)))
      (is (=  0 (big-sign BigInteger/ZERO)))
      (is (= -1 (big-sign bm1)))))

  )

  ;; My iteration of 
  ;; https://github.com/clojure/clojure/blob/master/test/clojure/test_clojure/sequences.clj
  ;;
(deftest test-range
  (let [t0 BigInteger/ZERO
        t1 BigInteger/ONE
        t2 (big-inc (BigInteger/ONE))
        t3 (big-inc t2)
        t4 (big-inc t3)
        t5 (big-inc t4)
        t6 (big-inc t5)
        t7 (big-inc t6)
        t8 (big-inc t7)
        t9 (big-inc t8)]
    (are [x y] (= x y)
         (range 0) ()                     ; exclusive end!
         (range 1) '(0)
         (range 5) '(0 1 2 3 4)

         (range -1) ()
         (range -3) ()

         (range 2.5) '(0 1 2)
         (range 7/3) '(0 1 2)

         (range 0 3) '(0 1 2)
         (range 0 1) '(0)
         (range 0 0) ()
         (range 0 -3) ()

         (range 3 6) '(3 4 5)
         (range 3 4) '(3)
         (range 3 3) ()
         (range 3 1) ()
         (range 3 0) ()
         (range 3 -2) ()

         (range -2 5) '(-2 -1 0 1 2 3 4)
         (range -2 0) '(-2 -1)
         (range -2 -1) '(-2)
         (range -2 -2) ()
         (range -2 -5) ()

         (take 3 (range 9 3 0)) '(9 9 9)
         (range 0 0 0) ()
         (range 3 9 1) '(3 4 5 6 7 8)
         (range 3 9 2) '(3 5 7)
         (range 3 9 3) '(3 6)
         (range 3 9 10) '(3)
         (range 3 9 -1) () )))

       ;; current variance with actual behavior
       #_(take 3 (range 3 9 0)) '(3 3 3)






