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

(deftest jbig-integer-sqrt
  (->
   (testing "Invertibility of jbig-sqrt and jbig-square; also tests jbig-average, jbig-le, and private functions."
     (let [i (pdump (jbig-rand 100))
           j (jbig-sqrt i)
           q (jbig-square i)
           s (jbig-sqrt q)]
       (is (.equals s i))
       (is (jbig-le (jbig-square j) i))
       (is (jbig-ge (jbig-square (jbig-inc j)) i))
       ))
   time
   pdump
   ))

(deftest jbig-integer-interop-tests
  
  (testing "Interop with java.math.BigInteger"
    (is (= (type BigInteger/ONE) (type (jbig-sum))))
    (is (= (type BigInteger/ONE) (type (jbig-sum BigInteger/ONE))))
    (is (= (type BigInteger/ONE) (type (jbig-sum BigInteger/ONE BigInteger/ONE)))))

  (testing "jbig-inc and jbig-dec"
    (is (.equals (jbig-inc BigInteger/ZERO) (BigInteger/ONE)))
    (is (.equals (jbig-dec BigInteger/ONE)  (BigInteger/ZERO)))
    (let [r (Random.)
          h (jbig-rand 100)]
      (is (.equals (jbig-inc (jbig-dec h)) h))
      (is (.equals (jbig-dec (jbig-inc h)) h)))
    )

  (testing "String representations"
    (is (= (str BigInteger/ZERO)          "0"))
    (is (= (str BigInteger/ONE)           "1"))
    (is (= (str (jbig-inc BigInteger/ONE)) "2"))
    (is (= (str (BigInteger. "999999999999999")) "999999999999999")))

  (testing "Coercions"
    (is (= (str (jbig-integer "9999")) "9999"))
    (is (= (str (jbig-integer 9999)) "9999"))
    (is (= (str (jbig-integer -9999)) "-9999"))
    (is (= (str (jbig-integer 9999N)) "9999"))
    (is (= (str (jbig-integer -9999N)) "-9999"))
    (is (= (str (jbig-integer 9999M)) "9999"))
    (is (= (str (jbig-integer -9999M)) "-9999"))
    (is (= (str (jbig-integer -9999999999999999)) "-9999999999999999"))
    (is (= (str (jbig-integer -9999999999999999N)) "-9999999999999999"))
    (is (= (str (jbig-integer -9999999999999999M)) "-9999999999999999")))

  (testing "Idempotency"
    (is (.equals (jbig-integer BigInteger/ZERO) BigInteger/ZERO))
    (is (.equals (jbig-integer BigInteger/ONE)  BigInteger/ONE))
    (let [r (Random.)
          h (jbig-rand 100)]
      (is (.equals (jbig-integer h) h))
      (is (.equals (jbig-integer h) h))))
  )


(deftest jbig-integer-operations

  (testing "big equivalence classes"
    (let [bm1 (jbig-dec BigInteger/ZERO)]
      (is      (jbig-pos? BigInteger/ONE))
      (is (not (jbig-pos? BigInteger/ZERO)))
      (is (not (jbig-pos? bm1)))

      (is (not (jbig-neg? BigInteger/ONE)))
      (is (not (jbig-neg? BigInteger/ZERO)))
      (is      (jbig-neg? bm1))

      (is      (jbig-non-neg? BigInteger/ONE))
      (is      (jbig-non-neg? BigInteger/ZERO))
      (is (not (jbig-non-neg? bm1)))

      (is (not (jbig-non-pos? BigInteger/ONE)))
      (is      (jbig-non-pos? BigInteger/ZERO))
      (is      (jbig-non-pos? bm1))

      (is (not (jbig-zero? BigInteger/ONE)))
      (is      (jbig-zero? BigInteger/ZERO))
      (is (not (jbig-zero? bm1)))

      (is (=  1 (jbig-sign BigInteger/ONE)))
      (is (=  0 (jbig-sign BigInteger/ZERO)))
      (is (= -1 (jbig-sign bm1)))))

  )
