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

  (testing "Importing of java.util.Random"
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
    (is (= (str (big-integer -9999999999999999M)) "-9999999999999999"))))

(deftest big-integer-operations

  (testing "big-range"
    (is (.equals (big-integer 2000000000000000000)
                 (first (big-range (big-integer 2000000000000000000)))))
    (is (every? identity
                (map (fn [^BigInteger x ^BigInteger y] (.equals x y))
                     (pdump (big-range (big-integer 42) (big-integer 44)))
                     (pdump (list (big-integer 42) (big-integer 43)))))))

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
)
    )
  )







