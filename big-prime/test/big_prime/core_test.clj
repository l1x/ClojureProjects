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
    (let [i (pdump (huge-random-number 100))
          q (pdump (big-square i))
          s (pdump (big-sqrt q))]
      (is (.equals s i))))
  
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
          h (huge-random-number 100)]
      (is (.equals (big-inc (big-dec h)) h))
      (is (.equals (big-dec (big-inc h)) h)))
    )

  (testing "String representations"
    (is (= (str BigInteger/ZERO)          "0"))
    (is (= (str BigInteger/ONE)           "1"))
    (is (= (str (big-inc BigInteger/ONE)) "2"))
    (is (= (str (BigInteger. "999999999999999")) "999999999999999"))
    )
  )







