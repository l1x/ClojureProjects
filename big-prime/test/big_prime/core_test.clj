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
  (testing "Correct importing of java.util.Random"
    (let [r (Random.)]
      (is (= 0 (.nextInt r 1)))
      (is (= 0 (rand-int 1)))))
  (testing "Invertibility of sqrt and square"
    (let [i (pdump (huge-random-number 100))
          q (pdump (big-square i))
          s (pdump (big-sqrt q))]
      (is (.equals s i))))
  )



