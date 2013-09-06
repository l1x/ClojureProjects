(ns maybe.core-test
  (:require [clojure.test :refer :all]
            [maybe.core :refer :all]
            [clojure.algo.monads :refer :all]))

;;; http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/

(deftest a-test
  (testing "m-bind on maybe"
    (is (=

         (with-monad
           identity-m
           (m-bind 1
                   (fn [a]
                     (m-bind (inc a)
                             (fn [b]
                               (m-result (* a b)))))))
         (domonad identity-m
                  [a 1
                   b (inc a)
                   ] (* a b)))

        "Explicit with-monad produces same result as domonad"

        ))

  (is (= 2 ((fn [x] (domonad maybe-m
                            [a x
                             b (inc a)]
                            (* a b))) 1))
      "Maybe monad produces positive result")

  (is (= nil ((fn [x] (domonad maybe-m
                              [a x
                               b nil]
                              (* a b))) 1))  ; Normally expect
                                        ; NullPointerException here
      "Maybe monad produces negative result on second argument (and avoids NullPointerException")
  
  (is (= nil ((fn [x] (domonad maybe-m
                              [a x
                               b (inc a)]
                              (* a b))) nil))  ; Normally expect
                                        ; NullPointerException here
      "Maybe monad produces negative result on first argument (and avoids NullPointerException")
  
  (is (thrown? NullPointerException (let [a 1
                                          b nil]
                                      (* a b)))
      "Non-monadic computation throws NullPointerException")
  )

(deftest exception-throwing-test
  (testing "exceptions are thrown"
    (is (thrown? ArithmeticException (/ 1 0)))
    (is (thrown-with-msg? ArithmeticException #"Divide by zero" (/ 1 0)))
    ))

(deftest comprehension-test
  (testing "sequence monad and comprehension"
    (is (= (domonad sequence-m
                    [a (range 5)
                     b (range a)]
                    (* a b))
           (for [a (range 5)
                 b (range a)]
             (* a b)))
        "Monadic sequence equals for comprehension")))

(deftest if-not-monad-test
)


