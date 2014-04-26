(ns die-harder.core-test
  (:require [clojure.test :refer :all]
            [die-harder.core :refer :all]))

(def mjs (make-jug-refs [3 5]))

(defn get-amount [i]
  (-> mjs (get-jug-ref-attribute i :amount)))

(defn are-amounts [i j]
  (is (= i (get-amount 0)))
  (is (= j (get-amount 1))))

(deftest a-test
  (testing "jugs initialization"
    (is (= 3 (-> mjs (get-jug-ref 0) deref :capacity)))
    (is (= 5 (-> mjs (get-jug-ref 1) deref :capacity)))

    (is (= 3 (-> mjs (get-jug-ref-attribute 0 :capacity))))
    (is (= 5 (-> mjs (get-jug-ref-attribute 1 :capacity))))

    (is (= 3 (do (fill-jug-ref mjs 0) (get-amount 0))))
    (is (= 3 (get-amount 0)))

    (is (= 1 1))

    (do (fill-jug-ref mjs 1)             (are-amounts 3 5)
        (empty-jug-ref mjs 0)            (are-amounts 0 5)
        (pour-in-from-other-ref mjs 0 1) (are-amounts 3 2)
        (empty-jug-ref mjs 0)            (are-amounts 0 2)
        (pour-in-from-other-ref mjs 0 1) (are-amounts 2 0)
        (fill-jug-ref mjs 1)             (are-amounts 2 5)
        (pour-in-from-other-ref mjs 0 1) (are-amounts 3 4)
        )))
