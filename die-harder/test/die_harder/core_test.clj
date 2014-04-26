(ns die-harder.core-test
  (:require [clojure.test :refer :all]
            [die-harder.core :refer :all]))

(def mjis (make-jugs [3 5]))

(deftest immutable-pour-test
  (= 0 (-> mjis (pour-from-other 0 1))))

(deftest immutables-test
  (testing "jugs, immutable version"
    (is (= 3 (:capacity (get-jug mjis 0))))
    (is (= 5 (:capacity (get-jug mjis 1))))
    (is (= [0 0] (map :amount mjis)))
    (is (= 3 (-> mjis
                 (fill-jug 0)
                 (get-jug 0)
                 :amount)))
    (is (= 0 (-> mjis
                 (fill-jug 0)
                 (empty-jug 0)
                 (get-jug 0)
                 :amount)))
    (is (= 4 (-> mjis
                 (fill-jug 1)
                 (pour-from-other 0 1)
                 (empty-jug 0)
                 (pour-from-other 0 1)
                 (fill-jug 1)
                 (pour-from-other 0 1)
                 (get-jug 1)
                 :amount
                 )))
    ))

(def mjs (make-jug-refs [3 5]))

(defn get-amount [i]
  (-> mjs (get-jug-ref-attribute i :amount)))

(defn are-amounts [i j]
  (is (= i (get-amount 0)))
  (is (= j (get-amount 1))))

(deftest mutables-test
  (testing "jugs, mutable ref version"
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
