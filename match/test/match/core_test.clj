(ns match.core-test
  (:require [clojure.test :refer :all]
            [match.core   :refer :all])
  (:use [clojure.core.match :only (match)])
  )

;;; See https://github.com/clojure/core.match/wiki/Basic-usage

(deftest basic-usage
  (testing "Matching Literals"
    (is (= 4 (let [x true
                   y true
                   z true]
               (match [x y z]
                      [_     false true] 1
                      [false true  _    ] 2
                      [_     _     false] 3
                      [_     _     true ] 4
                      :else 5))))
    (is (= 1 (let [x true]
               (match x                 ; none may have brackets
                      true  1           ; none...
                      false 2           ; none...
                      :else 3))))
    (is (= 1 (let [x true]
               (match [x]               ; or all must have brackets
                      [true]  1         ; all...
                      [false] 2         ; all...
                      :else 3))))
    )

  (testing "Binding"
    (is (= 2 (let [x 1
                   y 2]
               (match [x y]
                      [1 b] b
                      [a 2] a
                      :else nil)))))

  ;; This appears to do an exact-match on a sequence, rather than a
  ;; first-match or a bestmatch.
  (testing "Sequential types"
    (is (= :a2 (let [x [1 2 nil nil nil]]
                 (match [x]             ; all must have brackets, or
                        [([1]               :seq)] :a0
                        [([1 2]             :seq)] :a1
                        [([1 2 nil nil nil] :seq)] :a2
                        :else                      :a3))))
    (is (= :a2 (let [x [1 2 nil nil nil]]
                 (match x               ; none may have brackets
                        ([1]               :seq) :a0
                        ([1 2]             :seq) :a1
                        ([1 2 nil nil nil] :seq) :a2
                        :else                      :a3)))))

  (testing "Vector types"
    (is (= :a2 (let [x [1 2 3]]
                 (match [x]             ; all must have brackets, or
                        [[_ _ 2]] :a0
                        [[1 1 3]] :a1
                        [[1 2 3]] :a2
                        :else     :a3))))
    (is (= :a2 (let [x [1 2 3]]
                 (match x               ; none may have brackets
                        [_ _ 2] :a0
                        [1 1 3] :a1
                        [1 2 3] :a2
                        :else   :a3)))))
  ;; Test git remote change
    )
