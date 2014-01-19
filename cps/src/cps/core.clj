(ns cps.core)

;;; From http://www.slideshare.net/borgesleonardo/continuation-passing-style-and-macros-in-clojure-jan-2012

(defn *-cps [x y k]
  (k (* x y)))

(defn +-cps [x y k]
  (k (+ x y)))

(defn pyth-cps [a b k]
  (*-cps a a (fn [a2]
               (*-cps b b (fn [b2]
                            (+-cps a2 b2 k))))))

(defn fib-cps [n k]
  (letfn [(cont [n1]
            (fib-cps
             (- n 2)
             (fn [n2] (k (+ n1 n2)))
             ))]
    (if (<= n 1)
      (k n)
      (recur (- n 1) cont))))

;;; From http://pramode.net/clojure/2010/05/08/clojure-trampoline/

(declare funa funb)

(defn funa [n]
  (if (= n 0)
    0
    #(funb (dec n))))

(defn funb [n]
  (if (= n 0)
    0
    #(funa (dec n))))
