(ns cps.core)


;;; See the unit-test file for exercises on these definitions.

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
            (fib-cps                    ; expect StackOverflowError here
             (- n 2)                    ; n
             (fn [n2] (k (+ n1 n2)))     ; k
             ))]
    (if (<= n 1)
      (k n)
      (recur (- n 1) cont))))


(defn mk-cps [accept? end-value kend knext]
  (fn [n]
    ((fn [n k]
       (if (accept? n)
         (k end-value)
         (recur (dec n) (fn [v] (k (knext v n)))))
       )
     n kend)))

(def fac0 (mk-cps zero? 1N identity #(*' %1 %2)))
(def tri0 (mk-cps zero? 1N dec      #(+' %1 %2)))

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

(defn func [n]
  (if (= n 0)
    0
    (func (dec n))))

(defn fund [n]
  (if (= n 0)
    0
    #(fund (dec n))))

(defn fune [n k]
  (if (= n 0)
    (k 0)
    (fune (dec n) k)))

(defn funf [n k]
  (if (= n 0)
    (k 0)
    #(funf (dec n) k)))

(defn facr [n]
  (reduce *' (drop 1 (range (inc n)))))

(defn faca [n k]
  (if (= n 0N)
    (k 1N)
    (*' n (faca (dec n) k))))

(defn facb [n a k]
  (if (= n 0N)
    (k a)
    #(facb (dec n) (*' a n) k)))

(defn facc [n k]
  (if (= n 0N)
    (k 1N)
    #(facc (dec n) (fn [n-] (k (*' n n-))))))
