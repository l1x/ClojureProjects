(ns big-prime.core
  (:import java.util.Random)
  (:use [big-prime.utils]))

(set! *warn-on-reflection* true)

(defn sum
  ([] 0)
  ([x] x)
  ([x & more] (+ x (apply sum more))))

(defn nt-average
  "Number-theoretic mean using `quot' instead of `/', which latter produces rationals"
  ([] 0)
  ([x] x)
  ([x & more]
     (quot (+ x (apply sum more))
           (inc (count more)))))

(defn abs
  "Absolute value"
  [x]
  (if (< x 0) (- x) x))

(defn square [x] (* x x))

(defn- nt-improve
  "Improve a guess of a square root by number-theoretic average with the quotient of the guess with the target: an adaptation of Newton's method to the integer domain."
  [guess x]
  #_(pdump guess)
  #_(pdump (quot x guess))
  #_(pdump (nt-average guess (quot x guess)))
  (nt-average guess (quot x guess)))

(defn- good-enough?
  "A guess is good enough if its square is lte the target and the square of its increment is gt the target"
  [guess x]
  #_(pdump (square guess))
  #_(pdump (<= (square guess) x))
  #_(pdump (>  (square (inc guess)) x))
  (and
   (<= (square guess) x)
   (>  (square (inc guess)) x)))

(defn- nt-try-sqrt [guess x]
  (if (good-enough? guess x)
    guess
    (recur (nt-improve guess x) x)))

(defn nt-sqrt
  "Number-theoretic square root (largest integer whose square is less than or equal to the target)"
  [x]
  (nt-try-sqrt 1 x))

(defn- rand-digit [] (rand-int 10))

(defn big-rand
  [digits]
  (bigint
   (read-string 
    (apply
     str
     (let [ds
           (drop-while
            #(== % 0) ;; drop leading zeros
            (take digits (repeatedly rand-digit)))]
       (if (empty? ds)
         (list (rand-digit))
         ds) ;; in case the drop-while returns empty (all zeros)
       )))))

(defn jbig-sum
  ([] BigInteger/ZERO)
  ([^BigInteger x] x)
  ([^BigInteger x ^BigInteger & more] (.add x (apply jbig-sum more))))

(defn jbig-inc [^BigInteger x] (.add x BigInteger/ONE))
(defn jbig-dec [^BigInteger x] (.subtract x BigInteger/ONE))

(defn jbig-average
  ([] BigInteger/ZERO)
  ([^BigInteger x] x)
  ([^BigInteger x ^BigInteger & more]
     (.divide (.add x (apply jbig-sum more))
              (BigInteger. (str (+ 1 (count more)))))))

(defn jbig-lt [^BigInteger x ^BigInteger y] (== -1 (.compareTo x y)))
(defn jbig-le [^BigInteger x ^BigInteger y] (or (.equals x y) (jbig-lt x y)))
(defn jbig-gt [^BigInteger x ^BigInteger y] (==  1 (.compareTo x y)))
(defn jbig-ge [^BigInteger x ^BigInteger y] (or (.equals x y) (jbig-gt x y)))

(defn jbig-abs [^BigInteger x] (if (jbig-lt x BigInteger/ZERO) (.negate x) x))

(defn jbig-square [^BigInteger x] (.multiply x x))

(defn jbig-improve [^BigInteger guess ^BigInteger x]
  (jbig-average guess (.divide x guess)))

(defn jbig-good-enough? [^BigInteger guess ^BigInteger x]
  (and
   (jbig-le (jbig-square guess) x)
   (jbig-gt (jbig-square (jbig-inc guess)) x)))

(defn jbig-try-sqrt [^BigInteger guess ^BigInteger x]
  (if (jbig-good-enough? guess x)
    guess
    (recur (jbig-improve guess x) x)))

(defn jbig-sqrt [^BigInteger x] (jbig-try-sqrt BigInteger/ONE x))

(defn ^BigInteger jbig-rand [digits]
  (BigInteger. (apply str (take digits (repeatedly #(rand-int 10))))))

;; I could just live with "str" of everything, since "str" is
;; idempotent on Strings, but we may want more interesting coercions
;; later on; also it's not frugal to str and then unstr something
;; that's already a jbig-integer, and we want jbig-integer idempotent on
;; jbig-integers.
(defmulti jbig-integer type)
(defmethod jbig-integer (type "") [string] (BigInteger. string))
(defmethod jbig-integer (type BigInteger/ZERO) [jbig-i] jbig-i)
(defmethod jbig-integer :default  [thing]  (BigInteger. (str thing)))

(defn jbig-pos?     [^BigInteger j] (jbig-gt  j BigInteger/ZERO))
(defn jbig-neg?     [^BigInteger j] (jbig-lt  j BigInteger/ZERO))
(defn jbig-non-pos? [^BigInteger j] (jbig-le  j BigInteger/ZERO))
(defn jbig-non-neg? [^BigInteger j] (jbig-ge  j BigInteger/ZERO))
(defn jbig-zero?    [^BigInteger j] (.equals j BigInteger/ZERO))
(defn jbig-sign     [^BigInteger j] (cond
                                     (jbig-pos? j)  1
                                     (jbig-neg? j) -1
                                     :else         0
                                     ))


