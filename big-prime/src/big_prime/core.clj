(ns big-prime.core
  (:import java.util.Random)
  (:use [big-prime.utils]))

(set! *warn-on-reflection* true)

(defn big-sum
  ([] BigInteger/ZERO)
  ([^BigInteger x] x)
  ([^BigInteger x ^BigInteger & more] (.add x (apply big-sum more))))

(defn big-inc [^BigInteger x] (.add x BigInteger/ONE))
(defn big-dec [^BigInteger x] (.subtract x BigInteger/ONE))

(defn big-average
  ([] BigInteger/ZERO)
  ([^BigInteger x] x)
  ([^BigInteger x ^BigInteger & more]
     (.divide (.add x (apply big-sum more))
              (BigInteger. (str (+ 1 (count more)))))))

(defn big-lt [^BigInteger x ^BigInteger y] (== -1 (.compareTo x y)))
(defn big-le [^BigInteger x ^BigInteger y] (or (.equals x y) (big-lt x y)))
(defn big-gt [^BigInteger x ^BigInteger y] (==  1 (.compareTo x y)))
(defn big-ge [^BigInteger x ^BigInteger y] (or (.equals x y) (big-gt x y)))

(defn big-abs [^BigInteger x] (if (big-lt x BigInteger/ZERO) (.negate x) x))

(defn big-square [^BigInteger x] (.multiply x x))

(defn big-improve [^BigInteger guess ^BigInteger x]
  (big-average guess (.divide x guess)))

(defn big-good-enough? [^BigInteger guess ^BigInteger x]
  (and
   (big-le (big-square guess) x)
   (big-gt (big-square (big-inc guess)) x)))

(defn big-try-sqrt [^BigInteger guess ^BigInteger x]
  (if (big-good-enough? guess x)
    guess
    (recur (big-improve guess x) x)))

(defn big-sqrt [^BigInteger x] (big-try-sqrt BigInteger/ONE x))

(defn rand-bigint [^BigInteger bign ^Random rnd] 
  (let [bits (inc (.bitLength bign)) 
        bigr (BigInteger. bits rnd)] 
    (-> bign (.multiply bigr) (.shiftRight bits))))

(defn ^BigInteger big-rand [digits]
  (BigInteger. (apply str (take digits (repeatedly #(rand-int 10))))))

;; I could just live with "str" of everything, since "str" is
;; idempotent on Strings, but we may want more interesting coercions
;; later on; also it's not frugal to str and then unstr something
;; that's already a big-integer, and we want big-integer idempotent on
;; big-integers.
(defmulti big-integer type)
(defmethod big-integer (type "") [string] (BigInteger. string))
(defmethod big-integer (type BigInteger/ZERO) [big-i] big-i)
(defmethod big-integer :default  [thing]  (BigInteger. (str thing)))

(defn big-pos?     [^BigInteger j] (big-gt  j BigInteger/ZERO))
(defn big-neg?     [^BigInteger j] (big-lt  j BigInteger/ZERO))
(defn big-non-pos? [^BigInteger j] (big-le  j BigInteger/ZERO))
(defn big-non-neg? [^BigInteger j] (big-ge  j BigInteger/ZERO))
(defn big-zero?    [^BigInteger j] (.equals j BigInteger/ZERO))
(defn big-sign     [^BigInteger j] (cond
                                    (big-pos? j)  1
                                    (big-neg? j) -1
                                    :else         0
                                    ))

(defn big-range
  ([] (big-range BigInteger/ZERO))

  ([^BigInteger start]
     (cons start (lazy-seq (big-range (big-inc start)))))

  ([^BigInteger start ^BigInteger end]
     (if (big-gt end start)
       (cons start (lazy-seq (big-range (big-inc start) end)))
       ()))

  ([^BigInteger start ^BigInteger end ^BigInteger step]
     (let [op (if (big-pos? step) big-gt big-lt)])
     (if (op end start)
       (cons start (lazy-seq (big-range (.add start step) end)))
       ()))

  )
