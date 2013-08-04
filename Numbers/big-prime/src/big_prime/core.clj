(ns big-prime.core
  (:import java.util.Random)
  (:use [big-prime.utils]
        [clojure.core.contracts :as contracts]
        ))

;;; TODO: Build up the contracts in here. See nt-power for an example.

(set! *warn-on-reflection* true)

;;; Fast, trivial, sequential trial-division

(defn simple-factors
  "Return a list of factors of N."
  ([n] (simple-factors n 2 []))
  ([n k acc]
     (if (== 1 n)      
       acc
       (if (== 0 (rem n k))
         (recur (quot n k) k (conj acc k))
         (recur n (if (== k 2) (inc k) (+ 2 k)) acc)))))

;;; It's unclear whether ^clojure.lang.BigInt type hints actually
;;; improve perf. TODO: Use Criterium library to profile.

(defn sum 
  ([] 0)
  ([x] x)
  ([x & more] (reduce + x more)))

(defn product
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

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
  (nt-average guess (quot x guess)))

(defn- good-enough?
  "A guess is good enough if its square is lte the target and the square of its increment is gt the target"
  [guess x]
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
  (nt-try-sqrt 1 (bigint x)))

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

(defn nt-power [n m]
  ;; Also consider: (reduce * 1N (repeat m n))
  (letfn [(helper [n m acc]
             (cond
              (== m 0) 1N
              (== m 1) acc
              :else (recur n (dec m) (* n acc))))]
    (helper (bigint n) m (bigint n))))

(contracts/provide
 (nt-power
  "Constraints for number-theoretic power function"
  [n m] [(number? n) (not (neg? n))
         (number? m) (not (neg? m))
         =>
         number?
         pos?]))

(defn make-partition-book-ends [end p]
  (let [e (bigint end)
        q (quot e p)
        r (mod  e p)]
    (for [i (range p)]
      [(* i q) (+ (if (== i (dec p)) r 0)
                  (* (inc i) q))])))

(defn generate-trial-divisors [[start end]]
  (filter odd? (range (bigint start) (bigint end))))

(defn generate-trial-divisor-partitions [end p]
  (map generate-trial-divisors
       (make-partition-book-ends end p)))

(defn try-divisors [n divisors]
  (filter (fn [d] (== 0 (mod n d))) divisors))

(defn sieve [xs]
  (if (empty? xs)
    ()
    (let [x (first xs)]
      (cons x
            (lazy-seq (sieve
                       (filter #(not= 0 (mod % x))
                               (rest xs))))))))

#_(def primes (sieve (cons 2 (iterate (partial + 2N) 3))))

(defn tally-of-divisor [target divisor]
  (let [q (quot target divisor)
        r (mod  target divisor)]
    (if (== r 0)
      (cons 1 (lazy-seq (tally-of-divisor q divisor)))
      ()
      )))

(defn exponent-of-divisor [target divisor]
  (count (tally-of-divisor target divisor)))

(defn try-divisors-2
  ([n start end]
     (if (even? start)
       (case start
         (0 2) (try-divisors-2 n 3 end [])
         (try-divisors-2 n (inc start) end []))
       (if (== 1 start)
         (try-divisors-2 n 3 end [])
         (try-divisors-2 n start end []))))
  ([n k end acc]
     (if (or (== 1 n) (> k end))
       acc
       (if (== 0 (rem n k))
         (recur (quot n k) k end (conj acc k))
         (recur n (+ 2 k) end acc)))))

(defn divide-out [n k acc]
  (if (== 0 (rem n k))
    (recur (quot n k) k (conj acc k))
    [n acc]))

(defn find-divisors-2 [n p]
  (let [sn (inc n)
        ds (make-partition-book-ends sn p)
        cast-out-2s (divide-out n 2 [])
        target  (cast-out-2s 0)
        maybe-2 (cast-out-2s 1)]
    (concat
     maybe-2
     (mapcat (fn [[start end]]
               (try-divisors-2 target start end))
             ds))))

(defn find-divisors [n p]
  (let [sn (inc n)
        ds (generate-trial-divisor-partitions sn p)
        target  (if (even? n) (quot n 2) n)
        maybe-2 (if (even? n) '(2N) ())
        ]
    (sieve
     (filter #(not= 1 %)
             (concat maybe-2
                     (mapcat (fn [partn]
                               (try-divisors target partn))
                             ds))))))

(defn factors [n p]
  (let [t (bigint n)]
    (let [divisors (find-divisors t p)
          exponents (map (partial exponent-of-divisor t) divisors)
          ]
      [t (map vector divisors exponents)]
      )))

(defn find-divisors-parallel [n p]
  (let [sn (inc n)
        ds (generate-trial-divisor-partitions sn p)
        target  (if (even? n) (quot n 2) n)
        maybe-2 (if (even? n) '(2N) ())
        ]
    (sieve
     (filter #(not= 1 %)
             (concat maybe-2
                     (apply concat
                            (pmap (fn [partn]
                                      (try-divisors target partn))
                                    ds)))))))

(defn factors-parallel [n p]
  (let [t (bigint n)]
    (let [divisors (find-divisors-parallel t p)
          exponents (map (partial exponent-of-divisor t) divisors)
          ]
      [t (map vector divisors exponents)]
      )))

(defn check-factorization [[target factors]]
  (let [build (map (fn [[factor power]] (nt-power factor power))
                   factors)
        total (apply product build)]
    [target total (= target total) factors build])
  )


