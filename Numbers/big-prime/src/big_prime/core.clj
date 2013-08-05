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
     (if (> (* k k) n)
       (if (> n 1) (conj acc n) acc)
       (if (== 0 (rem n k))
         (recur (quot n k) k (conj acc k))
         (recur n (if (== k 2) (inc k) (+ 2 k)) acc)
         ))))

#_(def primes (sieve (cons 2 (iterate (partial + 2N) 3))))

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

(defn- rand-digit [] (rand-int 10))

(defn big-rand
  [digits]
  (bigint
   (read-string 
    (apply
     str
     (let [ds
           (drop-while
            #(== % 0)                   ;drop leading zeros
            (take digits (repeatedly rand-digit)))]
       (if (empty? ds)
         (list (rand-digit))
         ds)        ; in case the drop-while returns empty (all zeros)
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

(defn try-divisors
  ([n start end]
     (if (even? start)
       (case start
         (0 2) (try-divisors n 3 end [])
         (try-divisors n (inc start) end []))
       (if (== 1 start)
         (try-divisors n 3 end [])
         (try-divisors n start end []))))
  ([n k end acc]
     (if (or (== 1 n) (>= k end))
       acc
       (if (== 0 (rem n k))
         (recur (quot n k) k end (conj acc k))
         (recur n (+ 2 k) end acc)))))

(defn divide-out [n k acc]
  (if (== 0 (rem n k))
    (recur (quot n k) k (conj acc k))
    [n acc]))

(defn find-divisors [n p]
  (let [sn (inc n)
        ds (make-partition-book-ends sn p)
        [target maybe-2] (divide-out n 2 [])]
    (concat
     maybe-2
     (mapcat (fn [[start end]]
              (try-divisors target start end))
             ds))))

(defn sieve [xs]
  (if (empty? xs)
    ()
    (let [x (first xs)]
      (cons x
            (lazy-seq (sieve
                       (filter #(not= 0 (mod % x))
                               (rest xs))))))))

(defn factors [n p]
  (let [t (bigint n)]
    (let [divisors (find-divisors t p)
          finals (if (== 1 (count divisors))
                    divisors             ; Found a prime
                    (if (== t (last divisors))
                      (butlast divisors) ; Number itself is counted
                      divisors))         ; Unless it was depleted
          candidates (frequencies finals)
          sieved     (sieve (keys candidates))
          saved      (reduce #(into %1 {%2 (candidates %2)}) {} sieved)
          ]
      [t saved]
      )))

(defn check-factorization [[target factors]]
  (let [build (map (fn [[factor power]] (nt-power factor power)) factors)
        total (apply product build)]
    [target total (= target total) factors build])
  )


