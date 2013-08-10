(ns big-prime.core
  (:import java.util.Random)
  (:use [big-prime.utils]
        [big-prime.sqrt :as nt]
        [clojure.core.contracts :as contracts]
        ))

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

(defn try-divisors
  "From book-end trial-divisors, produce a sequence of actual divisors of n. The book-ends may have any values less than or equal to the target, and the left book-end must be less than or equal to the right book-end."
  ([n start end]
     (if (even? start)
       (case start
         ;; At the very beginning of a search with the small evens:
         (0 2) (try-divisors n 3 end [])
         ;; In some sequence of trials that happens to begin with an
         ;; even number:
         (try-divisors n (inc start) end []))
       ;; "start" is an odd number:
       (if (== 1 start)
         (try-divisors n 3 end [])
         (try-divisors n start end []))))
  ([n k end acc]
     (if (or (== 1 n) (>= k end))
       acc
       (if (== 0 (rem n k))
         (recur (quot n k) k end (conj acc k))
         (recur n (+ 2 k) end acc)))))

(contracts/provide
 (try-divisors
  "Constraints for book-ended trial division"
  [n start end & etc] [(<= start end)]
  ))

#_(def primes (sieve (cons 2 (iterate (partial + 2N) 3))))

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

(defn make-partition-book-ends [end p]
  (let [e (bigint end)
        q (quot e p)
        r (mod  e p)]
    (for [i (range p)]
      [(* i q) (+ (if (== i (dec p)) r 0)
                  (* (inc i) q))])))

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
        total (apply nt/product build)]
    [target total (= target total) factors build])
  )


