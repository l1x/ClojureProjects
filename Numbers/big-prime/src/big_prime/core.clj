(ns big-prime.core
  (:import java.util.Random)
  (:use [big-prime.utils]
        [big-prime.sqrt :as nt]
        [clojure.core.contracts :as contracts]
        ))

(set! *warn-on-reflection* true)

;;; Fast, trivial, sequential trial-division

(defn divides?
  "Tests whether k divides n; the order of the arguments is in the sense of an infix operator: read (divides? k n) as \"k divides? n\"."
  [k n] (== 0 (rem n k)))

(def does-not-divide? (complement divides?))

(defn simple-factors
  "Return a list of factors of N."
  ([n] (simple-factors n 2 []))
  ([n k acc]
     (if (> (* k k) n)
       (if (> n 1) (conj acc n) acc)
       (if (divides? k n)
         (recur (quot n k) k (conj acc k))
         (recur n (if (== k 2) (inc k) (+ 2 k)) acc)
         ))))

(defn try-divisors
  "From book-ended sequences of trial-divisors, produce a sequence of actual divisors of n. The book-ends may have any values less than or equal to the target, and the left book-end must be less than or equal to the right book-end."
  ([n start end]
     (if (even? start)
       (case start
         ;; At the very beginning of a search with the small evens:
         (0 2) (try-divisors n 3 end [] n)
         ;; In some sequence of trials that happens to begin with an
         ;; even number:
         (try-divisors n (inc start) end [] n))
       ;; "start" is an odd number:
       (if (== 1 start)
         (try-divisors n 3 end [] n)
         (try-divisors n start end [] n))))
  ([n k end acc original]
     (cond
      (== 1 n)   acc
      (>= k end) acc
      :else (if (divides? k n)
              (recur (quot n k)      k  end (conj acc k) original)
              (recur       n    (+ 2 k) end       acc    original)))))

(contracts/provide
 (try-divisors
  "Constraints for book-ended trial division"
  [n start end & etc] [(<= start end)]
  ))

(defn make-partition-book-ends [end p]
  (let [e (bigint end)
        q (quot e p)
        r (rem  e p)]
    (for [i (range p)]
      [(* i q) (+ (if (== i (dec p)) r 0)
                  (* (inc i) q))])))

(defn divide-out [n k acc]
  (if (divides? k n)
    (recur (quot n k) k (conj acc k))
    [n acc]))

(defn find-divisors [n p]
  (let [sqrt-n           (inc (nt/nt-sqrt n))
        ds               (make-partition-book-ends sqrt-n p)
        [target maybe-2] (divide-out n 2 [])
        found (map   (fn [[start end]] (try-divisors target start end))   ds)]
    (concat
     maybe-2
     (apply concat found)
     )))

(defn sieve [xs]
  (if (empty? xs)
    ()
    (let [x (first xs)]
      (cons x
            (lazy-seq (sieve
                       (filter (partial does-not-divide? x)
                               (rest xs))))))))

(defn factors [n p]
  (let [t (bigint n)]
    (let [divisors (find-divisors t p)
          residuals  (filter
                      #(not= 1 %)
                      (reductions
                       (fn [product divisor]
                         (if (divides? divisor product)
                           (quot product divisor) product))
                       t
                       divisors))
          res2       (sort (seq (clojure.set/difference (set residuals) (set divisors))))
          candidates (frequencies (concat divisors res2))
          sieved     (sieve (sort (keys candidates)))
          saved      (reduce #(into %1 {%2 (candidates %2)}) {} sieved)
          ]
      [t saved]
      )))

(defn check-factorization [[target factors]]
  (let [build (map (fn [[factor power]] (nt-power factor power)) factors)
        total (apply nt/product build)]
    [target total (= target total) factors build]))

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


