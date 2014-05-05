(ns nuprng.core
  (:use clojure.pprint)
  (:require [clojure.math.numeric-tower :as mathEx]))

;;; Problem statement
;;;
;;; Generate random music on piano white keys by sampling a given
;;; distribution of notes. Outcomes are the N=7 characters from 'A' to
;;; 'G' with proportions P = (5, 8, 13, 0, 6, 4, 3). Generate characters
;;; randomly and statistically in those proportions. Show that your
;;; solution has the given statistics. You may use a uniformly
;;; distributed floating-point pseudo-random number generator such as
;;; Unix "rand." Your randoms need not be cryptographically strong.
;;;
;;; Your code should handle the general case where the number of
;;; outcomes is N and the given proportions are large integers. The
;;; above is just one concrete example.
;;;
;;; Characterize the space and time complexity of your solution in
;;; "big-O" terms. If you divide your solution into preprocessing and
;;; production phases, you need only chracterize the complexity of the
;;; production phase and may assume unbounded resources for
;;; preprocessing.
;;;
;;; There are many solutions to this problem with a wide variety of
;;; complexities.

;;; Here is a very short solution that linearly searches the cumulative
;;; probability distribution implied by the given probability
;;; distribution function.

(defmacro pdump
  "Monitoring and debugging macro with semantics of 'identity'."
  [x]
  `(let [x# (try ~x (catch Exception e# (str e#)))]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         x#)))

(defn sample-linearly
  "Produce n samples of the outcome-frequency hashmap by linear
searching; this is O(N)-space, O(N)-time. "
  [n outcome-freqency-hashmap]
  (let [frqs (map identity outcome-freqency-hashmap)
        sum  (total frqs)]
    (map (fn [target]
           (loop [candidates frqs
                  so-far (frequency (first candidates))]
             (if (< target so-far)
               (outcome (first candidates))
               ;; The reason that this is guaranteed never to run off
               ;; the end of the input is a little subtle and worth some
               ;; thought to prove to yourself. The fact that we don't
               ;; check is not a matter of sloppy coding, it's a matter
               ;; of a provable fact.
               (recur (rest candidates)
                      ;; An alternative implementation is to subtract
                      ;; the current frequency from the target.
                      (+ so-far (frequency (second candidates)))))))
         (map (fn [_] (->> (rand)
                          (* sum)
                          mathEx/floor
                          int))
              (range n)))))


;;; The following presents an O(N)-space, O(1)-time solution using
;;; Walker's "Method of Aliases."  I know of no better solution. It
;;; requires significant preprocessing, which redistributes the counts
;;; such that each new bin contains no more than two outcomes; the total
;;; number of outcomes in each new bin is the same for all bins; each
;;; new bin contains at least one count of its original "home"
;;; outcome. To redistribute the counts, they must be proportionally
;;; increased so that the new total is disvisible both by N, so that the
;;; new bins will all contain the same total amounts; and by S, the sum
;;; of the original counts, so that that the old counts

;;; This is the form of data returned by "frequencies,"
(def loaded-die {:A 37, :B 0, :C 17, :D 5, :E 12, :F 11 :G 44})

(defn total
  "The sum of frequencies in a distribution, which is a sequence of
pairs of outcomes and frequencies."
  [frqs] (apply + (map frequency frqs)))

(def outcome
  "Get the outcome from a outcome-frequency pair."
  first)

(def frequency
  "Get the frequency from an outcome-frequency pair."
  second)

(defn N
  "The number of outcomes in a distribution."
  [frqs] (count frqs))

(defn S
  "The total of the frequencies in a distribution."
  [frqs] (total frqs))

(defn L
  "A new total of augmented frequencies after augmentation."
  [frqs] (mathEx/lcm (N frqs) (S frqs)))

(defn H
  "The amount in each bin after augmentation and redistribution."
  [frqs] (/ (L frqs) (N frqs)))

(defn augmentation-factor
  "The factor by which to increase each frequency prior to
redistribution."
  [frqs] (/ (L frqs) (S frqs)))

(defn augmented
  "The augmented frequencies after augmentation and prior to
redistribution."
  [frqs] (map #(vector
                (outcome %)
                (* (frequency %) (augmentation-factor frqs)))
              frqs))

;;; Preprocessing

(defn fill-shortest
  "Given a target height, a hashmap of :filled and :remaining bins, and
a sequence of outcome-frequency pairs, produce a new hashmap of :filled
and :remaining bins; the :filled item of that hashmap should be a vector
of :home and :other hashmaps, where each one specifies the two colors in
the bin after redistribution."
  [target filled frqs]
  (let [sorted   (sort-by frequency frqs)
        tallest  (last    sorted)
        shortest (first   sorted)
        deficit  (- target (frequency shortest))
        to-do    (drop 1 sorted)]
    {:filled
     (conj filled {:home shortest, :other [(outcome tallest) deficit]})
     :remaining
     (if (empty? to-do)
       to-do
       (conj (drop-last to-do)
             ;; make a new outcome-frequency pair.
             [(outcome tallest) (- (frequency tallest) deficit)]))}))

(defn redistribute
  "Given a target height for each bin and a sequence of beginning
outcome-frequency pairs, produce a vector of redistributed frequencies,
where each new bin is a hash map of the new :home outcome-frequency pair
and the new frequency of the :other outcome-frequency pair. "
  [target frqs]                     ; TODO: precondition frqs not empty?
  (loop [result (fill-shortest target [] frqs)]
    (if (empty? (:remaining result))
      (:filled result)
      (recur (fill-shortest target
                            (:filled result)
                            (:remaining result))))))

(defn sample-
  "Helper function that finds either the :home outcome or the :other
outcome from a sequence of redistributed frequencies."
  [target redistributed]
  (let [bucket (rand-nth redistributed)
        height (rand-int target)]
    (if (< height (frequency (:home bucket)))
      (outcome (:home bucket))
      (outcome (:other bucket))
      )))

(defn sample-walker
  "Takes n random samples from a hashmap of outcomes and frequencies;
preprocessing aside, this is O(N)-space, O(1)-time."
  [n outcome-freqency-hashmap]
  (let [redistributed (redistribute
                       (H outcome-freqency-hashmap)
                       (augmented outcome-freqency-hashmap))
        ansatz (first redistributed)
        target (+ (frequency (:home  ansatz))
                  (frequency (:other ansatz)))]
    (map (fn [_] (sample- target redistributed))
         (range n))))

;;; The following is no substitute for unit tests, but it produces a
;;; nice printed result of an experiment.

(defn -main [] (pprint
                (let [redis (augmented loaded-die)
                      h     (H loaded-die)
                      n     (N loaded-die)
                      s     (S loaded-die)
                      l     (L loaded-die)]
                  {"loaded-die" loaded-die
                   ,"original count"  n
                   ,"original total"  s
                   ,"gcd count total" (mathEx/gcd n s)
                   ,"lcm count total" l
                   ,"target height:"  h
                   ,"target total:"   (* h n)
                   ,"augmented heights" redis
                   ,"total augmented heights" (total redis)
                   ,"tallest and shortest" (fill-shortest h [] redis)
                   ,"redistributed" (redistribute h redis)
                   ,"sample-walker 10000"
                   (time
                    (frequencies
                     (sample-walker (* 10000 s)
                                    loaded-die)))
                   ,"sample-linearly 10000"
                   (time
                    (frequencies
                     (sample-linearly (* 10000 s)
                                      loaded-die)))})))
