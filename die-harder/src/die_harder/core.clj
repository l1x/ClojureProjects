(ns die-harder.core
  (:require clojure.pprint))

(defmacro pdump
  "Monitoring and debugging macro with semantics of 'identity'."
  [x]
  `(let [x# (try ~x (catch Exception e# (str e#)))]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         x#)))

(defn make-jugs  [capacities]
  (->>
   capacities
   (map-indexed (fn [i c] {:id i :capacity c :amount 0}))
   vec))

(defn get-jug [jugs i]
  (let [mj (jugs i)]
    (if (not= i (:id mj))
      (-> (str "data corrupted: " mj " should have id " i ".")
          Exception.
          throw))
    mj))

(defn fill-jug  [jugs i]
  (let [mj (get-jug jugs i)]
    (assoc jugs i
           (->> (:capacity mj)
                (assoc mj :amount)))
    ))

(defn spill-jug  [jugs i]
  (let [mj (get-jug jugs i)]
    (assoc jugs i
           (->> 0
                (assoc mj :amount)))
    ))

(defn pour-from  [jugs i other-j]
  (let [this             (get-jug jugs i                      )
        that             (get-jug jugs other-j                )
        this-amount      (:amount   this                      )
        that-amount      (:amount   that                      )
        this-capacity    (:capacity this                      )
        available-source that-amount
        available-space  (- this-capacity this-amount         )
        amount-to-pour   (min available-space available-source)]
    (-> jugs
        (assoc i       (->> (+ this-amount amount-to-pour)
                            (assoc this :amount)))
        (assoc other-j (->> (- that-amount amount-to-pour)
                            (assoc that :amount))))
    ))

(defn rand-int-excluding [n i]
  (loop [k (rand-int n)]
            (if (== k i)
              (recur (rand-int n))
              k)))

(defn range-excluding [n i]
  (->> (range n)
       (filter #(not= i %))))

(defn gen-fill  [i]   `(fill-jug  ~i))
(defn gen-spill [i]   `(spill-jug ~i))
(defn gen-pours [n i] (map (fn [j] `(pour-from ~i ~j))
                           (range-excluding n i)))

(defn all-moves [jugs last-move]
  (let [n   (count jugs)
        all (range n   )]
    (filter
     #(not= % last-move)
     (concat (map gen-fill  all)
             (map gen-spill all)
             (mapcat #(gen-pours n %) all)
             ))))

(defn detect-win [jugs target]
  (== target
      (apply + (map :amount jugs))))

(defn random-move [jugs]
  (let [n (count jugs)
        i (rand-int n)
        j (rand-int-excluding n i)]
    (rand-nth `((fill-jug ~i)
                (spill-jug ~i)
                (pour-from ~i ~j)))))

(defn execute-move [jugs move]
  (eval `(-> ~jugs ~move)))

(defn check-put [a-set a-member]
  (let [in? (contains? a-set a-member)]
    (if (not in?)
      [false (conj a-set a-member)]
      [true  a-set])
    ))

(defn- queue [& stuff] (into clojure.lang.PersistentQueue/EMPTY stuff))
(def ^:private pp clojure.pprint/pprint)

(defn try-moves [state moves target seen iters]
  (if (or (not moves) (> iters 100)) nil
      (let [trials (map (fn [move] {:state (execute-move (:state state) move)
                                   :trace (conj (:trace state) move)})
                        moves)
            ]
        (let [wins (filter (fn [trial] (detect-win (:state trial) target))
                           trials)]
          (if wins wins
              (let [unseen-trials
                    (filter (fn [trial] (contains? seen (:state trial)))
                            trials)
                    new-seen (map (partial conj seen) (map :state trials))]
                (map try-moves
                     unseen-trials
                     (map all-moves unseen-trials
                          (map (fn [trial] (-> trial :trace pop)) unseen-trials))
                     (repeat (count unseen-trials) target)
                     (repeat (count unseen-trials) new-seen)
                     (repeat (count unseen-trials) (inc iters)))))))))

;;; Mutable-Ref variation (discouraged, but may be necessary due to perf)

(defn make-jug-refs  [capacities]
  (->>
   capacities
   (map-indexed (fn [i c] {:id i :capacity c :amount 0}))
   (map ref)
   vec))

(defn get-jug-ref  [jugs i]
  (let [mj (jugs i)]
    (if (not= i (:id  @mj))
      (-> (str "data corrupted: " @mj " should have id " i ".")
          Exception.
          throw))
    mj))

(defn fill-jug-ref  [jugs i]
  (let [mj (get-jug-ref jugs i)]
    (dosync (ref-set mj (assoc @mj :amount (:capacity @mj))))))

(defn set-jug-ref-attribute  [jugs i attr new-value]
  (let [mj (get-jug-ref jugs i)]
    (dosync (ref-set mj (assoc @mj attr new-value))))  )

(defn spill-jug-ref  [jugs i]
  (set-jug-ref-attribute   jugs i :amount 0))

(defn get-jug-ref-attribute  [jugs i attr]
  (-> jugs (get-jug-ref i) deref attr))

(defn pour-from-ref  [jugs i other]
  (dosync
   (let [this             (get-jug-ref jugs i                  )
         that             (get-jug-ref jugs other              )
         this-amount      (:amount   @this                     )
         that-amount      (:amount   @that                     )
         this-capacity    (:capacity @this                     )
         available-source that-amount
         available-space  (- this-capacity this-amount         )
         amount-to-pour   (min available-space available-source)]
     (ref-set this (assoc @this :amount (+ this-amount amount-to-pour)))
     (ref-set that (assoc @that :amount (- that-amount amount-to-pour))))))
