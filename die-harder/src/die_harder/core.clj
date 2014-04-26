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

(defn empty-jug  [jugs i]
  (let [mj (get-jug jugs i)]
    (assoc jugs i
           (->> 0
                (assoc mj :amount)))
    ))

(defn pour-from-other  [jugs i other]
  (let [this             (get-jug jugs i                      )
        that             (get-jug jugs other                  )
        this-amount      (:amount   this                      )
        that-amount      (:amount   that                      )
        this-capacity    (:capacity this                      )
        available-source that-amount
        available-space  (- this-capacity this-amount         )
        amount-to-pour   (min available-space available-source)]
    (-> jugs
        (assoc i     (->> (+ this-amount amount-to-pour)
                          (assoc this :amount)))
        (assoc other (->> (- that-amount amount-to-pour)
                          (assoc that :amount))))
    ))

;;; Mutable Ref variation (discouraged, but may be necessary due to perf)

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

(defn empty-jug-ref  [jugs i]
  (set-jug-ref-attribute   jugs i :amount 0))

(defn get-jug-ref-attribute  [jugs i attr]
  (-> jugs (get-jug-ref i) deref attr))

(defn pour-in-from-other-ref  [jugs i other]
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

#_(ns my.data)

#_(defrecord Employee [name surname])

; Namescape 2 in "my/queries.clj", where a defrecord is used
#_(ns my.queries
  (:require my.data)
  (:import [my.data Employee]))

#_(do
  "Employees named Albert:"
  (filter #(= "Albert" (.name %))
    [(Employee. "Albert" "Smith")
     (Employee. "John" "Maynard")
     (Employee. "Albert" "Cheng")]))
