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

(defn nu-q [& stuff] (into clojure.lang.PersistentQueue/EMPTY stuff))
(def  pp             clojure.pprint/pprint)
(defn- or-default
  "Fetch first optional value from function arguments preceded by &."
  [val default] (if val (first val) default))

(defn bfs-eager [tree & visitor-]
  (let [visitor (or-default visitor- identity)]
   (loop
       [ret [],
        queue (nu-q tree)]
     (if (seq queue)
       (let [[node & children] (peek queue)]
         (visitor node)
         (recur (conj ret node) (into (pop queue) children)))
       ret))))

(defn bfs-lazy [tree]
  ((fn step [queue]
     (lazy-seq
      (when (seq queue)
        (let [[node & children] (peek queue)]
          (cons node
                (step (into (pop queue) children)))))))
   (conj clojure.lang.PersistentQueue/EMPTY tree)))

(defn try-move
  [jugs
   move-q
   states-seen
   target]
  (let [move  (peek move-q)
        trial (execute-move jugs move)]
    (if (detect-win trial)
      [true trial]
      (map
       (all-moves jugs move)))))

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
