(ns expt1.core
  (:require [expt1.k2                :as k2     ]
            clojure.pprint
            [rx.lang.clojure.interop :as rx]
            )
  (:import [rx
            Observable
            subscriptions.Subscriptions
            subjects.Subject
            subjects.PublishSubject])
  )

(defmacro pdump [x]
  `(let [x# (try ~x (catch Exception e# (str e#)))]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         x#)))

(defn- or-default [val default] (if val (first val) default))

(defn subscribe-collectors [obl & optional-wait-time]
  (let [wait-time (or-default optional-wait-time 1000)
        onNextCollector      (agent    [])
        onErrorCollector     (atom    nil)
        onCompletedCollector (promise    )]
    (let [collect-next      (rx/action [item] (send onNextCollector
                                                    (fn [state] (conj state item))))
          collect-error     (rx/action [excp] (reset!  onErrorCollector     excp))
          collect-completed (rx/action [    ] (deliver onCompletedCollector true))
          report-collectors (fn [    ]
                              {:onCompleted (deref onCompletedCollector wait-time false)
                               :onNext      (do (await-for wait-time onNextCollector)
                                                @onNextCollector)
                               :onError     @onErrorCollector
                               })]
      (-> obl
          (.subscribe collect-next collect-error collect-completed))
      (report-collectors))))

(let [o1 (PublishSubject/create
          (rx/fn [obr]
            (.onNext obr 42)
            (.onNext obr 43)
            (.onNext obr 44)
            ))]
  (pdump (.onNext o1 45))
  (-> o1
      subscribe-collectors
      pdump))
