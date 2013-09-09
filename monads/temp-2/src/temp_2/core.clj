(ns temp-2.core
  (:use clojure.algo.monads))

(defn- randomly-error [] (< (rand) 0.10))
(defn- computation [] {})
(defn- authorize [computation]
  (if (randomly-error) (throw (Exception. "auth errored"))
                       {:auth-token "John's credentials"}))
(defn- read-database [auth-token]
  (if (randomly-error) (throw (Exception. "database errored"))
                       {:name "John", :PO 421357}))
(defn- call-web-service [database-results]
  (if (randomly-error) (throw (Exception. "ws1 errored"))
                       [{:item "camera"}, {:item "shoes"}]))
(defn- filter-ws [web-service-call-results]
  (if (randomly-error) (throw (Exception. "filter errored"))
                       [{:item "camera"}]))
(defn- call-other-web-service [database-results]
  (if (randomly-error) (throw (Exception. "ws2 errored"))
                       [{:price 420.00M}]))
(defn- combine [filtered-web-service-results
               other-web-service-call-results]
  (if (randomly-error) (throw (Exception. "combine errored"))
      (concat filtered-web-service-results
              other-web-service-call-results)))
#_(println
  (try
    (let [db-results
          (-> (computation)
              authorize
              read-database
              )]
      (-> db-results
          call-web-service
          filter-ws
          (combine (call-other-web-service db-results))))
    (catch Exception e (.getMessage e))))

(defmonad if-not-error-m
  [m-result (fn [value] value)
   m-bind   (fn [value f]
              (if-not (:error value)
                (f value) 
                value))
  ])

(defmacro with-em [expr]
  `(with-monad if-not-error-m ~expr))

(defmacro em-blind [em-a ex-result]
  `(with-em (m-bind ~em-a (fn [_#] (m-result ~ex-result)))))

(defn- computation [] (with-em (m-result {})))
(defn- authorize [computation]
  (em-blind computation
            (if (randomly-error) {:error "auth errored"}
                {:auth-token "John's credentials"})))
(defn- read-database [auth-token]
  (with-em
    (m-bind auth-token
            (fn [_] (m-result (if (randomly-error) {:error "database errored"}
                                 {:name "John", :PO 421357}))))))
(defn- call-web-service [database-results]
  (with-em
    (m-bind database-results
            (fn [_] (m-result (if (randomly-error) {:error "ws1 errored"}
                                 [{:item "camera"}, {:item "shoes"}]))))))
(defn- filter-ws [web-service-call-results]
  (with-em
    (m-bind web-service-call-results
            (fn [_] (m-result (if (randomly-error) {:error "filter errored"}
                                 [{:item "camera"}]))))))
(defn- call-other-web-service [database-results]
  (with-em
    (m-bind database-results
            (fn [_] (m-result (if (randomly-error) {:error "ws2 errored"}
                                 [{:price 420.00M}]))))))
(defn- combine [filtered-web-service-results
               other-web-service-call-results]
  (with-em
    (m-bind filtered-web-service-results
            (fn [m-filtered]
              (m-result (if (randomly-error) {:error "combine errored"}
                            (m-bind other-web-service-call-results
                                    (fn [m-other]
                                      (concat m-filtered
                                              m-other)))))))))

(println
 (with-monad if-not-error-m
   (let [db-results
         (-> (computation)
             authorize
             read-database)]
     (-> db-results
         call-web-service
         filter-ws
         (combine (call-other-web-service db-results))))
   ))

