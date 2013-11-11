(ns server-1.handler
  (:use compojure.core)
  (:require [compojure.handler   :as handler ]
            [compojure.route     :as route   ]
            [clojure.pprint      :as pp      ]
            [clojure.data.json   :as json    ]
            [clj-http.client     :as client  ])
  (:import [rx
            Observable
            Observer
            subscriptions.Subscriptions
            subjects.Subject
            subjects.PublishSubject])
  )

(defn- java-io-ByteArrayInputStream-to-string [jiobais]
  (with-open [r jiobais]
    (loop [b (.read r) v []]
      (if (= b -1)
        (apply str v)
        (recur (.read r) (conj v (char b)))))))

(let [rsub (PublishSubject/create)]
  (defroutes app-routes
    (GET "/"  [] "Hello World")
    (POST "/" {payload :body}
          (pp/pprint "in POST")
          (let [payload-json
                (java-io-ByteArrayInputStream-to-string payload)
                payload-clj
                (json/read-str payload-json :key-fn keyword)]
            (pp/pprint payload-clj)
            payload-json))
    (route/resources "/")
    (route/not-found "Not Found")))

(def app
  (handler/site app-routes))
