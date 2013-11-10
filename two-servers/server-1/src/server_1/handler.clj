(ns server-1.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route   :as route]
            [clojure.pprint    :as pp]))

(defroutes app-routes
  (GET "/"  [] "Hello World")
  (POST "/" []
        (pp/pprint "in GET")
        "Hello Post")
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (handler/site app-routes))
