(defproject server-1 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.5"]
                 [clj-http                          "0.7.1"]
                 [org.thnetos/cd-client             "0.3.6"]
                 ;; ;; [org.slf4j/log4j-over-slf4j        "1.6.6"]
                 [org.slf4j/slf4j-log4j12           "1.7.5"]
                 [enlive                            "1.1.1"]
                 ;; [org.clojure/data.json             "0.2.2"]
                 ;; ;; [com.netflix.rxjava/rxjava-core    "0.9.1-SNAPSHOT"]
                 ;; ;; [com.netflix.rxjava/rxjava-clojure "0.9.1-SNAPSHOT"]
                 ;; [com.netflix.rxjava/rxjava-clojure "0.12.0"]
                 ;; [clojail                           "1.0.6"]
                 ]
  :plugins [[lein-ring "0.8.3"]]
  :ring {:handler server-1.handler/app}
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]]}})
