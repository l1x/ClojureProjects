(ns server-1.test.handler
  (:use clojure.test
        ring.mock.request
        server-1.handler))

(deftest test-app
  (testing "main route"
    (let [response (app (request :get "/"))]
      (is (= (:status response) 200))
      (is (= (:body response) "Hello World"))))

  (testing "main post"
    (let [res (app (request :post "/"))]
      (is (= (:stauts res 200)))
      (is (= (:body res) "Hello Post"))))

  (testing "not-found route"
    (let [response (app (request :get "/invalid"))]
      (is (= (:status response) 404)))))
