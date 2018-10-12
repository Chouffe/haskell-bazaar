(ns haskell-bazaar-frontend.api
  (:require [ajax.core :as ajax]))

(defn environment []
  (if goog.DEBUG :dev :prod))

(defn- host [environment]
  (case environment
    :prod "https://haskell-bazaar.com"
    :dev  "http://localhost"
    :test "http://localhost"))

(defn- port [environment]
  (case environment
    :prod 80
    :dev  8002
    :test 8002))

(defn base-url [environment]
  (case environment
    :prod (str "/api/v0/")
    (str (host environment) ":" (port environment) "/api/v0/" )))

(defn search [url search-query]
  (str url "search?q=" search-query))

(defn keywords [url]
  (str url "keywords"))

(defn item-url [url uuid]
  (str url "item-url/" uuid))

(def response-format
  (ajax/json-response-format {:keywords? true}))
