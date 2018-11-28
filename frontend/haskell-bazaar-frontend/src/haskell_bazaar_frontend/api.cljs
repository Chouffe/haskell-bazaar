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

(defn search [url]
  (str url "search"))

(defn items [url]
  (str url "items"))

(defn keywords [url]
  (str url "keywords"))

(defn feedback [url]
  (str url "feedback"))

(defn item-url [url uuid search-query]
  (str url "item-url/" uuid "?q=" search-query))

(defn mailing-list-subscribe [url]
  (str url "mailing-list/subscribe"))

(def response-format
  (ajax/json-response-format {:keywords? true}))
