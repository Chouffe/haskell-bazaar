(ns haskell-bazaar-frontend.db
  (:require
    [cljs.spec.alpha :as s]

    [re-frame.core :as re-frame]
    [datascript.core :as d]

    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.environment :as env]
    [haskell-bazaar-frontend.stubs :as stubs]))

;; TODO: move to a util namespace
(def showing->title
  {:all "all"
   :videos "videos"
   :papers "papers"
   :blog-posts "blog posts"
   :tutorials "tutorials"})

;; Clojure.Spec
;; ------------

;; Environment
(s/def ::environment #{:prod :dev :test})

;; Search
(s/def ::search-query (s/nilable string?))
(s/def ::search-error (s/nilable string?))
(s/def ::search-loading boolean?)
(s/def ::showing (set (keys showing->title)))
(s/def ::sorted #{:date})

;; Keyword
(s/def ::name string?)
(s/def ::kind #{"tag", "author"})
(s/def ::keyword (s/or
                   :author (s/keys :req-un [::kind ::firstName ::lastName])
                   :tag (s/keys :req-un [::kind ::name])))
(s/def ::keywords (s/coll-of ::keyword))

;; Tags
(s/def ::tag (s/keys :req-un [::name]))
(s/def ::tags (s/coll-of ::tag))

;; Author
(s/def ::lastName string?)
(s/def ::firstName string?)
(s/def ::author (s/keys :req-un [::lastName ::uuid ::firstName]))
(s/def ::authors (s/coll-of ::author :kind vector?)) ;; TODO: add :distinct true here

;; Item
(s/def ::uuid string?)
(s/def ::title string?)
(s/def ::description string?)
; (s/def ::type #{"Video"})
(s/def ::type (s/keys :req-un [::item-type]))
(s/def ::item-type #{:Video :Tutorial :Book :Article})
(s/def ::item
  (s/keys :req-un [::uuid
                   ::title
                   ::type
                   ::description]
          :opt [::tags ::authors]))
(s/def ::items (s/map-of ::uuid ::item))
(s/def ::search-item
  (s/keys :req-un [::uuid
                   ::title
                   ::type
                   ::description]
          :opt [::tags ::authors]))
(s/def ::search-items (s/map-of ::uuid ::search-item))

;; App-state
(s/def ::app-state
  (s/keys :req-un [::environment
                   ::search-query
                   ::search-error
                   ::search-loading
                   ::search-items
                   ::showing
                   ::sorted
                   ::keywords
                   ::items]))

;; TODO: add caching layer for search already performed in an interceptor
(defn default-db [env]
  {:environment    env
   :search-query   nil
   :search-error   nil
   :search-loading false
   :search-items   {}
   :showing        :all
   :sorted         :date
   ;; TODO: deprecate :keywords?
   :keywords       []
   ;; TODO: deprecate :items
   :items          {}
   ; :items          (utils/uuid-coll->hashmap stubs/search-result)
   })

(def title->showing
  (zipmap (vals showing->title) (keys showing->title)))
