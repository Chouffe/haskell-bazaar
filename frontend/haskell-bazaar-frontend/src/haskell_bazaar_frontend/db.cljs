(ns haskell-bazaar-frontend.db
  (:require [re-frame.core :as re-frame]
            [cljs.spec.alpha :as s]

            [haskell-bazaar-frontend.utils :as utils]
            [haskell-bazaar-frontend.environment :as env]

            ;; TODO: remove stubs: only for debug mode
            [haskell-bazaar-frontend.stubs :as stubs]
            ))

;; TODO: spec it out!
(def default-db
  {:environment    (env/environment) ;; dev/prod/test environment
   :search-query   nil
   :search-error   nil
   :search-loading false
   :showing        :all
   :sorted         :date
   :keywords       []
   :items          (utils/uuid-coll->hashmap stubs/search-result)
   ; :items          {}
   })
