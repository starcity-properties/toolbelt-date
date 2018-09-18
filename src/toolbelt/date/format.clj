(ns toolbelt.date.format
  (:refer-clojure :exclude [short])
  (:require
    [clj-time.format :as f]
    [clj-time.coerce :as c]
    [clojure.spec.alpha :as s]
    [toolbelt.date :as date]))

(def ^:private formatters
  {:short-date      (f/formatter "M/d/yy")
   :short-date-time (f/formatter "M/d/yy, h:mma")})


(defn short [date & [include-time]]
  (let [k (if include-time :short-date-time :short-date)]
    (f/unparse (formatters k) (c/to-date-time date))))

(s/fdef short
        :args (s/cat :date inst? :time (s/? boolean?))
        :ret string?)