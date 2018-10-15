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

(def ^:private default-formatter (formatters :short-date))


(defn short [date & [include-time]]
  (let [k (if include-time :short-date-time :short-date)]
    (f/unparse (formatters k) (c/to-date-time date))))

(s/fdef short
        :args (s/cat :date inst? :time (s/? boolean?))
        :ret string?)


(defn display
  "Returns a string representing the given DateTime instance in UTC and in the
  form determined by the given formatter."
  ([dt k]
    (display dt nil k))
  ([dt tz k]
   (f/unparse (get formatters k default-formatter)
              (if (some? tz)
                (date/tz-uncorrected dt tz)
                dt))))