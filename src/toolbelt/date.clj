(ns toolbelt.date
  (:refer-clojure :exclude [short])
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.spec.alpha :as s]))


;; =============================================================================
;; Formatting
;; =============================================================================


(def ^:private formatters
  {:short-date      (f/formatter "M/d/yy")
   :short-date-time (f/formatter "M/d/yy, h:mma")})


(defn short [date & [include-time]]
  (let [k (if include-time :short-date-time :short-date)]
    (f/unparse (get formatters k) (c/to-date-time date))))

(s/fdef short
        :args (s/cat :date inst? :time (s/? boolean?))
        :ret string?)


(def ^{:deprecated "0.2.0"} short-date
  short)


(defn ^{:deprecated "0.2.0"} short-date-time
  "Use `short` instead."
  [date]
  (short date true))


;; =============================================================================
;; Predicates
;; =============================================================================


(defn is-first-day-of-month? [d]
  (= (t/day (c/to-date-time d)) 1))

(s/fdef is-first-day-of-month?
        :args (s/cat :date inst?)
        :ret boolean?)


;; =============================================================================
;; Transformations
;; =============================================================================


(defn tz-corrected-dt [dt tz]
  (-> dt (t/from-time-zone tz) (t/to-time-zone t/utc)))


(def ^{:deprecated "0.2.0"} to-utc-corrected-date-time
  tz-corrected-dt)


(defn tz-corrected
  "Produce the UTC instant in time relative to timezone `tz`."
  [inst tz]
  (-> inst c/to-date-time (tz-corrected-dt tz) c/to-date))


(def ^{:deprecated "0.2.0"} to-utc-corrected-date
  tz-corrected)


(defn tz-uncorrected-dt [dt tz]
  (-> dt (t/to-time-zone tz) (t/from-time-zone t/utc)))


(def ^{:deprecated "0.2.0"} from-tz-date-time
  tz-uncorrected-dt)


(defn tz-uncorrected
  "Produce the absolute UTC instant from timezone `tz`."
  [inst tz]
  (-> inst c/to-date-time (tz-uncorrected-dt tz) c/to-date))


(def ^{:deprecated "0.2.0"} from-tz-date
  tz-uncorrected)


(defn end-of-day
  "Produce a date that is on the same day as `date`, but with time set to the
  last second in `date`."
  ([date]
   (end-of-day date t/utc))
  ([date tz]
   (let [[y m d] ((juxt t/year t/month t/day) (c/to-date-time date))]
     (-> (t/date-time y m d)
         (t/plus (t/days 1))
         (t/minus (t/seconds 1))
         (c/to-date)
         (to-utc-corrected-date tz)))))


(defn beginning-of-day
  "Produce a date that is on the same day as `date`, but with time set to the
  first second in `date`."
  ([date]
   (beginning-of-day date t/utc))
  ([date tz]
   (-> (c/to-date-time date)
       (t/floor t/day)
       (c/to-date)
       (to-utc-corrected-date tz))))


(defn beginning-of-month
  ([date]
   (beginning-of-month date t/utc))
  ([date tz]
   (-> date
       c/from-date
       t/first-day-of-the-month
       (beginning-of-day tz))))


(defn end-of-month
  ([date]
   (end-of-month date t/utc))
  ([date tz]
   (-> date
       c/from-date
       t/last-day-of-the-month
       (end-of-day tz))))


(defn- transform*
  [d f]
  (-> d
      c/to-date-time
      (f)
      c/to-date))


(defn plus
  "Returns a new java.util.Date corresponding to the given date (could be a long,
  date/time, or java.util.Date) moved forwards by the given Period(s).
  Using System/currentTimeMillis if no d is provided."
  ([p]
    (plus (System/currentTimeMillis) p))
  ([d p]
   (transform* d #(t/plus % p))))


(defn minus
  "Returns a new java.util.Date corresponding to the given date (could be a long,
  date/time, or java.util.Date) moved backwards by the given Period(s).
  Using System/currentTimeMillis if no d is provided."
  ([p]
   (plus (System/currentTimeMillis) p))
  ([d p]
   (transform* d #(t/minus % p))))


(defn interval
  "Returns an interval representing the span between the two given java.util.Date.
  Note that intervals are closed on the left and open on the right, and from < to."
  [from to]
  (t/interval (c/to-date-time from) (c/to-date-time to)))
