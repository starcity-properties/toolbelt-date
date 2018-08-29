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


(defn transform
  "'Transforms' a date where 'd' is a date of any type (timestamp,
  org.joda.DateTime, java.util.Date. etc), and returns a java.util.Date,
  after applying a transformation function 'f', which  is a function
  that takes a date/time instance and any supplied args and returns a date."
  [d f & args]
  (-> (apply f (c/to-date-time d) args)
      c/to-date))


(defn tz-corrected-dt [dt tz]
  (-> dt (t/from-time-zone tz) (t/to-time-zone t/utc)))


(def ^{:deprecated "0.2.0"} to-utc-corrected-date-time
  tz-corrected-dt)


(defn tz-corrected
  "Produce the UTC instant in time relative to timezone `tz`."
  [inst tz]
  (transform inst tz-corrected-dt tz))


(def ^{:deprecated "0.2.0"} to-utc-corrected-date
  tz-corrected)


(defn tz-uncorrected-dt [dt tz]
  (-> dt (t/to-time-zone tz) (t/from-time-zone t/utc)))


(def ^{:deprecated "0.2.0"} from-tz-date-time
  tz-uncorrected-dt)


(defn tz-uncorrected
  "Produce the absolute UTC instant from timezone `tz`."
  [inst tz]
  (transform inst tz-uncorrected-dt tz))


(def ^{:deprecated "0.2.0"} from-tz-date
  tz-uncorrected)


(defn end-of-day
  "Returns a java.util.Date representing the time 23:59:59 of the given date
  in timezone 'tz'. Arity 1 version: uses 'utc' timezone.

  'date' is a date instance as per 'toolbelt.date/transform'."
  ([date]
   (end-of-day date t/utc))
  ([date tz]
   (let [[y m d] ((juxt t/year t/month t/day) (c/to-date-time date))
         eod (t/date-time y m d 23 59 59)]
     (to-utc-corrected-date eod tz))))


(defn beginning-of-day
  "Returns a java.util.Date representing the time 00:00:01 of the given date
  in timezone 'tz'. Arity 1 version: uses 'utc' timezone.

  'date' is a date instance as per 'toolbelt.date/transform'."
  ([date]
   (beginning-of-day date t/utc))
  ([date tz]
   (-> (transform date t/floor t/day)
       (to-utc-corrected-date tz))))


(defn beginning-of-month
  "Returns a java.util.Date representing the first day of the month of
  the given date in timezone 'tz'. Arity 1 version: uses 'utc' timezone.

  'date' is a date instance as per 'toolbelt.date/transform'."
  ([date]
   (beginning-of-month date t/utc))
  ([date tz]
   (-> date
       (transform t/first-day-of-the-month)
       (beginning-of-day tz))))


(defn end-of-month
  "Returns a java.util.Date representing the first day of the month of
  the given date in timezone 'tz'. Arity 1 version: uses 'utc' timezone.

  'date' is a date instance as per 'toolbelt.date/transform'."
  ([date]
   (end-of-month date t/utc))
  ([date tz]
   (-> date
       (transform t/last-day-of-the-month)
       (end-of-day tz))))


(defn plus
  "Transforms a given date and returns a new java.util.Date moved forwards by the
  given Period(s). Arity 1 version: moved forwards from System/currentTimeMillis.

  'date' is a date instance as per 'toolbelt.date/transform'."
  ([period]
    (plus (System/currentTimeMillis) period))
  ([date period]
   (transform date t/plus period)))


(defn minus
  "Transforms a given date and returns a new java.util.Date moved backwards by the
  given Period(s). Arity 1 version: moved backwards from System/currentTimeMillis.

  'date' is a date instance as per 'toolbelt.date/transform'."
  ([period]
   (plus (System/currentTimeMillis) period))
  ([date period]
   (transform date t/minus period)))


(defn interval
  "Returns an interval representing the span between the two given dates.
  Note that intervals are closed on the left and open on the right.

  'from' and 'to' are date instances of any type where 'from' is before 'to'."
  [from to]
  (t/interval (c/to-date-time from) (c/to-date-time to)))


;; =============================================================================
;; Components
;; =============================================================================


(defn day
  "Return the day of month component of the given date.

  'date' is a date instance of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date]
  (t/day (c/to-date-time date)))


(defn month
  "Return the month component of the given date.

  'date' is a date instance of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date]
  (t/month (c/to-date-time date)))


(defn year
  "Return the year component of the given date.

  'date' is a date instance of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date]
  (t/year (c/to-date-time date)))

