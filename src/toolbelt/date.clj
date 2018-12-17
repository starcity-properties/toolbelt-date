(ns toolbelt.date
  (:refer-clojure :exclude [short > < <= >= min max])
  (:require [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [clojure.spec.alpha :as s])
  (:import (org.joda.time Period)
           (java.time ZoneId Instant)
           (java.util Date)
           (java.time.format DateTimeFormatter)))


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


(defn- timezone*
  "Returns a timezone for a supplied timezone id the timszone itself"
  [tz]
  (if (string? tz)
    (t/time-zone-for-id tz)
    tz))

(defn- norm-out*
  "Normalize the returned date value to a java.util.Date."
  [date]
  (c/to-date date))


(defn transform
  "'Transforms' a date where 'd' is a date of any type (timestamp,
  org.joda.DateTime, java.util.Date. etc), and returns a java.util.Date,
  after applying a transformation function 'f', which  is a function
  that takes a date/time instance and any supplied args and returns a date."
  [d f & args]
  (-> (apply f (c/to-date-time d) args)
      c/to-date))


(defn tz-corrected-dt [dt tz]
  (-> dt (t/from-time-zone (timezone* tz)) (t/to-time-zone t/utc)))


(defn tz-corrected
  "Produce the UTC instant in time relative to timezone `tz`."
  [inst tz]
  (transform inst tz-corrected-dt (timezone* tz)))


(defn tz-uncorrected-dt [dt tz]
  (-> dt (t/to-time-zone (timezone* tz)) (t/from-time-zone t/utc)))


(defn tz-uncorrected
  "Produce the absolute UTC instant from timezone `tz`."
  [inst tz]
  (transform inst tz-uncorrected-dt (timezone* tz)))


(defn start-of-day-local
  "Returns a java.util.Date representing the time 00:00:00 of the given date
  in timezone 'tz'.
  Arity 1: Adjust `date` in UTC time.
  Arity 2: Adjust `date` with respect to timezone `tz`.
  'date' is a date instance as per 'toolbelt.date/transform'."
  ([date]
   (start-of-day-local date "UTC"))
  ([date tz]
   (-> (transform date t/floor t/day)
       (tz-corrected tz))))


(defn start-of-day-utc
  "Takes a 'date' in UTC time representing a time in 'timezone', and returns the UTC corrected
  start of day of the local date in 'timezone'.

  Note: Will convert 'date' to local time in 'timezone' before calculating start of day, input should be in UTC.
  E.g.
  'date' represents Oct 12 at 23:00 in Pacific time so the input will be Oct 13 at 06:00.
  Returned will be Oct 12 at 07:00, which represents start of day of Oct 12 in Pacific time 'timezone'."
  ([date]
   (start-of-day-utc date "UTC"))
  ([date timezone]
   (start-of-day-utc date timezone timezone))
  ([date from-tz to-tz]
   (-> (tz-uncorrected date from-tz)
       (transform t/floor t/day)
       (tz-corrected to-tz))))


(defn start-of-month-local
  "Returns a java.util.Date representing the first day of the month of
  the given date in timezone 'tz'.
  Arity 1: Adjust `date` in UTC time.
  Arity 2: Adjust `date` with respect to timezone `tz`.
  'date' is a date instance as per 'toolbelt.date/transform'."
  ([date]
   (start-of-month-local date t/utc))
  ([date tz]
   (-> date
       (transform t/first-day-of-the-month)
       (start-of-day-local tz))))


(defn start-of-month-utc
  "Takes a 'date' in UTC time representing a time in 'timezone', and returns the UTC corrected
  start of the month of the local date in 'timezone'.

  Note: Will convert 'date' to local time in 'timezone' before calculating start of month, input should be in UTC.
  E.g.
  'date' represents Oct 31 at 23:00 in Pacific time so the input will be Nov 1 at 06:00.
  Returned will be Oct 1 at 07:00, which represents start of Oct in Pacific time 'timezone'."
  ([date]
   (start-of-month-utc date "UTC"))
  ([date timezone]
   (start-of-month-utc date timezone timezone))
  ([date from-tz to-tz]
   (-> (tz-uncorrected date from-tz)
       (start-of-month-local to-tz))))


(defn end-of-day-local
  "Returns a java.util.Date representing the time 23:59:59 of the given date
  in timezone 'tz'.
  Arity 1: Adjust `date` in UTC time.
  Arity 2: Adjust `date` with respect to timezone `tz`.
  'date' is a date instance as per 'toolbelt.date/transform'."
  ([date]
   (end-of-day-local date "UTC"))
  ([date tz]
   (let [[y m d] ((juxt t/year t/month t/day) (c/to-date-time date))
         eod (t/date-time y m d 23 59 59)]
     (tz-corrected eod tz))))


(defn end-of-day-utc
  "Takes a 'date' in UTC time representing a time in 'timezone', and returns a UTC corrected date representing the
  end of day of the local date in 'timezone'.

  Note: Will convert 'date' to local time in 'timezone' before calculating end of day, input should be in UTC.
  E.g.
  'date' represents Oct 12 at 23:00 in Pacific time so the input will be Oct 13 at 06:00.
  Returned will be Oct 13 at 06:59:59, which represents end of day of Oct 12 in Pacific time 'timezone'.
  Arity 1: Adjust `date` from and to UTC timezone.
  Arity 2: Adjust `date` from and to the supplied `timezone`.
  Arity 3: Adjust `date` from `from-tz` and return a new date represented in `to-tz`."
  ([date]
   (end-of-day-utc date "UTC"))
  ([date timezone]
   (end-of-day-utc date timezone timezone))
  ([date from-tz to-tz]
   (-> (tz-uncorrected date from-tz)
       (end-of-day-local to-tz))))


(defn end-of-month-local
  "Takes a `date` in local time and returns a new date representing the end of day of the last day
  of the month corrected to UTC.
  Arity 1: Adjust `date` in UTC time.
  Arity 2: Adjust `date` with respect to timezone `tz`."
  ([date]
   (end-of-month-local date t/utc))
  ([date tz]
   (-> date
       (transform t/last-day-of-the-month)
       (end-of-day-local tz))))


(defn end-of-month-utc
  "Takes a 'date' in UTC time representing a time in 'timezone', and returns the UTC corrected
  end of the month of the local date in 'timezone'.

  Note: Will convert 'date' to local time in 'timezone' before calculating start of month, input should be in UTC.
  E.g.
  'date' represents Oct 31 at 23:00 in Pacific time so the input will be Nov 1 at 06:00.
  Returned will be Oct 1 at 06:59:59, which represents Oct 31 23:59:59 in Pacific time 'timezone'.
  Arity 1: Adjust `date` from and to UTC timezone.
  Arity 2: Adjust `date` from and to the supplied `timezone`.
  Arity 3: Adjust `date` from `from-tz` and return a new date represented in `to-tz`."
  ([date]
   (end-of-month-utc date "UTC"))
  ([date timezone]
   (end-of-month-utc date timezone timezone))
  ([date from-tz to-tz]
   (-> (tz-uncorrected date from-tz)
       (end-of-month-local to-tz))))


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
   (minus (System/currentTimeMillis) period))
  ([date period]
   (transform date t/minus period)))


(defn interval
  "Returns an interval representing the span between the two given dates.
  Note that intervals are closed on the left and open on the right.

  'from' and 'to' are date instances of any type where 'from' is before 'to'."
  [from to]
  (t/interval (c/to-date-time from) (c/to-date-time to)))


(defn overlap
  "Returns an interval representing the overlap between the two supplied intervals."
  [i1 i2]
  (t/overlap i1 i2))


(defn within?
  "With 2 arguments: Returns true if the given Interval contains the given
   date. Note that if the date is exactly equal to the
   end of the interval, this function returns false.
   With 3 arguments: Returns true if the start date is
   equal to or before and the end date is equal to or after the test
   ReadablePartial."
  ([interval test]
   (t/within? interval (c/to-date-time test)))
  ([start end test]
   (apply t/within? (map c/to-date-time [start end test]))))


(defn in-days
  "Return the interval or period in days."
  [p]
  (t/in-days p))


(defn in-months
  "Return the interval or period in months."
  [p]
  (t/in-months p))


(defn in
  "Return the interval or period `p` in `unit` specified by a keyword:
  #{:years :months :days :weeks :hours :minutes :seconds :millis}"
  [unit p]
  (let [fns {:years   t/in-years
             :months  t/in-months
             :days    t/in-days
             :weeks   t/in-weeks
             :hours   t/in-hours
             :minutes t/in-minutes
             :seconds t/in-seconds
             :millis  t/in-millis}]
    ((fns unit (constantly -1)) p)))


(defn days
  "Given a number, returns a Period representing that many days."
  [n]
  (t/days n))


(defn months
  "Given a number, returns a Period representing that many months."
  [n]
  (t/months n))


(defn to-map
  "Returns a map with keys and values representing the supplied `date`. The returned map includes
  the following keys: #{:year :month :day :hour :minute :second :millisecond :day-of-week :week-of-year}
  with corresponding number values."
  [date]
  (let [[year month day hour minute second millis dow woy] ((juxt t/year t/month t/day t/hour t/minute t/second t/milli t/day-of-week t/week-number-of-year)
                                                             (c/to-date-time date))]
    {:year         year
     :month        month
     :day          day
     :hour         hour
     :minute       minute
     :second       second
     :millisecond  millis
     :day-of-week  dow
     :week-of-year woy}))


;; =============================================================================
;; Coercions
;; =============================================================================


(defn from-map
  "Returns a date given a map with keys and values representing a date time. Considers the following
  keys: #{:year :month :day :hour :minute :second :millisecond} with corresponding number values
  when creating the date."
  [{:keys [year month day hour minute second millisecond]}]
  (let [[y m d h min sec mill] (mapv (fnil identity 0) [year month day hour minute second millisecond])]
    (norm-out* (t/date-time y m d h min sec mill))))


(defn to-unix-time
  "Return the number of seconds after the Unix time
  Arity 1: returns the number of seconds after Unix time.
  Arity 2: returns the time in `unit` after Unix time: #{:millis :seconds}"
  ([dt]
   (to-unix-time dt :millis))
  ([dt unit]
   (when-some [date-long (c/to-long dt)]
     (case unit
       :seconds (quot date-long 1000)
       :millis date-long))))



(defn from-unix-time-millis
  "Return a date given the supplied number of millseconds `millis` since the UNIX time."
  [millis]
  (c/to-date millis))


(defn from-unix-time-secs
  "Return a date given the supplied number of seconds `millis` since the UNIX time."
  [secs]
  (from-unix-time-millis (* 1000 secs)))


(defn from-ISO
  "Parse an ISO instant in UTC and return a java date, where the ISO input is of the form:
  \"2018-12-13T16:02:36.815Z\"."
  [iso-instant]
  (let [acc (.parse (DateTimeFormatter/ISO_INSTANT) iso-instant)]
    (Date/from (Instant/from acc))))


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


;; =============================================================================
;; Periods
;; =============================================================================


(defn period
  "Given some keys with values, returns a Period that represents that amount of time.

  E.g. (period :months 2 :days 1) returns a Period representing a time period of
  2 months and 2 days.

  Possible keys are:
  #{:years :months :days :weeks :hours :minutes :seconds :millis}"
  [& {:keys [years months days weeks hours minutes seconds millis] :as keyvals}]
  (let [[y m w d h min sec mill] (mapv (fnil identity 0)
                                       [years months weeks days hours minutes seconds millis])]
    (Period. y m w d h min sec mill)))


;; =============================================================================
;; Operators
;; =============================================================================

(defn- compare*
  [f d & more]
  (if-some [unix-times (not-empty (map c/to-long more))]
    (boolean
      (apply f
             (c/to-long d)
             unix-times))
    true))


(defn <
  "Returns true if dates are in monotonically increasing order,
  otherwise false.

  Arguments are date instances of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date & more]
  (apply compare* clojure.core/< date more))


(defn <=
  "Returns true if dates are in monotonically non-decreasing order,
  otherwise false.

  Arguments are date instances of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date & more]
  (apply compare* clojure.core/<= date more))


(defn >
  "Returns true if dates are in monotonically decreasing order,
  otherwise false.

  Arguments are date instances of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date & more]
  (apply compare* clojure.core/> date more))


(defn >=
  "Returns true if dates are in monotonically non-increasing order,
  otherwise false.

  Arguments are date instances of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date & more]
  (apply compare* clojure.core/>= date more))


(defn max
  "Returns the largest date, i.e. the latest in time."
  [date & more]
  (norm-out* (t/latest (cons date more))))


(defn min
  "Returns the smallest date, i.e. the earliest in time."
  [date & more]
  (norm-out* (t/earliest (cons date more))))


;; =============================================================================
;; Helpers for next/previous
;; =============================================================================


(defn next-day
  [date]
  (plus date (days 1)))


(defn next-month
  [date]
  (plus date (months 1)))


;; =============================================================================
;; Properties
;; =============================================================================


(defn days-in-month
  "Takes a `date` in UTC time and returns the number of days in the month of the local time in `timezone`
  the date represents."
  ([date]
   (days-in-month date "UTC"))
  ([date timezone]
   (-> (.toInstant (c/to-date date))
       (.atZone (ZoneId/of timezone))
       (.toLocalDate)
       (.lengthOfMonth))))

(s/fdef days-in-month
  :args (s/cat :date (s/or :inst inst? :unix integer?)
               :timezone (s/? string?)))


;; =============================================================================
;; Deprecated
;; =============================================================================


(defmacro defdeprecated
  "Helper macro to def deprecated functions and invoke the new implementation. Taken from leiningen:
  https://github.com/technomancy/leiningen/commit/6e18fc495d485acb6942d08f3719a644f697bf27"
  [old new]
  `(let [new# ~(str (.getName (:ns (meta (resolve new)))) "/" (name new))
         warn# (delay (println "Warning:" '~old "is deprecated; use" new#))]
     (defn ~(vary-meta old assoc :doc (format "Compatibility alias for %s" new))
       [& args#]
       (force warn#)
       (apply ~(resolve new) args#))))


(def ^{:deprecated "0.2.0"} short-date short)

(defn ^{:deprecated "0.2.0"} short-date-time
  "Use `short` instead."
  [date]
  (short date true))


(declare ^{:deprecated "0.2.0"} from-tz-date)
(defdeprecated from-tz-date tz-uncorrected)

(declare ^{:deprecated "0.2.0"} from-tz-date-time)
(defdeprecated from-tz-date-time tz-uncorrected)

(declare ^{:deprecated "0.2.0"} to-utc-corrected-date-time)
(defdeprecated to-utc-corrected-date-time tz-corrected)

(declare ^{:deprecated "0.2.0"} to-utc-corrected-date)
(defdeprecated to-utc-corrected-date tz-corrected)

(defdeprecated short-date short)
(defdeprecated short-date-time short)

(declare ^{:deprecated "0.4.2"} beginning-of-day)
(defdeprecated beginning-of-day start-of-day-local)

(declare ^{:deprecated "0.4.2"} beginning-of-month)
(defdeprecated beginning-of-month start-of-month-utc)

(declare ^{:deprecated "0.4.2"} end-of-day)
(defdeprecated end-of-day end-of-day-local)

(declare ^{:deprecated "0.4.2"} end-of-month)
(defdeprecated end-of-month end-of-month-utc)