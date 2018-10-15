(ns toolbelt.date
  (:refer-clojure :exclude [assoc short > < <= >=])
  (:require
    [clj-time.core :as t]
    [clj-time.format :as f]
    [clj-time.coerce :as c]
    [clojure.spec.alpha :as s]
    [clojure.set :as sets])
  (:import (org.joda.time Period)))


(declare transform)

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
;; Timezone helpers
;; =============================================================================


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



;; =============================================================================
;; Transformations
;; =============================================================================

(def ^:private transform-fns
  {:nth-of-month (fn [dt n] (t/nth-day-of-the-month dt n))})


(defn- norm-in [d]
  (c/to-date-time d))


(defn- norm-out [d]
  (c/to-date d))


(defn transform
  "'Transforms' a date where 'd' is a date of any type (timestamp,
  org.joda.DateTime, java.util.Date. etc), and returns a java.util.Date,
  after applying a transformation function 'f', which  is a function
  that takes a date/time instance and any supplied args and returns a date."
  [d f & args]
  (norm-out
    (apply (or (transform-fns f) f)
           (norm-in d) args)))


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
  "Returns a java.util.Date representing the time 00:00:00 of the given date
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


(defn interval
  "Returns an interval representing the span between the two given dates.
  Note that intervals are closed on the left and open on the right.

  'from' and 'to' are date instances of any type where 'from' is before 'to'."
  [from to]
  (t/interval (norm-in from) (norm-in to)))


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


(defn days
  "Given a number, returns a Period representing that many days."
  [n]
  (t/days n))


(defn months
  "Given a number, returns a Period representing that many months."
  [n]
  (t/months n))


;; =============================================================================
;; Helpers
;; =============================================================================


(def ^:private in-fns
  {:millisecs t/in-millis
   :seconds   t/in-seconds
   :minutes   t/in-minutes
   :hours     t/in-hours
   :days      t/in-days
   :weeks     t/in-weeks
   :months    t/in-months
   :years     t/in-years})


(def ^:private field-fns
  {:millisec    t/milli
   :second      t/second
   :minute      t/minute
   :hour        t/hour
   :day-of-week t/day-of-week
   :day         t/day
   :month       t/month
   :year        t/year})


(defn- error-unknown-keys [f supported-ks ks]
  (let [supported-set (set supported-ks)
        test-set      (set ks)]
    (when-some [unknown (not-empty (sets/difference test-set
                                                    supported-set))]
      (throw (ex-info (str "unrecognized key: " unknown ", supported are " supported-set)
                      {:fn             f
                       :supported-keys supported-set
                       :unknown-keys   unknown})))))


;; =============================================================================
;; Time units
;; =============================================================================


(defn in-unit
  "For the given Period or Interval 'p', returns the time in the specified 'ks' time unit(s).
   Possible units are #{:millis :seconds :minutes :hours :days :weeks :months :years}.

   Returns a value if 'ks' is empty, otherwise a collection of values."
  [p k & ks]
  (error-unknown-keys in-unit (keys in-fns) (cons k ks))
  (let [fns (map in-fns (cons k ks))]
    (cond-> ((apply juxt fns) p)
            (empty? ks)
            first)))


(defn time-between
  "Returns the time in the specified 'ks' time unit(s) between two dates 'start' (incl) and 'end'
  (excl).
   Possible time units are #{:millis :seconds :minutes :hours :days :weeks :months :years}.

   Returns a value if 'ks' is empty, otherwise a collection of values."
  [start end k & ks]
  (error-unknown-keys time-between (keys in-fns) (cons k ks))
  (apply in-unit (interval start end) k ks))


;; =============================================================================
;; Fields
;; =============================================================================


(defn field
  "Returns the fields 'ks' of a given date where 'd' is a date instance of
  any type (e.g. timestamp, long, joda-date) and possible values for 'k' and
  'ks' are:
  #{:second :minute :hour :day-of-week :day :month :year}.

  Returns a value if 'ks' is empty, otherwise a collection of values."
  [d k & ks]
  (error-unknown-keys field (keys field-fns) (cons k ks))
  (let [fns (map field-fns (cons k ks))]
    (cond-> ((apply juxt fns) (norm-in d))
            (empty? ks)
            first)))


(defn day
  [d]
  (field d :day))


(defn period
  "Given some keys with values, returns a Period that represents that amount of time.

  E.g. (period :months 2 :days 1) returns a Period representing a time period of
  2 months and 2 days.

  Possible keys are:
  #{:years :months :days :weeks :hours :minutes :seconds :millis}"
  [& {:keys [years months days weeks hours minutes seconds millis] :as keyvals}]
  (error-unknown-keys period
                      #{:years :months :days :weeks :hours :minutes :seconds :millis}
                      (keys keyvals))

  (let [[y m w d h min sec mill] (mapv (fnil identity 0)
                                       [years months weeks days hours minutes seconds millis])]
    (Period. y m w d h min sec mill)))


;; =============================================================================
;; Operators
;; =============================================================================

(defn- compare*
  "Return true if compare fn 'f' is satisfied for all dates 'ds' applied in order.
  Otherwise false.

  'ds' are date instances of any type. Returns true if only one is provided."
  [f d & ds]
  (if-some [unix-times (not-empty (map c/to-long ds))]
    (boolean
      (apply f (c/to-long d) unix-times))
    true))


(defn <
  "Returns true if dates are in monotonically increasing order,
  otherwise false.

  Arguments are date instances of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date & ds]
  (apply compare* clojure.core/< date ds))


(defn <=
  "Returns true if dates are in monotonically non-decreasing order,
  otherwise false.

  Arguments are date instances of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date & ds]
  (apply compare* clojure.core/<= date ds))


(defn >
  "Returns true if dates are in monotonically decreasing order,
  otherwise false.

  Arguments are date instances of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date & ds]
  (apply compare* clojure.core/> date ds))


(defn >=
  "Returns true if dates are in monotonically non-increasing order,
  otherwise false.

  Arguments are date instances of any type (e.g. timestamp, java-date,
  date/time etc.)"
  [date & ds]
  (apply compare* clojure.core/>= date ds))


(defn plus
  "Transforms a given date and returns a new java.util.Date:
  Arity 1: moved forwards by given period 'p' from System/currentTimeMillis.
  Arity 2: moved forwards by given Period 'p'.
  Arity 3 or more: moved backwards by the given field specifiers as specified in
  toolbelt.date/period.

  Caution: The function will add each element of a period one by one from largest to smallest. Note that certain
  elements are of variable length (e.g. months), so when mixed with others it cannot be guaranteed that adding
  and subtracting the same period will give the same result as the initial value.

  'date' is a date instance as per 'toolbelt.date/transform'."
  ([p]
   (plus (System/currentTimeMillis) p))
  ([date p]
   (transform date t/plus p))
  ([date k v & keyvals]
   (plus date (apply period k v keyvals))))


(defn minus
  "Transforms a given date and returns a new java.util.Date moved backwards by the
  given Period(s). Note that month is of variable length, so the order in which operations
  occur will matter.
  Arity 1: moved backwards by given period 'p' from System/currentTimeMillis.
  Arity 2: moved backwards by given Period 'p'.
  Arity 3 or more: moved backwards by the given field specifiers as specified in
  toolbelt.date/period.

  Caution: The function will subtract each element of a period one by one from largest to smallest. Note that certain
  elements are of variable length (e.g. months), so when mixed with others it cannot be guaranteed that adding and
  subtracting the same period will give the same result as the initial value.

  'date' is a date instance as per 'toolbelt.date/transform'."
  ([p]
   (minus (System/currentTimeMillis) p))
  ([date p]
   (transform date t/minus p))
  ([date k v & keyvals]
   (minus date (apply period k v keyvals))))


;; =============================================================================
;; Adjustments
;; =============================================================================


(declare adjust)

(def ^:private adjust-fns
  {:inc-month      #(plus % :months 1)
   :inc-day        #(plus % :days 1)

   :dec-day        #(minus % :days 1)
   :dec-month      #(minus % :months 1)
   :dec-second     #(minus % :seconds 1)

   :start-of-day   #(transform % t/floor t/day)
   :end-of-day     #(adjust % :start-of-day :inc-day :dec-second)

   :start-of-month #(transform % t/floor t/month)
   :end-of-month   #(adjust % :start-of-month :inc-month :dec-second)})


(defn adjust
  [date f & fs]
  (let [dt  (norm-in date)
        fns (map #(get adjust-fns % %) (cons f fs))]
    (norm-out
      (reduce (fn [d f] (f d)) dt fns))))


(defn- date->map [dt]
  (let [norm-d (norm-in dt)]
    (into {}
          (map #(vector % (field norm-d %)))
          (keys field-fns))))


(defn- map->date
  [{:keys [year month day hour minute second millisec]}]
  (norm-out
    (t/date-time year month day hour minute second millisec)))


(defn assoc
  "Returns a date with the value 'v' associated to field 'k' on the given 'date'.
  E.g. (assoc <date> :day 5) will return a date with the day changed to the
  fifth of the month."
  [date k v & keyvals]
  (map->date
    (apply clojure.core/assoc (date->map date) k v keyvals)))



(defn next-day [d] (adjust d :inc-day))
(defn next-month [d] (adjust d :inc-month))


;; =============================================================================
;; Coerce
;; =============================================================================


(defn to-millis
  "Convert the date 'dt' to the number of milliseconds after the Unix epoch."
  [dt]
  (c/to-long dt))


(defn to-seconds
  "Convert the date 'dt' to the number of seconds after the Unix epoch."
  [dt]
  (c/to-epoch dt))


;; =============================================================================
;; Convenience
;; =============================================================================


(defn now
  "Returns a Date instant for the current instant in the UTC time zone."
  []
  (norm-out (t/now)))


(defn days-in-month
  "Returns the number of days of the month of the provided date instant."
  [date]
  (field (adjust date :end-of-month) :day))