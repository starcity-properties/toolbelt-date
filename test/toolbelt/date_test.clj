(ns toolbelt.date-test
  (:require
    [clj-time.core :as t]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.generators :as gen]
    [clojure.test :refer :all]
    [toolbelt.date :as date]
    [clj-time.coerce :as c]))

(deftest short-test
  (is (= "3/15/89" (date/short #inst "1989-03-15")))
  (is (= "3/15/89" (date/short #inst "1989-03-15" false)))
  (is (= "3/15/89, 12:00AM" (date/short #inst "1989-03-15" true))))


(deftest is-first-day-of-month-test
  (is (true? (date/is-first-day-of-month? #inst "2017-01-01")))
  (is (false? (date/is-first-day-of-month? #inst "2017-01-02"))))


(def ^:private tz (t/time-zone-for-id "America/Los_Angeles"))


(deftest tz-corrected-test
  (is (= #inst "2018-01-01T08:00:00.000-00:00"
         (date/tz-corrected #inst "2018-01-01T00:00:00.000-00:00" tz)))
  (is (= #inst "2018-01-01T00:00:00.000-00:00"
         (date/tz-corrected #inst "2018-01-01T00:00:00.000-00:00" t/utc))))


(deftest tz-uncorrected-test
  (is (= #inst "2018-01-01T00:00:00.000-00:00"
         (date/tz-uncorrected #inst "2018-01-01T08:00:00.000-00:00" tz)))
  (is (= #inst "2018-01-01T00:00:00.000-00:00"
         (date/tz-uncorrected #inst "2018-01-01T00:00:00.000-00:00" t/utc))))


(deftest tz-correction-test
  (let [inst #inst "2018-01-01T00:00:00.000-00:00"]
    (is (= inst (-> inst (date/tz-corrected tz) (date/tz-uncorrected tz))))))


(deftest end-of-day-test
  (is (= (date/end-of-day #inst "2018-01-01T00:00:00.000-00:00")
         #inst "2018-01-01T23:59:59.000-00:00"))
  (is (= (date/end-of-day #inst "2018-01-01T14:38:00.000-00:00")
         #inst "2018-01-01T23:59:59.000-00:00"))
  (is (= (date/end-of-day #inst "2018-01-01T23:59:59.000-00:00")
         #inst "2018-01-01T23:59:59.000-00:00"))
  (is (= (date/end-of-day #inst "2018-01-01T23:59:59.000-00:00" tz)
         #inst "2018-01-02T07:59:59.000-00:00")))


(deftest beginning-of-day-test
  (is (= (date/beginning-of-day #inst "2018-01-01T00:00:00.000-00:00")
         #inst "2018-01-01T00:00:00.000-00:00"))
  (is (= (date/beginning-of-day "2018-01-01T14:38:00.000-00:00")
         #inst "2018-01-01T00:00:00.000-00:00"))
  (is (= (date/beginning-of-day #inst "2018-01-01T00:00:00.000-00:00" tz)
         #inst "2018-01-01T08:00:00.000-00:00")))


(deftest beginning-of-month-test
  (is (= (date/beginning-of-month #inst "2018-01-01T00:00:00.000-00:00")
         #inst "2018-01-01T00:00:00.000-00:00"))
  (is (= (date/beginning-of-month #inst "2018-01-28T00:00:00.000-00:00")
         #inst "2018-01-01T00:00:00.000-00:00"))
  (is (= (date/beginning-of-month #inst "2018-01-28T00:00:00.000-00:00" tz)
         #inst "2018-01-01T08:00:00.000-00:00")))


(deftest end-of-month-test
  (is (= (date/end-of-month #inst "2018-01-01T00:00:00.000-00:00")
         #inst "2018-01-31T23:59:59.000-00:00"))
  (is (= (date/end-of-month #inst "2018-01-14T00:00:00.000-00:00")
         #inst "2018-01-31T23:59:59.000-00:00"))
  (is (= (date/end-of-month #inst "2018-01-31T23:59:59.000-00:00")
         #inst "2018-01-31T23:59:59.000-00:00"))
  (is (= (date/end-of-month #inst "2018-01-31T23:59:59.000-00:00" tz)
         #inst "2018-02-01T07:59:59.000-00:00")))


(defspec date-plus-minus-test
  100
  (prop/for-all
    [year (gen/double* {:min 1 :max 3000 :NaN? false})
     month (gen/double* {:min 1 :max 12 :NaN? false})
     p gen/pos-int]
    (let [inst   (c/to-date (t/date-time (int year) (int month)))]
      (testing "The inst plus a period is always equal that inst minus the same period."
        (are [inst2] (= inst inst2)
                     (-> (date/plus inst (t/seconds p))
                         (date/minus (t/seconds p)))
                     (-> (date/plus inst (t/minutes p))
                         (date/minus (t/minutes p)))
                     (-> (date/plus inst (t/hours p))
                         (date/minus (t/hours p)))
                     (-> (date/plus inst (t/days p))
                         (date/minus (t/days p)))
                     (-> (date/plus inst (t/weeks p))
                         (date/minus (t/weeks p)))
                     (-> (date/plus inst (t/months p))
                         (date/minus (t/months p)))
                     (-> (date/plus inst (t/years p))
                         (date/minus (t/years p))))))))
