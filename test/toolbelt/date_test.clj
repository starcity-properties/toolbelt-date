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


(deftest period-test
  (let [date #inst "2018-01-01T00:00:00.000-00:00"]
    (is (= #inst "2018-01-01T00:00:01.000-00:00"
           (date/plus date (date/period :seconds 1))
           (date/plus date (date/period :millis 1000))))
    (is (= #inst "2018-02-01T00:00:01.000-00:00"
           (date/plus date (date/period :seconds 1 :months 1))
           (date/plus date (date/period :seconds 1 :days 31))))
    (is (= #inst "2019-03-01T00:00:01.000-00:00"
           (date/plus date (date/period :seconds 1 :months 2 :years 1))))
    (is (= #inst "2018-01-15T00:00:00.000-00:00"
           (date/plus date (date/period :weeks 2))
           (date/plus date (date/period :days 14))))
    (is (= #inst "2018-01-01T00:00:00.100-00:00"
           (date/plus date (date/period :millis 100))))))


;; =============================================================================
;; Property based
;; =============================================================================


(defn- gen-date* []
  (gen/fmap (fn [n] (c/to-date n)) gen/int))


(defspec date-plus-minus-test
  ;; Run test 100 times with different generated data
  100
  (prop/for-all
    ;; Generate random date and integer to represent number of days
    [inst (gen-date*)
     p gen/int]
    (let [date-time (c/to-date-time inst)
          days      (t/days p)]
      (testing "The inst plus a period is always equal that inst minus the same period."
        (are [d2] (= inst d2)
          ;; org.joda.DateTime
          (-> (date/plus date-time days)
              (date/minus days))
          ;; java.util.Date
          (-> (date/plus inst days)
              (date/minus days))
          ;; Long (unix time)
          (-> (date/plus (c/to-long inst) days)
              (date/minus days)))))))


(defspec date-min-max-test
  ;; Run test 100 times with different generated data
  100
  (prop/for-all
    ;; Generate a vector of dates with 1 to 100 elements
    [dts (gen/vector (gen-date*) 1 100)]
    (testing "Min date is always the earliest and max date is always the latest."
      (let [sorted-dates (sort dts)]
        (is (= (apply date/max dts) (last sorted-dates))
            "Max date should be the latest date.")
        (is (= (apply date/min dts) (first sorted-dates))
            "Min date should be the earliest date.")))))
