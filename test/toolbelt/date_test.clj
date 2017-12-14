(ns toolbelt.date-test
  (:require [clojure.test :refer :all]
            [toolbelt.date :as date]
            [clj-time.core :as t]))

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
