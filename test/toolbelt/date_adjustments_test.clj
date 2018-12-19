(ns toolbelt.date-adjustments-test
  (:require
    [clojure.test :refer :all]
    [toolbelt.date :as date]))


(def timezone "America/Los_Angeles")
(def utc-timezone "UTC")


(deftest date-adjustments-test
  (testing "Adjust UTC corrected start/end of day (Pacific timezone)"
    (let [date #inst"2018-11-14T07:59:59.000-00:00"]
      (testing "Start of day"
        (is (= #inst"2018-11-14T00:00:00.000-00:00"
               (date/start-of-day-utc date))
            "Start of day without timezone should set date to start of day without correction.")
        (is (= #inst"2018-11-13T08:00:00.000-00:00"
               (date/start-of-day-utc date timezone))
            "Start of day should be the same day as a UTC corrected end of day.")
        (is (= #inst"2018-11-13T00:00:00.000-00:00"
               (date/start-of-day-utc date timezone utc-timezone))
            "Start of day from pacific to UTC time should be midnight in UTC end of day."))
      (testing "End of day"
        (is (= #inst"2018-11-14T23:59:59.000-00:00"
               (date/end-of-day-utc date))
            "End of day without timezone should set date to end of day without correction.")
        (is (= date (date/end-of-day-utc date timezone))
            "ENd of day should be the same as a UTC corrected end of day.")
        (is (= #inst"2018-11-13T23:59:59.000-00:00"
               (date/end-of-day-utc date timezone utc-timezone))
            "End of day from pacific to UTC timezone should be the end of day in UTC time."))))

  (testing "Adjust UTC corrected start/end of month (Pacific timezone)"
    (let [date #inst"2018-12-01T07:59:59.000-00:00"]
      (testing "Start of month"
        (is (= #inst"2018-12-01T00:00:00.000-00:00"
               (date/start-of-month-utc date))
            "Start of month without timezone should set date to start of month without correction.")
        (is (= #inst"2018-11-01T07:00:00.000-00:00"
               (date/start-of-month-utc date timezone))
            "Start of day should be the same day as a UTC corrected end of day.")
        (is (= #inst"2018-11-01T00:00:00.000-00:00"
               (date/start-of-month-utc date timezone utc-timezone))
            "Start of day from pacific to UTC time should be midnight in UTC end of day."))
      (testing "end of month"
        (is (= #inst"2018-12-31T23:59:59.000-00:00"
               (date/end-of-month-utc date))
            "Start of month without timezone should set date to start of month without correction.")
        (is (= date (date/end-of-month-utc date timezone))
            "End of of day should be the same as a UTC corrected end of day.")
        (is (= #inst"2018-11-30T23:59:59.000-00:00"
               (date/end-of-month-utc date timezone utc-timezone))
            "End of month from pacific to UTC timezone should be the end of day in UTC time."))))


  (testing "Adjust UTC corrected end of month (UTC timezone)"
    (let [date #inst"2018-12-01T07:59:59.000-00:00"]
      (is (= #inst"2018-12-01T00:00:00.000-00:00"
             (date/start-of-month-utc date utc-timezone))
          "Start of day should be the same day as a UTC corrected end of day.")
      (is (= #inst"2018-12-31T23:59:59.000-00:00"
             (date/end-of-month-utc date utc-timezone))
          "End of of day should be the same as a UTC corrected end of day."))))