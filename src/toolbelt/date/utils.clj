(ns toolbelt.date.utils
  (:require
    [clj-time.core :as t]
    [toolbelt.date :as date]))


;; =============================================================================
;; Licenses
;; =============================================================================


(defn license-period [timezone start term]
  "Return a tuple with two dates representing the license start and end dates
   respectively based on the supplied 'start' date and 'term' in months.

   The start date represents midnight of 'start' in local time corrected to UTC.
   The end date represents 23:59:59 on the last day of the 'term' in local time corrected to UTC.

  'start' is expected to be the UTC corrected date representing a date time in 'timezone'."
  (let [local-start-date (date/tz-uncorrected start timezone)
        license-start    (date/beginning-of-day local-start-date timezone)
        license-end      (-> (date/plus local-start-date (t/months term))
                             (date/minus (t/days 1))
                             (date/end-of-day timezone))]
    [license-start license-end]))