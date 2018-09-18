(ns toolbelt.date.utils
  (:require
    [toolbelt.date :as date]))

(defn within-time?
  [start end k v]
  (> v (date/time-between start end k)))