(defproject starcity/toolbelt-date "0.4.1-SNAPSHOT"
  :description "Utilities for working with dates in Clojure/Script."
  :url "https://github.com/starcity-properties/toolbelt-date"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [clj-time "0.14.2"]
                 [org.clojure/test.check "0.9.0"]
                 [com.taoensso/timbre "4.10.0"]]
  :deploy-repositories [["releases" {:url   "https://clojars.org/repo"
                                     :creds :gpg}]])
