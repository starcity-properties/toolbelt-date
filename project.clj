(defproject starcity/toolbelt-date "0.3.0"
  :description "Utilities for working with dates in Clojure/Script."
  :url "https://github.com/starcity-properties/toolbelt-date"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [clj-time "0.14.2"]]
  :deploy-repositories [["releases" {:url   "https://clojars.org/repo"
                                     :creds :gpg}]])
