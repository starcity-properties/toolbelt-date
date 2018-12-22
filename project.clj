(defproject starcity/toolbelt-date "0.5.0"
  :description "Utilities for working with dates in Clojure/Script."
  :url "https://github.com/starcity-properties/toolbelt-date"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0" :scope "provided"]
                 [clj-time "0.14.2"]
                 [org.clojure/test.check "0.9.0"]]
  :plugins [[s3-wagon-private "1.2.0"]]
  :repositories {"releases"  {:url        "s3://starjars/releases"
                              :username   :env/aws_access_key
                              :passphrase :env/aws_secret_key}
                 "snapshots" {:url        "s3://starjars/snapshots"
                              :username   :env/aws_access_key
                              :passphrase :env/aws_secret_key}})
