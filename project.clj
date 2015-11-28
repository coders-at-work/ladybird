(defproject ladybird "0.5.3-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [korma "0.3.2"]
                 ]
  :profiles {:dev {:dependencies [[mysql/mysql-connector-java "5.1.25"]
                                  [com.microsoft/sqljdbc4 "3.0"]]}}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  )
