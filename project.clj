(defproject coders-at-work/ladybird "0.8.2-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/tools.logging "0.3.1"]
                 [org.clojure/data.json "2.5.2"]
                 [korma "0.4.2"]
                 [metosin/reitit "0.4.2"]
                 [clojure.java-time "1.4.3"]
                 ;; json
                 [com.fasterxml.jackson.datatype/jackson-datatype-joda "2.11.0"]
                 ]
  :profiles {:dev {:dependencies [[midje "1.9.9"]
                                  [midje-notifier "0.3.0"]
                                  [org.clojure/clojure "1.9.0"]
                                  [mysql/mysql-connector-java "5.1.25"]
                                  [com.microsoft.sqlserver/mssql-jdbc "8.2.2.jre8"]
                                  [org.postgresql/postgresql "42.7.8"]
                                  ]}}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :deploy-repositories [["releases"  {:sign-releases false :url "https://clojars.org/repo"}]
                        ["snapshots" {:sign-releases false :url "https://clojars.org/repo"}]]
  )
