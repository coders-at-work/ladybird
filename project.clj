(defproject coders-at-work/ladybird "0.5.3-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [korma "0.4.2"]
                 ]
  :profiles {:dev {:dependencies [[mysql/mysql-connector-java "5.1.25"]
                                  [midje "1.8.2"]
                                  [midje-notifier "0.2.0"]
                                  ]}}
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  )
