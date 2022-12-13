(defproject aoc22 "0.1.0-SNAPSHOT"
  :description "Solutions to Advent of Code 2022"
  :url "https://github.com/coutego/advent-of-code-2022"
  :license {:name "MIT"
            :url "https://github.com/coutego/advent-of-code-2022/blob/main/LICENSE"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.taoensso/tufte "2.1.0"]]
  :main ^:skip-aot aoc22
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
