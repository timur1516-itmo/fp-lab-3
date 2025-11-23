(defproject fp-lab-3 "1.0"
  :description "Потоковая обработка данных на Clojure"
  :url "https://github.com/timur1516/fp-lab-3"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.12.2"]
                 [org.clojure/test.check "1.1.0"]
                 [org.clojure/tools.cli "1.1.230"]
                 [org.clojure/core.async "1.6.681"]]
  :main fp-lab-3.core
  :target-path "target/%s"
  :plugins [[dev.weavejester/lein-cljfmt "0.13.1"],
            [jonase/eastwood "1.4.3"],
            [com.github.clj-kondo/lein-clj-kondo "0.2.5"]]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
