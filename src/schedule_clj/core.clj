(ns schedule-clj.core
  (:require [schedule-clj.generate :as g])
  (:gen-class))

(defn -main
  "launch!"
  []
  (println (g/generate-random-student (g/generate-random-course-list 3366 55))))

