(ns schedule-clj.core
  (:require [schedule-clj.generate :as g]
            [schedule-clj.data :as d])
  (:gen-class))

(defn schedule-has-course?
  [schedule course-id]
  (->> schedule vals (filter #(= course-id (:course-id %))) seq))

(defn teacher-assigned-to-section?
  [schedule teacher section-id]
  (-> schedule
      (keyword section-id)
      :teachers
      (contains? (:teacher-id teacher))))

(defn teacher-count-preps
  [teacher schedule]
  (->> schedule
       vals
       (filter #(contains? (:teachers %) (:teacher-id teacher)))
       (map :course-id)
       distinct))

(defn register-student-to-section
  [schedule student section-id]
  (update schedule (keyword section-id) d/section-register-student student))

(defn assign-teacher-to-section
  [schedule teacher section-id]
  (update schedule (keyword section-id) d/section-assign-teacher teacher))


(defn schedule-student
  [schedule faculty rooms student]
  (comment "TODO: Need to fill this in"))

(defn -main
  "launch!"
  []
  (println (g/generate-random-student (g/generate-random-course-list 3366 55))))

#_(def test-course-catalog (g/generate-random-course-list 2266 10))
#_(def faculty (g/generate-faculty 2277 test-course-catalog))
#_(def bad-schedule (reduce
                     #(assoc %1 (keyword (:section-id %2)) %2)
                     {}
                     (map #(d/initialize-section (str (random-uuid)) %1 %2 %3)
                          test-course-catalog
                          d/PERIODS
                          (g/generate-rooms 2288 8))))
#_test-course-catalog
#_bad-schedule
#_faculty

#_(schedule-has-course? bad-schedule "f1f0754e-77a2-c6bd-af03-468a775eb82a")
#_(keyword :hello)
#_(assign-student-to-section bad-schedule 
                             (g/generate-random-student test-course-catalog)
                             "0708dc23-36a8-406d-b2d3-e32293d0cc8d")

#_(g/generate-random-student 3366 test-course-catalog)