(ns schedule-clj.core
  (:require [schedule-clj.generate :as g]
            [schedule-clj.data :as d])
  (:gen-class))

(defn all-course-sections
  [schedule course-id]
  (->> schedule vals (filter #(= course-id (:course-id %))) seq))

(defn lookup-section
  [schedule section-id]
  ((keyword section-id) schedule))

(defn lookup-teacher
  [faculty teacher-id]
  ((keyword teacher-id) faculty))

(defn lookup-course
  [course-catalog course-id]
  ((keyword course-id) course-catalog))

(defn lookup-teachers-with-cert
  [faculty cert]
  (filter #(d/teacher-has-cert? % cert) (vals faculty)))

(defn teacher-assigned-to-section?
  [schedule teacher section-id]
  (-> schedule
      (keyword section-id)
      :teachers
      (contains? (:teacher-id teacher))))

(defn teacher-preps
  [schedule teacher]
  (->> schedule
       vals
       (filter #(contains? (:teachers %) (:teacher-id teacher)))
       (map :course-id)
       distinct
       set))

(defn count-teacher-sections
  [schedule teacher]
  (->> schedule
       vals
       (filter #(contains? (:teachers %) (:teacher-id teacher)))
       count))

#_(< (count-teacher-sections bad-schedule (first (vals faculty))) (:max-num-classes (first (vals faculty))))

(defn can-teacher-take-section?
  [schedule teacher course]
  (and (< (count-teacher-sections schedule teacher) (:max-num-classes teacher))
       (d/teacher-has-cert? teacher (:required-cert course))
       (-> schedule
           (teacher-preps teacher)
           (conj (:course-id course))
           count
           (<= 2))))

(defn register-student-to-section
  [schedule student section-id]
  (update schedule (keyword section-id) d/section-register-student student))

(defn assign-teacher-to-section
  [schedule teacher section-id]
  (update schedule (keyword section-id) d/section-assign-teacher teacher))

(defn create-section
  [schedule faculty course-catalog course-id period]
  (let [course (lookup-course course-catalog course-id)]
    (if-let [teacher (->> faculty
                          vals
                          (filter #(can-teacher-take-section? schedule % course))
                          first)]
      (let [section (-> (random-uuid)
                        (d/initialize-section course period (d/initialize-room "222" 30))
                        (d/section-assign-teacher teacher))]
        (assoc schedule (keyword (:section-id section)) section))
      schedule)))

#_(create-section bad-schedule
                  faculty
                  test-course-catalog
                  "c8e30929-9b68-d302-1083-e3b463961d90"
                  :1st-per)

(defn schedule-student
  [schedule faculty rooms student]
  ;; TODO: Fill this in
  )

(defn -main
  "launch!"
  []
  (println (g/generate-random-student (g/generate-random-course-list 3366 55))))

#_(def test-course-catalog (reduce
                            #(assoc %1 (keyword (:course-id %2)) %2)
                            {}
                            (g/generate-random-course-list 2266 10)))
#_(def faculty (reduce
                #(assoc %1 (keyword (:teacher-id %2)) %2)
                {}
                (g/generate-faculty 2277 (vals test-course-catalog))))
#_(def bad-schedule (reduce
                     #(assoc %1 (keyword (:section-id %2)) %2)
                     {}
                     (map #(d/initialize-section (str (random-uuid)) %1 %2 %3)
                          test-course-catalog
                          d/PERIODS
                          (g/generate-rooms 2288 8))))
#_(def rooms (g/generate-rooms 2288 10))

#_test-course-catalog
#_faculty
#_bad-schedule
#_rooms

#_(all-course-sections bad-schedule "f1f0754e-77a2-c6bd-af03-468a775eb82a")
#_(assign-student-to-section bad-schedule
                             (g/generate-random-student (vals test-course-catalog))
                             "0708dc23-36a8-406d-b2d3-e32293d0cc8d")

#_(g/generate-random-student 3366 (vals test-course-catalog))
