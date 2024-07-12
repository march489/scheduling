(ns schedule-clj.core
  (:require [schedule-clj.generate :as g]
            [schedule-clj.data :as d]
            [clojure.java.io :as io])
  (:gen-class))

(def MAX-TEACHER-PREPS 2)

(defn all-course-sections
  "Returns a list of all sections in the schedule with the given course-id"
  [schedule course-id]
  (->> schedule vals (filter #(= course-id (:course-id %))) seq))

(defn lookup-section
  "Returns the section map in the schedule with the given id"
  [schedule section-id]
  (section-id schedule))

(defn lookup-teacher
  "Returns the teacher map in the faculty with the given id"
  [faculty teacher-id]
  (teacher-id faculty))

(defn lookup-course
  "Returns the course map in the catalog with the given id"
  [course-catalog course-id]
  (course-id course-catalog))

;; (defn lookup-teachers-with-cert
;;   "Returns a list of teachers with the given cert"
;;   [faculty cert]
;;   (filter #(d/teacher-has-cert? % cert) (vals faculty)))

;; (defn teacher-assigned-to-section?
;;   "Is the teacher assigned to the given section-id in the schedule?"
;;   [schedule teacher section-id]
;;   (-> schedule
;;       section-id
;;       :teachers
;;       (contains? (:teacher-id teacher))))

(defn teacher-preps
  "Lists the distinct courses that the teacher is currently assigned to teach"
  [schedule teacher]
  (->> schedule
       vals
       (filter #(contains? (:teachers %) (:teacher-id teacher)))
       (map :course-id)
       distinct
       set))

(defn count-teacher-sections
  "Counts the number of sections the teacher is currently assigned to"
  [schedule teacher]
  (->> schedule
       vals
       (filter #(contains? (:teachers %) (:teacher-id teacher)))
       count))

(defn can-teacher-take-section?
  "Determines if a teacher can add a section of the given course to their schedule, by checking 
   
   1. Is the teacher currently below the max number of classes they can teach?, 
   2. Does the teacher have the required cert for the class?, and 
   3. Would adding the class mean the teacher has more than the max number of distinct preps?"
  [schedule teacher course]
  (and (< (count-teacher-sections schedule teacher) (:max-num-classes teacher))
       (d/teacher-has-cert? teacher (:required-cert course))
       (-> schedule
           (teacher-preps teacher)
           (conj (:course-id course))
           count
           (<= MAX-TEACHER-PREPS))))

(defn find-available-teacher
  "Finds a teacher among the faculty that can add a section of that course to their schedule"
  [schedule faculty course]
  (->> faculty
       vals
       (filter #(can-teacher-take-section? schedule % course))
       first))

(defn find-non-overlapping-period
  [& pds])

(defn register-student-to-section
  "Updates the schedule to add a student to the roster for a section."
  [schedule student section-id]
  (update schedule section-id d/section-register-student student))

(defn assign-teacher-to-section
  "Updates the schedule to assign a teacher to teach a specfic section."
  [schedule teacher section-id]
  (update schedule section-id d/section-assign-teacher teacher))

(defn find-open-section-by-course-id
  "Looks through the schedule to find if there already exists a section of the course offered."
  [schedule course-id]
  (->> schedule
       vals
       (filter #(= course-id (:course-id %)))
       (filter #(< (count (:roster %)) (:max-size %)))
       first
       :section-id))

(defn create-new-section
  "Updates the schedule to include a new section for a course if there's a teacher who can teach it."
  [schedule faculty course-catalog course-id period]
  (let [course (lookup-course course-catalog course-id)]
    (if-let [teacher (find-available-teacher schedule faculty course)]
      (let [section (-> (random-uuid)
                        (d/initialize-section course period (d/initialize-room "222" 30))
                        (d/section-assign-teacher teacher))]
        (assoc schedule (:section-id section) section))
      schedule)))


(defn update-schedule-with-section
  [schedule faculty course-catalog course-id]
  (if (find-open-section-by-course-id schedule course-id)
    schedule
    (create-new-section schedule faculty course-catalog course-id :1st-per)))

(defn schedule-student-required-classes
  [schedule faculty course-catalog student]
  (let [updated-schedule (reduce #(update-schedule-with-section %1 faculty course-catalog %2)
                                 schedule
                                 (:requirements student))]
    (reduce #(register-student-to-section %1
                                          student
                                          (find-open-section-by-course-id updated-schedule %2))
            updated-schedule
            (:requirements student))))

(defn schedule-all-required-classes
  [schedule faculty course-catalog student-body]
  (reduce #(schedule-student-required-classes %1
                                              faculty
                                              course-catalog
                                              %2)
          schedule
          (vals student-body)))

;; #_(def test-course-catalog (g/generate-course-catalog 3366 55))
;; #_test-course-catalog

;; #_(def faculty (g/generate-faculty 1122 (vals test-course-catalog)))
;; #_faculty

;; #_(def student-body (g/generate-student-body 2233 test-course-catalog 90))
;; #_student-body

;; #_(def first-pass (schedule-all-required-classes {} faculty test-course-catalog student-body))
;; #_first-pass
;; #_(nth (vals first-pass) 1)

(defn -main
  "launch!"
  []
  (let [test-course-catalog (g/generate-course-catalog 3366 55)
        faculty (g/generate-faculty 1122 (vals test-course-catalog))
        student-body (g/generate-student-body 2233 test-course-catalog 49)
        ;; schedule (schedule-all-required-classes {} faculty test-course-catalog student-body)
        schedule (schedule-all-required-classes {} faculty test-course-catalog student-body)
        ]
    (doseq [section (vals schedule)]
      (println (str section \newline)))))


