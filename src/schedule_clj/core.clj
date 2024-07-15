(ns schedule-clj.core
  (:require [schedule-clj.generate :as g]
            [schedule-clj.data :as d])
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

(defn get-teacher-schedule
  "Returns a sub hash-map of the full schedule (structured the same way) of all of the sections
   the teacher is currently assigned to."
  [schedule teacher-id]
  (into {}
        (filter (fn [[_ section]] (contains? (:teachers section) teacher-id)) schedule)))

(defn get-teacher-preps
  "Lists the distinct courses that the teacher is currently assigned to teach"
  [schedule teacher-id]
  (->> (get-teacher-schedule schedule teacher-id)
       vals
       (map :course-id)
       distinct
       set))

(defn count-teacher-sections
  "Counts the number of sections the teacher is currently assigned to"
  [schedule teacher-id]
  (->> (get-teacher-schedule schedule teacher-id)
       count))

(defn can-teacher-take-section?
  "Determines if a teacher can add a section of the given course to their schedule, by checking 
   
   1. Is the teacher currently below the max number of classes they can teach?, 
   2. Does the teacher have the required cert for the class?, and 
   3. Would adding the class mean the teacher has more than the max number of distinct preps?"
  [schedule teacher course]
  (and (< (count-teacher-sections schedule (:teacher-id teacher)) (:max-num-classes teacher))
       (d/teacher-has-cert? teacher (:required-cert course))
       (-> schedule
           (get-teacher-preps (:teacher-id teacher))
           (conj (:course-id course))
           count
           (<= MAX-TEACHER-PREPS))))

(defn find-available-teacher
  "Finds a teacher among the faculty that can add a section of that course to their schedule"
  [schedule faculty course]
  (->> faculty
       vals
       (filter #(can-teacher-take-section? schedule % course))
       (sort-by count-teacher-sections >)
       first))

(defn get-student-schedule
  "Returns a sub hash-map of the full schedule (structured the same way) of all of the sections
   the student is currently registered for."
  [schedule student-id]
  (into {}
        (filter (fn [[_ section]] (contains? (:roster section) student-id)) schedule)))

#_(filter (fn [[_ section]] (contains? (:roster section) :46454778-0fc3-cf1e-ed8c-ceb5705255a8))
          first-pass)

#_((fn [[_ section]] (contains? (:roster section) :46454778-0fc3-cf1e-ed8c-ceb5705255a8)) 
   (first first-pass))

#_((fn [[_ section]] (:roster section))
   (section first-pass))

(defn find-non-overlapping-period
  [& pds]
  (let [find-single-period-complements (fn [pd] (partial (complement d/do-periods-overlap?) pd))
        filter-single-period (fn [coll pd] (filter (find-single-period-complements pd) coll))]
    (reduce #(filter-single-period %1 %2) d/PERIODS pds)))

(defn get-student-open-periods
  [schedule student-id]
  (let [scheduled-pds (->> (get-student-schedule schedule student-id)
                           vals
                           (map #(:period %)))]
    (apply find-non-overlapping-period scheduled-pds)))

#_(get-student-open-periods first-pass :46454778-0fc3-cf1e-ed8c-ceb5705255a8)

(defn register-student-to-section
  "Updates the schedule to add a student to the roster for a section. If no such section exists,
   returns the existing schedule and fails to register the student."
  [schedule student section-id]
  (if (and section-id 
           (section-id schedule))
    (update schedule section-id d/section-register-student student)
    schedule))

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
      (let [section (d/initialize-section (random-uuid) course period (d/initialize-room "222" 30))]
        (-> schedule
            (assoc (:section-id section) section)
            (assign-teacher-to-section teacher (:section-id section))))
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

#_(def test-course-catalog (g/generate-course-catalog 3366 55))
#_test-course-catalog

#_(def faculty (g/generate-faculty 1122 (vals test-course-catalog)))
#_faculty

#_(def student-body (g/generate-student-body 2233 test-course-catalog 49))
#_student-body

#_(def first-pass (schedule-all-required-classes {} faculty test-course-catalog student-body))
#_(first first-pass)

(defn -main
  "launch!"
  []
  (let [test-course-catalog (g/generate-course-catalog 3366 55)
        faculty (g/generate-faculty 1122 (vals test-course-catalog))
        student-body (g/generate-student-body 2233 test-course-catalog 49)
        schedule (schedule-all-required-classes {} faculty test-course-catalog student-body)]
    (doseq [[section-id section] schedule]
      (println (str section-id \newline section \newline)))
    (println (get-student-schedule schedule :46454778-0fc3-cf1e-ed8c-ceb5705255a8))))


