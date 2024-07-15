(ns schedule-clj.core
  (:require [schedule-clj.generate :as g]
            [schedule-clj.data :as d]
            [clojure.set :as s])
  (:gen-class))

(def MAX-TEACHER-PREPS 2)

(defn all-course-sections
  "Returns a list of all sections in the schedule with the given course-id"
  [schedule course-id]
  (->> schedule
       vals
       (filter #(= course-id (:course-id %)))
       seq
       (map :period)))

(defn lookup-section
  "Returns the section map in the schedule with the given id"
  [schedule course-id period]
  (->> schedule
       vals
       (filter #(= course-id (:course-id %)))
       (filter #(= period (:period %)))
       (map :section-id)
       first))

(defn lookup-teacher
  "Returns the teacher map in the faculty with the given id"
  [faculty teacher-id]
  (teacher-id faculty))

(defn lookup-course
  "Returns the course map in the catalog with the given id"
  [course-catalog course-id]
  (course-id course-catalog))

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

(defn get-student-schedule
  "Returns a sub hash-map of the full schedule (structured the same way) of all of the sections
   the student is currently registered for."
  [schedule student-id]
  (into {}
        (filter (fn [[_ section]] (contains? (:roster section) student-id)) schedule)))

(defn find-non-overlapping-period
  [& pds]
  (let [find-single-period-complements (fn [pd] (partial (complement d/do-periods-overlap?) pd))
        filter-single-period (fn [coll pd] (filter (find-single-period-complements pd) coll))]
    (reduce #(filter-single-period %1 %2) d/PERIODS pds)))

(defn get-teacher-open-periods
  [schedule teacher-id]
  (let [scheduled-periods (->> (get-teacher-schedule schedule teacher-id)
                               vals
                               (map #(:period %)))]
    (apply find-non-overlapping-period scheduled-periods)))

(defn can-teacher-take-section?
  "Determines if a teacher can add a section of the given course to their schedule, by checking 
   
   1. Is the teacher currently below the max number of classes they can teach?, 
   2. Does the teacher have the required cert for the class?, and 
   3. Would adding the class mean the teacher has more than the max number of distinct preps?"
  [schedule teacher course period]
  (and (< (count-teacher-sections schedule (:teacher-id teacher)) (:max-num-classes teacher))
       (d/teacher-has-cert? teacher (:required-cert course))
       (some #{period} (get-teacher-open-periods schedule (:teacher-id teacher)))
       (-> schedule
           (get-teacher-preps (:teacher-id teacher))
           (conj (:course-id course))
           count
           (<= MAX-TEACHER-PREPS))))

(defn find-available-teacher
  "Finds a teacher among the faculty that can add a section of that course to their schedule"
  [schedule faculty course period]
  (->> faculty
       vals
       (filter #(can-teacher-take-section? schedule % course period))
       (sort-by #(count-teacher-sections schedule %) >)
       first))

(defn get-student-open-periods
  [schedule student-id]
  (let [scheduled-pds (->> (get-student-schedule schedule student-id)
                           vals
                           (map #(:period %)))]
    (apply find-non-overlapping-period scheduled-pds)))

(defn register-student-to-section
  "Updates the schedule to add a student to the roster for a section. If no such section exists,
   returns the existing schedule and fails to register the student."
  [schedule student section-id]
  (if section-id
    (if (section-id schedule)
      (update schedule section-id d/section-register-student student)
      (do (println "Schedule doesn't contain section")
          schedule))
    (do (println "Null section id")
        schedule)))

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
    (if-let [teacher (find-available-teacher schedule faculty course period)]
      (let [section (d/initialize-section (random-uuid) course period (d/initialize-room "222" 30))]
        (-> schedule
            (assoc (:section-id section) section)
            (assign-teacher-to-section teacher (:section-id section))))
      schedule)))

(defn student-has-lunch?
  [schedule student-id]
  (->> (get-student-schedule schedule student-id)
       vals
       (map :course-id)
       (filter #(= :lunch %))
       seq))

(defn create-lunch-section
  [schedule period]
  (let [lunch-period (d/initialize-lunch-period (random-uuid) period)]
    (assoc schedule (:section-id lunch-period) lunch-period)))

(defn create-all-lunch-sections
  [schedule]
  (reduce create-lunch-section schedule '(:A-per :B-per :C-per :D-per)))

(defn update-schedule-with-new-section
  [schedule faculty course-catalog course-id period]
  (create-new-section schedule faculty course-catalog course-id period))

(defn schedule-student-required-classes
  [schedule faculty course-catalog student]
  (let [required-classes (seq (:requirements student))]
    (reduce #(let [existing-section-ids (all-course-sections %1 %2)
                   existing-periods (->> existing-section-ids
                                         (map schedule)
                                         (map :period))
                   student-free-periods (->> student
                                             :student-id
                                             (get-student-open-periods %1))]
               (if-let [valid-period (some (set student-free-periods) existing-periods)]
                 (register-student-to-section %1 student (lookup-section %1 %2 valid-period))
                 (let [updated-schedule (update-schedule-with-new-section %1
                                                                          faculty
                                                                          course-catalog
                                                                          %2
                                                                          (first student-free-periods))
                       new-section-id (lookup-section updated-schedule %2 (first student-free-periods))]
                   (register-student-to-section updated-schedule student new-section-id))))
            schedule
            required-classes)))

#_(def first-student (first (vals student-body)))
#_first-student
#_(create-all-lunch-sections {})
#_(def blank-schedule (create-all-lunch-sections {}))
#_blank-schedule

(defn schedule-all-required-classes
  [schedule faculty course-catalog student-body]
  (reduce #(schedule-student-required-classes %1
                                              faculty
                                              course-catalog
                                              %2)
          schedule
          (vals student-body)))

(defn get-student-missing-classes
  [schedule student]
  (let [required-classes (conj (:requirements student) :lunch)
        scheduled-classes (map :course-id
                               (-> schedule
                                   (get-student-schedule (:student-id student))
                                   vals))]
    (s/difference (set required-classes) (set scheduled-classes))))

(defn get-all-missing-classes
  [schedule student-body]
  (reduce #(when-let [missing-classes (seq (get-student-missing-classes schedule %2))]
             (assoc %1 (:student-id %2) (set missing-classes)))
          {}
          (vals student-body)))

(defn failure-summary!
  [schedule faculty student-body faculty-per-cert]
  (println)
  (println (str "Student Body (size): " (count student-body)))
  (println (str "Faculty (size): " (count faculty) ", Faculty/Cert: " faculty-per-cert))
  (println (str "No. of sections: " (count schedule)))
  (let [missing-classes (vals (get-all-missing-classes schedule student-body))
        correctly-scheduled (count (filter #(<= (count %) 1) missing-classes))
        missing-one (count (filter #(= 2 (count %)) missing-classes))
        missing-two (count (filter #(= 3 (count %)) missing-classes))
        missing-three (count (filter #(= 4 (count %)) missing-classes))
        missing-more (- (count missing-classes) correctly-scheduled missing-one missing-two missing-three)]
    (println (str "No. students fully scheduled: " correctly-scheduled "(" (float (/ correctly-scheduled (count student-body))) ")"))
    (println (str "No. students missing one: " missing-one))
    (println (str "No. students missing two: " missing-two))
    (println (str "No. students missing three: " missing-three))
    (println (str "No. students missing more: " missing-more))))

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
  (time (let [faculty-per-cert 16
              test-course-catalog (g/generate-course-catalog 3366 55)
              faculty (g/generate-faculty 1122 (vals test-course-catalog) faculty-per-cert)
              student-body (g/generate-heterogeneous-student-body 2233 test-course-catalog 500)
              schedule (schedule-all-required-classes (create-all-lunch-sections {}) faculty test-course-catalog student-body)]
          ;; (doseq [[section-id section] schedule]
          ;;   (println (str section-id \newline section \newline)))
          ;; (doseq [student-id (keys student-body)]
          ;;   (println (str "Student ID: " student-id))
          ;;   (doseq [[section-id section] (get-student-schedule schedule student-id)]
          ;;     (println (str "Section ID: " section-id ", Course ID: " (:course-id section) ", Period: " (:period section))))
          ;;   (println))

          ;; (doseq [[student-id missing-classes] (get-all-missing-classes schedule student-body)]
          ;;   (println (str student-id ", " missing-classes)))
          (failure-summary! schedule faculty student-body faculty-per-cert))))


