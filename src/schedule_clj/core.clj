(ns schedule-clj.core
  (:require [schedule-clj.generate :as g]
            [schedule-clj.data :as d]
            [clojure.set :as s]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.generators :as gen])
  (:gen-class))

(def MAX-TEACHER-PREPS 2)
(def DEFAULT-ROOM-CAPACITY 25)

(defn all-available-course-sections
  "Returns a list of all sections in the schedule with the given course-id that have open space"
  [schedule course-id]
  (->> schedule
       vals
       (filter #(= course-id (:course-id %)))
       (filter #(< (count (:roster %)) (:max-size %)))
       (sort-by #(count (:roster %)))
       seq
       (map :section-id)))

(defn lookup-section
  "Returns the section ID in the schedule with the given id"
  [schedule course-id period]
  (->> schedule
       vals
       (filter #(= course-id (:course-id %)))
       (filter #(= period (:period %)))
       (sort-by #(count (:roster %)))
       (map :section-id)
       first))

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

(defn find-non-overlapping-periods
  "Returns a list of all periods that don't overlap with any in the passed list."
  [& pds]
  (let [find-single-period-complements (fn [pd] (partial (complement d/do-periods-overlap?) pd))
        filter-single-period (fn [coll pd] (filter (find-single-period-complements pd) coll))]
    (reduce #(filter-single-period %1 %2) d/PERIODS pds)))

(defn get-teacher-open-periods
  "Finds periods that teachers have open in their schedules for a new section. "
  [schedule teacher-id]
  (let [scheduled-periods (->> (get-teacher-schedule schedule teacher-id)
                               vals
                               (map #(:period %)))]
    (apply find-non-overlapping-periods scheduled-periods)))

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
  "Gets the currently open periods in the student's schedule."
  [schedule student-id]
  (let [scheduled-pds (->> (get-student-schedule schedule student-id)
                           vals
                           (map #(:period %)))]
    (apply find-non-overlapping-periods scheduled-pds)))

(defn student-has-lunch?
  "Has the student been assigned to a lunch section?"
  [schedule student-id]
  (->> (get-student-schedule schedule student-id)
       vals
       (map :course-id)
       (filter #(= :lunch %))
       seq))

(defn register-student-to-lunch-section
  "Assign the student to a given lunch section."
  [schedule student lunch-period]
  (if lunch-period
    (let [lunch-section-id (lookup-section schedule :lunch lunch-period)]
      ;; (do (println (str "Lunch period: " lunch-period ", Lunch section id: " lunch-section-id))
      ;;     (update schedule lunch-section-id d/section-register-student student))
      (update schedule lunch-section-id d/section-register-student student))
    schedule))

(defn lunch-section-available?
  "Does the lunch schedule have space for an additional student?"
  [schedule lunch-period]
  (->> lunch-period
       (lookup-section schedule :lunch)
       schedule
       d/section-has-space?))

(defn verify-student-lunch
  "Checks if a student has been assigned to a lunch section,
   and if not, assigns them to an open lunch section."
  [schedule student]
  (if (student-has-lunch? schedule (:student-id student))
    schedule
    (->> student
         :student-id
         (get-student-open-periods schedule)
         (filter d/is-half-block?)
         (filter #(lunch-section-available? schedule %))
         sort
         first
         (register-student-to-lunch-section schedule student))))

#_(->> :2501c750-face-277a-c110-91b394c7463c
       student-body
       :student-id
       (get-student-open-periods first-pass)
       (filter d/is-half-block?)
       sort
       first
       (register-student-to-lunch-section first-pass (student-body :2501c750-face-277a-c110-91b394c7463c)))
#_(verify-student-lunch first-pass (student-body :2501c750-face-277a-c110-91b394c7463c))

(defn register-student-to-section
  "Updates the schedule to add a student to the roster for a section. If no such section exists,
   returns the existing schedule and fails to register the student."
  [schedule student section-id]
  (if section-id
    (if (section-id schedule)
      (-> schedule
          (update section-id d/section-register-student student)
          (verify-student-lunch student))
      schedule)
    (do (Exception. (str "Null section id; missing course for sutdent " (:student-id student)))
        schedule)))

(defn assign-teacher-to-section
  "Updates the schedule to assign a teacher to teach a specfic section."
  [schedule teacher section-id]
  (update schedule section-id d/section-assign-teacher teacher))

(defn create-new-section
  "Updates the schedule to include a new section for a course if there's a teacher who can teach it."
  [schedule faculty course-catalog course-id period]
  (let [course (lookup-course course-catalog course-id)]
    (if-let [teacher (find-available-teacher schedule faculty course period)]
      (let [section (d/initialize-section (random-uuid) course period (d/initialize-room "222" DEFAULT-ROOM-CAPACITY))]
        (-> schedule
            (assoc (:section-id section) section)
            (assign-teacher-to-section teacher (:section-id section))))
      schedule)))

(defn create-lunch-section
  "Creates a new lunch section during the specified period."
  [schedule period]
  (let [lunch-period (d/initialize-lunch-section (random-uuid) period)]
    (assoc schedule (:section-id lunch-period) lunch-period)))

(defn create-all-lunch-sections
  "Creates a lunch section for each half-block"
  [schedule]
  (reduce create-lunch-section schedule '(:A-per :B-per :C-per :D-per)))

(defn sort-sections-by-class-size
  "Sorts a list of sections by their roster size."
  [sections]
  (sort-by #(count (:roster %)) < sections))

(defn get-periods-from-section-ids
  "Given a list of section ids and a schedule, returns a list 
   of the periods those sections are scheduled for, sorted by roster size. "
  [schedule section-ids]
  (->> section-ids
       (map schedule)
       sort-sections-by-class-size
       (map :period)))

(defn select-class-period
  "Returns an existing class period that overlaps with an open slot
   in the student's schedule, indicating the student can add that class at that time slot.
   Otherwise, returns nil."
  [student-free-periods-set available-class-periods]
  (->> available-class-periods
       (filter #(contains? student-free-periods-set %))
       sort
       first))

(defn choose-new-period
  "Chooses a period for a new section ceing created in the schedule. Selectively chooses
   long or short blocks for certain departments."
  [course-catalog course-id student-free-periods]
  (let [cert (:required-cert (course-id course-catalog))]
    (cond (d/SCIENCE-CLASSES cert) (first (sort (seq student-free-periods)))
          (d/ART-CLASSES cert) (first (sort (seq student-free-periods)))
          (d/MATH-CLASSES cert) (last (sort (seq student-free-periods)))
          (d/LANGUAGE-CLASSES cert) (last (sort (seq student-free-periods)))
          :else (first (gen/shuffle (seq student-free-periods))))))

(defn schedule-student-required-classes
  "Registers students to their required classes. If a section of a required class 
   doesn't already exist, this will create a new section in an open slot 
   in the student's schedule and assign the student to that class."
  [schedule faculty course-catalog student]
  (let [required-classes (seq (:requirements student))]
    (reduce #(let [existing-section-ids (all-available-course-sections %1 %2)
                   existing-periods (get-periods-from-section-ids %1 existing-section-ids)
                   student-free-periods (->> student
                                             :student-id
                                             (get-student-open-periods %1)
                                             set)]
              ;;  (println (str "Current sections: " (str/join ", " existing-section-ids)))
              ;;  (println (str "Current section pds: " (str/join ", " existing-periods)))
              ;;  (println (str "Student free pds: " student-free-periods))
              ;;  (println)
               (if-let [valid-period (select-class-period student-free-periods existing-periods)]
                ;;  (do (println (str "valid per: " valid-period \newline))
                ;;      (println (str "Course ID = " %2))
                ;;      (register-student-to-section %1 student (lookup-section %1 %2 valid-period)))
                 (register-student-to-section %1 student (lookup-section %1 %2 valid-period))
                 (let [new-period (choose-new-period course-catalog %2 student-free-periods)
                       updated-schedule (create-new-section %1
                                                            faculty
                                                            course-catalog
                                                            %2
                                                            new-period)
                       new-section-id (lookup-section updated-schedule %2 new-period)]
                   (register-student-to-section updated-schedule student new-section-id))))
            schedule
            required-classes)))

(defn schedule-all-required-classes
  "Schedules all students in the student body to their required classes."
  [schedule faculty course-catalog student-body]
  (reduce #(schedule-student-required-classes %1
                                              faculty
                                              course-catalog
                                              %2)
          schedule
          (vals student-body)))

(defn get-student-missing-classes
  [schedule student]
  (let [required-classes  (:requirements student)
        scheduled-classes (map :course-id
                               (-> schedule
                                   (get-student-schedule (:student-id student))
                                   vals))]
    (s/difference (conj (set required-classes) :lunch) (set scheduled-classes))))

(defn get-all-missing-classes
  [schedule student-body]
  (reduce #(if-let [missing-classes (seq (get-student-missing-classes schedule %2))]
             (assoc %1 (:student-id %2) (set missing-classes))
             %1)
          {}
          (vals student-body)))

(defn failure-summary!
  [schedule faculty student-body faculty-per-cert]
  (println)
  (println (str "Student Body (size): " (count student-body)))
  (println (str "Faculty (size): " (count faculty) ", Faculty/Cert: " faculty-per-cert))
  (println (str "No. of sections: " (count schedule)))
  (let [missing-classes (vals (get-all-missing-classes schedule student-body))
        correctly-scheduled (- (count student-body) (count missing-classes))
        missing-one (count (filter #(= 1 (count %)) missing-classes))
        missing-two (count (filter #(= 2 (count %)) missing-classes))
        missing-three (count (filter #(= 3 (count %)) missing-classes))
        missing-more (- (count missing-classes) missing-one missing-two missing-three)]
    ;; (println (str "Missing classes raw: " missing-classes))
    (println (str "No. students fully scheduled: " correctly-scheduled " (" (float (/ correctly-scheduled (count student-body))) ")"))
    (println (str "No. students missing one: " missing-one))
    (println (str "No. students missing two: " missing-two))
    (println (str "No. students missing three: " missing-three))
    (println (str "No. students missing more: " missing-more))))

#_(def test-course-catalog (g/generate-course-catalog 3366 16))
#_test-course-catalog

#_(def faculty (g/generate-faculty 1122 (vals test-course-catalog) 6))
#_faculty

#_(def student-body (g/generate-heterogeneous-student-body 2233 test-course-catalog 801))
#_(def extra-student (val (first (take-last 1 student-body))))
#_extra-student

#_(def first-pass (schedule-all-required-classes (create-all-lunch-sections {}) faculty test-course-catalog student-body))
#_first-pass

(defn -main
  "launch!"
  []
  (time (let [faculty-per-cert 10
              test-course-catalog (g/generate-course-catalog 3366 55)
              faculty (g/generate-faculty 1122 (vals test-course-catalog) faculty-per-cert)
              student-body (g/generate-heterogeneous-student-body 2233 test-course-catalog 1400)
              schedule (schedule-all-required-classes (create-all-lunch-sections {}) faculty test-course-catalog student-body)]
          (io/delete-file "./resources/output.txt")
          (with-open [wrtr (io/writer "./resources/output.txt" :append true)]
            (.write wrtr (str "Current run time: " (str (java.time.LocalDateTime/now))))
            (.write wrtr (str \newline "Results:\n"))

            (doseq [section (sort-by :required-cert  (sort-by #(first (:teachers %)) (vals schedule)))]
              (.write wrtr (str "Teacher ID: " (str/join "" (take-last 12 (str (first (:teachers section)))))
                                ", Course ID: " (str/join "" (take-last 12 (str (:course-id section))))
                                ", pd: " (:period section)
                                ", subject: " (:required-cert section)
                                ", enrollment: " (count (:roster section))
                                ", max: " (:max-size section) \newline)))
            (doseq [student-id (keys student-body)]
              (.write wrtr (str "Student ID: " student-id "\n"))
              (doseq [[section-id section] (get-student-schedule schedule student-id)]
                (.write wrtr (str "Section ID: " section-id ", Course ID: " (:course-id section) ", Period: " (:period section) "\n")))
              (.write wrtr "\n"))
            (doseq [[student-id missing-classes] (get-all-missing-classes schedule student-body)]
              (.write wrtr (str student-id ", " missing-classes "\n")))
            (failure-summary! schedule faculty student-body faculty-per-cert)))))

