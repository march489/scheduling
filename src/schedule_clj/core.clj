(ns schedule-clj.core 
  (:require [schedule-clj.generate :as g]
            [schedule-clj.data :as d]
            [clojure.set :as s]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.generators :as gen]
            [clojure.stacktrace :as st])
  (:gen-class))



;;;;;;;;;;;;;;;;;;;;;;
;; Defining courses ;;
;;;;;;;;;;;;;;;;;;;;;;

(def ENDORSEMENTS
  "The specific ISBE endorsements required to teach classes. 
   This list is not comprehensive for the state of Illinois, 
   but only serves to cover the requisite endorsements in my building.
   The full list of endorsements can be found on the 
   [ILTS page](https://www.il.nesinc.com/PageView.aspx?f=GEN_Tests.html)"
  (list :english-language-arts
        :math
        :social-science-econ
        :social-science-geography
        :social-science-poli-sci
        :social-science-psych
        :social-science-history
        :world-langauge-arabic
        :world-language-mandarin
        :science-chemistry
        :science-physics
        :science-biology
        :cte
        :rotc
        :visual-arts
        :dance
        :music
        :theater-drama
        :phys-ed
        :sped))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classifying courses in the same discipline ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro install-department-utility-functions
  "Creates two utility functions given a `dept-name` and `included-endorsements`:
   
   - `<dept-name>-cert?`: Checks if a passed cert is one of the `included-endorsemensts`.

   - `<dept-name>-class?: Checks if a passed course or section has a `:required-endorsement`
   that is one of the `included-endorsements`.
   
   Example: `(install-department-utility-functions \"science\" :biology :physics)`

   This creates two functions: `science-cert?` and `science-class?`.

   - `(science-cert? :biology)` ; => :biology

   - `(science-cert? :math)` ; => nil

   - `(science-class? {:required-endorsement :biology})` ; => :biology

   - `(science-class {:required-endorsement :art})` => nil
   "
  [dept-name & included-endorsements]
  (let [cert? (symbol (str dept-name "-cert?"))
        class? (symbol (str dept-name "-class?"))]
    `(do
      (defn ~cert?
         ~(str "Is the `cert` in the " dept-name " department?")
         [cert#]
         (some #{cert#} (list ~@included-endorsements)))
      (defn ~class?
        ~(str "Is the class (e.g. `course` or `section`) 
              in the " dept-name " department?")
        [class#]
        (~cert? (:required-endorsement class#))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installing department utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-department-utility-functions "english" :english-language-arts)

(install-department-utility-functions "math" :math)

(install-department-utility-functions "social-science"
                                      :social-science-econ
                                      :social-science-geography
                                      :social-science-poli-sci
                                      :social-science-psych
                                      :social-science-history)

(install-department-utility-functions "world-language"
                                      :world-langauge-arabic
                                      :world-language-mandarin)

(install-department-utility-functions "science"
                                      :science-biology
                                      :science-chemistry
                                      :science-physics)

(install-department-utility-functions "cte" :cte)

(install-department-utility-functions "rotc" :rotc)

(install-department-utility-functions "art"
                                      :visual-arts
                                      :dance
                                      :music
                                      :theater-drama)

(install-department-utility-functions "phys-ed"
                                      :phys-ed)

(install-department-utility-functions "special-ed"
                                      :lbs1)


(defn required-space
  "Returns the required classroom space for a specific course."
  [course]
  (cond (science-class? course) :lab-room
        (art-class? course) :art-room
        (phys-ed-class? course) :gym-room
        (special-ed-class? course) :sped-room
        :else :standard-room))

;;;;;;;;;;;;;;;;;;;;;
;; Defiing periods ;;
;;;;;;;;;;;;;;;;;;;;;

(def PERIODS (list :1st-per
                   :2nd-per
                   :3rd-per
                   :4th-per
                   :5th-per
                   :6th-per
                   :7th-per
                   :8th-per
                   :A-per
                   :B-per
                   :C-per
                   :D-per))

(defn overlapping-periods?
  [pd-1 pd-2]
  (or (= pd-1 pd-2)
      (let [pds #{pd-1 pd-2}]
        (or (= pds #{:2nd-per :A-per})
            (= pds #{:2nd-per :B-per})
            (= pds #{:6th-per :A-per})
            (= pds #{:6th-per :B-per})
            (= pds #{:3rd-per :C-per})
            (= pds #{:3rd-per :D-per})
            (= pds #{:7th-per :C-per})
            (= pds #{:7th-per :D-per})))))

(defn half-block?
  [pd]
  (some #{pd} '(:A-per :B-per :C-per :D-per)))

(defn full-block?
  [pd]
  (and (some #{pd} PERIODS)
       (not (half-block? pd))))

(defn morning-period?
  [pd]
  (some #{pd} '(:1st-per :2nd-per :5th-per :6th-per :A-per :B-per)))

(defn afternoon-period?
  [pd]
  (and (some #{pd} PERIODS)
       (not (morning-period? pd))))

;;;;;;;;;;;;;
;; Faculty ;;
;;;;;;;;;;;;;

;; legal/constract constraints
(def GENED-MAX-TEACHER-PREPS
  "The contract number of distinct courses a general education teacher
   can be assigned to teach."
  2)

(def GENED-MAX-TEACHER-NUM-SECTIONS
  "The contract number of maximum sections that a general education teacher
   can be assigned to teach."
  5)

(defn initialize-teacher
  "Initializes a teacher with the minimum amount of data,
   intended to be chained together with functions that add/remove data"
  [id]
  {:teacher-id (keyword (str id))
   :max-num-classes GENED-MAX-TEACHER-NUM-SECTIONS 
   :certs #{}})

;;;;;;;;;;;;;;
;; Students ;;
;;;;;;;;;;;;;;

(defn initialize-student
  "Initializes a student with the minimum amount of data,
   intended to be chained together with functions that modify its fields."
  [id grade]
  {:student-id (keyword (str id))
   :grade grade
   :requirements '()
   :electives []
   :inclusion #{}
   :separate-class #{}})

;;;;;;;;;;;
;; Rooms ;;
;;;;;;;;;;;

(defn default-room-capacity
  "Room capacity for different kinds of rooms in the building.
   Throws an exception if the `room-type` is missing or invalid."
  [room-type]
  (condp = room-type
    :lab-room 35
    :sped-room 15
    :gym-room 60
    :art-room 30
    :standard-room 28))

(defn initialize-room
  [room-number room-type] 
  {:room-number room-number
   :room-type room-type
   :max-capacity (default-room-capacity room-type)})

;;;;;;;;;;;;;;;;;;;;;;;;
;; Courses & Sections ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(def DEFAULT-MINIMUM-SECTION-SIZE
  "The minimum number of students required for a section to be valid."
  10)

(defn initialize-course
  "Initializes a course with its id (a UUID) and the endorsement
   required to teach it." 
  ([course-id required-endorsement]
   {:course-id (keyword (str course-id))
    :required-endorsement required-endorsement}))

(defn initialize-section
  "initializes a section, an instantiation of a course, which includes
   specific teacher(s), time slot (period), and location (room).
   Intended to be chained together with functions that modify its fields."
  [section-id course period room & options]
  (let [opts (apply hash-map options)]
    {:section-id (keyword (str section-id))
     :course-id (:course-id course)
     :period period
     :room-number (:room-number room)
     :min-size DEFAULT-MINIMUM-SECTION-SIZE
     :max-size ()}))







(defn all-available-course-sections
  "Returns a list of all section-ids in the schedule with the given course-id 
   that have open space"
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

(defn can-gened-teacher-take-section?
  "Determines if a teacher can add a section of the given course to their schedule, by checking 
   
   1. Is the teacher currently below the max number of classes they can teach?, 
   2. Does the teacher have the required cert for the class?, and 
   3. Would adding the class mean the teacher has more than the max number of distinct preps?"
  [schedule teacher course period]
  (and (< (count-teacher-sections schedule (:teacher-id teacher)) (:max-num-classes teacher))
       (d/teacher-has-cert? teacher (:required-endorsement course))
       (some #{period} (get-teacher-open-periods schedule (:teacher-id teacher)))
       (-> schedule
           (get-teacher-preps (:teacher-id teacher))
           (conj (:course-id course))
           count
           (<= GENED-MAX-TEACHER-PREPS))))

(defn can-sped-teacher-take-section?
  "Similar to `can-gened-teacher-take-section?` except it relaxes the requirement
   that the sped teacher have the `:required-endorsement` and instead have the `:iep` cert. 
   The max-preps requirement is also relaxed."
  [schedule teacher period]
  (and (< (count-teacher-sections schedule (:teacher-id teacher)) (:max-num-classes teacher))
       (d/teacher-has-cert? teacher :lbs1)
       (some #{period} (get-teacher-open-periods schedule (:teacher-id teacher)))))

(defn find-available-gened-teacher
  "Finds a teacher among the faculty that can add a section of that course to their schedule"
  [schedule faculty course period]
  (->> faculty
       vals
       (filter #(can-gened-teacher-take-section? schedule % course period))
       (sort-by #(count-teacher-sections schedule %) >)
       first))

(defn find-available-sped-teacher
  [schedule faculty period]
  (->> faculty
       vals
       (filter #(can-sped-teacher-take-section? schedule % period))
       (sort-by #(count-teacher-sections schedule %) >)
       first))

(defn get-student-open-periods
  "Gets the currently open periods in the student's schedule."
  [schedule student-id]
  (->> (get-student-schedule schedule student-id)
       vals
       (map #(:period %))
       (apply find-non-overlapping-periods)))

(defn register-student-to-section
  "Updates the schedule to add a student to the roster for a section. If no such section exists,
   returns the existing schedule and fails to register the student."
  [schedule student section-id]
  (if section-id
    (if (section-id schedule)
      (update schedule section-id d/section-register-student student)
      schedule)
    (do (Exception. (str "Null section id; missing course for sutdent " (:student-id student)))
        schedule)))

(defn create-new-section
  "Updates the schedule to include a new section for a course if there's a teacher who can teach it."
  [schedule faculty course-catalog course-id period]
  (let [course (lookup-course course-catalog course-id)]
    (if-let [teacher (find-available-gened-teacher schedule faculty course period)]
      (let [section (d/initialize-section (random-uuid) course period (d/initialize-room "222" DEFAULT-ROOM-CAPACITY))]
        (-> schedule
            (assoc (:section-id section) section)
            (update (:section-id section) d/section-assign-teacher teacher)))
      schedule)))

(defn create-lunch-section
  "Creates a new lunch section during the specified period."
  [schedule period]
  (let [lunch-period (d/initialize-lunch-section (random-uuid) period)]
    (assoc schedule (:section-id lunch-period) lunch-period)))

(defn create-seminar-section
  "Creates a new sped seminar section during the specified period."
  [schedule period]
  (let [seminar-period (d/initialize-sped-seminar (random-uuid) period)]
    (assoc schedule (:section-id seminar-period) seminar-period)))

(defn create-all-lunch-sections
  "Creates a lunch section for each half-block"
  [schedule]
  (reduce create-lunch-section schedule '(:A-per :B-per :C-per :D-per)))

(defn create-all-seminar-sections
  "Creates a seminar section for each half-block"
  [schedule]
  (reduce create-seminar-section schedule '(:A-per :B-per :C-per :D-per)))

(defn initialize-blank-schedule
  "Creates a blank schedule with the required singleton classes,
   i.e. sped seminars and lunch sections during each half-block."
  []
  (-> {}
      create-all-lunch-sections
      create-all-seminar-sections))

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
  (let [cert (:required-endorsement (course-id course-catalog))]
    (cond (science-cert? cert) (first (sort (seq student-free-periods)))
          (art-cert? cert) (first (sort (seq student-free-periods)))
          (math-cert? cert) (last (sort (seq student-free-periods)))
          (world-language-cert? cert) (last (sort (seq student-free-periods)))
          :else (first (gen/shuffle (seq student-free-periods))))))

(defn schedule-single-student-class-gened
  "Schedules a student to a general education class. 
   Caller previous checked that this student does not have this `course-id`
   referenced in `:inclusion` or `:separate-class`."
  [schedule faculty course-catalog course-id student]
  (let [existing-section-ids (all-available-course-sections schedule course-id)
        existing-periods (get-periods-from-section-ids schedule existing-section-ids)
        student-free-periods (->> student
                                  :student-id
                                  (get-student-open-periods schedule)
                                  set)]
    (if-let [valid-period (select-class-period student-free-periods existing-periods)]
      (register-student-to-section schedule student (lookup-section schedule course-id valid-period))
      (let [new-period (choose-new-period course-catalog course-id student-free-periods)
            updated-schedule (create-new-section schedule
                                                 faculty
                                                 course-catalog
                                                 course-id
                                                 new-period)
            new-section-id (lookup-section updated-schedule course-id new-period)]
        (register-student-to-section updated-schedule student new-section-id)))))

(defn assign-inclusion-teacher
  [schedule faculty student section-id]
  (if-let [inclusion-teacher (find-available-sped-teacher schedule faculty (:period (section-id schedule)))]
    ;; (assign-teacher-to-section schedule inclusion-teacher section-id)
    (do (println "Found co-teacher!")
        (update schedule section-id d/section-assign-teacher inclusion-teacher))
    (do (println "No coteacher found! inclusion -> gened")
        (-> schedule
            (update section-id dissoc :inclusion)
            (update-in [section-id :roster] disj (:student-id student))))))

(defn schedule-single-student-class-inclusion
  "Schedules a student to an inclusion class. Needs to first check if 
   there's an existing inclusion section for this `course-id`. If there isn't,
   then it needs to check if there are gened classes that can turned into an
   inclusion class."
  [schedule faculty course-catalog course-id student]
  (do (println (str "Inclusion section for student: " (:student-id student) " and course: " course-id))
      (let [all-existing-section-ids (all-available-course-sections schedule course-id)
            existing-inclusion-section-ids (filter #(:inclusion (% schedule)) all-existing-section-ids)
            existing-inclusion-sections (map #(% schedule) existing-inclusion-section-ids)
            student-free-periods (->> student
                                      :student-id
                                      (get-student-open-periods schedule)
                                      set)]
        (if-let [valid-inclusion-period (select-class-period student-free-periods
                                                             (->> existing-inclusion-section-ids
                                                                  (map #(% schedule))
                                                                  (map :period)))]
          (let [valid-section-id (->> existing-inclusion-sections
                                      (filter #(= valid-inclusion-period (:period %)))
                                      first)]
            (do (println "Found inclusion section!")
                (register-student-to-section schedule student valid-section-id)))
          (let [updated-schedule (schedule-single-student-class-gened schedule faculty course-catalog course-id student)
                student-schedule (get-student-schedule updated-schedule (:student-id student))]
            (if-let [new-inclusion-section-coll (seq (filter #(= course-id (:course-id %)) (vals student-schedule)))]
              (do (println (str "gened -> inclusion, section id: " (:section-id (first new-inclusion-section-coll))))
                  (-> updated-schedule
                      (update (:section-id (first new-inclusion-section-coll)) assoc :inclusion true)
                      (assign-inclusion-teacher faculty student (:section-id (first new-inclusion-section-coll)))))
              (do (println "No suitable gened section found.")
                  updated-schedule)))))))

(defn schedule-single-student-class-dispatch
  "Registers a student to a section of a single course ID.
   Returns the updated schedule."
  [schedule faculty course-catalog course-id student]
  (cond
    (contains? (:inclusion student) course-id) (schedule-single-student-class-inclusion schedule faculty course-catalog course-id student)
    :else (schedule-single-student-class-gened schedule faculty course-catalog course-id student)))

(defn student-missing-required-classes
  "Returns a set of the student's required classes that the student
   has not yet been scheduled for."
  [schedule student]
  (let [required-classes  (:requirements student)
        scheduled-classes (map :course-id
                               (-> schedule
                                   (get-student-schedule (:student-id student))
                                   vals))
        missing-classes (s/difference (conj (set required-classes) :lunch) (set scheduled-classes))
        missing-inclusion-classes (s/intersection (:inclusion student) missing-classes)
        other-classes (s/difference missing-classes missing-inclusion-classes)]
    (concat (seq missing-inclusion-classes) (seq other-classes))))

(defn get-all-missing-classes
  "Returns a map of student-id keys with vals consisting of the set of required classes
   that they have not yet been schedulued for. "
  [schedule student-body]
  (reduce #(if-let [missing-classes (seq (student-missing-required-classes schedule %2))]
             (assoc %1 (:student-id %2) (set missing-classes))
             %1)
          {}
          (vals student-body)))

#_(def st-body (g/generate-heterogeneous-student-body 2255
                                                      (g/generate-course-catalog 3366 55)
                                                      1400))

(defn schedule-student-required-classes
  "Registers students to their required classes. If a section of a required class 
   doesn't already exist, this will create a new section in an open slot 
   in the student's schedule and assign the student to that class."
  [schedule faculty course-catalog student]
  (let [required-classes (seq (student-missing-required-classes schedule student))]
    (reduce #(schedule-single-student-class-dispatch %1 faculty course-catalog %2 student)
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
          (sort-by :priority > (vals student-body))))


(defn students-with-incorrect-lunches
  [schedule student-body]
  (let [get-lunch-count (fn [class-list] (count (filter #(= :lunch (:course-id %)) class-list)))
        lunch-map (-> student-body
                      (update-vals #(get-student-schedule schedule (:student-id %)))
                      (update-vals #(get-lunch-count (vals %))))]
    (into {} (for [[id lunches] lunch-map
                   :when (not= lunches 1)]
               [id lunches]))))

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

(defn -main
  "launch!"
  []
  (time (try (let [faculty-per-cert :nothing
                   course-catalog (g/generate-course-catalog 3366 55)
                   student-body (g/generate-heterogeneous-student-body 2233 course-catalog 1400)
                   faculty (g/generate-faculty 1122 course-catalog (vals student-body))
                   schedule (schedule-all-required-classes (initialize-blank-schedule) faculty course-catalog student-body)]
               (io/delete-file "./resources/output.txt")
               (with-open [wrtr (io/writer "./resources/output.txt" :append true)]
                 (.write wrtr (str "Current run time: " (str (java.time.LocalDateTime/now))))

                 (.write wrtr (str \newline "Faculty:\n"))
                 (doseq [teacher (sort-by (comp first :certs) (vals faculty))]
                   (.write wrtr (str "teacher-id: " (:teacher-id teacher) " certs: " (:certs teacher) \newline)))

                 (.write wrtr (str \newline))

                 (.write wrtr (str \newline "Sections:\n"))
                 (doseq [section (sort-by :required-endorsement  (sort-by #(first (:teachers %)) (vals schedule)))]
                   (.write wrtr (str
                                ;;  "Teacher IDs: " (str/join "" (take-last 12 (str (first (:teachers section)))))
                                 "Teacher IDs: " (str/join ", " (map #(str/join "" (take-last 12 (str %))) (:teachers section)))
                                 ", Course ID: " (str/join "" (take-last 12 (str (:course-id section))))
                                 ", pd: " (:period section)
                                 ", subject: " (:required-endorsement section)
                                 ", enrollment: " (count (:roster section))
                                 ", max: " (:max-size section) \newline)))
                 (.write wrtr (str \newline))
                 (doseq [student-id (keys student-body)]
                   (.write wrtr (str "Student ID: " student-id "\n"))
                   (doseq [[section-id section] (get-student-schedule schedule student-id)]
                     (.write wrtr (str "Section ID: " section-id ", Course ID: " (:course-id section) ", Period: " (:period section) "\n")))
                   (.write wrtr "\n"))
                 (doseq [[student-id missing-classes] (get-all-missing-classes schedule student-body)]
                   (.write wrtr (str student-id ", " missing-classes "\n")))

                 (.write wrtr (str \newline "Students with more than one lunch:" \newline))
                 (doseq [student (students-with-incorrect-lunches schedule student-body)]
                   (.write wrtr (str "Student id: " student \newline)))
                 (failure-summary! schedule faculty student-body faculty-per-cert)))
             (catch Exception e (st/print-stack-trace e)))))

