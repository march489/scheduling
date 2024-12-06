(ns schedule-clj.core
  (:require [schedule-clj.data :as d]
            [clojure.set :as s]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.generators :as gen]
            [clojure.stacktrace :as st]
            [clojure.test :as t])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling IDs & UUIDs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn as-keyword-id
  "Guarantees format of UUIDs used as keyword ids for students, teachers,
   courses, and sections."
  [id]
  {:post [(keyword? %)]}
  (cond (keyword? id) id
        (uuid? id) (keyword (str id))
        (string? id) (-> id
                         (str/replace #"[^a-zA-Z\d\s-]" "")
                         (str/replace #"\s+" "-")
                         keyword)
        :else (throw (IllegalArgumentException. (str (class id) " is invalid format for an id")))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making pointed maps ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn as-pointed-map
  "Creates a \"pointed map\", which uses a collection of existing maps,
   extracts a specific key that will serve as the \"id\" of said maps,
   and creates a new map with the keys being these ids."
  [maps id-key]
  {:pre [(keyword? id-key),
         (every? id-key maps),
         (coll? maps)]}
  (->> (map vector (map id-key maps) maps)
       (into {})))
;;;;;;;;;;;
;; Rooms ;;
;;;;;;;;;;;

(defn default-room-min-capacity
  "Room min capacity for different kinds of rooms in the building.
   Throws an exception if the `room-type` is missing or invalid."
  [room-type]
  (condp = room-type
    :auditorium 20
    :lab-room 10
    :sped-room 1
    :gym-room 20
    :art-room 5
    :standard-room 20
    :cafeteria 1))

(defn default-room-max-capacity
  "Room max capacity for different kinds of rooms in the building.
   Throws an exception if the `room-type` is missing or invalid."
  [room-type]
  (condp = room-type
    :auditorium 500
    :lab-room 35
    :sped-room 15
    :gym-room 60
    :art-room 30
    :standard-room 28
    :cafeteria 360))

(defn initialize-room
  "Initializes a room in the building. Certain rooms like the `:cafeteria`
   or `:auditorium` are special, and don't need a room number. Normal classroom spaces
   need both a room number and a type."
  ([room-type]
   {:pre [(keyword? room-type)]}
   (initialize-room (name room-type) room-type))
  ([room-number room-type]
   {:pre [(keyword? room-type)]
    :post [(string? (:room-number %))]}
   {:room-number  (if (keyword? room-number) (name room-number) (str room-number))
    :room-type room-type
    :min-capacity (default-room-min-capacity room-type)
    :max-capacity (default-room-max-capacity room-type)}))


;;;;;;;;;;;;;;;;;;;;;;
;; Defining courses ;;
;;;;;;;;;;;;;;;;;;;;;;

(def DEPARTMENTS
  "These are the departments that classroom teachers belong to. Every endorsed teacher
   falls into one of these departments."
  (list :english-language-arts
        :math
        :social-science
        :world-langauge
        :science
        :cte
        :rotc
        :art
        :phys-ed
        :special-ed))

(def CORE-SUBJECTS
  "These are the core subject areas (departments). importantly, these are the subjects
   in which students with IEPs can have instructional minutes."
  (list :english-language-arts
        :math
        :science
        :social-science
        :world-langauge))

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
        :phys-ed))

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
         ~(str "Is the class (e.g. `course` or `section`) in the " dept-name " department?")
         [class#]
         (~cert? (:required-endorsement class#))))))

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

(defn department
  [endorsement]
  {:pre [endorsement]}
  (cond (english-cert? endorsement) :english-language-arts
        (math-cert? endorsement) :math
        (social-science-cert? endorsement) :social-science
        (world-language-cert? endorsement) :world-language
        (science-cert? endorsement) :science
        (cte-cert? endorsement) :cte
        (rotc-cert? endorsement) :rotc
        (art-cert? endorsement) :art
        (phys-ed-cert? endorsement) :phys-ed
        (special-ed-cert? endorsement) :special-ed
        :else ((println (nil? endorsement))
               (throw (IllegalArgumentException. (str endorsement " is not a valid endorsement."))))))

(defn lunch?
  "Is the section a lunch section?"
  [class]
  (= :lunch (:course-id class)))

(defn sped-seminar?
  "Is the section a sped-seminar section?"
  [class]
  (= :sped-seminar (:course-id class)))


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
  (and (#{pd} PERIODS)
       (not (half-block? pd))))

(defn morning-period?
  [pd]
  (some #{pd} '(:1st-per :2nd-per :5th-per :6th-per :A-per :B-per)))

(defn afternoon-period?
  [pd]
  (and (#{pd} PERIODS)
       (not (morning-period? pd))))

;;;;;;;;;;;;;
;; Faculty ;;
;;;;;;;;;;;;;

;; legal/constract constraints

;; #TODO determine if this is better as a function cond on course-id 
;; or something similar
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
  {:teacher-id (as-keyword-id id)
   :max-num-classes GENED-MAX-TEACHER-NUM-SECTIONS})

;;;;;;;;;;;;;;
;; Students ;;
;;;;;;;;;;;;;;

(defn initialize-student
  "Initializes a student with the minimum amount of data,
   intended to be chained together with functions that modify its fields."
  [id grade & options]
  (let [{:keys [inclusion separate-class]} options]
    (as-> {:student-id (as-keyword-id id) :grade (str grade)} st
      (if inclusion (assoc st :inclusion (set inclusion)) st)
      (if separate-class (assoc st :separate-class (set separate-class)) st))))

(defn has-iep?
  "Does the student have an IEP?"
  [student]
  (or (:inclusion student)
      (:separate-class student)))

(defn has-seminar?
  "Does the student have seminar minutes? This information is stored 
   in `:separate-class #{:sped-seminar}."
  [student]
  (contains? (:separate-class student) :sped-seminar))

(defn student-iep-services-for-course
  "Returns the level of IEP student services a `student` has for a particular `course`.
   Returns either `:inclusion`, `:separate-class`, or `nil` if the student doesn't receive
   services for that course."
  [student course]
  (cond (lunch? course) nil
        (sped-seminar? course) :separate-class
        :else (let [department (department (:required-endorsement course))]
                (cond (contains? (:separate-class student) department) :separate-class
                      (contains? (:inclusion student) department) :inclusion))))


;; functions for analyzing the student body pointed map

(defn tickets
  "Returns a flat list of tickets from all students in the `student-body`."
  [student-body]
  (->> student-body
       vals
       (mapcat :tickets)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Courses & Sections ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Initializing courses


(def LUNCH-COURSE {:course-id :lunch})
(def SPED-SEMINAR-COURSE {:course-id :sped-seminar :required-endorsement :lbs1})

(defn initialize-course
  "Initializes a course with its id (a UUID) and the endorsement 
   required to teach it."
  [course-id required-endorsement]
  {:course-id (as-keyword-id course-id)
   :required-endorsement required-endorsement})

;; Initializing sections
(defmulti initialize-section
  "Instantiates a course into a section.
   Dispatches the initialization of different sections based on the `course-id`.
   Most classes are handled by the default implementation, but the dispatch handles 
   special cases like `:lunch` or `:sped-seminar`."
  (fn [_section-id course _period _room & _opts]
    (:course-id course)))

(defmethod initialize-section :lunch
  initialize-lunch-section
  [section-id course period room & _options]
  ^{:doc "Initializes a lunch section."}
  {:pre [(nil? (:required-endorsement course))
         (= :cafeteria (:room-type room)),
         (= :lunch (:course-id course)),
         (half-block? period)]}
  {:section-id (as-keyword-id section-id)
   :course-id :lunch
   :period period
   :room :cafeteria
   :environment :gen-ed
   :min-size (default-room-min-capacity :cafeteria)
   :max-size (default-room-max-capacity :cafeteria)})

(defmethod initialize-section :sped-seminar
  initialize-sped-seminar-section
  [section-id course period room & _options]
  ^{:doc "Initializes a special education seminar section"}
  {:pre [(= :sped-room (:room-type room)),
         (= :sped-seminar (:course-id course)),
         (half-block? period)]} ; #TODO check if this is a required constraint 
  {:section-id (as-keyword-id section-id)
   :course-id :sped-seminar
   :period period
   :room (:room-number room)
   :required-endorsement :lbs1
   :environment :separate-class
   :min-size (:min-capacity room)
   :max-size (:max-capacity room)})

(defmethod initialize-section :default
  initialize-default-section
  [section-id course period room & options]
  (let [{:keys [environment] :or {environment :gen-ed}} (apply hash-map options)]
    {:section-id (as-keyword-id section-id)
     :course-id (:course-id course)
     :period period
     :room (:room-number room)
     :required-endorsement (:required-endorsement course)
     :environment environment
     :min-size (:min-capacity room)
     :max-size (:max-capacity room)  ;; #TODO update logic with sped requirements
     }))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registration ticket ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn initialize-registration-ticket
  "Initializes registration ticket for a `student`. 
   A ticket should include the `course-id`. All other flags are optional,
   and have default values. `:track` refers to whether the class is `:regular-level`, 
   `:honors-level`, `:ap-level`, or `:dual-enrollment`."
  [student course & options]
  (let [services (student-iep-services-for-course student course)
        {elective :elective} (apply hash-map options)
        ticket {:course-id (as-keyword-id (:course-id course))
                :required-endorsement (:required-endorsement course)
                :student-id (:student-id student)}]
    (cond-> ticket
      elective (assoc :elective elective)
      services (assoc services true))))

(defn add-single-ticket ;; #TODO determine if its worth keeping this function.
  "Adds `registration-ticket` to student."
  [student registration-ticket]
  (update student :tickets (fnil conj []) registration-ticket))

(defn add-lunch-ticket
  "Add a registration ticket for lunch."
  [student]
  (let [lunch-ticket (initialize-registration-ticket student LUNCH-COURSE)]
    (update student :tickets (fnil conj []) lunch-ticket)))

(defn add-seminar-ticket
  "Adds a seminar registration ticket to a student's tickets if the student
   has `:sped-seminar` separate class minutes."
  [student]
  (let [seminar-ticket (initialize-registration-ticket student SPED-SEMINAR-COURSE)]
    (add-single-ticket student seminar-ticket)))

(defn add-registration-tickets
  "Adds several tickets to a student."
  [student & ticket-options]
  (let [{required-tickets :required-tickets
         elective-tickets :elective-tickets} ticket-options]
    (reduce add-single-ticket student (concat required-tickets
                                              (map #(assoc % :elective true) elective-tickets))))
  ;; (reduce add-single-ticket student tickets)
  )








;; (defn -main
;;   "launch!"
;;   []
;;   (time (try (let [faculty-per-cert :nothing
;;                    course-catalog (g/generate-course-catalog 3366 55)
;;                    student-body (g/generate-heterogeneous-student-body 2233 course-catalog 1400)
;;                    faculty (g/generate-faculty 1122 course-catalog (vals student-body))
;;                    schedule (schedule-all-required-classes (initialize-blank-schedule) faculty course-catalog student-body)]
;;                (io/delete-file "./resources/output.txt")
;;                (with-open [wrtr (io/writer "./resources/output.txt" :append true)]
;;                  (.write wrtr (str "Current run time: " (str (java.time.LocalDateTime/now))))

;;                  (.write wrtr (str \newline "Faculty:\n"))
;;                  (doseq [teacher (sort-by (comp first :certs) (vals faculty))]
;;                    (.write wrtr (str "teacher-id: " (:teacher-id teacher) " certs: " (:certs teacher) \newline)))

;;                  (.write wrtr (str \newline))

;;                  (.write wrtr (str \newline "Sections:\n"))
;;                  (doseq [section (sort-by :required-endorsement  (sort-by #(first (:teachers %)) (vals schedule)))]
;;                    (.write wrtr (str
;;                                 ;;  "Teacher IDs: " (str/join "" (take-last 12 (str (first (:teachers section)))))
;;                                  "Teacher IDs: " (str/join ", " (map #(str/join "" (take-last 12 (str %))) (:teachers section)))
;;                                  ", Course ID: " (str/join "" (take-last 12 (str (:course-id section))))
;;                                  ", pd: " (:period section)
;;                                  ", subject: " (:required-endorsement section)
;;                                  ", enrollment: " (count (:roster section))
;;                                  ", max: " (:max-size section) \newline)))
;;                  (.write wrtr (str \newline))
;;                  (doseq [student-id (keys student-body)]
;;                    (.write wrtr (str "Student ID: " student-id "\n"))
;;                    (doseq [[section-id section] (get-student-schedule schedule student-id)]
;;                      (.write wrtr (str "Section ID: " section-id ", Course ID: " (:course-id section) ", Period: " (:period section) "\n")))
;;                    (.write wrtr "\n"))
;;                  (doseq [[student-id missing-classes] (get-all-missing-classes schedule student-body)]
;;                    (.write wrtr (str student-id ", " missing-classes "\n")))

;;                  (.write wrtr (str \newline "Students with more than one lunch:" \newline))
;;                  (doseq [student (students-with-incorrect-lunches schedule student-body)]
;;                    (.write wrtr (str "Student id: " student \newline)))
;;                  (failure-summary! schedule faculty student-body faculty-per-cert)))
;;              (catch Exception e (st/print-stack-trace e)))))

