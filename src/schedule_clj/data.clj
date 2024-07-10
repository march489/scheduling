(ns schedule-clj.data
  (:require [schedule-clj.dao :as dao]
            [schedule-clj.utils :as utils]
            [clojure.string :as str]))

(def CERTS '(:english
             :math
             :social-science
             :arabic
             :mandarin
             :chemistry
             :physics
             :biology
             :cte
             :rotc
             :visual
             :dance
             :music
             :performance
             :sped
             :ell))

(def PERIODS '(:1st-per
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

(defn initialize-teacher
  "Initializes a teacher with the minimum amount of data,
   intended to be chained together with functions that add/remove data"
  [id]
  {:teacher-id (keyword (str id))
   :max-num-classes 5 ;; default value
   :certs #{}})

(defn initialize-student
  "Initializes a student with the minimum amount of data,
     intended to be chained together with functions that add/remove data"
  [id grade]
  {:student-id (keyword (str id))
   :grade grade
   :requirements '()
   :electives []})

(defn initialize-course
  "Defines a course, including its upper and lower limits.
   The `required-cert` is roughly equivalent to the department,
   but can be more specific in the case of science and language"
  ([course-id required-cert]
   (initialize-course course-id required-cert 20 30))
  ([course-id required-cert min-size max-size]
   {:course-id (keyword (str course-id))
    :required-cert required-cert
    :min-size min-size
    :max-size max-size}))

(defn initialize-room
  "Initializes a room with the minimum amount of data,
   intended to be chained together with functions that add/remove data"
  [room-number max-size]
  {:room-number room-number :max-size max-size :concurrency? false})

(defn initialize-section
  "Initializes a section with the minimum amount of data,
   intended to be chained together with functions that add/remove data"
  [id course period room]
  {:section-id (keyword (str id))
   :course-id (:course-id course)
   :period period
   :max-size (min (:max-size course) (:max-size room))
   :min-size (:min-size course)
   :required-cert (:required-cert course)
   :teachers #{}
   :roster #{}})

(defn teacher-set-max-classes
  "Updates the teacher to now have a new maximum class limit"
  [teacher new-max]
  (assoc teacher :max-num-classes new-max))

(defn teacher-add-cert
  "Adds a certification to a teacher"
  [teacher new-cert]
  (if-let [c (some #{(utils/as-keyword new-cert)} CERTS)]
    (update teacher :certs conj c)
    teacher))

(defn teacher-add-cert-list
  "Adds every cert on a list to the teacher"
  [teacher certs-ls]
  (reduce teacher-add-cert teacher certs-ls))

(defn teacher-remove-cert
  "Removes a certification from a teacher"
  [teacher cert-to-remove]
  (update teacher :certs disj (utils/as-keyword cert-to-remove)))

(defn teacher-has-cert?
  "Determines whether a teacher has a given certification,
   with the flexibility to accept the names of certifications as either
   a string or a keyword"
  [teacher cert]
  ((utils/as-keyword cert) (:certs teacher)))

(defn teacher-count-preps-db
  [teacher]
  (dao/count-teacher-preps teacher))

(defn teacher-count-sections-db
  [teacher]
  (dao/count-teacher-sections teacher))

(defn teacher-lookup-db
  [teacher-id]
  (when-let [results (seq (dao/teacher-lookup dao/db (str teacher-id)))]
    (-> (initialize-teacher teacher-id)
        (teacher-set-max-classes (-> results
                                     first
                                     :max_classes))
        (teacher-add-cert-list (->> results
                                    (map :cert)
                                    (map #(str/replace % ":" ""))
                                    (map keyword))))))

(defn student-add-required-class
  [student new-class]
  (update student :requirements conj new-class))

(defn student-remove-required-class
  [student required-class]
  (update student :requirements disj required-class))

(defn student-add-elective
  [student elective]
  (update student :electives conj elective))

(defn student-remove-elective
  [student elective]
  (update student :electives disj elective))

(defn studet-add-label
  [student label]
  (assoc student (utils/as-keyword label) true))

(defn student-remove-label
  [student label]
  (dissoc student (utils/as-keyword label)))

(defn course-set-max
  [course max-size]
  (assoc course :max-size max-size))

(defn course-set-min
  [course min-size]
  (assoc course :min-size min-size))

(defn section-assign-teacher
  [section teacher]
  (update section :teachers conj (:teacher-id teacher)))

(defn section-register-student
  [section student]
  (update section :roster conj (:student-id student)))

(defn room-set-concurrency
  [room b?]
  (assoc room :concurrency? b?))