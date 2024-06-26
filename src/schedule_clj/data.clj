(ns schedule-clj.data
  (:require [schedule-clj.dao :as dao]
            [schedule-clj.utils :as utils]))

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

(defn initialize-teacher
  "Initializes a teacher with the minimum amount of data,
   intended to be chained together with functions that add/remove data"
  [id]
  {:teacher-id (str id)
   :max-num-classes 5 ;; default value
   :certs #{}})

(defn initialize-student
  "Initializes a student with the minimum amount of data,
     intended to be chained together with functions that add/remove data"
  [id grade]
  {:student-id (str id)
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
   {:course-id (str course-id)
    :required-cert required-cert
    :min-size min-size
    :max-size max-size}))

(defn initialize-room
  "Initializes a room with the minimum amount of data,
     intended to be chained together with functions that add/remove data"
  [room-number max-size]
  {:room-number room-number :max-size max-size})

(defn initialize-section
  "Initializes a section with the minimum amount of data,
     intended to be chained together with functions that add/remove data"
  [course period room]
  {:id (random-uuid)
   :course-id (:course-id course)
   :period period
   :max-size (min (:max-size course) (:max-size room))
   :min-size (:min-size course)
   :required-cert (:required-cert course)})

(defn teacher-set-max-classes
  "Updates the teacher to now have a new maximum class limit"
  [teacher new-max]
  (assoc teacher :max-num-classes new-max))

(defn teacher-add-cert
  "Adds a certification to a teacher"
  [teacher new-cert]
  (update teacher :certs conj (utils/as-keyword new-cert)))

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

(defn teacher-count-preps
  [teacher]
  (dao/count-teacher-preps teacher))

(defn teacher-count-sections
  [teacher]
  (dao/count-teacher-sections teacher))

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


;; (defn can-teacher-take-section?
;;   [section teacher]
;;   (and))

(defn assign-teacher-to-section
  [section teacher]
  (update section :teachers conj (:teacher-id teacher)))

