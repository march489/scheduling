(ns schedule-clj.data)

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
(def SCIENCE-CLASSES #{:chemistry
                       :physics
                       :biology})
(def ART-CLASSES #{:visual
                   :dance
                   :music
                   :performance})
(def LANGUAGE-CLASSES #{:mandarin :arabic})
(def MATH-CLASSES #{:math})

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

(defn do-periods-overlap?
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

(defn is-half-block?
  [pd]
  (some #{pd} '(:A-per :B-per :C-per :D-per)))

(defn is-full-block?
  [pd]
  (and (some #{pd} PERIODS)
       (not (is-half-block? pd))))

(defn overlaps-with-lunch-period?
  [pd]
  (->> PERIODS
       (filter is-half-block?)
       (map #(do-periods-overlap? pd %))
       (some identity)))

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
   :electives []
   :priority 0
   :inclusion #{}
   :separate-class #{}})

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
  {:room-number room-number
   :max-size max-size
   :periods #{}
   :concurrency? false})

(defn initialize-section
  "Initializes a section with the minimum amount of data,
   intended to be chained together with functions that add/remove data"
  [id course period room]
  {:section-id (keyword (str id))
   :course-id (:course-id course)
   :period period
   :room-number (:room-number room)
   :max-size (min (:max-size course) (:max-size room))
   :min-size (:min-size course)
   :required-cert (:required-cert course)
   :teachers #{}
   :roster #{}})

(defn initialize-lunch-section
  [id period]
  {:section-id (keyword (str id))
   :course-id :lunch
   :required-cert :any
   :teachers #{}
   :period period
   :roster #{}
   :max-size 350})

(defn teacher-set-max-classes
  "Updates the teacher to now have a new maximum class limit"
  [teacher new-max]
  (assoc teacher :max-num-classes new-max))

(defn teacher-add-cert
  "Adds a certification to a teacher"
  [teacher new-cert]
  (if-let [c (some #{new-cert} CERTS)]
    (update teacher :certs conj c)
    teacher))

(defn teacher-add-cert-list
  "Adds every cert on a list to the teacher"
  [teacher certs-ls]
  (reduce teacher-add-cert teacher certs-ls))

(defn teacher-remove-cert
  "Removes a certification from a teacher"
  [teacher cert-to-remove]
  (update teacher :certs disj cert-to-remove))

(defn teacher-has-cert?
  "Determines whether a teacher has a given certification,
   with the flexibility to accept the names of certifications as either
   a string or a keyword"
  [teacher cert]
  (contains? (:certs teacher) cert))

(defn student-set-priority
  [student]
  (-> student
      (assoc :priority 0)
      (update :priority + (count (:inclusion student)))
      (update :priority + (* 5 (count (:separate-class student))))))

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
  (assoc student label true))

(defn student-remove-label
  [student label]
  (dissoc student label))

(defn student-assign-lunch
  [student lunch-period]
  (assoc student :lunch lunch-period))

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

(defn section-has-space?
  [section]
  (< (count (:roster section)) (:max-size section)))

(defn room-set-concurrency
  [room b?]
  (assoc room :concurrency? b?))