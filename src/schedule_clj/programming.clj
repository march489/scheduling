(ns schedule-clj.programming
  (:require [schedule-clj.core :as c]
            [schedule-clj.generate :as gen] ;; temporary during development
            ))

;;;;;;;;;;;;;;;;;;
;; The schedule ;;
;;;;;;;;;;;;;;;;;;

(defn empty-schedule
  "Generates an empty schedule, which consists of programmed sections
   and unresolved tickets. `::sections` must be represented as a pointed-map."
  []
  {::sections {}
   ::passes 0
   ::unresolved-tickets nil})

;;;;;;;;;;;;;;;;;;;;;;;
;; Managing sections ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn course-sections
  "Returns sub-map of all sections in the schedule with a specific `course-id`."
  [schedule course-id]
  (let [sections (::sections schedule)]
    (select-keys sections (for [[id sect] sections :when (= course-id (::c/course-id sect))] id))))

(defn lunch-sections
  "Returns a sub-map of the schedule `::sections` consisting of the four lunch sections."
  [schedule]
  {:post [(<= (count %) 4)]} ;; asserts at there are at most 4 lunch sections
  (course-sections schedule ::c/lunch))

(defn section-student-roster
  "Returns the roster of student-ids for students registerd to a particular `section-id`,
   which consists of a pointed map of registration tickets."
  [schedule section-id]
  (get-in schedule [::sections section-id ::c/roster]))

(defn section-registered-teachers
  "Returns the pointed map of teacher(s) assigned to teach this section."
  [schedule section-id]
  (merge (get-in schedule [::sections section-id ::c/teacher])
         (get-in schedule [::sections section-id ::c/co-teacher])))

(defn section-register-student
  "Updates the schedule to add the registration ticket to the roster, 
   thus registering the student to the section. If the supplied `section-id` doesn't exist,
   the ticket will be added to the list of `::unresolved-tickets`."
  [schedule section-id registration-ticket]
  (if (section-id (::sections schedule))
    (update-in schedule
               [::sections section-id ::c/roster]
               assoc (::c/student-id registration-ticket) registration-ticket)
    (update schedule ::unresolved-tickets conj registration-ticket)))

(defn section-deregister-student
  "Updates the schedule to remove the passed `student-id` from the passed `section-id`.
   The removed student's ticket is added to the list of `::unresolved-tickets`.
   If the section doesn't exist or the student is not in that section, 
   the original schedule is returned."
  [schedule section-id student-id]
  (if-let [found-student-ticket (student-id (section-student-roster schedule section-id))]
    (-> schedule
        (update-in [::sections section-id ::c/roster] dissoc student-id)
        (update ::unresolved-tickets conj found-student-ticket))
    schedule))

(defn section-assign-teacher
  "Updates the schedule to assign the `teacher` to the section.
   Assumes the teacher being passed is a valid choice.
   Throws an assertion error if the passed `section-id` doesn't exist."
  [schedule section-id teacher-id]
  {:pre [(get-in schedule [::sections section-id])]}
  (assoc-in schedule [::sections section-id ::c/teacher] teacher-id))

(defn section-assign-coteacher
  "Updates the schedule to assign the `co-teacher` to the section.
   Assumes the teacher being passed is a valid choice.
   Throws an assertion error if the passed `section-id` doesn't exist."
  [schedule section-id co-teacher-id]
  {:pre [(get-in schedule [::sections section-id])]}
  (assoc-in schedule [::sections section-id ::c/co-teacher] co-teacher-id))

(defn destroy-section
  "Updates the schedule to destroy the associated section and remove it entirely.
   All of the student tickets previously registered to the section are added 
   to the list of `::unresolved-tickets`."
  [schedule section-id]
  (if-let [found-section (get-in schedule [::sections section-id])]
    (let [invalid-tickets (-> found-section ::c/roster vals)]
      (-> schedule
          (update ::sections dissoc section-id)
          (update ::unresolved-tickets #(apply conj % invalid-tickets))))
    schedule))




;;;;; Some defined examples for testing
#_(def my-section (c/initialize-section (random-uuid)
                                        c/LUNCH-COURSE
                                        ::c/A-per
                                        (c/initialize-room ::c/cafeteria)))

#_(def my-student (gen/random-student (gen/random-course-catalog 55)))

#_(def test-schedule
    (-> (empty-schedule)
        (assoc-in [::sections (::c/section-id my-section)]  my-section)))

#_(section-deregister-student (section-register-student test-schedule
                                                        (::c/section-id my-section)
                                                        (c/initialize-registration-ticket my-student
                                                                                          c/LUNCH-COURSE))
                              :7d7c2ae9-98d6-4da1-8ed3-69f837c430f4 ;; section-id
                              :7512bbb4-b22b-0979-7902-1b82f37050bb ;; student-id
                              )
#_(section-roster (section-register-student test-schedule
                                            (::c/section-id my-section)
                                            (c/initialize-registration-ticket my-student
                                                                              c/LUNCH-COURSE))
                  :7d7c2ae9-98d6-4da1-8ed3-69f837c430f4 ;; section-id
                  )
#_(lunch-sections (section-register-student test-schedule
                                            (::c/section-id my-section)
                                            (c/initialize-registration-ticket my-student
                                                                              c/LUNCH-COURSE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing & accessing tickets ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-tickets
  "Extracts a flat list of registration tickets from the `student-body`."
  [student-body]
  (mapcat ::c/tickets (vals student-body)))

(defn estimate-number-of-sections
  [[course-id number-of-tickets]]
  [course-id (inc
              (quot number-of-tickets
                    (c/default-room-max-capacity (c/required-space {::c/course-id course-id}))))])

(defn course-section-estimates
  [registration-tickets]
  (let [freqs (->> registration-tickets
                   (map ::c/course-id)
                   frequencies)]
    (into {} (map estimate-number-of-sections freqs))))

(defn course-base-priorities
  "Calculates the frequencies of each `course-id` among the tickets."
  [registration-tickets]
  (let [section-count-estimates (course-section-estimates registration-tickets)
        max-sections (apply max (vals section-count-estimates))]
    (update-vals section-count-estimates #(- max-sections %))))

#_(sort-by second > (course-base-priorities (get-tickets (gen/random-student-body 1200 (gen/random-course-catalog 50)))))

(defn ticket-priority
  [base-priorities registration-ticket]
  (let [base-priority (get base-priorities (::c/course-id registration-ticket))]
    (cond-> base-priority
      (::c/inclusion registration-ticket) (-> inc inc (* 2))
      (::c/separate-class registration-ticket) (-> inc inc (* 3))
      (::c/elective registration-ticket) (- 1)
    ;;   (c/lunch? registration-ticket)  ;; #TODO Find the right setting for lunch
      )))

(defn radix-sorted-registration-tickets
  [student-body]
  (let [registration-tickets (get-tickets student-body)
        base-priorities (course-base-priorities registration-tickets)]
    (->> (reduce #(update %1 (ticket-priority base-priorities %2) (fnil conj []) %2)
                 {}
                 registration-tickets)
         (sort #(> (first %1) (first %2)))
         (map second)
         flatten)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Questions about teachers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn section-teacher-registered?
  "Is the teacher indicated by the `teacher-id` registered as the teacher or co-teacher
   for the passed `section-id` in the `schedule`?"
  [schedule section-id teacher-id]
  (when-let [section (get-in schedule [::sections section-id])]
    (or (= teacher-id (::c/teacher section))
        (= teacher-id (::c/co-teacher section)))))

(defn teacher-sections
  "Returns a pointed map of sections in `(::sections schedule)` for which the teacher 
   indicated by the `teacher-id` is listed as a teacher or co-teacher."
  [schedule teacher-id]
  (c/as-pointed-map (into {}
                          (for [[section-id section] (::sections schedule)
                                :when (section-teacher-registered? schedule section-id teacher-id)]
                            [section-id section]))
                    ::c/section-id))

(defn teacher-preps
  "Returns a set of the `course-id`s of the distinct courses that the teacher is
   currently registered to teach at least one section of."
  [schedule teacher-id]
  (->> teacher-id
       (teacher-sections schedule)
       vals
       (map ::c/course-id)
       distinct
       set))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Questions about students ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn section-student-registered?
  "Is the student registered to this section?"
  [schedule section-id student-id]
  (contains? (section-student-roster schedule section-id) student-id))

(defn student-has-lunch?
  "Is the student indicated by the `student-id` registered to a lunch section?"
  [schedule student-id]
  (let [lunch-section-ids (keys (lunch-sections schedule))]
    (some #(section-student-registered? schedule % student-id) lunch-section-ids)))

