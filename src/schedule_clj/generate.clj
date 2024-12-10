(ns schedule-clj.generate
  (:require [schedule-clj.core :as c]
            [clojure.data.generators :as g]))

;; marginal/total probability of a student having a probability
(def IEP-PROBABILTY 0.08)

;; conditional probabilities -- given student already has an IEP
(def GEOMETRIC-CLASSES-WITH-IEP-MINUTES 0.6)
(def MAX-CLASSES-WITH-IEP-MINUTES 4)
(def RESOURCE-PROBABILITY 0.2)
(def SEPARATE-CLASS-PROBABILITY 0.05)

(def COURSE-SEED 6130)
(def STUDENT-SEED 4140)
(def ENDORSEMENT-SEED 8310)

;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn weighted
  [m]
  (let [cumulative-weights (reductions + (vals m))
        spinner (g/uniform 1 (inc (last cumulative-weights)))
        cumulative-distribution (map vector (keys m) cumulative-weights)]
    (ffirst (drop-while #(< (second %) spinner) cumulative-distribution))))

(def WEIGHTED-ENDORSEMENTS
  "The frequency of each endorsement in the generated data."
  (into {}
        (map vector
             c/ENDORSEMENTS
             (let [r (java.util.Random. ENDORSEMENT-SEED)]
               (for [_ c/ENDORSEMENTS]
                 (binding [g/*rnd* r] (g/geometric 0.1)))))))

;;;;;;;;;;;;;;;;;;;;
;; Random courses ;;
;;;;;;;;;;;;;;;;;;;;

(defn random-course
  "Generates a course with a random uuid and a random required cert,
   but always with a min class size of 20 and a max class size of 30.
   Can be optionally called with a specific seed to generate 
   the same course every time."
  []
  (c/initialize-course (g/uuid) (weighted WEIGHTED-ENDORSEMENTS)))

(defn random-course-catalog
  "Generates a list of `num-courses` many courses."
  [num-courses]
  (-> (let [r (java.util.Random. COURSE-SEED)]
        (repeatedly num-courses
                    #(binding [g/*rnd* r]
                       (random-course))))
      (c/as-pointed-map ::c/course-id)))

;;;;;;;;;;;;;;;;;;;;
;; Random student ;;
;;;;;;;;;;;;;;;;;;;;

;; students are generated starting from a course catalog to flesh out their preferences
(def DEFAULT-NUMBER-REQD-CLASSES 5)
(def DEFAULT-NUMBER-ELECTIVE-CLASSES 3)

(defn random-iep-minutes
  "For a student designated to have IEP minutes, randomly assigns departments
   for the student to have inclusion minutes, separate class minutes, or both.
   Additionally, randomly assigns seminar minutes to some students."
  []
  (let [classes-with-minutes (min MAX-CLASSES-WITH-IEP-MINUTES
                                  (g/geometric GEOMETRIC-CLASSES-WITH-IEP-MINUTES))]
    (update-vals
     (reduce (fn [m _]
               (if (< (g/float) SEPARATE-CLASS-PROBABILITY)
                 (update m ::c/separate-class conj (g/rand-nth c/CORE-SUBJECTS))
                 (update m ::c/inclusion conj (g/rand-nth c/CORE-SUBJECTS))))
             (if (< (g/float) RESOURCE-PROBABILITY) {::c/eparate-class #{::c/sped-seminar}} {})
             (range classes-with-minutes))
     set)))

(defn random-registration-tickets
  "Generates a random registration ticket for a student given
   a list of potential courses, ruling out those for which the student
   is already ticketed."
  [reqd-count course-catalog student & options]
  (->> course-catalog
       seq ;; need to turn into seq in order to shuffle correctly
       g/shuffle
       (take reqd-count)
       (map second)
       (map #(apply c/initialize-registration-ticket student % options))))

(defn random-student
  "Generates a random student from a `course-catalog`."
  [course-catalog]
  (let [new-student (merge (c/initialize-student (g/uuid) (g/uniform 7 13))
                           (when (< (g/float) IEP-PROBABILTY) (random-iep-minutes)))
        tickets (random-registration-tickets (+ DEFAULT-NUMBER-REQD-CLASSES
                                                DEFAULT-NUMBER-ELECTIVE-CLASSES)
                                             course-catalog
                                             new-student)
        [elective-tickets required-tickets] (split-at DEFAULT-NUMBER-ELECTIVE-CLASSES tickets)]
    (cond-> new-student
      true (c/add-registration-tickets ::c/elective-tickets elective-tickets ::c/required-tickets required-tickets)
      true c/add-lunch-ticket
      (c/has-seminar? new-student) c/add-seminar-ticket)))

(defn random-student-body
  "Generates a student body from a course catalog."
  [num-students course-catalog]
  (let [r (java.util.Random. STUDENT-SEED)]
    (c/as-pointed-map (repeatedly num-students
                                  #(binding [g/*rnd* r]
                                     (random-student course-catalog)))
                      ::c/student-id)))

#_(->> (random-student-body 1300 (random-course-catalog 55))
       vals
       (mapcat ::c/tickets))

#_(= (random-student-body 1000 (random-course-catalog 55))
     (random-student-body 1000 (random-course-catalog 55)))


;; (defn generate-student-cohort-list
;;   "Generates a cohort of students with unique student-ids but otherwise identical in terms of
;;    grade level, required classes, and electives."
;;   [seed course-list num-students]
;;   (let [student (generate-random-student seed course-list)
;;         r (java.util.Random. seed)]
;;     (map #(assoc student :student-id (keyword (str %))) (repeatedly num-students #(binding [g/*rnd* r] (g/uuid))))))

;; ;; (defn generate-faculty
;; ;;   "Generates a list of faculty based on a course list
;; ;;    where each required certification shows up at leat 
;; ;;    `num-teachers-per-cert` many times."
;; ;;   [seed course-catalog students]
;; ;;   (let [num-students-with-iep (->> students
;; ;;                                    (filter #(or (seq (:inclusion %))
;; ;;                                                 (seq (:separate-class %))))
;; ;;                                    count)
;; ;;         department-enrollment (assoc (->> students
;; ;;                                           (map #(:requirements %))
;; ;;                                           flatten
;; ;;                                           (map #(:required-cert (% course-catalog)))
;; ;;                                           frequencies)
;; ;;                                      :sped
;; ;;                                      num-students-with-iep)
;; ;;         r (java.util.Random. seed)]
;; ;;     (do (println (str "Num students with IEP: " num-students-with-iep))
;; ;;         (println department-enrollment)
;; ;;         (-> (for [[cert enrollment] department-enrollment]
;; ;;               (map #(-> (binding [g/*rnd* r] (g/uuid))
;; ;;                         c/initialize-teacher
;; ;;                         (c/teacher-add-cert %))
;; ;;                    (repeat (+ (if (= cert :sped) 8 3) (quot enrollment 100)) cert))) ;; #TODO Replace magic numbers
;; ;;             flatten
;; ;;             (make-super-map :teacher-id)))))

;; ;; (defn generate-rooms
;; ;;   ([seed num-rooms]
;; ;;    (generate-rooms seed num-rooms 0 0))
;; ;;   ([seed num-rooms prob-small-room prob-large-room]
;; ;;    (let [r (java.util.Random. seed)
;; ;;          room-numbers (map #(str (+ 100 %)) (range num-rooms))]
;; ;;      (->> room-numbers
;; ;;           (map #(c/initialize-room % 30))
;; ;;           (map #(binding [g/*rnd* r]
;; ;;                   (let [p (g/float)]
;; ;;                     (cond (< p prob-small-room) (assoc % :max-size 18)
;; ;;                           (> p (- 1 prob-large-room)) (assoc % :max-size 60)
;; ;;                           :else %))))))))
