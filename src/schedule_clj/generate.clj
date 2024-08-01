(ns schedule-clj.generate
  (:require [schedule-clj.data :as d]
            [clojure.data.generators :as g]))

(def IEP-FREQ 0.03)
(def SEPARATE-CLASS-FREQ 0.1)

(defn generate-random-course
  "Generates a course with a random uuid and a random required cert,
   but always with a min class size of 20 and a max class size of 30.
   Can be optionally called with a specific seed to generate
   the same course every time."
  ([]
   (d/initialize-course (g/uuid) (g/rand-nth d/COURSE-CERTS)))
  ([seed]
   (let [r (java.util.Random. seed)]
     (binding [g/*rnd* r]
       (d/initialize-course (g/uuid) (g/rand-nth d/COURSE-CERTS))))))

(defn generate-random-course-with-limits
  "Generates a course with a random uuid and random 
   upper and lower class size limits. If run with a seed,
   it will produce the same course every time."
  ([]
   (let [low (g/uniform 4 7)
         high (g/uniform 14 17)]
     (-> (generate-random-course)
         (d/course-set-min low)
         (d/course-set-max high))))
  ([seed]
   (let [r (java.util.Random. seed)]
     (binding [g/*rnd* r]
       (let [low (g/uniform 4 7)
             high (g/uniform 14 17)]
         (-> (generate-random-course)
             (d/course-set-min low)
             (d/course-set-max high)))))))

(defn generate-random-course-list
  "Generates a list of `num-courses` random courses, 
   each with a random uuid and required certification.
   If run with a specific seed, it will produce 
   the exame same course list every time."
  ([num-courses]
   (repeatedly
    num-courses
    #(let [p (g/float)]
       (if (< p 0.1)
         (generate-random-course-with-limits)
         (generate-random-course)))))
  ([seed num-courses]
   (let [r (java.util.Random. seed)]
     (repeatedly
      num-courses
      #(binding [g/*rnd* r]
         (let [p (g/float)]
           (if (< p 0.1)
             (generate-random-course-with-limits)
             (generate-random-course))))))))

#_(comment
    "NOTE: The function below doesn't produce reproducible randomness
   starting from a given seed. When a lazy sequence is involved
   (in this case, with `repeatedly`), the binding needs to happen
   every iteration"
    (defn generate-random-course-list*
      ([num-classes]
       (generate-random-course-list* 3366 num-classes))
      ([seed num-classes]
       (let [r (java.util.Random. seed)]
         (binding [g/*rnd* r]
           (repeatedly
            num-classes
            #(let [p (g/float)]
               (if (< p 0.1)
                 (generate-random-course-with-limits)
                 (generate-random-course)))))))))

(defn generate-student-with-iep
  "Takes a student, and with probability `IEP-FREQ` adds special education minutes in one of two ways:
   Either an inclusion class or a separate class."
  [student]
  (let [courses (take (count (take-while #(< % IEP-FREQ)
                                         (repeatedly g/float)))
                      (g/shuffle (concat (:requirements student) (:electives student))))]
    (reduce #(update %1 (if (< (g/float) SEPARATE-CLASS-FREQ) :separate-class :inclusion) conj %2)
            student
            courses)))

(defn generate-random-student
  "Generates a student with a random uuid and random required classes 
   and random elective choices selected from a given `course-list`. If run with a seed,
   the student will have the same uuid, required classes, and electives every time."
  ([seed course-list]
   (let [r (java.util.Random. seed)]
     (binding [g/*rnd* r]
       (generate-random-student course-list))))
  ([course-list]
   (-> (loop
        [student (d/initialize-student (g/uuid) (str (g/uniform 7 13)))
         course-list course-list]
         (let [new-class (g/rand-nth course-list)]
           (cond
             (= 3 (count (:electives student))) student
             (< (count (:requirements student)) 5) (recur (d/student-add-required-class student (:course-id new-class))
                                                          (remove #{new-class} course-list))
             :else (recur (d/student-add-elective student (:course-id new-class))
                          (remove #{new-class} course-list)))))
       generate-student-with-iep
       d/student-set-priority)))

(defn generate-student-list
  "Generates a full student body of random students with random selected required classes
   and electives from a course list"
  [seed course-list num-students]
  (let [r (java.util.Random. seed)]
    (repeatedly num-students #(binding [g/*rnd* r] (generate-random-student course-list)))))

(defn generate-student-cohort-list
  "Generates a cohort of students with unique student-ids but otherwise identical in terms of
   grade level, required classes, and electives."
  [seed course-list num-students]
  (let [student (generate-random-student seed course-list)
        r (java.util.Random. seed)]
    (map #(assoc student :student-id (keyword (str %))) (repeatedly num-students #(binding [g/*rnd* r] (g/uuid))))))

(defn make-super-map
  "Given a list of `maps`, creates a new map where the vals are the maps from 
   the original list and the keys are values under"
  [maps key]
  (reduce #(assoc %1 (key %2) %2)
          {}
          maps))

(defn generate-faculty
  "Generates a list of faculty based on a course list
   where each required certification shows up at leat 
   `num-teachers-per-cert` many times."
  [seed course-catalog students]
  (let [department-enrollment (->> students
                                   (map #(:requirements %))
                                   flatten
                                   (map #(:required-cert (% course-catalog)))
                                   frequencies)]
    (let [r (java.util.Random. seed)]
      (-> (for [[cert enrollment] department-enrollment]
            (map #(-> (binding [g/*rnd* r] (g/uuid))
                      d/initialize-teacher
                      (d/teacher-add-cert %))
                 (repeat (+ 3 (quot enrollment 100)) cert)))
          flatten
          (make-super-map :teacher-id)))))

(defn generate-rooms
  ([seed num-rooms]
   (generate-rooms seed num-rooms 0 0))
  ([seed num-rooms prob-small-room prob-large-room]
   (let [r (java.util.Random. seed)
         room-numbers (map #(str (+ 100 %)) (range num-rooms))]
     (->> room-numbers
          (map #(d/initialize-room % 30))
          (map #(binding [g/*rnd* r]
                  (let [p (g/float)]
                    (cond (< p prob-small-room) (assoc % :max-size 18)
                          (> p (- 1 prob-large-room)) (assoc % :max-size 60)
                          :else %))))))))
#_(generate-rooms 2266 10 0.1 0.05)

(defn generate-course-catalog
  [seed num-classes]
  (make-super-map (generate-random-course-list seed num-classes) :course-id))

#_(generate-course-catalog 2266 20)

(defn generate-homogeneous-student-body
  [seed course-catalog num-students]
  (make-super-map (generate-student-cohort-list seed (vals course-catalog) num-students)
                  :student-id))

(defn generate-heterogeneous-student-body
  [seed course-catalog num-students]
  (make-super-map (generate-student-list seed (vals course-catalog) num-students)
                  :student-id))

#_(generate-homogeneous-student-body 1199 (generate-course-catalog 2266 20) 20)
#_(generate-heterogeneous-student-body 1199 (generate-course-catalog 2266 20) 20)