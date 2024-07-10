(ns schedule-clj.generate
  (:require [schedule-clj.data :as d]
            [clojure.data.generators :as g]))

(defn generate-random-course
  "Generates a course with a random uuid and a random required cert,
   but always with a min class size of 20 and a max class size of 30.
   Can be optionally called with a specific seed to generate
   the same course every time."
  ([]
   (d/initialize-course (g/uuid) (g/rand-nth d/CERTS)))
  ([seed]
   (let [r (java.util.Random. seed)]
     (binding [g/*rnd* r]
       (d/initialize-course (g/uuid) (g/rand-nth d/CERTS))))))

#_(generate-random-course)

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

#_(generate-random-course-with-limits)

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

#_(generate-random-course-list 5)

(comment
  "NOTE: The function below doesn't produce reproducible randomness
   starting from a given seed. When a lazy sequence is involved
   (in this case, with `repeatedly`), the binding needs to happen
   every iteration")
;; (defn generate-random-course-list*
;;   ([num-classes]
;;    (generate-random-course-list* 3366 num-classes))
;;   ([seed num-classes]
;;    (let [r (java.util.Random. seed)]
;;      (binding [g/*rnd* r]
;;        (repeatedly
;;         num-classes
;;         #(let [p (g/float)]
;;            (if (< p 0.1)
;;              (intialize-random-course-with-limits)
;;              (initialize-random-course))))))))

(defn generate-random-student
  "Generates a student with a random uuid and random required classes 
   and random elective choices selected from a given `course-list`. If run with a seed,
   the student will have the same uuid, required classes, and electives every time."
  ([seed course-list]
   (let [r (java.util.Random. seed)]
     (binding [g/*rnd* r]
       (generate-random-student course-list))))
  ([course-list]
   (loop
    [student (d/initialize-student (g/uuid) (str (g/uniform 7 13)))
     course-list course-list]
     (let [new-class (g/rand-nth course-list)]
       (cond
         (= 3 (count (:electives student))) student
         (< (count (:requirements student)) 5) (recur (d/student-add-required-class student (:course-id new-class))
                                                      (remove #{new-class} course-list))
         :else (recur (d/student-add-elective student (:course-id new-class))
                      (remove #{new-class} course-list)))))))

#_(generate-random-student (generate-random-course-list 10))

(defn generate-student-body
  "Generates a full student body of random students with random selected required classes
   and electives from a course list"
  [seed course-list num-students]
  (let [r (java.util.Random. seed)]
    (repeatedly num-students #(binding [g/*rnd* r] (generate-random-student course-list)))))

#_(generate-student-body 2266 (generate-random-course-list 20) 2)

(defn generate-student-cohort
  "Generates a cohort of students with unique student-ids but otherwise identical in terms of
   grade level, required classes, and electives."
  [seed course-list num-students]
  (let [student (generate-random-student seed course-list)
        r (java.util.Random. seed)]
    (map #(assoc student :student-id (keyword (str %))) (repeatedly num-students #(binding [g/*rnd* r] (g/uuid))))))

#_(generate-random-student 2266 (generate-random-course-list 3366 10))
#_(generate-student-cohort 2266 (generate-random-course-list 3366 10) 3)

(defn generate-faculty
  ;; TODO: FIXME: Currently returns exactly 1 teacher per cert,
  ;; need additional arg `num-teachers-per-cert`
  ;; which can be toggled to make sure each cert is covered that many times
  "Generates a list of faculty based on a course list
   where each required certification shows up at leat 
   `num-teachers-per-cert` many times."
  [seed course-list]
  (let [needed-certs (->> course-list (map :required-cert) distinct)
        r (java.util.Random. seed)]
    (map #(-> (binding [g/*rnd* r] (g/uuid)) d/initialize-teacher (d/teacher-add-cert %)) needed-certs)))

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

#_(= (generate-rooms 3366 25 0.2 0.05)
     (generate-rooms 3366 25 0.2 0.05))

#_(->> (generate-random-course-list 2266 10) (map :required-cert) distinct)
#_(generate-faculty 3366 (generate-random-course-list 2266 10))