(ns schedule-clj.data-test
  (:require [schedule-clj.data :as d]
            [clojure.data.generators :as g]
            [clojure.test :refer [deftest is testing]]))


(deftest initialize-teacher-test
  (testing "Does `initialize-teacher` correctly initialize a teacher"
    (let [r (java.util.Random. 3366)]
      (binding [g/*rnd* r]
        (let [teacher (d/initialize-teacher (g/uuid))]
          (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:teacher-id teacher)))
          (is (= 5 (:max-num-classes teacher)))
          (is (= #{} (:certs teacher))))))))

(deftest initialize-student-test
  (testing "Does `initialize-student` correctly initialize a student"
    (let [r (java.util.Random. 3366)]
      (binding [g/*rnd* r]
        (let [student (d/initialize-student (g/uuid) (g/uniform 7 13))]
          (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:student-id student)))
          (is (= 7 (:grade student)))
          (is (some? (:requirements student)))
          (is (empty? (:requirements student)))
          (is (some? (:electives student)))
          (is (empty? (:electives student))))))))

(deftest initialize-course-test
  (testing "Does `initialize-course` correctly initialize a course"
    (testing "with default settings"
      (let [r (java.util.Random. 3366)]
        (binding [g/*rnd* r]
          (let [course (d/initialize-course (g/uuid) :physics)]
            (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:course-id course)))
            (is (= :physics (:required-cert course)))
            (is (= 20 (:min-size course)))
            (is (= 30 (:max-size course)))))))
    (testing "with custom settings"
      (let [r (java.util.Random. 3366)]
        (binding [g/*rnd* r]
          (let [course (d/initialize-course (g/uuid) :physics (g/uniform 4 7) (g/uniform 14 17))]
            (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:course-id course)))
            (is (= :physics (:required-cert course)))
            (is (= 4 (:min-size course)))
            (is (= 16 (:max-size course)))))))))

(deftest initialize-room-test
  (testing "Does `initialize-room` correctly initialize a course?"
    (let [room (d/initialize-room "222" 28)]
      (is (= "222" (:room-number room)))
      (is (= 28 (:max-size room)))
      (is (not (:concurrency? room))))))

(deftest initialize-section-test
  (testing "Does `initialize-section` correctly initialize a section from a course"
    (let [r (java.util.Random. 3366)]
      (binding [g/*rnd* r]
        (let [course (d/initialize-course (g/uuid) (g/rand-nth d/CERTS))
              period (g/rand-nth d/PERIODS)
              room (d/initialize-room "222" 28)
              section (d/initialize-section (g/uuid) course period room)]
          (is (= section
                 {:section-id "50aaf662-fdfa-a479-e6bd-a27fe70f94d0",
                  :course-id "d7b3b968-9e07-3cab-fcfb-e5e471477ab5",
                  :period :B-per,
                  :max-size 28,
                  :min-size 20,
                  :required-cert :english})))))))

(deftest teacher-set-max-classes-test
  (testing "Does `teacher-set-max-classes` correctly update the :max-classes entry?"
    (let [r (java.util.Random. 3366)]
      (binding [g/*rnd* r]
        (let [teacher (-> (g/uuid)
                          (d/initialize-teacher)
                          (d/teacher-set-max-classes 3))]
          (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:teacher-id teacher)))
          (is (= 3 (:max-num-classes teacher)))
          (is (= #{} (:certs teacher))))))))

(deftest teacher-add-cert-test
  (testing "Does `teacher-add-cert` correctly update the :cert list?"
    (let [r (java.util.Random. 3366)]
      (binding [g/*rnd* r]
        (let [teacher (-> (g/uuid)
                          (d/initialize-teacher)
                          (d/teacher-add-cert "math")
                          (d/teacher-add-cert :physics)
                          (d/teacher-add-cert :iep))]
          (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:teacher-id teacher)))
          (is (= 5 (:max-num-classes teacher)))
          (is (= #{:math :physics} (:certs teacher))))))))

(deftest teacher-remove-cert-test
  (testing "Does `teacher-remove-cert` correctly update the :cert list?"
    (let [r (java.util.Random. 3366)]
      (binding [g/*rnd* r]
        (let [teacher (-> (g/uuid)
                          (d/initialize-teacher)
                          (d/teacher-add-cert "math")
                          (d/teacher-add-cert :physics)
                          (d/teacher-add-cert :sped)
                          (d/teacher-remove-cert "physics"))]
          (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:teacher-id teacher)))
          (is (= 5 (:max-num-classes teacher)))
          (is (= #{:math :sped} (:certs teacher))))))))

(deftest teacher-has-cert-test
  (testing "Does has-cert? correct show the absence/presence of a cert?"
    (let [r (java.util.Random. 3366)]
      (binding [g/*rnd* r]
        (let [teacher (-> (g/uuid)
                          (d/initialize-teacher)
                          (d/teacher-add-cert "math")
                          (d/teacher-add-cert :physics)
                          (d/teacher-add-cert :iep))]
          (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:teacher-id teacher)))
          (is (= 5 (:max-num-classes teacher)))
          (is (= #{:math :physics} (:certs teacher)))
          (is (d/teacher-has-cert? teacher :math))
          (is (d/teacher-has-cert? teacher :physics))
          (is (not (d/teacher-has-cert? teacher "iep")))
          (is (not (d/teacher-has-cert? teacher :mandarin))))))))

