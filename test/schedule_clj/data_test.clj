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
          (is (= #{:math :physics :iep} (:certs teacher))))))))

(deftest teacher-remove-cert-test
  (testing "Does `teacher-remove-cert` correctly update the :cert list?"
    (let [r (java.util.Random. 3366)]
      (binding [g/*rnd* r]
        (let [teacher (-> (g/uuid)
                          (d/initialize-teacher)
                          (d/teacher-add-cert "math")
                          (d/teacher-add-cert :physics)
                          (d/teacher-add-cert :iep)
                          (d/teacher-remove-cert "physics"))]
          (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:teacher-id teacher)))
          (is (= 5 (:max-num-classes teacher)))
          (is (= #{:math :iep} (:certs teacher))))))))

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
          (is (= #{:math :physics :iep} (:certs teacher)))
          (is (d/teacher-has-cert? teacher :math))
          (is (d/teacher-has-cert? teacher :physics))
          (is (d/teacher-has-cert? teacher "iep"))
          (is (not (d/teacher-has-cert? teacher :mandarin))))))))

