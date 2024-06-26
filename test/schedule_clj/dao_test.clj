(ns schedule-clj.dao-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [schedule-clj.utils :as utils]
   [schedule-clj.dao :as dao]
   [clojure.java.jdbc :as jdbc]))

(def TEACHER-ID-1 (random-uuid))
(def TEACHER-NAME-1 (random-uuid))
(def TEACHER-MAX-CLASSES-1 5)
(def TEACHER-CERT-1 :math)
(def TEACHER-CERT-2 :social-science)
(def STUDENT-ID-1 (random-uuid))
(def STUDENT-NAME-1 (random-uuid))
(def STUDENT-GRADE-1 (random-uuid))
(def COURSE-ID-1 (random-uuid))
(def COURSE-ID-2 (random-uuid))
(def COURSE-NAME-1 (random-uuid))
(def COURSE-NAME-2 (random-uuid))
(def COURSE-MINSIZE-1 20)
(def COURSE-MAXSIZE-1 30)
(def SECTION-ID-1 (random-uuid))
(def SECTION-ID-2 (random-uuid))
(def SECTION-ID-3 (random-uuid))
(def SECTION-ID-4 (random-uuid))
(def SECTION-ID-5 (random-uuid))


(def test-teacher-1
  {:teacher-id TEACHER-ID-1
   :name TEACHER-NAME-1
   :max-classes TEACHER-MAX-CLASSES-1})

(def test-student-1
  {:student-id STUDENT-ID-1
   :name STUDENT-NAME-1
   :grade STUDENT-GRADE-1})

(def test-cert-1
  {:teacher-id TEACHER-ID-1
   :cert TEACHER-CERT-1})

(def test-cert-2
  {:teacher-id TEACHER-ID-1
   :cert TEACHER-CERT-2})

(def test-course-1
  {:course-id COURSE-ID-1
   :name COURSE-NAME-1
   :required-cert TEACHER-CERT-1
   :min-size COURSE-MINSIZE-1
   :max-size COURSE-MAXSIZE-1})

(def test-course-2
  {:course-id COURSE-ID-2
   :name COURSE-NAME-2
   :required-cert TEACHER-CERT-1
   :min-size COURSE-MINSIZE-1
   :max-size COURSE-MAXSIZE-1})

(def test-section-1
  {:section-id SECTION-ID-1
   :course-id COURSE-ID-1
   :room-no "222"
   :setting "general"
   :period "1st"})

(def test-section-2
  {:section-id SECTION-ID-2
   :course-id COURSE-ID-1
   :room-no "222"
   :setting "general"
   :period "A"})

(def test-section-3
  {:section-id SECTION-ID-3
   :course-id COURSE-ID-1
   :room-no "222"
   :setting "general"
   :period "B"})

(def test-section-4
  {:section-id SECTION-ID-4
   :course-id COURSE-ID-2
   :room-no "326"
   :setting "general"
   :period "D"})

(def test-section-5
  {:section-id SECTION-ID-5
   :course-id COURSE-ID-2
   :room-no "222"
   :setting "inclusion"
   :period "5th"})

(def test-assignment-1
  {:teacher-id TEACHER-ID-1
   :section-id SECTION-ID-1})

(def test-assignment-2
  {:teacher-id TEACHER-ID-1
   :section-id SECTION-ID-2})

(def test-assignment-3
  {:teacher-id TEACHER-ID-1
   :section-id SECTION-ID-3})

(def test-assignment-4
  {:teacher-id TEACHER-ID-1
   :section-id SECTION-ID-4})

(def test-assignment-5
  {:teacher-id TEACHER-ID-1
   :section-id SECTION-ID-5})

(deftest concat-cols-many-cols-test
  (testing "Does concat-cols make the right select string?"
    (is (= "teacher_id, room_no, cert, name"
           (dao/concat-cols (list :teacher-id :room-no :cert :name))))))

(deftest concat-cols-no-cols-test
  (testing "Does concat-cols return an empty string with an empty argument?"
    (is (= ""
           (dao/concat-cols (list))))))

(deftest where-clause-with-keys-test
  (testing "Does where-clause form correctly using keys?"
    (is (= " where room.room_no = section.room_no"
           (dao/write-where-clause :room :section :room-no)))))

(deftest where-clause-without-keys-test
  (testing "Does the where-clause form correctly using strings and/or keys?"
    (is (= " where room.room_no = section.room_no"
           (dao/write-where-clause :room :section :room-no)))))

(deftest inner-join-clause-test
  (testing "Is the inner join clause well-formed?"
    (is (= "teacher inner join certs where teacher.teacher_id = certs.teacher_id"
           (dao/write-inner-join-clause :teacher :certs :teacher-id)))))

(deftest select-from-join-test
  (testing "Does select-from-join return the appropriate information from a join?"
    (is (= (list {:name (str TEACHER-NAME-1) :cert (str TEACHER-CERT-1)}
                 {:name (str TEACHER-NAME-1) :cert (str TEACHER-CERT-2)})
           (dao/select-from-join dao/db
                                 :teacher
                                 :certs
                                 :teacher-id
                                 :name
                                 :cert)))))

(deftest count-teacher-preps-test
  (testing "Do we have the correct query to count the distinct number of preps a teacher has?"
    (is (= 2
           (dao/count-teacher-preps test-teacher-1)))))

(deftest count-teacher-sections-test
  (testing "Do we have the correct query to count the number of classes a teacher has?"
    (is (= 5
           (dao/count-teacher-sections test-teacher-1)))))

(deftest teacher-lookup-test
  (testing "Do we look up the correct teacher given their id?"
    (is (= (list {:teacher_id (str TEACHER-ID-1)
                  :name (str TEACHER-NAME-1)
                  :max_classes 5,
                  :teacher_id_2 (str TEACHER-ID-1)
                  :cert ":math"}
                 {:teacher_id (str TEACHER-ID-1)
                  :name (str TEACHER-NAME-1)
                  :max_classes 5,
                  :teacher_id_2 (str TEACHER-ID-1)
                  :cert ":social-science"})
           (dao/teacher-lookup dao/db (str TEACHER-ID-1))))))

(defn setup-testing-db
  [db]
  (println "Setting up testing db")
  (dao/teardown-scheduling-db db)
  (dao/create-scheduling-db db)
  (jdbc/insert! db :teacher (utils/db-friendly-keys test-teacher-1))
  (jdbc/insert! db :student (utils/db-friendly-keys test-student-1))
  (jdbc/insert! db :certs (utils/db-friendly-keys test-cert-1))
  (jdbc/insert! db :certs (utils/db-friendly-keys test-cert-2))
  (jdbc/insert! db :course (utils/db-friendly-keys test-course-1))
  (jdbc/insert! db :course (utils/db-friendly-keys test-course-2))
  (jdbc/insert! db :section (utils/db-friendly-keys test-section-1))
  (jdbc/insert! db :section (utils/db-friendly-keys test-section-2))
  (jdbc/insert! db :section (utils/db-friendly-keys test-section-3))
  (jdbc/insert! db :section (utils/db-friendly-keys test-section-4))
  (jdbc/insert! db :section (utils/db-friendly-keys test-section-5))
  (jdbc/insert! db :assignment (utils/db-friendly-keys test-assignment-1))
  (jdbc/insert! db :assignment (utils/db-friendly-keys test-assignment-2))
  (jdbc/insert! db :assignment (utils/db-friendly-keys test-assignment-3))
  (jdbc/insert! db :assignment (utils/db-friendly-keys test-assignment-4))
  (jdbc/insert! db :assignment (utils/db-friendly-keys test-assignment-5)))

(utils/db-friendly-keys test-teacher-1)

(defn teardown-testing-db
  [db]
  (dao/teardown-scheduling-db db)
  (println "Testing db torn down"))

(defn dao-fixture
  [test-run]
  (setup-testing-db dao/db)
  (test-run)
  (teardown-testing-db dao/db))

(use-fixtures :once dao-fixture)