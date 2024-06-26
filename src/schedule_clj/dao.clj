(ns schedule-clj.dao
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [schedule-clj.utils :as utils]))

;; DB specification
(def db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "db/database.db"})

(def std-opts
  {:conditional? true})

;; INPUT TABLES
;; :student table
(def student-table-specs
  [[:student_id :text :primary :key]
   [:name :text]
   [:grade :text]])

;; :teacher table
(def teacher-table-specs
  [[:teacher_id :text :primary :key]
   [:name :text]
   [:max_classes :int]])

;; :certs table
(def certs-table-specs
  [[:teacher_id :text]
   [:cert :text]
   ["FOREIGN KEY (teacher_id) REFERENCES teacher(teacher_id)"]
   ["PRIMARY KEY (teacher_id, cert)"]])

;; :course table
(def course-table-specs
  [[:course_id :text :primary :key]
   [:name :text]
   [:required_cert :text]
   [:min_size :int]
   [:max_size :int]])

;; :course-preference table
(def course-preferences-table-specs
  [[:teacher_id :text]
   [:course_id :text]
   ["FOREIGN KEY (teacher_id) REFERENCES teacher(teacher_id)"]
   ["FOREIGN KEY (course_id) REFERENCES course(course_id)"]
   ["PRIMARY KEY (teacher_id, course_id)"]])

;; :room table
(def room-table-specs
  [[:room_no :text :primary :key]
   [:max_cap :int]
   [:supports_concurrent :int]]) ;boolean 

;; :room-preference table
(def room-preferences-table-specs
  [[:teacher_id :text]
   [:room_no :text]
   ["FOREIGN KEY (teacher_id) REFERENCES teacher(teacher_id)"]
   ["FOREIGN KEY (room_no) REFERENCES room(room_no)"]
   ["PRIMARY KEY (teacher_id, room_no)"]])

;; OUTPUT TABLES
;; :section table
(def section-table-specs
  [[:section_id :text :primary :key]
   [:course_id :text]
   [:room_no :text]
   [:setting :text]
   [:period :text]
   ["FOREIGN KEY (room_no) REFERENCES room(room_no)"]
   ["FOREIGN KEY (course_id) REFERENCES course(course_id)"]])

;; :enrollment table
(def enrollment-table-specs
  [[:student_id :text]
   [:section_id :text]
   ["FOREIGN KEY (student_id) REFERENCES student(student_id)"]
   ["FOREIGN KEY (section_id) REFERENCES section(section_id)"]
   ["PRIMARY KEY (student_id, section_id)"]])

;; :assignment table
(def assignment-table-specs
  [[:teacher_id :text]
   [:section_id :text]
   ["FOREIGN KEY (teacher_id) REFERENCES teacher(teacher_id)"]
   ["FOREIGN KEY (section_id) REFERENCES section(section_id)"]
   ["PRIMARY KEY (teacher_id, section_id)"]])

(defn create-scheduling-db
  [db]
  (try (jdbc/db-do-commands db
                            [;;                           
                             (jdbc/create-table-ddl :student student-table-specs std-opts)
                             (jdbc/create-table-ddl :teacher teacher-table-specs std-opts)
                             (jdbc/create-table-ddl :certs certs-table-specs std-opts)
                             (jdbc/create-table-ddl :course course-table-specs std-opts)
                             (jdbc/create-table-ddl :course_preference course-preferences-table-specs std-opts)
                             (jdbc/create-table-ddl :room room-table-specs std-opts)
                             (jdbc/create-table-ddl :room_preference room-preferences-table-specs std-opts)
                             (jdbc/create-table-ddl :section section-table-specs std-opts)
                             (jdbc/create-table-ddl :enrollment enrollment-table-specs std-opts)
                             (jdbc/create-table-ddl :assignment assignment-table-specs std-opts)])
       (catch Exception e
         (println (.getMessage e)))))

(defn teardown-scheduling-db
  [db]
  (try (jdbc/db-do-commands db [(jdbc/drop-table-ddl :assignment std-opts)
                                (jdbc/drop-table-ddl :enrollment std-opts)
                                (jdbc/drop-table-ddl :section std-opts)
                                (jdbc/drop-table-ddl :room_preference std-opts)
                                (jdbc/drop-table-ddl :room std-opts)
                                (jdbc/drop-table-ddl :course_preference std-opts)
                                (jdbc/drop-table-ddl :course std-opts)
                                (jdbc/drop-table-ddl :certs std-opts)
                                (jdbc/drop-table-ddl :teacher std-opts)
                                (jdbc/drop-table-ddl :student std-opts)])
       (catch Exception e
         (println (.getMessage e)))))

(defn concat-cols
  [cols]
  (->> cols
       (map utils/dash->underscore)
       (map name)
       (str/join ", ")))

(defn write-where-clause
  [table-1 table-2 key]
  (let [table-1 (name (utils/dash->underscore table-1))
        table-2 (name (utils/dash->underscore table-2))
        key (name (utils/dash->underscore key))]
    (apply str " where "
           table-1
           "."
           key
           " = "
           table-2
           "."
           key)))

(defn write-inner-join-clause
  [table-1 table-2 key]
  (apply str
         (name table-1)
         " inner join "
         (name table-2)
         (write-where-clause table-1 table-2 key)))

(defn select-from-join
  "Performs the query 'select `cols` from `table-1` 
   inner join `table-2` where `table-1.key = table-2.key`'"
  [db table-1 table-2 key & cols]
  (let [joined-cols (concat-cols cols)]
    (->> (write-inner-join-clause table-1 table-2 key)
         (apply str "select " joined-cols " from ")
         (jdbc/query db))))

(defn count-teacher-preps
  "Accesses the database to count the number of distinct course-ids the teacher
   has been assigned to. "
  [teacher]
  (->> (:teacher-id teacher)
       (format "select * from teacher inner join assignment on teacher.teacher_id = assignment.teacher_id inner join section on assignment.section_id = section.section_id inner join course on section.course_id = course.course_id where teacher.teacher_id = '%s'")
       (jdbc/query db)
       (map :course_id)
       distinct
       count))

(defn count-teacher-sections
  "Accesses the database to count the number of sections the teacher has been assigned to"
  [teacher]
  (->> (:teacher-id teacher)
       (format "select * from teacher inner join assignment on teacher.teacher_id = assignment.teacher_id inner join section on assignment.section_id = section.section_id inner join course on section.course_id = course.course_id where teacher.teacher_id = '%s'")
       (jdbc/query db)
       count))

(defn teacher-lookup
  [db teacher-id]
  (->> teacher-id
       (format "select * from teacher inner join certs on teacher.teacher_id = certs.teacher_id where teacher.teacher_id = '%s'")
       (jdbc/query db)))