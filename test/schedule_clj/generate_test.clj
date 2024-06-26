(ns schedule-clj.generate-test
  (:require [schedule-clj.generate :as gen]
            [clojure.test :refer [deftest is testing]]))

(deftest generate-random-course-test
  (testing "Do we consistently generate a random course with all fields"
    (let [course (gen/generate-random-course 3366)]
      (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:course-id course)))
      (is (= :english (:required-cert course)))
      (is (= 20 (:min-size course)))
      (is (= 30 (:max-size course))))))

(deftest generate-random-course-with-limits-test
  (testing "Do we consistently generate a random course with all fields and limits?"
    (let [course (gen/generate-random-course-with-limits 3366)]
      (is (= "0bfd118c-3c9d-08b9-d45a-b3f2f56abe1f" (:course-id course)))
      (is (= :chemistry (:required-cert course)))
      (is (= 6 (:min-size course)))
      (is (= 16 (:max-size course))))))

(deftest generate-random-course-list-test
  (testing "Do we consistently generate the same list of random courses?"
    (let [course-list (gen/generate-random-course-list 3366 10)]
      (is (= course-list
             '({:course-id "9e073caa-fcfb-e5e4-7147-7ab50bfd118c", :required-cert :arabic, :min-size 20, :max-size 30}
               {:course-id "50aaf662-fdfa-a479-e6bd-a27fe70f94d0", :required-cert :cte, :min-size 20, :max-size 30}
               {:course-id "883e6c16-fd06-8299-6ef4-59c6ba2e3ee1", :required-cert :chemistry, :min-size 4, :max-size 16}
               {:course-id "e63c0ae3-9bb9-8242-4a1b-66d695fab4f8", :required-cert :visual, :min-size 20, :max-size 30}
               {:course-id "24efc37c-eac8-2e8e-616b-d86d86affa17", :required-cert :mandarin, :min-size 5, :max-size 15}
               {:course-id "4d395782-54a7-0f4b-6a09-74ffa764611c", :required-cert :social-science, :min-size 20, :max-size 30}
               {:course-id "592438fa-8f8a-dd90-1ba9-d128f49900c3", :required-cert :math, :min-size 20, :max-size 30}
               {:course-id "41ab9688-7063-8f97-9225-7d14ad267732", :required-cert :visual, :min-size 20, :max-size 30}
               {:course-id "79a408a9-ef93-9f01-ffef-5a4973bafa7c", :required-cert :cte, :min-size 20, :max-size 30}
               {:course-id "c2df23d0-9249-b520-adb3-a4b974532484", :required-cert :performance, :min-size 20, :max-size 30}))))))

(deftest generate-random-student-test
  (testing "Do we consistently generate the same student?"
    (let [course-list (gen/generate-random-course-list 3366 10)
          student (gen/generate-random-student 2266 course-list)]
      (is (= student
             {:student-id "ebca8c0d-f1f0-754e-77a2-c6bcaf03468a",
              :grade "9",
              :requirements
              '("e63c0ae3-9bb9-8242-4a1b-66d695fab4f8"
                "79a408a9-ef93-9f01-ffef-5a4973bafa7c"
                "24efc37c-eac8-2e8e-616b-d86d86affa17"
                "9e073caa-fcfb-e5e4-7147-7ab50bfd118c"
                "4d395782-54a7-0f4b-6a09-74ffa764611c"),
              :electives
              ["592438fa-8f8a-dd90-1ba9-d128f49900c3"
               "c2df23d0-9249-b520-adb3-a4b974532484"
               "41ab9688-7063-8f97-9225-7d14ad267732"]})))))

(deftest generate-student-body-test
  (testing "Do we consistently generate the same student body with their preferences?"
    (let [course-list (gen/generate-random-course-list 3366)
          student-body (gen/generate-student-body 2266 course-list 2)]
      (is (= student-body
             '({:student-id "ebca8c0d-f1f0-754e-77a2-c6bcaf03468a",
                :grade "9",
                :requirements
                ("17eb1790-845d-7c70-362b-7712e8972ca5"
                 "9d6b2d0b-6444-4e41-3f41-84e806cce051"
                 "0c625d6c-be30-1ce6-958a-26ec45e0f543"
                 "8d1d7ca6-ba0b-64c0-dc93-717499e4acee"
                 "175874a9-ff83-a4d8-566b-3f4a03893e2e"),
                :electives
                ["66cddbe3-a545-5e1b-6f67-8b882151c2a4"
                 "3a51cff7-feae-4a9e-44dc-47734ee33234"
                 "7ff2445d-7f27-45ea-6911-14bc619c1249"]}
               {:student-id "8ff1c4b2-b943-73e6-fe3f-b99719fe0e9c",
                :grade "10",
                :requirements
                ("32b12c47-efc9-2cbf-9641-d4d50778c4c5"
                 "6c8eafdf-3613-1a92-50e1-6224362d448c"
                 "a7c6d2ff-d654-d053-b7d4-cc272bdb3a49"
                 "97cceb4c-fb65-e21d-e9b5-8f48634ec30e"
                 "eeedcc7a-619b-dd29-5792-35891c8f13be"),
                :electives
                ["f813ed66-15b7-a57e-7afe-a094c14f51a7"
                 "38cad307-3cba-d4bd-8811-e1707e3c8390"
                 "ef474124-2f6d-77a3-7451-a73104331009"]}))))))