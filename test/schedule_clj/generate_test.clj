(ns schedule-clj.generate-test
  (:require [schedule-clj.generate :as g]
            [clojure.test :refer [deftest is testing]]))

(deftest generate-random-course-test
  (testing "Do we consistently generate a random course with all fields"
    (let [course (g/generate-random-course 3366)]
      (is (= "d7b3b968-9e07-3cab-fcfb-e5e471477ab5" (:course-id course)))
      (is (= :english (:required-cert course)))
      (is (= 20 (:min-size course)))
      (is (= 30 (:max-size course))))))

(deftest generate-random-course-with-limits-test
  (testing "Do we consistently generate a random course with all fields and limits?"
    (let [course (g/generate-random-course-with-limits 3366)]
      (is (= "0bfd118c-3c9d-08b9-d45a-b3f2f56abe1f" (:course-id course)))
      (is (= :chemistry (:required-cert course)))
      (is (= 6 (:min-size course)))
      (is (= 16 (:max-size course))))))

(deftest generate-random-course-list-test
  (testing "Do we consistently generate the same list of random courses?"
    (let [course-list (g/generate-random-course-list 3366 10)]
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
    (let [course-list (g/generate-random-course-list 3366 10)
          student (g/generate-random-student 2266 course-list)]
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
    (let [course-list (g/generate-random-course-list 3366 10)
          student-body (g/generate-student-body 2266 course-list 2)]
      (is (= student-body
             '({:student-id "ebca8c0d-f1f0-754e-77a2-c6bcaf03468a",
                :grade "9",
                :requirements
                ("e63c0ae3-9bb9-8242-4a1b-66d695fab4f8"
                 "79a408a9-ef93-9f01-ffef-5a4973bafa7c"
                 "24efc37c-eac8-2e8e-616b-d86d86affa17"
                 "9e073caa-fcfb-e5e4-7147-7ab50bfd118c"
                 "4d395782-54a7-0f4b-6a09-74ffa764611c"),
                :electives
                ["592438fa-8f8a-dd90-1ba9-d128f49900c3"
                 "c2df23d0-9249-b520-adb3-a4b974532484"
                 "41ab9688-7063-8f97-9225-7d14ad267732"]}
               {:student-id "3f0c42d7-1005-5de6-9388-81da8daec984",
                :grade "10",
                :requirements
                ("e63c0ae3-9bb9-8242-4a1b-66d695fab4f8"
                 "41ab9688-7063-8f97-9225-7d14ad267732"
                 "592438fa-8f8a-dd90-1ba9-d128f49900c3"
                 "24efc37c-eac8-2e8e-616b-d86d86affa17"
                 "4d395782-54a7-0f4b-6a09-74ffa764611c"),
                :electives
                ["79a408a9-ef93-9f01-ffef-5a4973bafa7c"
                 "883e6c16-fd06-8299-6ef4-59c6ba2e3ee1"
                 "50aaf662-fdfa-a479-e6bd-a27fe70f94d0"]}))))))

(deftest generate-student-cohort-test
  (testing "Does `generate-student-cohort` consistently generate a cohort of students with unique ids but are otherwise identical?"
    (let [course-list (g/generate-random-course-list 3366 10)
          cohort (g/generate-student-cohort 2266 course-list 3)]
      (is (= cohort
             '({:student-id "ebca8c0d-f1f0-754e-77a2-c6bcaf03468a",
                :grade "9",
                :requirements
                ("e63c0ae3-9bb9-8242-4a1b-66d695fab4f8"
                 "79a408a9-ef93-9f01-ffef-5a4973bafa7c"
                 "24efc37c-eac8-2e8e-616b-d86d86affa17"
                 "9e073caa-fcfb-e5e4-7147-7ab50bfd118c"
                 "4d395782-54a7-0f4b-6a09-74ffa764611c"),
                :electives
                ["592438fa-8f8a-dd90-1ba9-d128f49900c3"
                 "c2df23d0-9249-b520-adb3-a4b974532484"
                 "41ab9688-7063-8f97-9225-7d14ad267732"]}
               {:student-id "775eb82a-390e-38c4-8d5c-ab5549eb52dc",
                :grade "9",
                :requirements
                ("e63c0ae3-9bb9-8242-4a1b-66d695fab4f8"
                 "79a408a9-ef93-9f01-ffef-5a4973bafa7c"
                 "24efc37c-eac8-2e8e-616b-d86d86affa17"
                 "9e073caa-fcfb-e5e4-7147-7ab50bfd118c"
                 "4d395782-54a7-0f4b-6a09-74ffa764611c"),
                :electives
                ["592438fa-8f8a-dd90-1ba9-d128f49900c3"
                 "c2df23d0-9249-b520-adb3-a4b974532484"
                 "41ab9688-7063-8f97-9225-7d14ad267732"]}
               {:student-id "02b6dba4-022e-cb7d-7126-8ec6899ff739",
                :grade "9",
                :requirements
                ("e63c0ae3-9bb9-8242-4a1b-66d695fab4f8"
                 "79a408a9-ef93-9f01-ffef-5a4973bafa7c"
                 "24efc37c-eac8-2e8e-616b-d86d86affa17"
                 "9e073caa-fcfb-e5e4-7147-7ab50bfd118c"
                 "4d395782-54a7-0f4b-6a09-74ffa764611c"),
                :electives
                ["592438fa-8f8a-dd90-1ba9-d128f49900c3"
                 "c2df23d0-9249-b520-adb3-a4b974532484"
                 "41ab9688-7063-8f97-9225-7d14ad267732"]}))))))
