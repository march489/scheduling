(ns schedule-clj.utils-test
  (:require [clojure.test :refer [deftest is testing]]
            [schedule-clj.utils :as utils]))

(deftest dash->underscore-leaves-underscores
  (testing "Does `dash->underscore` leave underscores unchanged?"
    (is (= :hello_world
           (utils/dash->underscore :hello_world)))))

((deftest dash->underscore-mixed
   (testing "Does `dash->underscore handle keys with a mix of dashes and underscores?"
     (is (= :hello_clojure_world
            (utils/dash->underscore :hello-clojure_world))))))

(deftest db-friendly-keys-test
  (testing "Does db-friendly keys update keys with dashes and maintain associations?"
    (let [original-map {:hello-world "Hello world!" :goodbye_clojure "Goodbye, Clojure!"}
          updated-map (utils/db-friendly-keys original-map)]
      (is (nil? (:hello-world updated-map)))
      (is (= "Hello world!" (:hello_world updated-map)))
      (is (= "Goodbye, Clojure!" (:goodbye_clojure updated-map)))
   	  (is (= 2 (count (keys updated-map)))))))