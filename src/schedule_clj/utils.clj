(ns schedule-clj.utils
  (:require [clojure.string :as str]))

(defn as-keyword
  "Converts a string into the keyword version of itself, replacing all
   whitespace with a single dash to be consistent with the model"
  [word]
  (-> word name str/lower-case str/trim (str/replace #"\s+" "-") keyword))

(defn dash->underscore
  "Returns a copy of the keyword with all dashes replaced by underscores,
   which is used to store/fetch them from the database. "
  [keyw]
  (when (keyword? keyw)
    (-> keyw name (str/replace #"-" "_") keyword)))

(defn db-friendly-keys
  "Returns the same map, but dashes in keys are turned into underscores\n
   E.g. `{:compound-key 12}` becomes `{:compound_key 12}`"
  [m]
  (update-keys m dash->underscore))

