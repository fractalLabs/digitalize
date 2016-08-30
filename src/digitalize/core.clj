(ns digitalize.core
  (:use clojure.set)
  (:require [clojure.string :as s]))

(defn category
  "Category of the objects type"
  [o]
  (cond
    (number? o) :number
    (string? o) :string
    (map? o) :map
    (coll? o) :coll       ;treating maps as if they were not colls
    (nil? o) :nil
    :else :unknown))

(defn remove-nils
  "Remove nils and empty elements in the collection"
  [o]
  (case (category o)
    :coll (remove #(or (nil? %)
                       (if (coll? %)
                         (empty? %)))
                  (map remove-nils o))
    :map (apply merge (for [[k v] o
                            :when (not (nil? v))]
                        {k v}))
    o))

(defn- single-dashes
  "If a string contains consecutive dashes, leave just one"
  [s]
  (s/replace s #"[-]+" "-"))

(defn trim-dashes
  "Like clojure.string/trim but for dashes at the
  beginning or end"
  [s]
  (single-dashes
    (s/replace
     (s/replace s #"^-*" "")
     #"-*$"
     "")))

(def chars-to-dash
  "List of chars to convert to dashes"
  (zipmap (map str "!@$%^&*()_+?/- #,")
                 (repeat "-")))

(def standard-keys
  "Lat long names in spanish"
  {"latitud" "latitude",
   "longitud" "longitude"
   "ano" "fecha"})

(defn change-standard-keys
  [s]
  (loop [les-keys standard-keys]
    (if (empty? les-keys)
      s
      (if (= (ffirst les-keys) s)
        (second (first les-keys))
        (recur (rest les-keys))))))

(def acentos
  "Non standard characters to convert"
  {"á" "a", "é" "e", "í" "i", "ó" "o", "ú" "u", "ñ" "n"})

(defn str-replace
  "like clojure.string/replace but takes a map of replacements"
  [replacements-map s]
  (loop [v (vec replacements-map) s s]
    (if (empty? v)
      s
      (recur (rest v)
             (s/replace s
                        (ffirst v)
                        (second (first v)))))))

(defn safe-name
  "like name but will also cast numbers"
  [o]
  (if (number? o)
    (str o)
    (name o)))

(defn standard-name
  "Make a string more idiomatic"
  [o]
  (let [k (change-standard-keys (trim-dashes
                                 (str-replace (merge chars-to-dash
                                                     ;standard-keys
                                                     acentos)
                                              (s/lower-case
                                               (safe-name o)))))]
    k))

(defn standard-keyword
  "Convert a string to a more idiomatic keyword"
  [o]
  (keyword (standard-name o)))

;TODO: doesnt cover cases like .9
(defn snumber?
  "Does this string look like a number?"
  [o]
  (= o (first (re-find #"[\-]?[0-9,]+(\.[0-9]*)?" o))))

(defn try-int
  "Try to coerce to int, return 0 on exception"
  [x]
  (try (int x)
       (catch Exception e 0)))

(defn simplify-int [x]
  (let [i (int x)]
    (if (== x i)
      i
      x)))

(defn str->number
  "Cast to number. Tries to be permissive:
  supports whitespace at the beginning or end
  and commas"
  [s]
  (simplify-int (Double/valueOf (s/replace s "," ""))))

(defn digitalize
  "Remove nils and empty stuff,
  Make map keys more idiomatic and parse numbers"
  [o & {:keys [clean-keys clean-numbers]
        :or {clean-keys true,
             clean-numbers true}}]
  (try (let [data (case (category o)
                    :coll (if-not (empty? o)
                            (map #(digitalize % :clean-keys clean-keys
                                              :clean-numbers clean-numbers)
                                 o))
                    :map  (let [ks (keys o)
                                vs (vals o)
                                new-ks (if clean-keys
                                         (map standard-keyword ks)
                                         ks) ;dont touch keys ks
                                new-vs (map #(digitalize % :clean-keys clean-keys
                                                         :clean-numbers clean-numbers)
                                            vs)]
                            (zipmap new-ks new-vs))
                    :string (let [s (s/trim o)]
                              (cond
                                (snumber? s) (if clean-numbers
                                               (str->number s)
                                               s)
                                (empty? s) nil
                                :else s))
                    :number (if clean-numbers
                              (simplify-int o)
                              o)
                    o)]
         (remove-nils data))
       (catch Exception e o)))
