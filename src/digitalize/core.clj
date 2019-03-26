(ns digitalize.core
  (:use clojure.set)
  (:require [clojure.string :as s]
            [digitalize.numbers :refer :all]
            [digitalize.strings :refer :all]))

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

(defn digitalize
  "Remove nils and empty stuff,
  Make map keys more idiomatic and parse numbers"
  [o & {:keys [clean-keys clean-numbers]
        :or   {clean-keys true,
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

(defn keys-replacer
  [o newks]
  (if (map? (first o))
    (map #(zipmap (keys-replacer (keys %) newks)
                  (vals %))
         o)
    (map #(get newks % %)
         o)))
