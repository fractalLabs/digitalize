(ns digitalize.strings
  (:require [clojure.string :as str]))


(defn- single-dashes
  "If a string contains consecutive dashes, leave just one"
  [s]
  (str/replace s #"[-]+" "-"))

(defn trim-dashes
  "Like clojure.string/trim but for dashes at the
  beginning or end"
  [s]
  (single-dashes
    (str/replace
     (str/replace s #"^-*" "")
     #"-*$"
     "")))

(def chars-to-dash
  "Convert non alphanumerical chars to dashes"
  {#"[^0-9a-zA-Z]" "-"})

(def standard-keys
  "Lat long names in spanish"
  {"latitud" "latitude",
   "longitud" "longitude"
   "ano" "fecha"
   "anio" "fecha"
   "etiqueta" "variable"
   "nombre-variable" "variable"
   "descripcion-variable" "descripcion"
   })

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
             (str/replace s
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
                                 (str-replace (merge acentos
                                                     chars-to-dash
                                                     ;standard-keys
                                                    )
                                              (str/lower-case
                                               (safe-name o)))))]
    k))

(defn standard-keyword
  "Convert a string to a more idiomatic keyword"
  [o]
  (keyword (standard-name o)))

(defn standarize-vals [maps ks]
  (map #(merge % (zipmap ks (map (fn [v] (standard-name (v %)))
                                 ks)))
       maps))
