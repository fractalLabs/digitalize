(ns digitalize.numbers
  (:require [clojure.string :as str]
            [digitalize.strings :refer :all]))

(defn re-number [s]
  (re-find #"[\-]?[$]?[0-9,]+(\.[0-9]*)?" s))

(defn re-numbers [s]
  (re-seq #"[\-]?[$]?[0-9,]+[\.]?[0-9]*" s))


;TODO: doesnt cover cases like .9
(defn snumber?
  "Does this string look like a number?"
  [s]
  (= s (first (re-number s))))

(defn try-int
  "Try to coerce to int, return nil on exception"
  [x]
  (try (int x)
       (catch Exception e nil)))

(defn simplify-int
  "Si el numero es igual a su entero, podemos castearlo a entero"
  [x]
  (let [i (int x)]
    (if (== x i)
      i
      x)))

(defn str-remove
  [s & removals]
  (loop [s s removals removals]
    (if (empty? removals)
      s
      (recur (str/replace s (first removals) "")
             (rest removals)))))

(defn str-remove-numberformat [s]
  (str-remove s "," "$"))

(defn str->number
  "Cast to number. Tries to be permissive:
  supports whitespace at the beginning or end
  and commas"
  [s]
  (simplify-int
   (Double/valueOf (str/replace (str/replace s "," "") "$" ""))))

(defn is-any-substring? [s & substrings]
  (loop [substrings substrings res false]
    (if (empty? substrings)
      res
      (recur (rest substrings)
             (or res (str/last-index-of s (first substrings)))))))

(defn gently-coerce
  "Gently coerce a string into numbers"
  [s]
  (try
    (let [re         (re-numbers (str-remove-numberformat s))
          stdr       (standard-name s)
          millon?    (is-any-substring? stdr "millon")
          mil?       (is-any-substring? (str-remove stdr "millon") "mil")
          multiplier (* (if millon? 1000000 1)
                        (if mil? 1000 1))]
      (case (count re)
        0 nil
        1 (* multiplier (str->number (first re)))
        (map gently-coerce re)))
    (catch Exception e)))

(defn gently-coerce-nums [maps k]
  (map #(let [v (% k)]
          (if (string? v)
            (assoc % k (gently-coerce v))
            %))
       maps))

(defn predominantly-numeric? [maps k]
  (> (count (filter #(number? (k %))
                    maps))
     (count (filter #(string? (k %))
                    maps))))

(defn numeric-coersion [maps]
  (let [ks (keys (first maps))
        numeric-ks (filter #(predominantly-numeric? maps %)
                           ks)]
    (loop [maps maps numeric-ks numeric-ks]
      (if (empty? numeric-ks)
        maps
        (recur (gently-coerce-nums maps (first numeric-ks))
               (rest numeric-ks))))))
