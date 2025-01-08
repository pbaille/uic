(ns uic.utils
  (:require
   [#?(:cljs cljs.pprint :clj clojure.pprint) :as pp]))

;; printlng debugging
;; ------------------------------------------------------------

(defn pp [& xs]
  (mapv pp/pprint xs))

(defn prob
  "print all its arguments and return the last"
  [& xs]
  (println "---")
  (apply pp xs) (last xs))

(defn error [& xs]
  (throw (#?(:cljs js/Error
             :clj  Exception.)
          (apply str xs))))

;; words
;; ------------------------------------------------------------

(defn word? [x]
  (or (string? x)
      (symbol? x)
      (keyword? x)))

(defn mksym [& xs]
  (->> xs (map name) (apply str) symbol))

;; numbers
;; --------------------------------------------------------------------

(defn geometric-scale [base delta]
  (let [up (next (iterate #(* % (+ 1 delta)) base))
        down (next (iterate #(* % (- 1 delta)) base))]
    (fn [i]
      (cond (zero? i) base
            (pos? i) (nth up (dec i))
            (neg? i) (nth down (dec (- i)))))))

;; collection
;; ---------------------------------------------------------------------

(defn map-vals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn remove-nil-vals [x]
  (into {} (remove (comp nil? val) x)))

(defn get-flags [m]
  (map key (filter (comp true? val) m)))
