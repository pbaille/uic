(ns uic.styles
  (:refer-clojure :exclude [compile])
  (:require [stylefy.core :as stylefy]
            [uic.utils :as u]
            [uic.styles.mixins :as s]
            [clojure.string :as str]
            [uix.core]))

;; impl
;; -----------------------------------------------------------

;; km is short for keyword-map
;; this will serve to treat html-attributes and style-mixins
;; the intent is to ease a bit creation and composition of this kind of maps
;; with this you should be able to avoid writing 'merge 'assoc 'assoc-in etc...
;; I tend to think that those kinds of maps representing 95% of our map usage, they deserves concise api

(defn km+
  "deeply-merge 2 kms"
  [x y]
  (cond
    (nil? x) y
    (nil? y) x

    (and (map? x) (map? y))
    (merge-with km+ x y)

    :else y))

(defn km
  "build a keyword map
   special :& key is used as spread key, it can contain either a map or a seq of maps."
  ([] {})
  ([x]
   (cond (nil? x) {}
         (map? x)
         (if-let [spread (:& x)]
           (km+
            (dissoc x :&)
            (if (sequential? spread)
              (reduce km+ {} (map km spread))
              (km spread)))
           x)
         :else (u/error "bad argument to " `km ":\n" x)))
  ([x & xs]
   (reduce km+ (map km (cons x xs)))))

;; style
;; --------------------------------------------------------------

;; the following is built on top of https://github.com/Jarzka/stylefy
;; and uic.styles.mixins
;; the intent is to provide a concise API to declare components with styles
;; It will support regular hiccup inline-styles along with css nested selectors, pseudo classes and modes
;; the whole in a unified and expressive API powered by a css-mixin library

(def modes
  #{:active :blank :checked :current :default :link
    :disabled :enabled :focus :hover :valid :visited})

(def pseudo-elements
  #{"after" "backdrop" "before" "cue" "first-letter" "first-line" "part"
    "grammar-error" "marker" "placeholder" "selection" "spelling-error"})

(defn modal-selector [x]
  (keyword (str "&" x)))

(defn pseudo-selector [x]
  (and (keyword? x)
       (namespace x)
       (pseudo-elements (name x))
       (str "&::" (name x))))

(defn selector?
  "only support class and id selector for now."
  [x]
  (or (string? x)
      (-> x name first #{"." "#"})))

(defn expand-mixins
  [x]
  (reduce (fn [ret [k v]]
            (merge ret (if-let [f (s/mixin k)]
                       (if (vector? v) (apply f v) (f v))
                       {k v})))
          {} x))

(defn compile [x]
  (let [f (partial u/map-vals compile)]
    (-> (update x :self expand-mixins)
        (update :pseudo f)
        (update :modes f)
        (update :sub f))))

(declare mk)

(defn parse [x]
  (reduce
   (fn [a [k v]]
     (cond
       (selector? k) (assoc-in a [:sub k] (mk v))
       (modes k) (assoc-in a [:modes (modal-selector k)] (mk v))
       (pseudo-selector k) (assoc-in a [:pseudo (pseudo-selector k)] (mk v))
       :else (assoc-in a [:self k] v)))
   {} x))

(defn mk [x]
  (cond
    (nil? x) {}
    (map? x) (-> x parse compile)
    :else (u/error "bad argument to " `mk ":\n" x)))

(defn emit
  [{:as parsed-style
    :keys [sub self modes pseudo]}]
  (into [self]
        (map (fn [[k v]] (into [k] (emit v)))
             (concat modes pseudo sub))))

(defn ->stylefy-opts
  ([style]
   (let [[style & manual] (-> style mk emit)]
     (if (seq manual)
       (assoc style :stylefy.core/manual (vec manual))
       style))))

(defn merge-stylefy-opts
  [styles]
  (reduce (fn [style1 style2]
            (let [base (merge style1 style2)]
              (if (:stylefy.core/manual base)
                (assoc base
                       :stylefy.core/manual (into (get style1 :stylefy.core/manual [])
                                                  (get style2 :stylefy.core/manual [])))
                base)))
          {} styles))

#?(:clj (do :compiler

            (defn compiler_dynamic? [x]
              (or (symbol? x)
                  (seq? x)
                  (cond (map? x) (or (contains? x :&)
                                     (some (fn [[k v]]
                                             (or (compiler_dynamic? k)
                                                 (compiler_dynamic? v)))
                                           x))
                        (vector? x) (some compiler_dynamic? x))))

            (comment (defn compiler_merge [x y]
                       (cond
                         (nil? x) y
                         (nil? y) x

                         (and (map? x) (map? y))
                         (merge-with compiler_merge x y)

                         (or (compiler_dynamic? x)
                             (compiler_dynamic? y)) `(km+ ~x ~y)

                         :else y))

                     (defn compiler_unspread [x]
                       (cond (or (symbol? x) (seq? x)) x
                             (vector? x) (reduce compiler_merge {} x)
                             (map? x) (if-let [spread (:& x)]
                                        (if (vector? spread)
                                          (reduce compiler_merge (dissoc x :&) spread)
                                          (compiler_merge x spread))
                                        x)
                             (nil? x) {}
                             :else (u/error "bad argument " `compiler_unspread ":\n" x)))

                     (defn compiler_usable-styles [styles]
                       (if (compiler_dynamic? styles)
                         #_`(let [s# ~styles]
                              (uix.core/use-memo (fn [] (usable s#))
                                [s#]))
                         `(usable-memo ~styles)
                         (usable-memo styles)))

                     (defn compile-props [styles props]
                       (do u/prob :expand-props
                           (if (not styles)
                             props
                             `(stylefy/use-style ~(compiler_usable-styles (compiler_unspread styles))
                                                 ~(compiler_unspread props))))))

            (do :new

                (defn compiler_split [m]
                  (assert (map? m)
                          (str "compiler_split expect a map, got:\n" m))
                  (reduce (fn [ret [k v]]
                            (if (compiler_dynamic? v)
                              (update ret :dynamic assoc k v)
                              (update ret :static assoc k v)))
                          {:dynamic {} :static {} :spread (:& m)}
                          (dissoc m :&)))

                (defn compiler_prepare-styles [styles]
                  (cond (map? styles) (let [{:keys [static dynamic spread]} (compiler_split styles)
                                            static-opts (->stylefy-opts static)]
                                        (cond (not (seq static))
                                              (let [xs (cons dynamic spread)]
                                                (case (count xs)
                                                  0 {}
                                                  1 `(->stylefy-opts ~(first xs))
                                                  `(merge-stylefy-opts (mapv ->stylefy-opts [~dynamic ~@spread]))))

                                              (or (seq dynamic) (seq spread))
                                              `(merge-stylefy-opts [~static-opts
                                                                    ~@(map (fn [x] `(->stylefy-opts ~x))
                                                                           (cons dynamic spread))])

                                              :else static-opts))

                        (vector? styles) (compiler_prepare-styles {:& styles})

                        :else `(->stylefy-opts ~styles)))

                (defn compile-props2 [styles props]
                  (if (not styles)
                    props
                    (u/prob :compile-props2
                            `(stylefy/use-style ~(compiler_prepare-styles styles)
                                                ~(compiler_unspread props))))))))
