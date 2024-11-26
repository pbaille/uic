(ns ezlix.styles
  (:require [stylefy.core :as stylefy]
            [ezlix.utils :as u]
            [ezlix.styles.mixins :as s]
            [clojure.string :as str]
            [helix.hooks :as hh]))

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
;; and ezlix.styles.mixins
;; the intent is to provide a concise API to declare components with styles
;; It will support regular hiccup inline-styles along with css nested selectors, pseudo classes and modes
;; the whole in a unified and expressive API powered by a css-mixin library

(def style_modes
  #{:active :blank :checked :current :default :link
    :disabled :enabled :focus :hover :valid :visited})

(def style_pseudo-elements
  #{"after" "backdrop" "before" "cue" "first-letter" "first-line" "part"
    "grammar-error" "marker" "placeholder" "selection" "spelling-error"})

(defn style_key [& xs]
  (keyword (str/join "-" (map name xs))))

(defn style_modal-selector [x]
  (keyword (str "&" x)))

(defn style_pseudo-selector [x]
  (and (keyword? x)
       (namespace x)
       (style_pseudo-elements (name x))
       (str "&::" (name x))))

(defn style_selector? [x]
  (or (string? x)
      (-> x name first #{"." "#"})))

(defn style_expand-mixins
  [x]
  (reduce (fn [ret [k v]]
            (km+ ret (if-let [f (s/mixin k)]
                       (if (vector? v) (apply f v) (f v))
                       {k v})))
          {} (km x)))

(defn style_compile [x]
  (let [f (partial u/map-vals style_compile)]
    (-> (update x :self style_expand-mixins)
        (update :pseudo f)
        (update :modes f)
        (update :sub f))))

(declare style_mk)

(defn style_parse [x]
  (reduce
   (fn [a [k v]]
     (cond
       (style_selector? k) (assoc-in a [:sub k] (style_mk v))
       (style_modes k) (assoc-in a [:modes (style_modal-selector k)] (style_mk v))
       (style_pseudo-selector k) (assoc-in a [:pseudo (style_pseudo-selector k)] (style_mk v))
       :else (assoc-in a [:self k] v)))
   {} x))

(defn style_mk [x]
  (cond
    (nil? x) {}
    (map? x) (-> x style_parse style_compile)
    :else (u/error "bad argument to " `style_mk ":\n" x)))

(defn style_emit
  [{:as parsed-style
    :keys [sub self modes pseudo]}]
  (into [self]
        (map (fn [[k v]] (into [k] (style_emit v)))
             (concat modes pseudo sub))))

(defn style_usable [style]
  #_(println "usable styles " style)
  (do u/prob :usable-styles
   (let [[style & manual] (-> style style_mk style_emit)]
     (if (seq manual)
       (assoc style :stylefy.core/manual (vec manual))
       style))))

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

            (defn compiler_merge [x y]
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
              ;(println "deps-check " env styles)
              ;(println (compiler_deps-free? env styles))
              (if (compiler_dynamic? styles)
                `(let [styles# ~styles]
                   (hh/use-memo [styles#] (style_usable ~styles#)))
                (style_usable styles)))

            (defn compile-props [styles props]
              (do u/prob :expand-props
                      (if (not styles)
                        props
                        `(stylefy/use-style ~(compiler_usable-styles (compiler_unspread styles))
                                            ~(compiler_unspread props)))))))
