(ns uic.styles.colors
  (:require [thi.ng.math.core :as m]
            [thi.ng.color.core :as c]
            [thi.ng.color.presets :as cp]
            [thi.ng.color.gradients :as cg]
            [uic.utils :as u]))

(def user-presets* (atom {}))

(do :greys

    (def GREY_SCALE_SIZE 22)

    (defn grey [n]
      (let [v (/ n GREY_SCALE_SIZE)]
        (c/rgba v v v 1)))

    (def greyscale (vec (next (reverse (map grey (range (inc GREY_SCALE_SIZE)))))))

    (defn greymap [prefix xs]
      (into {} (map-indexed
                (fn [i v]
                  [(keyword (str (name prefix) "grey" i)) v])
                xs)))

    (def lightgreys
      (greymap :light (take 7 greyscale)))

    (def midgreys
      (->> greyscale (drop 7) (take 7) (greymap :mid)))

    (def darkgreys
      (greymap :dark (drop 14 greyscale)))

    (def greys
      (merge
       lightgreys
       midgreys
       darkgreys
       (greymap "" greyscale))))

(defn get-user-preset
  ([k]
   (or (get greys k)
       (get-user-preset @user-presets* k)))
  ([presets k]
   (when-let [v (get presets k)]
     (if (keyword? v)
       (get-user-preset v)
       v))))

(defn hex->rgba [hex-kw]
  (let [s (name hex-kw)]
    (if (= \# (first s))
      (c/as-rgba (c/hex->int s)))))

(def modifiers
  {:alpha
   (fn [rgba x]
     (if (fn? x)
       (update rgba :a x)
       (assoc rgba :a x)))
   :lightness
   (fn [rgba x]
     (let [hsla (c/as-hsla rgba)]
       (c/as-rgba (if (fn? x)
                    (update hsla :l x)
                    (assoc hsla :l x)))))
   :saturation
   (fn [rgba x]
     (let [hsla (c/as-hsla rgba)]
       (c/as-rgba (if (fn? x)
                    (update hsla :s x)
                    (assoc hsla :s x)))))
   :hue
   (fn [rgba x]
     (let [hsla (c/as-hsla rgba)]
       (c/as-rgba (if (fn? x)
                    (update hsla :h x)
                    (assoc hsla :h x)))))})

(def modifiers-aliases
  {:a (modifiers :alpha)
   :s (modifiers :saturation)
   :l (modifiers :lightness)
   :h (modifiers :hue)})

(defn apply-modifier [col [k x]]
  (when-let [m (and col
                    (or (modifiers k)
                        (modifiers-aliases k)))]
    (m col x)))

(defn color
  [x & mods]
  (let [base (cond
               (satisfies? c/IColorComponents x)
               (c/as-rgba x)

               (u/word? x)
               (let [kw (keyword x)]
                 (or (and (cp/colors kw) (cp/preset-rgb kw))
                     (get-user-preset kw)
                     (hex->rgba kw)))
               :else nil)]
    (reduce (fn [c mod]
              (or (apply-modifier c mod)
                  (reduced nil)))
            base (apply merge mods))))

(defn css-color
  [x & mods]
  (some-> (apply color x mods) c/as-css deref))

(defn shades [col n]
  (let [col (c/as-hsla (color col))]
    (mapv (fn [m] (color (assoc col :l (/ m (dec n)))))
          (range n))))

(defn mix [col1 col2 ratio]
  (let [c1 (color col1)
        c2 (color col2)]
    (if (and c1 c2)
      (color (m/mix c1 c2 ratio)))))

(def color-presets (keys cp/colors))


(comment

  (color2 "green" 0.4)

  (color :green)
  (color2 :grey3)
  (cp/preset-rgb :grey3)
  (deref (c/as-css (c/as-rgba (get greys :grey3))))
  (get-user-preset :lightgrey)
  (cp/preset-hsv :green)
  (cp/preset-rgb :green)

  @(c/as-css (c/rgba 1 0 0 0.5))

  @(c/as-css (assoc (cp/preset-rgb :green)
                    :a 0.5))

  (m/mix (assoc (cp/preset-rgb :green)
                :a 0.5)
         (cp/preset-rgb :white)
         0.5)

  (mix :red :blue 0.5)
  (-> (hsla :red) c/as-int24 c/as-css deref)

  (color :lightgrey)
  (color :lightgrey))
