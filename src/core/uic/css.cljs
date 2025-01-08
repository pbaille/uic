(ns uic.css
  (:require ["@css-hooks/react" :as css-hooks]
            [uix.core :refer [$]]
            [uix.dom]
            [uic.styles.mixins :as mixins]))


(defn wrap-css [css comp]
  ($ :div
     ($ :style {:dangerouslySetInnerHTML #js {:__html ((:styleSheet css))}})
     comp))

(defn expand-mixins
  [x]
  (clj->js (reduce (fn [ret [k v]]
                     (merge ret (if-let [f (mixins/mixin k)]
                                  (if (vector? v) (apply f v) (f v))
                                  {k v})))
                   {} x)))

(defn create-hooks [m]
  (assoc (js->clj (apply css-hooks/createHooks (vals m))
                  :keywordize-keys true)
         :map m))


(def DEFAULT_HOOKS
  (create-hooks {:hover "&:hover"
                 :child "> .child"}))

(defn css
  ([styles] (css DEFAULT_HOOKS styles))
  ([hooks styles]
   (let [selectors (keys (:map hooks))
         sub-styles (select-keys styles selectors)
         styles (into {} (remove (comp (set selectors) key) styles))]
     (reduce (fn [ret sel]
               (((:on hooks) (get (:map hooks)
                                  sel)
                             (expand-mixins (get sub-styles sel)))
                ret))
             (expand-mixins styles)
             selectors))))

(comment
  (wrap-css (css-hooks/createHooks "&:hover")
            ($ :div "hello"))
  (css (create-hooks {:hover "&:hover"})
       {:bg {:color :blue}
        :hover {:bg {:color :green}}})
  (js->clj (css DEFAULT_HOOKS
                {:bg {:color :blue}
                 :hover {:bg {:color :green}}})))
