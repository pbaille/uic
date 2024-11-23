(ns ezlix.component
  #?(:clj (:require [helix.core :refer [$]]
                    [ezlix.styles :as styles]
                    [clojure.string :as str])
     :cljs (:require-macros [ezlix.component :refer [c]])))

#?(:clj (do (defn parse-tag [x]
              (let [id? (some #{\#} (seq (name x)))
                    [tag & segs] (str/split (name x) #"[#\.]")
                    [id & classes] (if id? segs (cons nil segs))]
                {:tag (if-not (= "" tag) tag "div")
                 :extra-props (merge {}
                                     (when id {:id id})
                                     (when (seq classes) {:class (str/join " " classes)}))}))

            (defn parse-c [xs]
              (let [[tag xs] (if (keyword? (first xs))
                               [(first xs) (rest xs)]
                               [:div xs])
                    [props children] (if (map? (first xs)) [(first xs) (rest xs)] [{} xs])]
                {:tag tag :props props :children children}))

            (defmacro c
              "easy component interface on top of helix"
              [& xs]
              (let [{:keys [tag props children]} (parse-c xs)
                    {:keys [tag extra-props]} (parse-tag tag)
                    spread (if-let [styles (:style props)]
                             (macroexpand-1 `(styles/props ~styles ~(merge extra-props (:& props {}))))
                             props)]
                `($ ~tag
                    ~(merge (dissoc props :style :&)
                            extra-props
                            (when spread {:& spread}))
                    ~@children)))))
