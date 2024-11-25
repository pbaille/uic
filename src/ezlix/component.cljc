(ns ezlix.component
  (:require [ezlix.styles :as styles])
  #?(:clj (:require [helix.core :refer [$]]
                    [clojure.string :as str]
                    [ezlix.utils :as u])
     :cljs (:require-macros [ezlix.component :refer [c c*]])))

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
                    [props children] (if (map? (first xs)) [(first xs) (rest xs)] [{} xs])
                    {:keys [tag extra-props]} (parse-tag tag)]
                {:tag tag :props (merge props extra-props) :children children}))

            (defn parse-c* [xs]
              (update (parse-c xs) :children first))

            (defmacro c
              "easy component interface on top of helix"
              [& xs]
              (let [{:keys [tag props children]} (parse-c xs)
                    spread (if-let [styles (:style props)]
                             (styles/compile-props styles (:& props {}))
                             (:& props))]
                (u/prob :expand-c
                        `($ ~tag
                            ~(merge (dissoc props :style :&)
                                    (when spread {:& spread}))
                            ~@children))))

            (defmacro c*
              "easy component interface on top of helix"
              [& xs]
              (let [{:keys [tag props children]} (parse-c* xs)
                    spread (if-let [styles (:style props)]
                             (styles/compile-props styles (:& props {}))
                             (:& props))]
                (u/prob :expand-c*
                        `($ ~tag
                            ~(-> props
                                 (assoc :children children)
                                 (dissoc :style :&)
                                 (merge (when spread {:& spread})))))))))
