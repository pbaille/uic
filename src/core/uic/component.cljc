(ns uic.component
  (:require [uic.styles :as styles]
            [uix.core :refer [$]])
  #?(:clj (:require [clojure.string :as str]
                    [uic.utils :as u])
     :cljs (:require-macros [uic.component :refer [c]])))

#?(:clj (do (defn parse-c [xs]
              (let [[tag xs] (if (keyword? (first xs))
                               [(first xs) (rest xs)]
                               [:div xs])
                    [props children] (if (map? (first xs)) [(first xs) (rest xs)] [{} xs])]
                {:tag tag :props props :children children}))

            (defmacro c
              "easy component interface on top of helix"
              [& xs]
              (let [{:keys [tag props children]} (parse-c xs)]
                (do u/prob :expand-c
                    `($ ~tag
                        ~(if-let [styles (:style props)]
                           (styles/compile-props2 styles (dissoc props :style))
                           props)
                        ~@children))))))