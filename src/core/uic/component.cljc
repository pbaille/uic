(ns uic.component
  (:require [uic.styles :as styles]
            [uix.core :refer [$]])
  #?(:clj (:require [clojure.string :as str]
                    [uic.utils :as u])
     :cljs (:require-macros [uic.component :refer [c sc]])))

#?(:clj (do (defn parse-c [xs]
              (let [[tag xs] (if (ident? (first xs))
                               [(first xs) (rest xs)]
                               [:div xs])
                    [props children] (if (map? (first xs)) [(first xs) (rest xs)] [{} xs])]
                {:tag tag :props props :children children}))

            (defmacro c
              "component"
              [& xs]
              (let [{:keys [tag props children]} (parse-c xs)]
                (do u/prob :expand-c
                    `($ ~tag
                        ~(if-let [styles (:style props)]
                           (styles/compile-props2 styles (dissoc props :style))
                           props)
                        ~@children))))

            (defmacro sc
              "styled component"
              [& xs]
              (let [[tag styles & children]
                    (if (keyword? (first xs)) xs (cons :div xs))]
                `($ ~tag
                    ~(styles/compile-props2 styles {})
                    ~@children)))))
