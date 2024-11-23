(ns ezlix.state
  (:require [clojure.string :as str]
            [ezlix.utils :as u]
            #?@(:cljs [[refx.alpha :as rf]
                       [refx.interceptor :as interceptor]
                       [refx.interceptors :as interceptors]
                       [refx.db :as refx.db]
                       [helix.hooks :as hooks]
                       [cljs.pprint :as pp]]))
  #?(:cljs (:require-macros [ezlix.state :refer [sub dbf event effect]])))

;; I will try to implement a nice way to declare re-frame subs/events/fxs

;; re-frame reserved keys

:rf-sub
:rf-upd
:rf-event
:rf-effect

(defrecord RFHandler
           [name type doc interceptors handler])

(do :help

    (defn coerce-event [e]
      (if (vector? e) e [e]))

    (defn prefix-keyword [prefix kw]
      (keyword (str (name prefix) "." (name kw))))

    (defn prefix-event-key [prefix event]
      (into [(prefix-keyword prefix (first event))]
            (rest event)))

    (defn path->keyword [path]
      (keyword (str/join "." (map name path))))

    (defn all-paths [x]
      (cond
        (instance? RFHandler x) (list (list x))
        (map? x) (mapcat (fn [[k v]] (map #(cons k %) (all-paths v))) x)
        (vector? x) (mapcat all-paths x)
        :else (list (list x)))))

#?(:cljs (do (defn >>
               ([e] (rf/dispatch (coerce-event e)))
               ([x & xs] (>> (into [:do x] xs))))
             (defn <<
               ([e] (rf/use-sub (coerce-event e)))
               ([x & xs] (<< (into [x] xs))))))

#?(:clj (do

          (defn parse-rf-handler

            [[type fst & nxt]]

            (let [[name fst & nxt]
                  (if (symbol? fst)
                    (cons fst nxt)
                    (concat [nil fst] nxt))

                  [doc & nxt]
                  (if (string? fst)
                    (cons fst nxt)
                    (concat ["" fst] nxt))

                  [interceptors argv return]
                  (case (count nxt)
                    2 (cons [] nxt)
                    3 nxt)]

              {:type type
               :doc doc
               :interceptors interceptors
               :handler `(fn ~@(when name [name]) ~argv ~return)}))

          (defmacro rfn [& xs]
            (list `map->RFHandler (parse-rf-handler xs)))

          (defmacro sub [& xs]
            `(rfn :rf-sub ~@xs))

          (defmacro signal [[deps event-pattern] & body]
            (u/prob :expand-signal
                    `(map->RFHandler {:type :rf-sub
                                      :handler (fn [~(vec (keys deps)) ~event-pattern]
                                                 ~@body)
                                      :inputs ~(vec (vals deps))})))

          (defmacro dbf [& xs]
            `(rfn :rf-dbf ~@xs))

          (defmacro event [& xs]
            `(rfn :rf-event ~@xs))

          (defmacro effect [& xs]
            `(rfn :rf-effect ~@xs))))

#?(:cljs (do :global-registration

             (defn register
               ([x]
                (doseq [p (all-paths x)]
                  (let [raw-handler? (fn? (last p))
                        [path {:keys [interceptors handler type]}]
                        (if raw-handler?
                          [(drop-last 2 p) {:type (last (butlast p)) :handler (last p)}]
                          [(butlast p) (last p)])
                        key (keyword (str/join "." (map name path)))]
                    (case type
                      :rf-sub (rf/reg-sub key handler)
                      :rf-dbf (rf/reg-event-db key interceptors handler)
                      :rf-event (rf/reg-event-fx key interceptors handler)
                      :rf-effect (rf/reg-fx key handler)))))
               ([x & xs]
                (doseq [x (cons x xs)]
                  (register x))))

             (def default-tree
               {:get (ezlix.state/sub [db [_ x & xs]]
                                      (cond (not x) db
                                            xs (get-in db (cons x xs))
                                            (keyword? x) (get db x)
                                            (sequential? x) (get-in db x)))

                :put (ezlix.state/dbf self [db [_ p v & pvs]]
                                      (let [db (cond (sequential? p) (if (seq p) (assoc-in db p v) v)
                                                     (keyword? p) (assoc db p v))]
                                        (if pvs
                                          (self db (into [nil] pvs))
                                          db)))

                :upd (ezlix.state/dbf self [db [_ p f & pfs]]
                                      (let [db (cond (sequential? p) (if (seq p) (update-in db p f) (f db))
                                                     (keyword? p) (update db p f))]
                                        (if pfs
                                          (self db (into [nil] pfs))
                                          db)))

                :do (ezlix.state/event [_ [_ & xs]] {:dispatch-n xs})

                :fx (ezlix.state/event [_ [_ fxs]] fxs)

                :pp [(ezlix.state/event [_ [_ & xs]] {:pp xs})
                     (ezlix.state/effect [xs] (doseq [x xs] (pp/pprint x)))]})

             (println "easy-frame registering base operation")

             (register default-tree)))

;; prelude
#?(:cljs (do :sub-frames

             (defn sub-effects [root-key]
               (interceptor/->interceptor
                {:id ::sub-effects
                 :after (fn [context]
                          (u/prob :prefixed
                                  (assoc context :effects
                                         (reduce (fn [effects [k v]]
                                                   (assoc effects (prefix-keyword root-key k) v))
                                                 {} (interceptor/get-effect context)))))}))

             (defn register-frame
               [{:keys [tree id]}]
               (let [mk-key (partial prefix-keyword id)
                     prefixer (sub-effects id)
                     sub-db (refx.interceptors/path id)]

                 (rf/reg-sub (mk-key :db)
                             (fn [db _] (get db id)))

                 (rf/reg-fx (mk-key :db)
                            (fn [v]
                              (swap! refx.db/app-db assoc id v)))

                 (rf/reg-event-fx (mk-key :dispatch)
                                  (fn [_ [k & xs]]
                                    {:dispatch (into [(mk-key k)]
                                                     xs)}))

                 (rf/reg-event-fx (mk-key :dispatch-n)
                                  (fn [_ [& xs]]
                                    {:dispatch-n (mapv (fn [[k & xs]]
                                                         (into [(mk-key k)] xs))
                                                       xs)}))

                 (doseq [p (all-paths (merge default-tree tree))]
                   (let [[path {:keys [interceptors handler type inputs]}] [(butlast p) (last p)]
                         key (mk-key (path->keyword path))]
                     (case type
                       :rf-dbf (rf/reg-event-db key (into [sub-db] interceptors) handler)
                       :rf-event (rf/reg-event-fx key (into [prefixer] interceptors) handler)
                       :rf-effect (rf/reg-fx key handler)
                       :rf-sub (rf/reg-sub key
                                           (if inputs
                                             (let [prefixed-inputs (mapv (partial prefix-event-key id) inputs)]
                                               (fn [_ _] (mapv rf/sub prefixed-inputs)))
                                             (fn [_ _] (rf/sub [(mk-key :db)])))
                                           handler))))
                 (println "subframe registered: " id)
                 [(fn init-db [db] (rf/dispatch-sync [:fx {(mk-key :db) db}]))
                  (fn sub [e] (<< (into [(mk-key (first e))] (rest e))))
                  (fn dispatch [e] (>> (into [(mk-key (first e))] (rest e))))]))

             (defn use-frame [id db tree]
               (hooks/use-memo :once (let [[init << >>] (register-frame {:id id :tree tree})]
                                       (init db)
                                       [<< >>])))))


^{:clj-kondo/ignore true}
(comment
  (require '[re-frame.registrar :as rr])
  (deref re-frame.registrar/kind->id->handler)
  (effect [x] (println x))
  (register {:iop (dbf self [db [_ p v & pvs]]
                       (let [db (cond (sequential? p) (assoc-in db p v)
                                      (keyword? p) (assoc db p v))]
                         (if pvs
                           (self db (into [nil] pvs))
                           db)))})
  (macroexpand '(effect self [x] (println x)))
  (register {:mod1 {:log (effect [x] (println x))
                    :count [(sub [db] (get db :count))]}}
            [{:inc (dbf [db] (update db :count (fnil inc 0)))}
             {:pouet (raw-effect [x y] (println "pouet " x y))}])

  (rf/dispatch [:pouet 1 2])
  (all-paths {:a [1 2 3] :b {:c 2 :d {:f 4}}}))
