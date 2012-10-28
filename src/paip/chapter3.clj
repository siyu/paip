(ns paip.chapter-3
  (:use [clojure.set :only [difference union]]))

(declare achieve apply-op appropriate-p)

(defn gps
  "General Problem Solver: achieve all goals"
  [init-state goals ops]
  (let [state (atom init-state)]
    (when (every? (partial achieve state ops) goals) "solved")))

(defn achieve
  "A goal is achieved if it already holds"
  [state ops goal]
  (or (get @state goal)
      (some (partial apply-op state ops)
            (filter (partial appropriate-p goal) ops))))

(defn apply-op
  "Print a message and update state if op is applicable."
  [state ops op]
  (when (every? (partial achieve state ops) (:preconds op))
    (println "executing" (:action op))
    (swap! state difference (:del-list op))
    (swap! state union (:add-list op))
    true))

(defn appropriate-p
  "An op is appropriate to a goal if it is in it's add-list"
  [goal op]
  (get (:add-list op) goal))

(def school-ops
  [{:action :drive-son-to-school
    :preconds #{:son-at-home :car-works}
    :add-list #{:son-at-school}
    :del-list #{:son-at-home}}
   {:action :shop-installs-battery
    :preconds #{:car-needs-battery :shop-knows-problem :shop-has-money}
    :add-list #{:car-works}}
   {:action :tell-shop-problem
    :preconds #{:in-communication-with-shop}
    :add-list #{:shop-knows-problem}}
   {:action :telephone-shop
    :preconds #{:know-phone-number}
    :add-list #{:in-communication-with-shop}}
   {:action :look-up-number
    :preconds #{:have-phone-book}
    :add-list #{:know-phone-number}}
   {:action :give-shop-money
    :preconds #{:have-money}
    :add-list #{:shop-has-money}
    :del-list #{:have-money}}])

(gps #{:son-at-home :car-needs-battery :have-money :have-phone-book}
     #{:son-at-school}
     school-ops)

(gps #{:son-at-home :car-needs-battery :have-money}
     #{:son-at-school}
     school-ops)

(gps #{:son-at-home :car-works}
     #{:son-at-school}
     school-ops)
