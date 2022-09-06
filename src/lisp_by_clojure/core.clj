(ns lisp-by-clojure.core
  (:require [clojure.spec.alpha :as s]))

(defprotocol SExp)

(s/def ::s-exp #(extends? SExp (type %)))
(defn- s-exp-type [maybe-s-exp] (and (when-not (s/valid? ::s-exp maybe-s-exp) nil) (type s-exp-type)))

(defmulti evaluate s-exp-type)
(defmethod evaluate nil [_] :error/must-be-s-exp-type)
(defmethod evaluate :default [this] this)

(defmulti evaluate-list s-exp-type)
(defmethod evaluate nil [_] :error/must-be-s-exp-type)
(defmethod evaluate-list :default [this] (evaluate this))

(defrecord Int [^int raw] SExp)
(defrecord Fn [f] SExp)
(defrecord Nil [] SExp)

(defrecord Cons [l r] SExp)
(s/def :cons/l ::s-exp)
(s/def :cons/r ::s-exp)
(s/def :cons/cons
  (s/keys :req-un [:cons/l :cons/r]))
(s/fdef ->Cons
  :args (s/cat :l :cons/l :r :cons/r)
  :ret :cons/cons)
(defmethod evaluate Cons
  [{:keys [l r]}]
  (let [l- (evaluate l)]
    (condp = (type l-)
      Fn ((:f l-) (evaluate-list r))
      (->Cons l- (evaluate-list r)))))

(defn add [^Cons args]
  (let [{:keys [l r]} args]
    (loop [left l
           right r
           total 0]
      (cond
        (and (= Int (type left)) (= Cons (type right)))
        (recur (:l right) (:r right) (+ total (:raw left)))

        (and (= Int (type left)) (= Nil (type right)))
        (->Int (+ total (:raw left)))

        :else
        :error/to-add))))


(evaluate (Cons. (Fn. add) (Cons. (Int. 10) (Cons. (Int. 5) (Nil.)))))
;; => {:raw 15}