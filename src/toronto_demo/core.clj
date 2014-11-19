(ns toronto-demo.core
  (:refer-clojure :exclude [defn])
  (:require [clojure.core.typed :as t
             :refer [defn defalias Rec U]]))

(defalias Expr
  "A Clojure AST represented with immutable maps"
  (Rec [Expr]
    (U '{:op ':if
         :test Expr
         :then Expr
         :else Expr}
       '{:op ':const
         :val t/Int}
       '{:op ':do
         :exprs (t/Vec Expr)}
       '{:op ':local
         :name t/Sym}
       '{:op ':fn
         :local '{:name t/Sym}
         :body Expr})))

(t/ann sum [Expr -> t/Int])
(defmulti sum :op)

(defmethod sum :if
  [{:keys [test then else] :as expr}]
  (apply + (map sum [test then else])))

(defmethod sum :const
  [{:keys [val]}]
  val)

(defmethod sum :do
  [{:keys [exprs]}]
  (apply + (map sum exprs)))

(defmethod sum :local [{:keys []}] 0)

(defmethod sum :fn
  [{:keys [body]}]
  (sum body))

(t/ann make-if [Expr Expr Expr -> Expr])
(defn make-if [test then else]
  {:op :if
   :test test
   :then then
   :else else})

(t/ann make-const [t/Int -> Expr])
(defn make-const [i]
  {:op :const :val i})

(t/ann make-local [t/Sym -> Expr])
(defn make-local [s]
  {:op :local :name s})

(t/ann make-do [Expr * -> Expr])
(defn make-do [& es]
  {:op :do 
   :exprs (vec es)})

(sum
  (make-if (make-const 1) 
           (make-const 1) 
           (make-const 1)))

(declare parse)

(t/ann parse-if [(t/Seq t/Any) -> Expr]) 
(defn parse-if [s]
  (let [[_ test then else] (vec s)]
    (make-if (parse test)
             (parse then)
             (parse else))))

(t/ann parse-do [(t/ASeq t/Any) -> Expr]) 
(defn parse-do [s]
  (let [[_do_ & body] (vec s)]
    (apply make-do (map parse body))))

(t/ann ^:no-check parse [t/Any -> Expr])
(defn parse [syn]
  (cond
    (symbol? syn) (make-local syn)
    (integer? syn) (make-const syn)
    (list? syn) (case (first syn)
                  if (parse-if syn)
                  do (parse-do syn)
                  fn (assert nil))))

(sum (parse '(do 1 2 3)))
