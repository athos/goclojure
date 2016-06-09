(ns goclojure.core
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str])
  (:import [java.util.regex Pattern]))

(defmacro ?= [x]
  `(let [x# ~x]
     (prn '[DEBUG] '~x x#)
     x#))

(def ^:private %specials
  '#{def if fn* let* letfn* set! do throw try catch finally loop* recur case*
     monitor-enter monitor-exit . new deftype* reify* quote var import*})

(def ^:private %keywords
  {(symbol "nil") nil, (symbol "true") true, (symbol "false") false})

(declare abbrev-expand expand-symbol expand-seq expand-compound expand-special)

(defn- list->sorted-set [list]
  (apply sorted-set-by #(<= (count (str %1)) (count (str %2))) list))

(defn- global-env []
  (list->sorted-set (concat (keys %keywords) %specials (keys (ns-map *ns*)))))

;;
;; Abbreviation entry point
;; 
(defmacro goclojure
  ([expr]
   (let [globals (global-env)
         locals (set (keys &env))]
     (abbrev-expand globals locals expr)))
  ([expr1 & exprs]
   `(do ~@(for [expr (cons expr1 exprs)] `(goclojure ~expr)))))

;; expander
(defn abbrev-expand [globals locals expr]
  (cond (symbol? expr)
        #_> (expand-symbol globals locals expr)
        (and (seq? expr) (not (empty? expr)))
        #_> (expand-compound globals locals expr)
        (vector? expr)
        #_> (vec (expand-seq globals locals expr))
        (map? expr)
        #_> (into {} (expand-seq globals locals expr))
        (set? expr)
        #_> (set (expand-seq globals locals expr))
        :else expr))

(defn- matching-name [globals sym]
  (let [re (->> (seq (str sym))
                (map #(Pattern/quote (str %)))
                (str/join ".*?")
                re-pattern)]
    (first (filter #(re-find re (str %)) globals))))

(defn- expand-seq [globals locals s]
  (map #(abbrev-expand globals locals %) s))

(defn- expand-args [globals locals [op & args]]
  `(~op ~@(expand-seq globals locals args)))

(defn- expand-symbol [globals locals sym]
  (let [g (gensym), obj (get %keywords sym g)]
    (if (not= g obj)
      obj
      (or (and (or (locals sym) (globals sym)) sym)
          (matching-name globals sym)
          sym))))

(defn- expand-compound [globals locals [op & args :as form]]
  (let [op' (abbrev-expand globals locals op)]
    (if (symbol? op')
      (cond (not= op op')
            #_> (recur globals locals `(~op' ~@args))
            (%specials op)
            #_> (expand-special globals locals form)
            (locals op)
            #_> (expand-args globals locals form)
            :else
            #_> (let [v (resolve op)]
                  (if (:macro (meta v))
                    (recur globals locals (apply v form nil args))
                    (expand-args globals locals form))))
      (expand-args globals locals `(~op' ~@args)))))

(defmulti ^:private expand-special (fn [_ _ [op & _]] op))
(defmethod expand-special :default [globals locals form]
  (expand-args globals locals form))

(defn- expand-bindings [globals locals bindings k]
  (loop [[name init & bindings] bindings, locals locals, bindings' []]
    (if (nil? name)
      (k locals bindings')
      (recur bindings
             (conj locals name)
             (conj bindings' name (abbrev-expand globals locals init))))))

(defmethod expand-special 'def [globals locals [_ vname expr]]
  `(def ~vname ~(abbrev-expand (conj globals vname) locals expr)))

(defmethod expand-special 'fn* [globals locals [_ maybe-name & sigs]]
  (let [fname (and (symbol? maybe-name) maybe-name)
        sigs' (if fname sigs (cons maybe-name sigs))
        sigs' (if (vector? (first sigs')) (list sigs') sigs')
        locals' (if fname (conj locals fname) locals)]
    `(fn* ~@(if fname [fname] nil)
       ~@(for [[args & body] sigs']
           `(~args ~@(expand-seq globals (into locals' args) body))))))

(defmethod expand-special 'let* [globals locals [_ bindings & body]]
  (expand-bindings globals locals bindings
    (fn [locals' bindings']
      `(let* ~bindings' ~@(expand-seq globals locals' body)))))

(defmethod expand-special 'letfn* [globals locals [_ bindings & body]]
  (let [bindings' (partition 2 bindings)
        fnames (map first bindings')
        inits (map second bindings')
        locals' (into locals fnames)]
    `(letfn* ~(vec (interleave fnames (expand-seq globals locals' inits)))
       ~@(expand-seq globals locals' body))))

(defmethod expand-special 'loop* [globals locals [_ bindings & body]]
  (expand-bindings globals locals bindings
    (fn [locals' bindings']
      `(loop* ~bindings' ~@(expand-seq globals locals' body)))))

(defmethod expand-special 'catch [globals locals [_ ename & body]]
  `(catch ~ename ~@(expand-seq globals (conj locals ename) body)))

(defmethod expand-special 'quote [_ _ form]
  form)

(defmethod expand-special 'var [globals _ [_ vname]]
  `(var ~(matching-name globals vname)))

(defn shortest-abbreviation
  ([sym] (shortest-abbreviation (global-env) sym))
  ([globals sym]
   (first (for [cs (rest (comb/subsets (str sym)))
                :let [n (apply str cs)]
                :when (= sym (matching-name globals n))]
            (symbol n)))))
