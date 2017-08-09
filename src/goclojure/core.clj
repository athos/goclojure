(ns goclojure.core
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]
            [net.cgrand.macrovich :as macros])
  (:import [java.util.regex Pattern]))

(def ^:private %specials
  (into '#{def if fn* let* letfn* set! do throw try catch finally loop*
           recur case* . new deftype* quote var}
        (macros/case :clj '#{monitor-enter monitor-exit reify* import*})))

(def ^:private %keywords
  {(symbol "nil") nil, (symbol "true") true, (symbol "false") false})

(defn- list->sorted-set [list]
  (apply sorted-set-by #(<= (count (str %1)) (count (str %2))) list))

(defn- make-global-env [env]
  (macros/case
    :clj (-> (concat (keys %keywords) %specials (keys (ns-map *ns*)))
             list->sorted-set)
    :cljs (-> (concat (keys %keywords) %specials
                      (keys (get-in env [:ns :uses]))
                      (keys (get-in env [:ns :use-macros]))
                      (keys (get-in env [:ns :rename]))
                      (keys (get-in env [:ns :rename-macros]))
                      (keys (get-in env [:ns :defs])))
              list->sorted-set)))

(defn- make-local-env [env]
  (macros/case
    :clj (set (keys env))
    :cljs (set (keys (:locals env)))))

(defn- matching-name [globals sym]
  (let [re (->> (seq (str sym))
                (map #(Pattern/quote (str %)))
                (str/join ".*?")
                re-pattern)]
    (first (filter #(re-find re (str %)) globals))))

(declare abbrev-expand)

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

(defmulti ^:private expand-special (fn [_ _ [op & _]] op))
(defmethod expand-special :default [globals locals form]
  (expand-args globals locals form))

(defn- expand-compound [globals locals [op & args :as form]]
  (let [op' (abbrev-expand globals locals op)]
    (if (symbol? op')
      (cond (not= op op')
            (recur globals locals `(~op' ~@args))

            (%specials op)
            (expand-special globals locals form)

            (locals op)
            (expand-args globals locals form)

            :else
            (let [v (resolve op)]
              (if (:macro (meta v))
                (recur globals locals (apply v form nil args))
                (expand-args globals locals form))))
      (expand-args globals locals `(~op' ~@args)))))

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

(defn- expand-bindings [globals locals bindings k]
  (loop [[name init & bindings] bindings, locals locals, bindings' []]
    (if (nil? name)
      (k locals bindings')
      (recur bindings
             (conj locals name)
             (conj bindings' name (abbrev-expand globals locals init))))))

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

;;;
;;; Public APIs
;;;

(defn abbrev-expand [globals locals expr]
  (cond (symbol? expr)
        (expand-symbol globals locals expr)

        (and (seq? expr) (not (empty? expr)))
        (expand-compound globals locals expr)

        (vector? expr)
        (vec (expand-seq globals locals expr))

        (map? expr)
        (into {} (expand-seq globals locals expr))

        (set? expr)
        (set (expand-seq globals locals expr))

        :else expr))

;; Abbreviation entry point

(defmacro goclojure
  ([expr]
   (let [globals (make-global-env &env)
         locals (make-local-env &env)]
     (abbrev-expand globals locals expr)))
  ([expr1 & exprs]
   `(do ~@(for [expr (cons expr1 exprs)] `(goclojure ~expr)))))

;; name abbreviator

(defn- shortest-abbreviation* [globals sym]
  (first (for [cs (rest (comb/subsets (str sym)))
               :let [n (apply str cs)]
               :when (= sym (matching-name globals n))]
           (symbol n))))

(macros/case
  :clj
  (defn shortest-abbreviation
    ([sym] (shortest-abbreviation (make-global-env nil) sym))
    ([globals sym] (shortest-abbreviation* globals sym)))
  :cljs
  (defmacro shortest-abbreviation
    ([sym] `(shortest-abbrevition '~(make-global-env &env) '~sym))
    ([globals sym] `'~(shortest-abbrevition* globals sym))))
