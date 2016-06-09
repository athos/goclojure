(ns example
  (:require [goclojure.core :refer [goclojure]]))

;;
;; Non-goclojure version of code:
;;
;; (defn fib []
;;   (letfn [(rec [a b]
;;             (lazy-seq
;;               (cons a (rec b (+ a b)))))]
;;     (rec 0 1)))
;;
;; (println (take 10 (fib))) ;=> prints (0 1 1 2 3 5 8 13 21 34)
;;

;; Using goclojure, this can be written like the following:

(goclojure

 (dn fib []
   (lf [(r [a b]
          (zq (cns a (r b (+ a b)))))]
     (r 0 1)))

 ;; (pnn (tk 10 (b)))

)
