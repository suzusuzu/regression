(ns regresion.util.dsl
  (:gen-class))

(defmacro defconf
  [& args]
  `(let [m# (hash-map ~@args)]
     (def ^:dynamic ~'conf m#)))
