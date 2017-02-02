(ns regresion.core
  (:use [regresion.util.dsl])
  (:use [regresion.util.gp])
  (:gen-class))

(declare conf)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do (binding [*ns* (the-ns 'regresion.core)]
      (load-file "conf.clj"))
      (gp-run conf)))
