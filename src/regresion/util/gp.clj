(ns regresion.util.gp
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn n-args [f]
      (-> f class .getDeclaredMethods first .getParameterTypes alength))

(defn random-tree
      [leafs inners depth]
      (let [leafs-num (count leafs)
            inners-num (count inners)
            sum (+ (count leafs) (count inners))]
           (letfn [(f
                     [depth']
                     (if (zero? (dec depth'))
                       (rand-nth leafs)
                       (if (and (< (rand-int sum) leafs-num)
                               (>= (rand-int depth) (dec depth')))
                           (rand-nth leafs)
                           (let [inner-node (rand-nth inners)
                                 arg-num (n-args inner-node)]
                                (case arg-num
                                      1 (list inner-node (f (dec depth')))
                                      2 (list inner-node (f (dec depth')) (f (dec depth')))
                                      3 (list inner-node (f (dec depth')) (f (dec depth')) (f (dec depth')))
                                      )))))] (f depth)))
      )


(defn get-node
      [tree index]
      (if (empty? index)
        tree
        (get-node ((resolve (first index)) tree) (rest index))))


(defn get-node-indexes
      [tree]
      (let [res-list (atom [['identity]])]
           (letfn [(get-indexes
                     [child-num index]
                     (loop [child-num' child-num conjs []]
                               (if (<= child-num' 1)
                                 (conj conjs (apply vector (concat index (conj (apply vector (take child-num' (repeat 'rest))) 'first))))
                                 (recur (dec child-num') (conj conjs (apply vector (concat index (conj (apply vector (take child-num' (repeat 'rest))) 'first))))))))
                   (f
                     [index]
                     (if (seq? (get-node tree index))
                     (let [child-num (count (rest (get-node tree index)))]
                          (let [indexes (get-indexes child-num index)]
                               (do (swap! res-list concat indexes)
                                   (doall (pmap f indexes)))))))]
                  (do (f ['identity])
                      @res-list))))


(defn get-inner-node-indexes
  [tree]
  (let [res-list (atom [['identity]])]
    (letfn [(get-indexes
              [child-num index]
              (loop [child-num' child-num conjs []]
                (if (<= child-num' 1)
                  (conj conjs (apply vector (concat index (conj (apply vector (take child-num' (repeat 'rest))) 'first))))
                  (recur (dec child-num') (conj conjs (apply vector (concat index (conj (apply vector (take child-num' (repeat 'rest))) 'first))))))))
            (f
              [index]
              (if (seq? (get-node tree index))
                (let [child-num (count (rest (get-node tree index)))]
                      (let [indexes (apply vector (filter #(and (list? (get-node tree %)) (> (count (rest (get-node tree %))) 0))  (get-indexes child-num index)))]
                           (do (swap! res-list concat indexes)
                               (doall (pmap f indexes)))))
                     ))]
           (do (f ['identity])
                  @res-list))))

(defn random-node-index
      [tree]
      (rand-nth (get-node-indexes tree)))

(defn random-inner-node-index
      [tree]
      (rand-nth (get-inner-node-indexes tree)))

(defn mutate
      [leafs inners tree depth]
      (let [rand-index (random-node-index tree)]
           (letfn [(f
                     [tree' index]
                     (cond
                       (= index rand-index) (random-tree leafs inners depth)
                       (or (not (seq? tree'))
                           (empty? tree')) tree'
                       :else (cons (f (first tree') (conj index 'first)) (f (rest tree') (conj index 'rest)))))] (f tree ['identity]) )))

(defn cross
      [tree1 tree2]
      (let [rand-index1 (random-node-index tree1)
            rand-index2 (random-node-index tree2)]
           (letfn [(f
                     [tree subtree index rand-index]
                     (cond
                       (= index rand-index) subtree
                       (or (not (seq? tree))
                           (empty? tree)) tree
                       :else (cons (f (first tree) subtree (conj index 'first) rand-index) (f (rest tree) subtree (conj index 'rest) rand-index))))]
                  [(f tree1 (get-node tree2 rand-index2) ['identity] rand-index1 ) (f tree2 (get-node tree1 rand-index1) ['identity] rand-index2 )])))

(defn not-same-shuffle
      [list]
      (if (reduce #(and %1 %2) (map #(= (first list) %) list))
        list
        (loop [list' (shuffle list)]
              (if (= list list')
                (recur (shuffle list))
                list'))))

(defn inverse
      [tree]
      (if-not (list? tree)
        tree
      (let [rand-index (random-inner-node-index tree)]
           (letfn [(f
                     [tree' index]
                     (cond
                       (= index rand-index) (cons (first tree') (apply list (not-same-shuffle (rest tree'))))
                       (or (not (seq? tree'))
                           (empty? tree')) tree'
                       :else (cons (f (first tree') (conj index 'first)) (f (rest tree') (conj index 'rest)))))] (f tree ['identity]))))
      )

(defn generate-island
  [num leafs inners depth]
  (apply vector (take num (repeatedly #(random-tree leafs inners depth)))))


(defn tree-roulette
  [island evaluation-func]
  (apply vector (reduce concat [] (pmap #(take (evaluation-func %) (repeat %)) island)))
  )

(defn evolution
  [island island-num cross-p mutation-p inversion-p evaluation-func select-roulette leafs inners depth]
  (let [evolution-roulette (concat
                             (take (* cross-p 10) (repeat :cross))
                             (take (* mutation-p 10) (repeat :mutation))
                             (take (* inversion-p 10) (repeat :inversion))
                             (take (- 1000 (* 10 (+ cross-p mutation-p inversion-p))) (repeat :id)))
        tree-roulette (select-roulette island evaluation-func)]
    (loop [island' []]
      (if (>= (count island') island-num)
        island'
        (let [rand-f (rand-nth evolution-roulette)]
          (cond
            (= rand-f :cross) (if (> (- island-num (count island')) 1)
                                (recur (apply vector (concat island' (cross (rand-nth tree-roulette) (rand-nth tree-roulette)))))
                                (recur (conj island' (rand-nth (cross (rand-nth tree-roulette) (rand-nth tree-roulette))))))
            (= rand-f :mutation) (recur (conj island' (mutate leafs inners (rand-nth tree-roulette) depth)))
            (= rand-f :inversion) (recur (conj island' (inverse (rand-nth tree-roulette))))
            :else (recur (conj island' (rand-nth tree-roulette)))))))))

(defn generate-elite-roulette
      [num]
      (fn [island evaluation-func] (take num (sort-by evaluation-func island))))

(defn generate-eva-func
      [func range]
      (fn [tree] (let [f (eval (list 'fn '[x] tree))]
                   (reduce + (pmap #(if (> (func %) (f %) ) (- (func %) (f %)) (- (f %) (func %))) range )))))

(defn min-regresion
      [island eva-func]
      (first (take 1 (sort-by eva-func island))))


(defn migrate
  [islands migrators-size]
  (let [migrating-genes (reduce conj []
                          (pmap #(take migrators-size (shuffle %)) islands))]
        (apply vector (pmap #(apply vector (distinct (conj % (first (rand-nth migrating-genes))))) islands))
        ))

(defn gp-run
  [conf]
  (let [population (:population conf)
        leafs (:leaf-node conf)
        inners (:inner-node conf)
        depth (:depth conf)
        cross-p (:cross-probability conf)
        inverse-p (:inversion-probability conf)
        mutate-p (:mutation-probability conf)
        limit-generation-num (:limit-generation-num conf)
        island-num (:island-num conf)
        migrate-num (:migrate-num conf)
        right-func (:right-func conf)
        range (:range conf)
        migrate-interval (:migrate-interval conf)
        eva-func (generate-eva-func right-func range)
        elite-num (:elite-num conf)
        min-tree (atom '())
        min-result (atom 100000000000000)]
    (loop [islands (apply vector (take island-num (repeatedly #(generate-island population leafs inners depth))))
           num 0]
      (if (or (> num limit-generation-num)
              (zero? @min-result))
        (do (println "=====================================")
            (println num " , " @min-tree " , " @min-result))
        (let [
              min-tree' (min-regresion (reduce conj [] (pmap #(min-regresion % eva-func) islands)) eva-func)
              min-result' (eva-func min-tree')]
          (do
            (if (<= min-result' @min-result)
              (do (reset! min-result min-result')
                  (reset! min-tree min-tree')))
            (println num " , " min-tree' " , " min-result')
            (if (= (mod num migrate-interval) (dec migrate-interval))
              (recur (pmap  #(evolution % population cross-p mutate-p inverse-p eva-func (generate-elite-roulette elite-num) leafs inners depth) (migrate islands migrate-num)) (inc num))
              (recur (pmap  #(evolution % population cross-p mutate-p inverse-p eva-func (generate-elite-roulette elite-num) leafs inners depth) islands) (inc num))
              )))))))
