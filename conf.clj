(defn safe-div [x y]
  (try (/ x y)
       (catch ArithmeticException _
                0)))

(defconf
  :leaf-node (apply vector (concat (range 1 101) (take 100 (repeat 'x)))) ;葉ノードの要素
  :inner-node ['+ '- '* 'regresion.core/safe-div]        ;内部ノードの要素
  :population 100                                           ;個体数
  :depth     5                                              ;木の深さ
  :limit-generation-num 10000                              ;最大世代
  :island-num 10                                            ;島の数
  :cross-probability 80                                    ;交配確率
  :inversion-probability 1                                ;逆位確率
  :mutation-probability 1                                 ;突然変異確率
  :migrate-num 5                                            ;移住数
  :right-func      (fn [x] (Math/sin x))                     ;導出する関数
  :range            (apply vector (pmap #(/ % 100.0) (range 1000)))                    ;xの値域
  :migrate-interval 10                                        ;移住間隔
  :elite-num         10                                     ;エリート選択個体数
  )
