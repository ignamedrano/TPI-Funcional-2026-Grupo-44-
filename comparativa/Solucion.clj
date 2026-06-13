(defn suma-lista [lista]
  (if (empty? lista)
    0
    (+ (first lista)
       (suma-lista (rest lista)))))

(suma-lista '(1 2 3 4 5))