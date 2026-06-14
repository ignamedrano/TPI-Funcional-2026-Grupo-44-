
















;; =========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un mismo número de tiempo UNIX, siempre retorna el mismo símbolo de color)
;; ESTRATEGIA: Función Predicado / Condicional (Clasifica rangos numéricos usando operadores lógicos)
;; IMPACTO: No destructiva (No altera datos en memoria, solo devuelve símbolos constantes)
;; =========================================================

(defun timer (tiempo-unix)
  (let ((tiempo-ciclo (mod tiempo-unix 216)))
    (cond
      ((< tiempo-ciclo 90) 'rojo)
      ((< tiempo-ciclo 210) 'verde)
      (t 'amarillo))
  )
)




; req 2 extension1
(defun timer (tiempo-unix)
  (let ((tiempo-ciclo (mod tiempo-unix 225)))
    (cond
      ((< tiempo-ciclo 90) 'rojo)
      ((< tiempo-ciclo 93) 'rojo-intermitente)
      ((< tiempo-ciclo 213) 'verde)
      ((< tiempo-ciclo 216) 'verde-intermitente)
      ((< tiempo-ciclo 222) 'amarillo)
      (t 'amarillo-intermitente))))














