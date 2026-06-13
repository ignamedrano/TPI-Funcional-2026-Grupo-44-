
















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
