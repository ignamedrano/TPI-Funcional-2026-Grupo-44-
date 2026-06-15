;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura (Para los mismos tiempos de rojo, verde y amarillo
;;              siempre devuelve la misma duración total)
;; ESTRATEGIA: Directa (Suma de los tiempos de cada fase)
;; IMPACTO: No destructiva
;; ========================================================
(defun duracion-ciclo (s-rojo s-verde s-amarillo)
    (+ s-rojo s-verde s-amarillo))
;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura (Para una misma duración siempre genera
;;              la misma recomendación)
;; ESTRATEGIA: Condicional (Implementada mediante cond)
;; IMPACTO: No destructiva
;; ========================================================
(defun recomendacion-ciclo (duracion)
    (let ((duracion-ciclo duracion))
        (cond
            ((< duracion 35)
             "RECOMENDACION: Aumentar duracion del ciclo")

            ((<= duracion 150)
             "RECOMENDACION: Mantener duracion")

            (t
             "RECOMENDACION: Reducir duracion del ciclo"))))
;; Caso OK
(print(recomendacion-ciclo (duracion-ciclo 15 5 10)))
;; Caso Alternativo (Tiempos en cero: da la recomendación mínima de aumentar duración)
(print(recomendacion-ciclo (duracion-ciclo 0 0 0)))
;(print(recomendacion-ciclo (duracion-ciclo 15 "cinco" 10))) ; Error matemático por texto en segundos
