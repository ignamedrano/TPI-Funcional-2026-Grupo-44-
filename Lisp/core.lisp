
;; =========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un mismo número de tiempo UNIX, siempre retorna el mismo símbolo de color)
;; ESTRATEGIA: Función Predicado / Condicional (Clasifica rangos numéricos usando operadores lógicos)
;; IMPACTO: No destructiva (No altera datos en memoria, solo devuelve símbolos constantes)
;; =========================================================

(defun timer (tiempo-unix)
  (let ((tiempo-ciclo (mod tiempo-unix 216))) ; obtenemos resto del tiempo ingresado por el total de un ciclo para saber donde estamos parados
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



;req 3 extension1

(defun log-transicion (tiempo-actual tiempo-fin color-anterior)
  (let ((color-nuevo (timer tiempo-actual)))
  (cond
    ((> tiempo-actual tiempo-fin) 'no-hay-cambios)
           ((not (eq color-anterior color-nuevo))
            (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
                    tiempo-actual color-anterior color-nuevo)
            (log-transicion (+ tiempo-actual 1) tiempo-fin color-nuevo))
           (t (log-transicion (+ tiempo-actual 1) tiempo-fin color-anterior)))))


;req 4 extension: a y b
(defun duracion-ciclo (s-rojo s-rojo-int s-verde s-verde-int s-amarillo s-amarillo-int)
  (+ s-rojo s-rojo-int s-verde s-verde-int s-amarillo s-amarillo-int))

;b
(defun recomendacion-ciclo (duracion)
  (cond
    ((< duracion 35)
     "RECOMENDACION: Aumentar duracion del ciclo")
    ((<= duracion 150)
     "RECOMENDACION: Mantener duracion")
    (t "RECOMENDACION: Reducir duracion del ciclo")))













