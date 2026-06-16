;req2
(defun timer (tiempo-unix)  ; recibe un tipo de dato numero en formato unix
  (let ((tiempo-ciclo (mod tiempo-unix 216))) 
    (cond
      ((< tiempo-ciclo 90) 'rojo)
      ((< tiempo-ciclo 210) 'verde)
      (t 'amarillo)) ; devuelve un simbolo dependiendo de donde este el semaforo
  )
)

;req2 extension
(defun timer (tiempo-unix)
  (let ((tiempo-ciclo (mod tiempo-unix 225)))
    (cond
      ((< tiempo-ciclo 90) 'rojo)
      ((< tiempo-ciclo 93) 'rojo-intermitente)
      ((< tiempo-ciclo 213) 'verde)
      ((< tiempo-ciclo 216) 'verde-intermitente)
      ((< tiempo-ciclo 222) 'amarillo)
      (t 'amarillo-intermitente))))
;req3 extension
(defun log-transicion (tiempo-actual tiempo-fin color-anterior)
  (let ((color-nuevo (timer tiempo-actual)))
  (cond
    ((> tiempo-actual tiempo-fin) 'no-hay-cambios)
           ((not (eq color-anterior color-nuevo))
            (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
                    tiempo-actual color-anterior color-nuevo)
            (log-transicion (+ tiempo-actual 1) tiempo-fin color-nuevo))
           (t (log-transicion (+ tiempo-actual 1) tiempo-fin color-anterior)))))

(print (log-transicion 86 94 'desconocido));ok

;req4 extension
;a

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

(print (recomendacion-ciclo (duracion-ciclo 90 3 120 3 6 3)));ok

(terpri)
(terpri)
