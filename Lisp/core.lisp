;; FUNCIÓN: log-transicion
;; NATURALEZA: No pura
;; ESTRATEGIA: Recursiva
;; IMPACTO: No destructiva
;; PROPÓSITO: Registrar e imprimir en pantalla cada cambio
;;            de estado del semáforo indicando el tiempo,
;;            el color anterior y el nuevo color. 

(defun log-transicion (tiempo-actual tiempo-fin color-anterior)
  (let ((color-nuevo (timer tiempo-actual)))
    (cond
      ((> tiempo-actual tiempo-fin)
       'no-hay-cambios)

      ((not (eq color-anterior color-nuevo))
       (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
               tiempo-actual color-anterior color-nuevo)
       (log-transicion (+ tiempo-actual 1)
                       tiempo-fin
                       color-nuevo))

      (t (log-transicion (+ tiempo-actual 1)
                       tiempo-fin
                       color-anterior)))))
