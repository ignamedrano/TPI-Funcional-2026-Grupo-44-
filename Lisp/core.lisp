;; ========================================================
;; FUNCIÓN: log-transicion
;; NATURALEZA: Impura (Genera un efecto secundario explícito en la pantalla mediante la función format)
;; ESTRATEGIA: Recursiva de cola (Las llamadas recursivas dependen de la evaluación del flujo condicional)
;; IMPACTO: No destructiva (No muta ninguna estructura de datos en memoria)
;; ======================================================== 

(defun log-transicion (tiempo-actual tiempo-fin color-anterior)
  (let ((color-nuevo (timer tiempo-actual)))
    (cond
      ((> tiempo-actual tiempo-fin) 'no-hay-cambios)
        ((not (eq color-anterior color-nuevo))
         (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
               tiempo-actual color-anterior color-nuevo)
       (log-transicion (+ tiempo-actual 1) tiempo-fin color-nuevo))
      (t (log-transicion (+ tiempo-actual 1) tiempo-fin color-anterior)))))

;;(compile 'log-transicion) ;para entornos antiguos, descomentamos y forzamos la compilacion 
;; Caso OK
(print(log-transicion 0 3000 'desconocido))
(print(log-transicion 200 1000 'desconocido))    
;; Caso Alternativo (Tiempo inicial es mayor al final, no-hay-cambios)
(print(log-transicion 500 100 'desconocido))
;(print(log-transicion "doscientos" 1000 'desconocido)) ; Error matemático al comparar texto
