(defun duracion-ciclo(s-rojo s-verde s-amarillo)
       (+ s-rojo s-verde s-amarillo))

(defun recomendacion-ciclo (duracion)
       (let ((duracion-ciclo duracion))
       (cond
         ((< duracion 35)
     "RECOMENDACION: Aumentar duracion del ciclo")

     ((<= duracion 150)
     "RECOMENDACION: Mantener duracion")

         (t "RECOMENDACION: Reducir duracion del ciclo"))))
