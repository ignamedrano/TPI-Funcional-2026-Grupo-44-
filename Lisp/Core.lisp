(defun transicion(color-actual, cambiar-a)
    (cond
        ((eq color-actual 'rojo) (eq cambiar-a 'verde) 'verde)
        ((eq color-actual 'verde) (eq cambiar-a 'amarillo) 'amarillo)
        ((eq color-actual 'amarillo) (eq cambiar-a 'rojo) 'rojo)
        (t (list color-actual 'accion-por-defecto))
    )
)

(defun transicion-valida (color-actual cambiar-a)

    (cond
        ((and (eq color-actual 'rojo)(eq cambiar-a 'verde)) t)
        ((and (eq color-actual 'verde)(eq cambiar-a 'amarillo)) t)
        ((and (eq color-actual 'amarillo)(eq cambiar-a 'rojo)) t)
        (t nil)
    )
)

(defun transicion (color-actual cambiar-a)
	(cond
		((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
		(list 'en-verde "cambiar-a-verde"))

		((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))(list 'en-rojo "cambiar-a-rojo"))

		((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))(list 'en-amarillo "cambiar-a-amarillo"))
		(t (list color-actual 'accion-por-defecto))
    )
)

(defun timer-rojo(tiempo-unix)
    let ((actual-tiempo (tiempo-unix)))
    (cond
        ((<= (- actual-tiempo tiempo-unix) 90) 'rojo)
        (t  timer-amarillo(tiempo-unix))
    )
)

(defun timer-amarillo(tiempo-unix)
    let ((actual-tiempo (tiempo-unix)))
    (cond
        ((<= (- actual-tiempo tiempo-unix) 6) 'amarillo)
        (t timer-verde(tiempo-unix))
    )
)

(defun  timer-verde(tiempo-unix)
    let ((actual-tiempo (tiempo-unix)))
    (cond
        ((<= (- actual-tiempo tiempo-unix) 120) 'verde)
        (t timer-rojo(tiempo-unix))
    )
)

(defun timer (tiempo-unix)
    timer-rojo(tiempo-unix)
)