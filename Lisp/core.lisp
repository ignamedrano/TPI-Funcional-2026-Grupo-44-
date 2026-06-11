;; =========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Mapea entradas a salidas estables sin alterar variables globales ni entorno)
;; ESTRATEGIA: Funcion predicado / condicional (Evalua combinaciones de simbolos mediante cond)
;; IMPACTO: No destructiva (Crea listas nuevas mediante list sin modificar estructuras previas)
;; =========================================================

(defun transicion (color-actual cambiar-a)
	(cond
		((and (eq color-actual 'en-rojo) (eq cambiar-a 'amarillo))
		(list 'en-rojo "cambiar-a-amarillo"))

		((and (eq color-actual 'en-amarillo) (eq cambiar-a 'verde))
        (list 'en-amarillo "cambiar-a-verde"))

		((and (eq color-actual 'en-verde) (eq cambiar-a 'rojo))
        (list 'en-verde "cambiar-a-rojo"))

		(t (list color-actual 'accion-por-defecto))))

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
