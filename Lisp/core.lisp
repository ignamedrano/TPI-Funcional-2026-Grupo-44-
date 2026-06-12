;; =========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Mapea entradas a salidas estables sin alterar variables globales ni entorno)
;; ESTRATEGIA: Funcion predicado / condicional (Evalua combinaciones de simbolos mediante cond)
;; IMPACTO: No destructiva (Crea listas nuevas mediante list sin modificar estructuras previas)
;; =========================================================

(defun transicion (color-actual cambiar-a)
	(cond
		((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
		(list 'en-rojo "cambiar-a-verde"))

		((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
        (list 'en-verde "cambiar-a-amarillo"))

		((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
        (list 'en-amarillo "cambiar-a-rojo"))

		(t (list color-actual 'accion-por-defecto))))
