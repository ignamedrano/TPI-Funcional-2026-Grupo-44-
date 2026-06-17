;;; =========================================================
;;; FUNCION: transicion
;;; NATURALEZA: Pura (Mapea entradas a salidas estables sin alterar variables globales ni entorno)
;;; ESTRATEGIA: Funcion predicado / condicional (Evalua combinaciones de simbolos mediante cond)
;;; IMPACTO: No destructiva (Crea listas nuevas mediante list sin modificar estructuras previas)
;;; =========================================================

(defun transicion (color-actual cambiar-a)
	(cond
		((and (eq color-actual 'en-rojo) (eq cambiar-a 'verde))
		(list 'en-rojo "cambiar-a-verde"))

		((and (eq color-actual 'en-verde) (eq cambiar-a 'amarillo))
        (list 'en-verde "cambiar-a-amarillo"))

		((and (eq color-actual 'en-amarillo) (eq cambiar-a 'rojo))
        (list 'en-amarillo "cambiar-a-rojo"))

		(t (list color-actual 'accion-por-defecto))))

;; Casos OK
(print (transicion 'en-rojo 'verde))
(print (transicion 'en-verde 'amarillo))
(print (transicion 'en-amarillo 'rojo))
;; Caso Alternativo (El semáforo ignora la orden porque no es la secuencia correcta)
(print (transicion 'en-rojo 'amarillo))
;; Caso de Error
(print (transicion "en-rojo" 'verde)) ; Error de tipo: usa texto en vez de símbolos

(terpri)
(terpri)

;; =========================================================
;; FUNCIÓN: timer
;; NATURALEZA: Pura (Dado un mismo número de tiempo UNIX, siempre retorna el mismo símbolo de color)
;; ESTRATEGIA: Función Predicado / Condicional (Clasifica rangos numéricos usando operadores lógicos)
;; IMPACTO: No destructiva (No altera datos en memoria, solo devuelve símbolos constantes)
;; =========================================================

;(sb-ext:unlock-package :sb-ext); desbloquear la palabra reservada timer (SBCL)

(defun timer (tiempo-unix)  ; recibe un tipo de dato numero en formato unix
  (let ((tiempo-ciclo (mod tiempo-unix 216))) 
    (cond
      ((< tiempo-ciclo 90) 'rojo)
      ((< tiempo-ciclo 210) 'verde)
      (t 'amarillo)) ; devuelve un simbolo dependiendo de donde este el semaforo
  )
)

;;Caso ok
(print (timer 120))

(terpri)
(terpri)

;;; ========================================================
;;; FUNCIÓN: simular-distribucion
;;; NATURALEZA: Pura (Calcula estadísticas porcentuales operando únicamente sobre sus argumentos locales)
;;; ESTRATEGIA: Recursividad de Cola (La llamada a sí misma es la última acción atómica en cada rama)
;;; IMPACTO: No destructiva (Genera dinámicamente una lista nueva al llegar al caso base final)
;;; ========================================================

(defun simular-distribucion (tiempo-restante color-actual tiempo-en-color acum-rojo acum-amarillo acum-verde)
  (cond
    ;; Si ya pasaron los 3600 segundos, se frena el bucle y devuelve los porcentajes finales.
    ((<= tiempo-restante 0)
     (list 'rojo (* (/ acum-rojo 3600.0) 100)
           'amarillo (* (/ acum-amarillo 3600.0) 100)
           'verde (* (/ acum-verde 3600.0) 100)))
    ;; Si un color cumplió su ciclo, llamamos a la recursión cambiando el color y reseteando su timer.
    ((and (eq color-actual 'en-rojo) (>= tiempo-en-color 90))
     (simular-distribucion tiempo-restante 'en-verde 0 acum-rojo acum-amarillo acum-verde))

    ((and (eq color-actual 'en-verde) (>= tiempo-en-color 120))
     (simular-distribucion tiempo-restante 'en-amarillo 0 acum-rojo acum-amarillo acum-verde))

    ((and (eq color-actual 'en-amarillo) (>= tiempo-en-color 6))
     (simular-distribucion tiempo-restante 'en-rojo 0 acum-rojo acum-amarillo acum-verde))

    ;; Restamos 1 al tiempo restante, sumamos 1 al timer del color y 1 al acumulador correspondiente.
    ((eq color-actual 'en-rojo)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) (+ acum-rojo 1) acum-amarillo acum-verde))

    ((eq color-actual 'en-verde)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) acum-rojo acum-amarillo (+ acum-verde 1)))

    ((eq color-actual 'en-amarillo)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) acum-rojo (+ acum-amarillo 1) acum-verde))
     (t (list color-actual 'No-corresponde))))


;(compile 'simular-distribucion) ; para entornos antiguos, descomentamos y forzamos la compilacion
(print(simular-distribucion 3600 'en-rojo 0 0 0 0 )) ; ok
(print(simular-distribucion 3600 'en-verde 40 100 20 200)); alternativo empezando en verde y con acumuladores avanzados

(terpri)
(terpri)

;;Extension 1, Requerimiento 1
(defun transicion1 (color-actual cambiar-a)
  (cond
    ((and (eq color-actual 'en-amarillo-intermitente) (eq cambiar-a 'rojo))
       (list 'en-amarillo-intermitente "cambiar-a-rojo"))

    ((and (eq color-actual 'en-rojo) (eq cambiar-a 'rojo-intermitente))
     (list 'en-rojo "cambiar-a-rojo-intermitente"))

    ((and (eq color-actual 'en-rojo-intermitente) (eq cambiar-a 'verde))
     (list 'en-rojo-intermitente "cambiar-a-verde")))

    ((and (eq color-actual 'en-verde) (eq cambiar-a 'verde-intermitente))
       (list 'en-verde "cambiar-a-verde-intermitente"))

    ((and (eq color-actual 'en-verde-intermitente) (eq cambiar-a 'amarillo))
       (list 'en-verde-intermitente "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo) (eq cambiar-a 'amarillo-intermitente))
       (list 'en-amarillo "cambiar-a-amarillo-intermitente"))
      
    (t (list color-actual 'accion-por-defecto)))

;;Caso ok
(print (transicion 'en-amarillo-intermitente 'rojo))
(print (transicion 'en-rojo 'rojo-intermitente))
(print (transicion 'en-rojo-intermitente 'verde))

(terpri)
(terpri)

;;Extension 1, Requerimiento 2
(defun timer (tiempo-unix)
  (let ((tiempo-ciclo (mod tiempo-unix 225)))
    (cond
      ((< tiempo-ciclo 90) 'rojo)
      ((< tiempo-ciclo 93) 'rojo-intermitente)
      ((< tiempo-ciclo 213) 'verde)
      ((< tiempo-ciclo 216) 'verde-intermitente)
      ((< tiempo-ciclo 222) 'amarillo)
      (t 'amarillo-intermitente))))

;Extension 1, Requerimiento 3
(defun log-transicion (tiempo-actual tiempo-fin color-anterior)
  (let ((color-nuevo (timer tiempo-actual)))
  (cond
    ((> tiempo-actual tiempo-fin) 'no-hay-cambios)
           ((not (eq color-anterior color-nuevo))
            (format t "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
                    tiempo-actual color-anterior color-nuevo)
            (log-transicion (+ tiempo-actual 1) tiempo-fin color-nuevo))
           (t (log-transicion (+ tiempo-actual 1) tiempo-fin color-anterior)))))

;;Caso ok
(print (log-transicion 86 94 'desconocido))

(terpri)
(terpri)

;Extension 1, Requerimiento 4-A
(defun duracion-ciclo (s-rojo s-rojo-int s-verde s-verde-int s-amarillo s-amarillo-int)
  (+ s-rojo s-rojo-int s-verde s-verde-int s-amarillo s-amarillo-int))

;Extension 1, Requerimiento 4-B
(defun recomendacion-ciclo (duracion)
  (cond
    ((< duracion 35)
     "RECOMENDACION: Aumentar duracion del ciclo")
    ((<= duracion 150)
     "RECOMENDACION: Mantener duracion")
    (t "RECOMENDACION: Reducir duracion del ciclo")))

;;Caso ok
(print (recomendacion-ciclo (duracion-ciclo 90 3 120 3 6 3)))

(terpri)
(terpri)

;;Extension 1, Requerimiento 5

(defun ciclos-por-tiempo (minutos)
  (floor (/ (* minutos 60) 225)))

;;Caso ok
(print (ciclos-por-tiempo 60))

(terpri)
(terpri)

;;Extension 1, Requerimiento 6
(defun simular-distribucion (tiempo-restante color-actual tiempo-en-color 
                             acum-rojo acum-rojo-int acum-verde acum-verde-int acum-amarillo acum-amarillo-int)
  (cond
    ;; Devuelve la lista con los 6 porcentajes calculados sobre los 3600 segundos
    ((<= tiempo-restante 0)
     (list 'rojo (* (/ acum-rojo 3600.0) 100)
           'rojo-intermitente (* (/ acum-rojo-int 3600.0) 100)
           'verde (* (/ acum-verde 3600.0) 100)
           'verde-intermitente (* (/ acum-verde-int 3600.0) 100)
           'amarillo (* (/ acum-amarillo 3600.0) 100)
           'amarillo-intermitente (* (/ acum-amarillo-int 3600.0) 100)))

    ;; Control de saltos de estado según la nueva lógica de tiempos (90, 3, 120, 3, 6, 3)
    ((and (eq color-actual 'en-rojo) (>= tiempo-en-color 90))
     (simular-distribucion tiempo-restante 'en-rojo-intermitente 0 acum-rojo acum-rojo-int acum-verde acum-verde-int acum-amarillo acum-amarillo-int))

    ((and (eq color-actual 'en-rojo-intermitente) (>= tiempo-en-color 3))
     (simular-distribucion tiempo-restante 'en-verde 0 acum-rojo acum-rojo-int acum-verde acum-verde-int acum-amarillo acum-amarillo-int))

    ((and (eq color-actual 'en-verde) (>= tiempo-en-color 120))
     (simular-distribucion tiempo-restante 'en-verde-intermitente 0 acum-rojo acum-rojo-int acum-verde acum-verde-int acum-amarillo acum-amarillo-int))

    ((and (eq color-actual 'en-verde-intermitente) (>= tiempo-en-color 3))
     (simular-distribucion tiempo-restante 'en-amarillo 0 acum-rojo acum-rojo-int acum-verde acum-verde-int acum-amarillo acum-amarillo-int))

    ((and (eq color-actual 'en-amarillo) (>= tiempo-en-color 6))
     (simular-distribucion tiempo-restante 'en-amarillo-intermitente 0 acum-rojo acum-rojo-int acum-verde acum-verde-int acum-amarillo acum-amarillo-int))

    ((and (eq color-actual 'en-amarillo-intermitente) (>= tiempo-en-color 3))
     (simular-distribucion tiempo-restante 'en-rojo 0 acum-rojo acum-rojo-int acum-verde acum-verde-int acum-amarillo acum-amarillo-int))

    ;; Incremento paso a paso del tiempo y de los acumuladores correspondientes
    ((eq color-actual 'en-rojo)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) (+ acum-rojo 1) acum-rojo-int acum-verde acum-verde-int acum-amarillo acum-amarillo-int))
    
    ((eq color-actual 'en-rojo-intermitente)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) acum-rojo (+ acum-rojo-int 1) acum-verde acum-verde-int acum-amarillo acum-amarillo-int))

    ((eq color-actual 'en-verde)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) acum-rojo acum-rojo-int (+ acum-verde 1) acum-verde-int acum-amarillo acum-amarillo-int))
    
    ((eq color-actual 'en-verde-intermitente)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) acum-rojo acum-rojo-int acum-verde (+ acum-verde-int 1) acum-amarillo acum-amarillo-int))

    ((eq color-actual 'en-amarillo)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) acum-rojo acum-rojo-int acum-verde acum-verde-int (+ acum-amarillo 1) acum-amarillo-int))
    
    ((eq color-actual 'en-amarillo-intermitente)
     (simular-distribucion (- tiempo-restante 1) color-actual (+ tiempo-en-color 1) acum-rojo acum-rojo-int acum-verde acum-verde-int acum-amarillo (+ acum-amarillo-int 1)))))

;(compile 'simular-distribucion) ; para entornos antiguos, descomentamos y forzamos la compilacion
;; Caso OK (Simulación de una hora completa)
(print (simular-distribucion 3600 'en-rojo 0 0 0 0 0 0 0))


