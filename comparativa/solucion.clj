;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Predicado (usa cond para evaluar estados)
;; IMPACTO: No destructiva
;; PROPÓSITO: Modelar las transiciones válidas del semáforo devolviendo el estado actual y la acción a
;;            realizar. Si la transición no es válida retorna una acción por defecto.

(defn transicion [color-actual cambiar-a]
  (cond
    (and (= color-actual :en-rojo)
         (= cambiar-a :verde))
    [:en-rojo "cambiar-a-verde"]

    (and (= color-actual :en-verde)
         (= cambiar-a :amarillo))
    [:en-verde "cambiar-a-amarillo"]

    (and (= color-actual :en-amarillo)
         (= cambiar-a :rojo))
    [:en-amarillo "cambiar-a-rojo"]

    :else
    [color-actual :accion-por-defecto]))

;; FUNCIÓN: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Predicado (usa cond para determinar el estado)
;; IMPACTO: No destructiva
;; PROPÓSITO: Determinar el estado del semáforo según el tiempo Unix recibido. Incluye los estados
;;            intermitentes agregados en la Extensión 1.

(defn timer [tiempo-unix]
  (let [tiempo-ciclo (mod tiempo-unix 225)]
    (cond
      (< tiempo-ciclo 90)  :rojo
      (< tiempo-ciclo 93)  :rojo-intermitente
      (< tiempo-ciclo 213) :verde
      (< tiempo-ciclo 216) :verde-intermitente
      (< tiempo-ciclo 222) :amarillo
      :else                :amarillo-intermitente)))
