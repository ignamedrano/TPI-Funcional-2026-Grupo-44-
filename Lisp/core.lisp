


















(defun timer (tiempo-unix)
  (let ((tiempo-ciclo (mod tiempo-unix 216)))
    (cond
      ((< tiempo-ciclo 90) 'rojo)
      ((< tiempo-ciclo 210) 'verde)
      (t 'amarillo))
  )
)
