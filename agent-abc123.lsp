;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent ABC123.
;; <Descripció de les funcions d'aquest fitxer>

"FUNCIÓN PRINCIPAL QUE LLAMA EL CONTROLADOR"
(defun agent-xyz999 (visio)
  (format t "He recibido una vision con ~A casillas~%" (length visio))

  ;; BUSCAMOS SI HAY ALGUNA BOLA EN LA VISIÓN QUE HEMOS RECIBIDO
  (let ((bolas-vistas (count-if #'(lambda (casilla) (member 'bolla casilla)) visio)))
    (if (> bolas-vistas 0)
        (format t "¡OJO! El agente ve ~A bolas en el mapa.~%" bolas-vistas)
        (format t "El agente no ve ninguna bola todavía.~%")))

  (format t "L'Agente està pensant...\n~%")
  nil ; de momento no hace ninguna accion
)

;; DECIDIR QUÉ HACE CADA BOLA QUE TIENES VIVA
(defun decidir-accio-bolla (bolla dades)
  ;; ¿Está cerca de la base enemiga? -> Pinta
  ;; ¿Está lejos? -> Mou
)

;; BUSCAR EL CAMINO O EL OBJETIVO MÁS CERCANO
(defun buscar-objectiu-proper (posicio-actual dades)
  ;; Aquí es donde usas la distancia euclidiana para saber a dónde ir
)


;; Recordau que qualsevol funció no predefinida necessària per a l'agent, ha d'estar definida en aquest fitxer
;; i ha d'estar prefixada amb "agent-abc123-" per evitar conflictes amb altres agents o mòduls.
;; Per exemple:
(defun agent-abc123-longitud (llista)
    "Retorna la longitud d'una llista."
    (cond ((null llista) 0)
          (t (+ 1 (agent-abc123-longitud (cdr llista)))))
)