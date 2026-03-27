;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent ABC123.
;; <Descripció de les funcions d'aquest fitxer>

;; FUNCIÓN PRINCIPAL QUE LLAMA EL CONTROLADOR
(defun agent-xyz999 (dades)
  ;; 'dades' es una lista con: el mapa visible, tus puntos, tus bolas...
  ;; Esta función debe devolver una lista de órdenes como:
  ;; '((crea-bolla (2 3)) (mou (4 5)) (pinta (6 7)))
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