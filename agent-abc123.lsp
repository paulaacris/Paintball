;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent ABC123.
;; <Descripció de les funcions d'aquest fitxer>

"REGLAS"
"
  se puede hacer mas de una accion por turno
  las bases solo pueden crear una unica bola por turno
  una bolla pot actuar en el mateix torn en què s'ha creat
  una unica bola solo d aun paso por turno, pero puedes mover todas las bolas tuyas  
  una bola puede moverse y luego disparar a otra bola / base enemiga

  que la base cree una bola cuesta 50 de pintura 
  en tu turno:
    - sumas 2 de pintura
    - por cada laboratorio capturado sumas 1 de pintura
"

"EL AGENTE DEVUELVE UNA LISTA DE ACCIONES"
"
  mover una bola -> (mou (de pos) (a pos))
  hacer que una bola pinte a otra -> (pinta (esto))
  crar una bolla -> (crar-bolla color (on)) 
"

"LÓGICA DEL AGENTE"
"
  BASE
    si pintura >=50 -> creamos una bola de un color aleatorio
    si ve la base enemiga (y no hay ninguna bola ya encima) la crea lo mas cerca de la base posible (lo que deje el rango)
  
  BOLLA
    si hay <20 de pintura 
      si puedes le disparas a un laboratorio que no sea tuyo de antes
      si solo lo ves te mueves hacia el 
    si la bola ve la base enemiga y no esta pintada del color del que puede s¡disparar la bola
      si estan a distanci ade tiro disparan 
      si no se mueve hacia ella 
    si no ve la base se mueve siempre en una direccion, si llega al borde cambia de direccion 
"


"FUNCIÓN PRINCIPAL QUE LLAMA EL CONTROLADOR"
(defun agent-xyz999 (mi-unidad mi-visio pintura)
  
  (format t "-> Soy la unidad en la posicion ~A y veo ~A casillas~%" (car mi-unidad) (length mi-visio))

  ; Dejamos la acción falsa de momento para que el controlador no de error
  (list '(mou (10 10) (10 11)))
)