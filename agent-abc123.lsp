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
    si pintura >50 -> creamos una bola del color que menos tengamos, si todas iguales aleatorio
    si ve la base enemiga (y no hay ninguna bola ya encima) la crea lo mas cerca de la base posible (lo que deje el rango)
  
  BOLLA
    si la bola ve la base enemiga y no esta pintada del color del que puede s¡disparar la bola
      si estan a distanci ade tiro disparan 
      si no se mueve hacia ella 
    si hay <20 de pintura 
      si puedes le disparas a un laboratorio que no sea tuyo de antes
      si solo lo ves te mueves hacia el 
    si no ve la base se mueve siempre en una direccion, si llega al borde cambia de direccion 
"

"FUNCIÓN PRINCIPAL QUE LLAMA EL CONTROLADOR"
(defun agent-xyz999 (mi-unidad mi-visio pintura)
  (let* ( (es-base (member 'base mi-unidad)) )
    (cond ( es-base 
            nil ) ; Si es base, de momento no hace nada (evita el error)
          ( t 
            (decidir-bolla mi-unidad mi-visio pintura) ) ; Si es bola, se mueve
    )
  ) 
)

"====================================================================="
" LOGICA BASE"
"====================================================================="

"DECIDE QUE ACCION VA A HACER LA BASE"
"(defun decidir-base (pos-actual mi-visio pintura)
  ; si tenemos >= 50 de pintura creamos una bola
  (cond (  (> pintura 50) 
            pos cambiar
            que el color vaya cambiando
           (list (list 'crear-bolla 'r pos))  ) ; devolvemos la accion de crear
        (  t nil  ) ; si no hay pintura no hacemos nada 
  )
)
"
"====================================================================="
" LOGICA BOLA"
"====================================================================="

"DECIDE QUE ACCION VA A HACER LA BOLA"
(defun decidir-bolla (mi-unidad mi-visio pintura)
  (let* ( (pos-actual (car mi-unidad))
          (x-actual (car pos-actual))
          (y-actual (car (cdr pos-actual)))
          (objetivo '(0 0)) 

          ; TRES OPCIONES DE MOVIMIENTO
          ; movemos x y 
          (pos-diag (aproxima-posicio pos-actual objetivo))
          ; movemos solo x
          (pos-x (list (calcular-paso x-actual (car objetivo)) y-actual))
          ; movemos solo y
          (pos-y (list x-actual (calcular-paso y-actual (car (cdr objetivo)))))
        )
    (cond ; si la posicion a la que nos queriamos mover es caminable 
          (  (es-caminable pos-diag mi-visio)
             (list (list 'mou pos-actual pos-diag))  )
          ; si la posicion anterior no es caminable intentamos mover solo la x 
          (  (es-caminable pos-x mi-visio)
             (list (list 'mou pos-actual pos-x))  )
          ; si la posicion anterior no es caminable intentamos mover solo la y 
          (  (es-caminable pos-y mi-visio)
             (list (list 'mou pos-actual pos-y))  )
    )
  )
)

"CALCULA LA SIGUIENTE COORDENADA A LA QUE TIENES QUE MOVERTE PARA ACERCARTE AL OBJETIVO"
; la bola solo se mueve, como máximo, una casilla en direccion x y una casilla en direccion y
(defun calcular-paso (actual objetivo)
  (cond ; si ya estamos en el sitio no nos movemos 
        ( (= actual objetivo) actual )
        ; si xa > xo nos movemos uno a la izquierda (restamos)
        ( (> actual objetivo) (- actual 1) )
        ; si xa < xo nos movemos uno a la derecha (sumamos)
        ( t (+ actual 1) )
  )
)

"GENERA LA NUEVA POSICIÓN (X Y) HACIA UN OBJETIVO"
(defun aproxima-posicio (p-act p-obj)
  (let* ( (x-act (car p-act))
          (y-act (cadr p-act))
          (x-obj (car p-obj))
          (y-obj (cadr p-obj))
        )
    (list (calcular-paso x-act x-obj)
          (calcular-paso y-act y-obj))
  )
)

"COMPRUEBA SI UNA CASILLA ES CAMINABLE (No es agua)"
(defun es-caminable (pos visio)
  (let ((casilla (trobar-casella-fila pos visio)))
    (cond ( (null casilla) nil ) ; Si no la vemos no vamos
          ( (member 'aigua casilla) nil ) ; Si es agua, no podemos
          ( (member 'bolla casilla) nil ) ; Si ya hay otra bola, no podemos
          ( t t ) ; En cualquier otro caso (tierra o base), se puede pasar
    )
  )
)