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
  hacer que una bola pinte a otra -> (pinta (de esto) (a esto))
  crar una bolla -> (crar-bolla color (desde) (on)) 
"

"LÓGICA DEL AGENTE"
"
  BASE
    si pintura >=50 -> creamos una bola del color que menos tengamos, si todas iguales aleatorio
    si ve la base enemiga (y no hay ninguna bola ya encima) la crea lo mas cerca de la base posible (lo que deje el rango)
  
  BOLLA
    si ve  aotra bola y esta a tiro y no esta pintada del color del que puede disparar la bola le dispara 
    si la bola ve la base enemiga y no esta pintada del color del que puede disparar la bola
      si estan a distanci de tiro disparan 
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
            ;nil)
            (list '(crear-bolla r (1 4) (2 4))))
            ;(decidir-base mi-unidad mi-visio pintura) ) ; Si es base
          ( t 
            (decidir-bolla mi-unidad mi-visio) ) ; Si es bola
    )
  ) 
)

"====================================================================="
" LOGICA BASE"
"====================================================================="

"DECIDE QUE ACCION VA A HACER LA BASE"
(defun decidir-base (mi-unidad mi-visio pintura)
  ; si tenemos >= 50 de pintura creamos una bola
  (let* ( (pos-actual (car mi-unidad)) ) ; coordenadas (x y) de la base
    (cond (  (>= pintura 50)  
          ; creamos una bola roja justo en la posicion de la base
          (list (list 'crear-bolla 'r pos-actual)) )
          (  t nil  ) ; si no hay suficiente pintura no hacemos nada
    )
  )
)

"====================================================================="
" LOGICA BOLA"
"====================================================================="

"DECIDE QUE ACCION VA A HACER LA BOLA"
(defun decidir-bolla (mi-unidad mi-visio)
  (let* ( (pos-actual (car mi-unidad))
          (x-actual (car pos-actual))
          (y-actual (car (cdr pos-actual)))

          (meu-equip (nth 1 mi-unidad))

          (tr-moure (nth 4 mi-unidad))
          (tr-pintar (nth 5 mi-unidad))

          ; seleccionar objetivo 
          (objetivo (cercar-objetiu-enemic meu-equip mi-visio))
        ) 

    (append (decidir-pintar pos-actual objetivo tr-pintar mi-visio)
            (decidir-moure pos-actual objetivo tr-moure mi-visio))
  )
)

"====================================================================="
" DECISIÓN DE PINTAR"
"====================================================================="

; decide si la bola puede y tiene que disparar 
; condiciones para pintar:
; 1: tr-pintar < 1
; 2: hay un objetivo visible 
; 3: el objetivo esta a tiro

(defun decidir-pintar (pos-actual objetivo tr-pintar mi-visio)
  (cond (  (>= tr-pintar 1) nil  ) ; aun en recuperación, no puede disparar
        (  (null objetivo) nil  ) ; no vemos ningun objetivo
        (  (> (dist-sq pos-actual objetivo) bolla_pintar) nil  ) ; el objetivo esta fuera del rango de disparo
        ; se puede disparar
        (  t (list (list 'pinta pos-actual objetivo)))
  )
)

"====================================================================="
" DECISIÓN DE MOVERSE"
"====================================================================="

; DECIDE HACIA DONDE MOVERSE 
; CONDICIONES PARA MODERSE:
; 1: tr-moure < 1
; ESTRATEGIA DE MOVIMIENTO:
; - si hay un objetivo nos acercamos a el (primero diagonal, luego solo x, luego solo y)
; - si no hya objetivo vamos al centro del mapa (10 10)
; - si ninguna de las tres posiciones es caminable, intentamos movernos +1 en x 

(defun decidir-moure (pos-actual objetivo tr-moure mi-visio)
  (cond (  (>= tr-moure 1) nil  ) ; aun en recuperacion
        (  t 
            (let* ( (x-actual (car pos-actual))
                    (y-actual (car (cdr pos-actual)))
                  
              
                    ; si no hay objetivo enemigo visible nos movemos al centro del mapa
                    (destino (cond (objetivo objetivo)
                                  (t '(10 10) )))

                    ; calculamos las tres opciones de movimiento hacia el destino
                    (pos-diag (aproxima-posicio pos-actual destino))                ; diagonal (mueve x e y a la vez)
                    (pos-x (list (calcular-paso x-actual (car destino)) y-actual))  ; solo mueve x
                    (pos-y (list x-actual (calcular-paso y-actual (cadr destino)))) ; solo mueve y
                    (pos-fall (list (+ x-actual 1) y-actual))                       ; fallback: +1 en x
                  )

              (cond ( (es-caminable pos-diag mi-visio)
                      (list (list 'mou pos-actual pos-diag)) )  ; opción preferida: acercarse en diagonal
                    ( (es-caminable pos-x mi-visio)
                      (list (list 'mou pos-actual pos-x)) )     ; segunda opción: solo x
                    ( (es-caminable pos-y mi-visio)
                      (list (list 'mou pos-actual pos-y)) )     ; tercera opción: solo y
                    ( t
                      (list (list 'mou pos-actual pos-fall)) )  ; último recurso
              )

            )
        )
  )
)


"====================================================================="
" AUXILIARES"
"====================================================================="

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

"COMPRUEBA SI UNA CASILLA ES CAMINABLE"
(defun es-caminable (pos visio)
  (let ((casilla (trobar-casella-fila pos visio)))
    (cond ( (null casilla) nil ) ; Si no la vemos no vamos
          ( (member 'aigua casilla) nil ) ; Si es agua, no podemos
          ( (member 'bolla casilla) nil ) ; Si ya hay otra bola, no podemos
          ( (member 'base casilla) nil ) ; Si es base, no podemos
          ( (member 'lab casilla) nil ) ; Si es lab, no podemos
          ( t t ) ; En cualquier otro caso se puede pasar
    )
  )
)

"BUSCA UN ELEMENTO ESPECÍFICO EN LA VISIÓN QUE NO SEA DE MI EQUIPO"
(defun cercar-element-enemic (element meu-equip visio)
  (cond ( (null visio) nil )
        ( t
          (let* ( (casilla (car visio))
                  (pos (car casilla))
                  ; Comprobamos si está el elemento (base, lab o bolla) 
                  ; y que NO esté la etiqueta de nuestro equipo en la casilla
                  (es-objectiu (and (member element casilla) 
                                    (not (member meu-equip casilla))))
                )
            (cond ( es-objectiu pos ) ; Si lo encuentra, devuelve la posición
                  ( t (cercar-element-enemic element meu-equip (cdr visio)) ) ; Si no, sigue mirando
            )
          )
        )
  )
)

"BUSCA POR PRIORIDAD: 1. Base, 2. Lab, 3. Bolla"
"BUSCA POR PRIORIDAD (VERSIÓN REDUCIDA CON OR)"
(defun cercar-objetiu-enemic (meu-equip visio)
  (or (cercar-element-enemic 'base meu-equip visio)   ; Busca base primero
      (cercar-element-enemic 'lab meu-equip visio)    ; Si falla, busca lab
      (cercar-element-enemic 'bolla meu-equip visio)) ; Si falla, busca bola
)