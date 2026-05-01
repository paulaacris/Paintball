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
            ;(list '(crear-bolla r (1 4) (2 4))))
            (decidir-base mi-unidad mi-visio pintura) ) ; Si es base
          ( t 
            (decidir-bolla mi-unidad mi-visio) ) ; Si es bola
    )
  ) 
)

"====================================================================="
" LOGICA BASE"
"====================================================================="

"
- la base crea una bola si tiene >= 50 de pintura
- crea la bola del color que menos tenga en el tablero
- si en su vision está la base enemiga, crea la bola en esa direccion, si no la crea en una posición aleatoria dentro del rango
"

"DECIDE QUE ACCION VA A HACER LA BASE"
"DECIDE QUE ACCION VA A HACER LA BASE"
(defun decidir-base (mi-unidad mi-visio pintura)
  (let* ( (pos-actual (car mi-unidad))
          ; Averiguamos de qué equipo somos mirando nuestra propia casilla
          (meu-equip (cond ((member 'e1 mi-unidad) 'e1) (t 'e2)))

          ; 1. ELEGIR COLOR: El que menos bolas tenga en la visión
          (color-nuevo (color-menys-frequent meu-equip mi-visio))

          ; 2. ELEGIR POSICIÓN DE CREACIÓN
          (base-enemic (cercar-element-enemic 'base meu-equip mi-visio))
          (pos-destino (cond ( base-enemic 
                               ; Si vemos la base enemiga, creamos la bola apuntando hacia ella
                               (aproxima-posicio pos-actual base-enemic) ) 
                             ( t 
                               ; Si no la vemos, en una casilla aleatoria alrededor de la base
                               (posicio-aleatoria-voltant pos-actual mi-visio) )    
                       ))
        )
        
    ; Si tenemos >= 50 de pintura creamos la bola
    (cond ( (>= pintura 50)  
            (list (list 'crear-bolla color-nuevo pos-actual pos-destino)) )
          ( t nil ) 
    )
  )
)

"CUENTA CUÁNTAS BOLAS DE UN EQUIPO Y COLOR HAY EN LA VISIÓN"
(defun comptar-bolles-color (equip color visio)
  (cond ( (null visio) 0 )
        ( t (let* ((casilla (car visio)))
              (cond ( (and (member 'bolla casilla)
                           (member equip casilla)
                           (member color casilla))
                      (+ 1 (comptar-bolles-color equip color (cdr visio))) )
                    ( t (comptar-bolles-color equip color (cdr visio)) )
              )
            )
        )
  )
)

"DEVUELVE EL COLOR MENOS FRECUENTE DE TU EQUIPO EN LA VISIÓN"
(defun color-menys-frequent (equip visio)
  (let* ( (cr (comptar-bolles-color equip 'r visio))
          (cg (comptar-bolles-color equip 'g visio))
          (cb (comptar-bolles-color equip 'b visio))
          
          ; Encontramos cuál es el número más bajo
          (min-val (min cr cg cb))
          
          ; Construimos una lista solo con los colores que tienen ese número mínimo
          ; Si hay empate, meterá a los empatados (ej: (r g))
          (candidatos (append (cond ((= cr min-val) '(r)) (t nil))
                              (cond ((= cg min-val) '(g)) (t nil))
                              (cond ((= cb min-val) '(b)) (t nil))))
        )
    ; Elegimos uno al azar de los que han empatado en el mínimo
    (nth (random (length candidatos)) candidatos)
  )
)

"DEVUELVE UNA POSICIÓN ALEATORIA A 1 CASILLA DE DISTANCIA"
(defun posicio-aleatoria-voltant (pos visio)
  (let* ( (opcions (list
                    (list (+ (car pos) 1) (cadr pos))
                    (list (- (car pos) 1) (cadr pos))
                    (list (car pos) (+ (cadr pos) 1))
                    (list (car pos) (- (cadr pos) 1))
                    (list (+ (car pos) 1) (+ (cadr pos) 1))
                    (list (- (car pos) 1) (- (cadr pos) 1))
                    (list (+ (car pos) 1) (- (cadr pos) 1))
                    (list (- (car pos) 1) (+ (cadr pos) 1))))
         ; solo las casillas que podamos pisar (sin bola, sin agua, sin base)
         (lliures (filtrar-posicions-lliures opcions visio))
       )
    (cond ( (null lliures) nil )  ; si todas están ocupadas no creamos bola
          ( t (nth (random (length lliures)) lliures) )
    )
  )
)

; filtra de una lista de posiciones las que son caminables
(defun filtrar-posicions-lliures (posicions visio)
  (cond ( (null posicions) nil )
        ( (es-caminable (car posicions) visio)
          (cons (car posicions) (filtrar-posicions-lliures (cdr posicions) visio)) )
        ( t (filtrar-posicions-lliures (cdr posicions) visio) )
  )
)

"====================================================================="
" LOGICA BOLA"
"====================================================================="

"DECIDE QUE ACCION VA A HACER LA BOLA"
(defun decidir-bolla (mi-unidad mi-visio)
  (let* ( (pos-actual  (car mi-unidad))
          (meu-equip   (nth 1 mi-unidad))
          (color-bola  (nth 2 mi-unidad))
          (tr-moure    (nth 4 mi-unidad))
          (tr-pintar   (nth 5 mi-unidad))

          ; buscamos objetivo para pintar: bola enemiga > base enemiga > lab enemigo
          ; pero solo si realmente vale la pena disparar ahí
          ; Para PINTAR: base enemiga a tiro > lab enemigo a tiro > bola enemiga a tiro
          ; Buscamos por prioridad estratégica, pero solo si está dentro del rango de disparo
          (candidat-pintar (buscar-objectiu-a-tiro pos-actual meu-equip color-bola mi-visio))

          (objetivo-pintar (cond ( candidat-pintar candidat-pintar )
                       ( t nil ) ))

          ; buscamos objetivo para moverse: base > lab > bola
          ; también filtramos los que ya no valen
          (candidat-moure (or (cercar-element-enemic 'base  meu-equip mi-visio)
                              (cercar-element-enemic 'lab   meu-equip mi-visio)
                              (cercar-element-enemic 'bolla meu-equip mi-visio)))

          (objetivo-moure (cond ( (and candidat-moure
                                       (objectiu-val-la-pena candidat-moure meu-equip color-bola mi-visio))
                                  candidat-moure )
                                ( t nil ) ))

          ; si el objetivo de movimiento ya está a tiro, no nos acercamos más
          ; (ya lo tenemos a rango, no tiene sentido seguir avanzando)
          (objetivo-moure-final (cond ( (and objetivo-moure
                                             (<= (dist-sq pos-actual objetivo-moure) bolla_pintar))
                                        nil )  ; está a tiro -> no nos movemos hacia él
                                      ( t objetivo-moure ) )) ; no está a tiro -> nos acercamos
        )

    (append (decidir-pintar pos-actual objetivo-pintar      tr-pintar mi-visio)
            (decidir-moure  pos-actual objetivo-moure-final tr-moure  mi-visio))
  )
)

; Busca el mejor objetivo a tiro por orden de prioridad estratégica:
; base enemiga > lab enemigo > bola enemiga
; Solo devuelve objetivos que valgan la pena (no pintados ya de nuestro color)
(defun buscar-objectiu-a-tiro (pos-actual meu-equip color-bola visio)
  (let* ( (base  (cercar-element-enemic 'base  meu-equip visio))
          (lab   (cercar-element-enemic 'lab   meu-equip visio))
          (bolla (cercar-element-enemic 'bolla meu-equip visio)) )
    (cond ( (and base
                 (<= (dist-sq pos-actual base) bolla_pintar)
                 (objectiu-val-la-pena base meu-equip color-bola visio))
            base )
          ( (and lab
                 (<= (dist-sq pos-actual lab) bolla_pintar)
                 (objectiu-val-la-pena lab meu-equip color-bola visio))
            lab )
          ( (and bolla
                 (<= (dist-sq pos-actual bolla) bolla_pintar)
                 (objectiu-val-la-pena bolla meu-equip color-bola visio))
            bolla )
          ( t nil )
    )
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

; Comprueba si un objetivo en la visión vale la pena para esta bola
; (no disparamos a algo que ya tiene nuestro color, ni a un lab que ya es nuestro)
(defun objectiu-val-la-pena (pos-objetivo meu-equip color-bola mi-visio)
  (let* ( (casilla (trobar-casella-fila pos-objetivo mi-visio)) )
    (cond ( (null casilla) nil ) ; no la vemos
          ; si es un lab y ya es nuestro, no vale la pena
          ( (and (member 'lab casilla) (member meu-equip casilla)) nil )
          ; si es una bola/base y ya tiene nuestro color, no vale la pena
          ( (and (member color-bola casilla) 
                 (not (member 'lab casilla))) nil )
          ( t t ) ; en cualquier otro caso sí vale la pena
    )
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
                    
                    ( (es-caminable (list (+ x-actual 1) y-actual) mi-visio)
                      (list (list 'mou pos-actual (list (+ x-actual 1) y-actual))) )
                    ( (es-caminable (list (- x-actual 1) y-actual) mi-visio)
                      (list (list 'mou pos-actual (list (- x-actual 1) y-actual))) )
                    ( (es-caminable (list x-actual (+ y-actual 1)) mi-visio)
                      (list (list 'mou pos-actual (list x-actual (+ y-actual 1)))) )
                    ( (es-caminable (list x-actual (- y-actual 1)) mi-visio)
                      (list (list 'mou pos-actual (list x-actual (- y-actual 1)))) )
                    ( t nil )  ; si TODO está bloqueado, no nos movemos (mejor nil que un movimiento ilegal)
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