;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent ABC123.
;; <Descripció de les funcions d'aquest fitxer>

"AGENTE MENOS LISTO (e2 - blanco)"
"es igual que el agente listo pero tiene un 25% de probabilidad de elegir una accion aleatoria en vez de la correcta"

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
(defun agent-abc123 (info-unitat)
  (let* ( ; Extraemos los datos estrictamente en el orden que pide la UIB
          (ronda             (nth 0 info-unitat))
          (equip             (nth 1 info-unitat))
          (pintura           (nth 2 info-unitat))
          (id-unitat         (nth 3 info-unitat))
          (tipus-unitat      (nth 4 info-unitat))
          (coordenada        (nth 5 info-unitat))
          (colors-pintat     (nth 6 info-unitat))
          (color-propi       (nth 7 info-unitat))
          (tr-pintar         (nth 8 info-unitat))
          (tr-moure          (nth 9 info-unitat))
          (visio             (nth 10 info-unitat))
          (memoria           (nth 11 info-unitat))
        )

    (cond ( (eq tipus-unitat 'base)
            ; Reconstruimos "mi-unidad" como le gusta a tu función decidir-base
            (let ((mi-unidad (list coordenada 'base equip colors-pintat)))
              (decidir-base mi-unidad visio pintura) ) )
          ( t 
            ; Reconstruimos "mi-unidad" como le gusta a tu función decidir-bolla
            (let ((mi-unidad (list coordenada equip color-propi colors-pintat tr-moure tr-pintar)))
              (decidir-bolla mi-unidad visio) ) )
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

"DECIDE QUE ACCION VA A HACER LA BASE (CON 25% DE ERROR)"
(defun decidir-base (mi-unidad mi-visio pintura)
  (let* ( (pos-actual (car mi-unidad))
          (meu-equip (cond ((member 'e1 mi-unidad) 'e1) (t 'e2)))
          
          ; Tiramos el dado (devuelve 0, 1, 2 o 3)
          (dado-error (random 4))
        )
        
    (cond ( (>= pintura 50)  
            (cond ( (= dado-error 0) ; 25% de probabilidad de error (si sale 0)
                    ; Modo torpe: color aleatorio y posición aleatoria
                    (let* ((color-torpe (nth (random 3) '(r g b)))
                           (pos-torpe (posicio-aleatoria-voltant pos-actual mi-visio)))
                      (list (list 'crear-bolla color-torpe pos-actual pos-torpe))
                    )
                  )
                  ( t ; 75% de probabilidad de jugar bien
                    ; Modo listo: color necesario y apuntando al enemigo
                    (let* ((color-nuevo (color-menys-frequent meu-equip mi-visio))
                           (base-enemic (cercar-element-enemic 'base meu-equip nil mi-visio))
                           (pos-destino (cond ( base-enemic (aproxima-posicio pos-actual base-enemic) ) 
                                              ( t (posicio-aleatoria-voltant pos-actual mi-visio) ) )))
                      (list (list 'crear-bolla color-nuevo pos-actual pos-destino))
                    )
                  )
            )
          )
          ( t nil ) 
    )
  )
)

"CUENTA CUÁNTAS BOLAS DE UN EQUIPO Y COLOR HAY EN LA VISIÓN"
(defun comptar-bolles-color (equip color visio)
  (cond ( (null visio) 0 )
        ( t (let* ((casilla (car visio))
                   ; Recortamos la lista para quedarnos solo con la parte de la bola: (bolla equip color ...)
                   (info-bolla (member 'bolla casilla))) 
                   
              (cond ( (and info-bolla ; Si hay una bola
                           (eq (nth 1 info-bolla) equip) ; Si es de nuestro equipo
                           (eq (nth 2 info-bolla) color)) ; Si EL COLOR DE LA BOLA es el que buscamos
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

"DECIDE QUE ACCION VA A HACER LA BOLA (CON 25% DE ERROR)"
(defun decidir-bolla (mi-unidad mi-visio)
  (let* ( (pos-actual  (car mi-unidad))
          (meu-equip   (nth 1 mi-unidad))
          (color-bola  (nth 2 mi-unidad))
          (tr-moure    (nth 4 mi-unidad))
          (tr-pintar   (nth 5 mi-unidad))
          
          ; Tiramos el dado (devuelve 0, 1, 2 o 3)
          (dado-error (random 4))
        )

    (cond ( (= dado-error 0) ; 25% de probabilidad de error (si sale 0)
            ; Modo torpe: no pinta, solo se mueve a lo loco
            (let ((pos-boja (posicio-aleatoria-voltant pos-actual mi-visio)))
               (cond ( (and pos-boja (< tr-moure 1))
                       (list (list 'mou pos-actual pos-boja)) )
                     ( t nil )
               )
            )
          )
          ( t ; 75% de probabilidad de jugar bien
            ; Mantenemos tu lógica original perfecta
            (let* ( (candidat-pintar (buscar-objectiu-a-tiro pos-actual meu-equip color-bola mi-visio))
                    (objetivo-pintar (cond ( candidat-pintar candidat-pintar ) ( t nil ) ))
                    (candidat-moure (or (cercar-element-enemic 'base  meu-equip color-bola mi-visio)
                                        (cercar-element-enemic 'lab   meu-equip color-bola mi-visio)
                                        (cercar-element-enemic 'bolla meu-equip color-bola mi-visio)))
                    (objetivo-moure (cond ( (and candidat-moure
                                                 (objectiu-val-la-pena candidat-moure meu-equip color-bola mi-visio))
                                            candidat-moure )
                                          ( t nil ) ))
                  )
              (append (decidir-pintar pos-actual objetivo-pintar tr-pintar mi-visio)
                      (cond ( (and objetivo-moure (<= (dist-sq pos-actual objetivo-moure) bolla_pintar))
                              nil ) 
                            ( t 
                              (decidir-moure pos-actual objetivo-moure tr-moure mi-visio) )
                      )
              )
            )
          )
    )
  )
)

; Busca el mejor objetivo a tiro por orden de prioridad estratégica:
; base enemiga > lab enemigo > bola enemiga
; Solo devuelve objetivos que valgan la pena (no pintados ya de nuestro color)
(defun buscar-objectiu-a-tiro (pos-actual meu-equip color-bola visio)
  (let* ( (base  (cercar-element-enemic 'base  meu-equip color-bola visio))
          (lab   (cercar-element-enemic 'lab   meu-equip color-bola visio))
          (bolla (cercar-element-enemic 'bolla meu-equip color-bola visio)) )
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
; Comprueba si un objetivo en la visión vale la pena para esta bola
(defun objectiu-val-la-pena (pos-objetivo meu-equip color-bola mi-visio)
  (let* ( (casilla (trobar-casella-fila pos-objetivo mi-visio)) )
    (cond ( (null casilla) nil ) ; no la vemos
          ; Si es un lab: vale la pena SIEMPRE QUE NO SEA YA NUESTRO
          ( (member 'lab casilla) 
            (not (member meu-equip casilla)) )
          ; Si es bola/base: vale la pena SIEMPRE QUE NO TENGA YA NUESTRO COLOR
          ( t 
            (let ((colores-pintados (car (last casilla))))
               (cond ( (listp colores-pintados)
                       (not (member color-bola colores-pintados)) )
                     ( t t ) ) ; Si no hay lista de colores, vale la pena
            )
          )
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
; - si no hay objetivo visible, explorar: ir hacia la casella libre más lejana de la visión
; - si no hay ninguna casella explorable visible, no nos movemos (nil)

(defun decidir-moure (pos-actual objetivo tr-moure mi-visio)
  (cond (  (>= tr-moure 1) nil  ) ; aun en recuperacion
        (  t 
            (let* ( (x-actual (car pos-actual))
                    (y-actual (cadr pos-actual))
                  
                    ; si no hay objetivo, buscamos la casella libre más lejana para explorar
                    ; así la bola tiende a alejarse y cubrir terreno nuevo
                    (destino (cond (objetivo objetivo)
                                  (t (casella-exploracio pos-actual mi-visio))))

                    ; calculamos las tres opciones de movimiento hacia el destino
                    (pos-diag (cond (destino (aproxima-posicio pos-actual destino)) (t nil)))
                    (pos-x    (cond (destino (list (calcular-paso x-actual (car destino)) y-actual)) (t nil)))
                    (pos-y    (cond (destino (list x-actual (calcular-paso y-actual (cadr destino)))) (t nil)))
                  )

              (cond ( (and pos-diag (es-caminable pos-diag mi-visio))
                      (list (list 'mou pos-actual pos-diag)) )  ; opción preferida: acercarse en diagonal
                    ( (and pos-x (es-caminable pos-x mi-visio))
                      (list (list 'mou pos-actual pos-x)) )     ; segunda opción: solo x
                    ( (and pos-y (es-caminable pos-y mi-visio))
                      (list (list 'mou pos-actual pos-y)) )     ; tercera opción: solo y
                    
                    ( (es-caminable (list (+ x-actual 1) y-actual) mi-visio)
                      (list (list 'mou pos-actual (list (+ x-actual 1) y-actual))) )
                    ( (es-caminable (list (- x-actual 1) y-actual) mi-visio)
                      (list (list 'mou pos-actual (list (- x-actual 1) y-actual))) )
                    ( (es-caminable (list x-actual (+ y-actual 1)) mi-visio)
                      (list (list 'mou pos-actual (list x-actual (+ y-actual 1)))) )
                    ( (es-caminable (list x-actual (- y-actual 1)) mi-visio)
                      (list (list 'mou pos-actual (list x-actual (- y-actual 1)))) )
                    ( t nil )  ; si TODO está bloqueado, no nos movemos
              )

            )
        )
  )
)

"DEVUELVE LA CASELLA LIBRE MÁS LEJANA DE LA VISIÓN PARA EXPLORAR"
"Si no hay ninguna casella libre visible, devuelve nil"
"pos-actual: coordenada propia, visio: llista de caselles"
(defun casella-exploracio (pos-actual visio)
  (casella-mes-llunyana pos-actual (filtrar-posicions-lliures (llista-posicions visio) visio))
)

"EXTRAE LA LISTA DE COORDENADAS DE TODAS LAS CASELLES DE LA VISIÓN"
"visio: llista de caselles"
(defun llista-posicions (visio)
  (cond ( (null visio) nil )
        ( t (cons (car (car visio)) (llista-posicions (cdr visio))) )
  )
)

"DEVUELVE LA POSICIÓN MÁS LEJANA DE pos-actual EN UNA LISTA DE POSICIONES"
"Usa dist-sq para comparar. Si la lista está vacía, devuelve nil."
"pos-actual: coordenada, posicions: llista de coordenades"
(defun casella-mes-llunyana (pos-actual posicions)
  (cond ( (null posicions) nil )
        ( t (casella-mes-llunyana-aux pos-actual (cdr posicions) (car posicions) (dist-sq pos-actual (car posicions))) )
  )
)

"Auxiliar recursiva de casella-mes-llunyana"
"millor: la millor posició trobada fins ara, dist-millor: la seva distància"
(defun casella-mes-llunyana-aux (pos-actual posicions millor dist-millor)
  (cond ( (null posicions) millor )
        ( t (let* ( (d (dist-sq pos-actual (car posicions))) )
              (cond ( (> d dist-millor)
                      (casella-mes-llunyana-aux pos-actual (cdr posicions) (car posicions) d) )
                    ( t
                      (casella-mes-llunyana-aux pos-actual (cdr posicions) millor dist-millor) )
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
"Y QUE ADEMÁS VALGA LA PENA VISITAR (no pintado ya de nuestro color, o lab no capturado por nosotros)"
"element: símbol ('base 'bolla 'lab), meu-equip: símbol ('e1 'e2), color-bola: símbol ('r 'g 'b)"
"visio: llista de caselles"
"BUSCA UN ELEMENTO ESPECÍFICO EN LA VISIÓN QUE NO SEA DE MI EQUIPO"
"Y QUE ADEMÁS VALGA LA PENA VISITAR (no pintado ya de nuestro color, o lab no capturado por nosotros)"
(defun cercar-element-enemic (element meu-equip color-bola visio)
  (cond ( (null visio) nil )
        ( t
          (let* ( (casilla (car visio))
                  (pos (car casilla))
                  ; Condición base: que tenga el elemento y no sea nuestro equipo
                  (es-enemic (and (member element casilla) 
                                  (not (member meu-equip casilla))))
                  ; Para labs: solo vale si NO es ya nuestro
                  ; Para base/bolla: solo vale si NO tiene ya nuestro color pintado
                  (val-la-pena (cond
                    ( (not es-enemic) nil )
                    ( (equal element 'lab)
                      (not (member meu-equip casilla)) )
                    ( t
                      (let ((colores-pintados (car (last casilla))))
                         (cond ( (listp colores-pintados)
                                 (not (member color-bola colores-pintados)) )
                               ( t t ) )
                      )
                    )
                  ))
                )
            (cond ( val-la-pena pos )
                  ( t (cercar-element-enemic element meu-equip color-bola (cdr visio)) )
            )
          )
        )
  )
)

"BUSCA POR PRIORIDAD: 1. Base, 2. Lab, 3. Bolla"
"BUSCA POR PRIORIDAD (VERSIÓN REDUCIDA CON OR)"
"meu-equip: símbol, color-bola: símbol ('r 'g 'b), visio: llista de caselles"
(defun cercar-objetiu-enemic (meu-equip color-bola visio)
  (or (cercar-element-enemic 'base  meu-equip color-bola visio)
      (cercar-element-enemic 'lab   meu-equip color-bola visio)
      (cercar-element-enemic 'bolla meu-equip color-bola visio))
)