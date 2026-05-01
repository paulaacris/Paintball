;; =====================================================================
;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Paula Cristina Morro Negre, Matilde Navarro Robles.
;; Professor: Miquel Cabot.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent XYZ999.
;; =====================================================================

"AGENTE INTELIGENTE PRINCIPAL (e1 - negro)"

"====================================================================="
" PUNTO DE ENTRADA "
"====================================================================="

"FUNCIÓN PRINCIPAL QUE LLAMA EL CONTROLADOR"
(defun agent-xyz999 (info-unitat)
  (let* ( (ronda             (nth 0 info-unitat))
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

    ; a la funcion decidir-base le pasamos una lista con:
    ; ((x y) base equipo (colores pintada))
    ; a la funcion decidir-bolla le pasamos una lista con:
    ; ((x y) equipo color-propio (colores pintada))
    (cond ( (eq tipus-unitat 'base)
            (let ((mi-unidad (list coordenada 'base equip colors-pintat)))
              (decidir-base mi-unidad visio pintura) ) )
          ( t 
            (let ((mi-unidad (list coordenada equip color-propi colors-pintat tr-moure tr-pintar)))
              (decidir-bolla mi-unidad visio) ) )
    )
  ) 
)

"====================================================================="
" LOGICA BASE "
"====================================================================="

"SI TIENE >= 50 PINTURA, CREA UNA BOLA DEL COLOR QUE MENOS HAY"
(defun decidir-base (mi-unidad mi-visio pintura)
  (let* ( (pos-actual (car mi-unidad))
          ; sabemos de que equipo somos mirando nuestra propia casilla
          (meu-equip (cond ((member 'e1 mi-unidad) 'e1) (t 'e2)))

          ; elegir color que menos bolas tenga en la visión
          (color-nuevo (color-menys-frequent meu-equip mi-visio))

          ; ELEGIR POSICIÓN DE CREACIÓN
          (base-enemic (cercar-element-enemic 'base meu-equip nil mi-visio)) ; nil porque la base no tiene color propio 
          (pos-destino (cond ( base-enemic 
                               ; si vemos la base enemiga, creamos la bola apuntando hacia ella
                               (aproxima-posicio pos-actual base-enemic) ) 
                             ( t 
                               ; si no la vemos, en una casilla aleatoria alrededor de la base
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

;; =====================================================================
;; FUNCIONES AUXILIARES PARA LA LÓGICA DE LA BASE
;; =====================================================================

"DEVUELVE EL COLOR MENOS FRECUENTE DE TU EQUIPO EN LA VISIÓN"
(defun color-menys-frequent (equip visio)
  (let* ( (cr (comptar-bolles-color equip 'r visio))
          (cg (comptar-bolles-color equip 'g visio))
          (cb (comptar-bolles-color equip 'b visio))
          
          ; Encontramos cuál es el número más bajo
          (min-val (min cr cg cb))
          
          ; construimos una lista solo con los colores que tienen ese número mínimo
          ; si hay empate, meterá a los empatados 
          (candidatos (append (cond ((= cr min-val) '(r)) (t nil))
                              (cond ((= cg min-val) '(g)) (t nil))
                              (cond ((= cb min-val) '(b)) (t nil))))
        )
    ; elegimos uno al azar un de los colores que han empatado en el mínimo
    (nth (random (length candidatos)) candidatos)
  )
)

"CUENTA CUÁNTAS BOLAS DE UN EQUIPO Y COLOR HAY EN LA VISIÓN"
(defun comptar-bolles-color (equip color visio)
  (cond ( (null visio) 0 )
        ( t (let* ((casilla (car visio))
                   (info-bolla (member 'bolla casilla))) 
                   
              (cond ( (and info-bolla                     ; si hay una bola
                           (eq (nth 1 info-bolla) equip)  ; si es de nuestro equipo
                           (eq (nth 2 info-bolla) color)) ; si el color de la bola es el que buscamos
                      (+ 1 (comptar-bolles-color equip color (cdr visio))) ) ; sumamos 1 y seguimos mirando
                      
                    ( t (comptar-bolles-color equip color (cdr visio)) ) ; no sumamos nada y seguimos mirando
              )
            ) )
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

"====================================================================="
" LOGICA BOLA "
"====================================================================="

"EVALÚA OBJETIVOS Y DECIDE SI DISPARAR, MOVERSE, O EXPLORAR"
(defun decidir-bolla (mi-unidad mi-visio)
  (let* ( (pos-actual  (car mi-unidad))
          (meu-equip   (nth 1 mi-unidad))
          (color-bola  (nth 2 mi-unidad))
          (tr-moure    (nth 4 mi-unidad))
          (tr-pintar   (nth 5 mi-unidad))

          ; miramos si tenemos algo a tiro para pintar
          (candidat-pintar (buscar-objectiu-a-tiro pos-actual meu-equip color-bola mi-visio))

          (objetivo-pintar (cond ( candidat-pintar candidat-pintar )
                                 ( t nil ) ))

          ; miramos hacia donde deberiamos movernos
          (candidat-moure (or (cercar-element-enemic 'base  meu-equip color-bola mi-visio)
                              (cercar-element-enemic 'lab   meu-equip color-bola mi-visio)
                              (cercar-element-enemic 'bolla meu-equip color-bola mi-visio)))

          ; si hemos encontrado un objetivo (candidat-moure) miramos si vale la pena ir hacia el
          (objetivo-moure (cond ( (and candidat-moure
                                       (objectiu-val-la-pena candidat-moure meu-equip color-bola mi-visio))
                                  candidat-moure )
                                ( t nil ) ))
        )

    ; cada bola puede hacer dos acciones:
    (append ; 1. pintar
            (decidir-pintar pos-actual objetivo-pintar tr-pintar mi-visio)
            ; 2. moverse
            (cond ( (and objetivo-moure (<= (dist-sq pos-actual objetivo-moure) bolla_pintar))
                    nil ) ; si ya tenemos el objetivo a tiro no nos seguimos movienod hacia el 
                  ( t 
                    (decidir-moure pos-actual objetivo-moure tr-moure mi-visio) ) ; si no tenemos objetivo a tiro si que nos movemos
            )
    )
  )
)

;; =====================================================================
;; DECISIÓN DE PINTAR
;; =====================================================================

"DECIDE SI LA BOLA PUEDE Y TIENE QUE PINTAR"
(defun decidir-pintar (pos-actual objetivo tr-pintar mi-visio)
  (cond (  (>= tr-pintar 1) nil  ) ; aun en recuperación, no dispara
        (  (null objetivo) nil  ) ; no vemos ningun objetivo, no dispara
        (  (> (dist-sq pos-actual objetivo) bolla_pintar) nil  ) ; el objetivo esta fuera del rango de disparo, no dispara
        ; se puede disparar
        (  t (list (list 'pinta pos-actual objetivo)))
  )
)

"BUSCA EL MEJOR OBJETIVO A TIRO POR ORDEN DE PRIORIDAD"
(defun buscar-objectiu-a-tiro (pos-actual meu-equip color-bola visio)
  (let* ( (base (cercar-element-enemic 'base  meu-equip color-bola visio))
          (lab (cercar-element-enemic 'lab   meu-equip color-bola visio))
          (bolla (cercar-element-enemic 'bolla meu-equip color-bola visio)) )
    
    ; prioridad 1: base
    (cond ( (and base                                                     ; si hay una base dentro de la vision 
                 (<= (dist-sq pos-actual base) bolla_pintar)              ; esta a tiro
                 (objectiu-val-la-pena base meu-equip color-bola visio))  ; y vale la pena
            base )                                                        ; el objetivo es la base
    ; prioridad 1: lab
          ( (and lab                                                      ; si hay un lab dentro de la vision 
                 (<= (dist-sq pos-actual lab) bolla_pintar)               ; esta a tiro
                 (objectiu-val-la-pena lab meu-equip color-bola visio))   ; y vale la pena
            lab )                                                         ; el objetivo es el lab
    ; prioridad 1: bolla
          ( (and bolla                                                    ; si hay una bola dentro de la vision 
                 (<= (dist-sq pos-actual bolla) bolla_pintar)             ; esta a tiro
                 (objectiu-val-la-pena bolla meu-equip color-bola visio)) ; y vale la pena
            bolla )                                                       ; el objetivo es la bola
          ( t nil )
    )
  )
)

"DECIDE SI UN OBJETIVO VALE LA PENA"
(defun objectiu-val-la-pena (pos-objetivo meu-equip color-bola mi-visio)
  (let* ( (casilla (trobar-casella-fila pos-objetivo mi-visio)) )
    
    (cond ( (null casilla) nil ) ; no la vemos
          ; si es un lab vale la pena si no es nuestro ya
          ( (member 'lab casilla) 
            (not (member meu-equip casilla)) )
          ; si es bola/base vale la pena si no tiene nuestro color ya
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

;; =====================================================================
;; DECISIÓN DE MOVERSE
;; =====================================================================

"DECIDE SI PUEDE Y HACIA DONDE TIENE QUE MOVERSE LA BOLA"
(defun decidir-moure (pos-actual objetivo tr-moure mi-visio)
  (cond (  (>= tr-moure 1) nil  ) ; aun en recuperacion, no se mueve
        (  t 
            (let* ( (x-actual (car pos-actual))
                    (y-actual (cadr pos-actual))
                  
                    ; si no hay objetivo, buscamos la casella libre más lejana para explorar (asi la bola tiende a alejarse y cubrir terreno nuevo)
                    (destino (cond (objetivo objetivo)
                                  (t (casella-exploracio pos-actual mi-visio))))

                    ; calculamos las tres opciones de movimiento hacia el destino
                    (pos-diag (cond (destino (aproxima-posicio pos-actual destino)) (t nil)))
                    (pos-x (cond (destino (list (calcular-paso x-actual (car destino)) y-actual)) (t nil)))
                    (pos-y (cond (destino (list x-actual (calcular-paso y-actual (cadr destino)))) (t nil)))
                  )

              (cond ( (and pos-diag (es-caminable pos-diag mi-visio))
                      (list (list 'mou pos-actual pos-diag)) )  ; acercarse en diagonal
                    ( (and pos-x (es-caminable pos-x mi-visio))
                      (list (list 'mou pos-actual pos-x)) )     ; mover solo x
                    ( (and pos-y (es-caminable pos-y mi-visio))
                      (list (list 'mou pos-actual pos-y)) )     ; mover solo y
                    ; si ninguna de las anteriores se puede nos intentamos mover en cualquier dirección
                    ( (es-caminable (list (+ x-actual 1) y-actual) mi-visio)
                      (list (list 'mou pos-actual (list (+ x-actual 1) y-actual))) )
                    ( (es-caminable (list (- x-actual 1) y-actual) mi-visio)
                      (list (list 'mou pos-actual (list (- x-actual 1) y-actual))) )
                    ( (es-caminable (list x-actual (+ y-actual 1)) mi-visio)
                      (list (list 'mou pos-actual (list x-actual (+ y-actual 1)))) )
                    ( (es-caminable (list x-actual (- y-actual 1)) mi-visio)
                      (list (list 'mou pos-actual (list x-actual (- y-actual 1)))) )
                    ( t nil )  ; si todo está bloqueado, no nos movemos
              )

            )
        )
  )
)

"DEVUELVE LA CASELLA LIBRE MÁS LEJANA DE LA VISIÓN PARA EXPLORAR"
(defun casella-exploracio (pos-actual visio)
  (casella-mes-llunyana pos-actual (filtrar-posicions-lliures (llista-posicions visio) visio))
)

"DEVUELVE LA POSICIÓN MÁS LEJANA DE pos-actual EN UNA LISTA DE POSICIONES"
(defun casella-mes-llunyana (pos-actual posicions)
  (cond ( (null posicions) nil )
        ( t (casella-mes-llunyana-aux pos-actual (cdr posicions) (car posicions) (dist-sq pos-actual (car posicions))) )
  )
)

"AUXILIAR RECURSIVA DE  casella-mes-llunyana"
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

"EXTRAE LA LISTA DE COORDENADAS DE TODAS LAS CASELLES DE LA VISIÓN"
(defun llista-posicions (visio)
  (cond ( (null visio) nil )
        ( t (cons (car (car visio)) (llista-posicions (cdr visio))) )
  )
)

"====================================================================="
" AUXILIARES PARA LA LÓGICA DE LA BOLA Y LA BASE"
"====================================================================="

"FILTRA DE UNA LISTA DE POSICIONES LAS QUE SON CAMINABLES"
(defun filtrar-posicions-lliures (posicions visio)
  (cond ( (null posicions) nil )
        ( (es-caminable (car posicions) visio)
          (cons (car posicions) (filtrar-posicions-lliures (cdr posicions) visio)) )
        ( t (filtrar-posicions-lliures (cdr posicions) visio) )
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

"CALCULA LA SIGUIENTE COORDENADA A LA QUE TIENES QUE MOVERTE PARA ACERCARTE AL OBJETIVO"
(defun calcular-paso (actual objetivo)
  ; la bola solo se mueve, como máximo, una casilla en direccion x y una casilla en direccion y
  (cond ( (= actual objetivo) actual )        ; si ya estamos en el sitio no nos movemos 
        ( (> actual objetivo) (- actual 1) )  ; si xa > xo nos movemos uno a la izquierda (restamos)
        ( t (+ actual 1) )                    ; si xa < xo nos movemos uno a la derecha (sumamos)
  )
)

"COMPRUEBA SI UNA CASILLA ES CAMINABLE"
(defun es-caminable (pos visio)
  (let ((casilla (trobar-casella-fila pos visio)))
    (cond ( (null casilla) nil )          ; si no la vemos no vamos
          ( (member 'aigua casilla) nil ) ; si es agua, no podemos
          ( (member 'bolla casilla) nil ) ; si ya hay otra bola, no podemos
          ( (member 'base casilla) nil )  ; si es base, no podemos
          ( (member 'lab casilla) nil )   ; si es lab, no podemos
          ( t t )                         ; en cualquier otro caso se puede pasar
    )
  )
)

"BUSCA POR PRIORIDAD: 1. Base, 2. Lab, 3. Bolla"
(defun cercar-objetiu-enemic (meu-equip color-bola visio)
  (or (cercar-element-enemic 'base  meu-equip color-bola visio)
      (cercar-element-enemic 'lab   meu-equip color-bola visio)
      (cercar-element-enemic 'bolla meu-equip color-bola visio))
)

"BUSCA UN ELEMENTO ESPECÍFICO EN LA VISIÓN QUE NO SEA DE MI EQUIPO"
"Y QUE ADEMÁS VALGA LA PENA VISITAR (no pintado ya de nuestro color, o lab no capturado por nosotros)"
(defun cercar-element-enemic (element meu-equip color-bola visio)
  (cond ( (null visio) nil )
        ( t
          (let* ( (casilla (car visio))
                  (pos (car casilla))
                  ; que tenga el elemento y no sea nuestro equipo
                  (es-enemic (and (member element casilla) 
                                  (not (member meu-equip casilla))))
                  ; Para labs: solo vale si NO es ya nuestro
                  ; Para base/bolla: solo vale si NO tiene ya nuestro color pintado
                  (val-la-pena (cond ( (not es-enemic) nil ) ; si no es enemigo no vale la pena
                                     ( (equal element 'lab) ; si es un lab
                                       (not (member meu-equip casilla)) ) ; solo vale la pena si no es nuestro ya
                                     ( t
                                       (let ((colores-pintados (car (last casilla)))) ; si es base/bola
                                            (cond ( (listp colores-pintados)
                                                     (not (member color-bola colores-pintados)) ) ; solo vale la pena si no tiene nuestro color ya 
                                                  ( t t ) )
                                       ) )
                  ))
                )
            (cond ( val-la-pena pos ) ; si vale la pena lo devolvemos 
                  ( t (cercar-element-enemic element meu-equip color-bola (cdr visio)) ) ; si no miramos el siguiente 
            )
          ) )
  )
)