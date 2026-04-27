;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del controlador principal.
;; <Descripció de les funcions d'aquest fitxer>

;; Necessari per a l'optimització de crides recursives.
; (load 'common) ; https://almy.us/files/xl305req.zip
; (load 'tco)    ; https://github.com/antoni-oliver/defun-tco

(setq nom_mapa "paintball/maps/bait.map")
(setq bolla_moviment 2)
(setq base_creacio 2)
(setq bolla_pintar 5)
(setq bolla_visio 20)
(setq base_visio 64)

; ESTADO
; (mapa unitats pintura-e1 pintura-e2 turno)
    ; unitats: todas las bolas activas del juego ((x y) equipo color (colores pintada))
    ; pintura: numero del 0 all 200
    ; torn: e1 / e2

"COSAS A CAMBIAR"
"MIRAR DE CAMBIAR TODOS LOS NTH POR ¿CDR?"

" CARCAR ARCHIVOS "
(load 'paintball/grafics)
(load 'paintball/agent-abc123)

"====================================================================="
" SETUP "
"====================================================================="

"LEE EL ARCHIVO .MAP Y DEVUELVE EL CONTENIDO"
(defun carregar-dades-mapa (nom-fitxer)
  (let* ((f (open nom-fitxer))  ; abrimos el fichero 
         (mapa (read f)))       ; leemos el fichero 
        (close f)               ; cerranis el fichero
    mapa)                       ; devolvemos lo que hemos leido del fichero 
)

"FUNCIÓN PARA PONER COORDENADAS EN CADA CASILLA DE UNA FILA 
Y, SI ES UNA BASE, LE AÑADE UNA LISTA VACÍA AL FINAL PARA FUTUROS COLORES PINTADA"
(defun preparar-mapa-fila (fila x y)
  (cond (  (null fila) nil  ) ; si no hay más casillas, fin
        ; si la casilla es una base 
        (  (member 'base (car fila)) 
           ; juntamos la lista de coordenadas, contenido original del mapa y la lista vacia 
           (cons (append (list (list x y)) (car fila) (list '())) 
                 ; añadimos el resto de casillas 
                 (preparar-mapa-fila (cdr fila) (+ x 1) y))  )
        ; si no es una base
        ; juntamos la lista de coordenadas y el contenido original del mapa con el resto de casillas
        (  t (cons (append (list (list x y)) (car fila)) (preparar-mapa-fila (cdr fila) (+ x 1) y))  ) 
  )
)

"FUNCIÓN QUE RECORRE EL MAPA Y LLAMA A preparar-mapa-fila PARA CADA UNA"
(defun preparar-mapa (mapa &optional (y 0))
  (cond (  (null mapa) nil  ) ; Si no hay más filas, fin
        (  t (cons (preparar-mapa-fila (car mapa) 0 y) ; Procesamos la fila actual
             (preparar-mapa (cdr mapa) (+ y 1)))) ; Siguiente fila (y + 1)
  )
)

"====================================================================="
" FUNCIONES QUE LUEGO HAY QUE BORRAR "
"====================================================================="

;; Función para imprimir el mapa de forma legible en la consola 
;;(para comprobar que las coordenadas en el mapa se insertan bien)
(defun imprimir-mapa (mapa)
  (cond (  (null mapa) (format t "--- Fin del mapa ---~%")  ) ; Si no hay más filas, termina
    (  t 
      (format t "~A~%" (car mapa))  ; Imprime la fila actual y un salto de línea
      (imprimir-mapa (cdr mapa))  )   ; Llama a la siguiente fila
  )
)

"====================================================================="
" BUCLE PRINCIPAL DEL JUEGO "
"====================================================================="

"PUNTO DE ENTRADA, LO QUE ESCRIBES EN LA CONSOLA"
(defun inici ()

    (let* ( ; MAPA
            ; cargamso el mapa
            (mapa-fitxer (carregar-dades-mapa nom_mapa))
            ; añadimos las coordenadas
            (mapa-convertido (preparar-mapa mapa-fitxer))
            ; ESTADO
            ; creamos el estado inicial del juego 
            (estat-inicial (list mapa-convertido '(((3 1) e1 r (r)) ((10 10) e1 r (r)) ((19 19) e2 r (r))) 200 200 'e1))
          )

        ; dibujamos el mapa  
        (dibuixa-mapa estat-inicial) ; Le pasa el mapa a grafics.lsp   

        (format t "--- ESTADO INICIAL CREADO ---~%")
        (format t "Turno de: ~A | Pintura restante de E1: ~A | Pintura restante de E2: ~A | Bolas: ~A" (nth 4 estat-inicial) (nth 2 estat-inicial) (nth 3 estat-inicial) (nth 1 estat-inicial))
        (format t "\n-----------------------------~%")
        (read)

        ; empezamos a jugar
        (format t "\nCOMENÇA LA PARTIDA!\n~%")
        (juga-torn estat-inicial)
    )
)

"FUNCIÓN QUE JESTIONA EL FLUJO DE CADA TURNO"
(defun juga-torn (estat)
    (let* ( (mapa (car estat))                  ; mapa
            (unitats (car (cdr estat)))         ; lista de bolas
            (pintura-e1 (car (cddr estat)))     ; pintura de e1
            (pintura-e2 (car (cdddr estat)))    ; pintura de e2
            (torn-actual (car (cddddr estat)))  ; turno
        
            ; pintura que tiene el equipo de quien es el turno 
            (pintura-base (cond ((eq torn-actual 'e1) pintura-e1)
                                        (t pintura-e2)))
            
            ; al inicio del turno sumamos 2 de pintura al equipo +1 por laboratorio que tengan
            (ingresos (+ 2 (comptar-laboratoris torn-actual mapa)))
            (pintura-total (+ pintura-base ingresos))

            ; necesitamos saber la posicion de la base del equipo que juega 
            (casella-base (trobar-base torn-actual mapa))   ; casilla entera de la base
            (pos-base (car casella-base))                   ; coordenadas (x y) de la base
            ; y las bolas que le pertenecen 
            (bolles-propies (filtrar-bolles-equip torn-actual unitats)) ; lista de bolas que pertenecen al equipo (((x y) e1) ((x y) e1))

            ; junstamos la base y las bolas en una sola lista 
            (totes-les-unitats (cons casella-base bolles-propies))

            ; le preguntamos, a cada unidad por separado con su propia vision, lo que quiere hacer
            (accions (demanar-accions-equip totes-les-unitats mapa pintura-total unitats))

            ; calculamos que ve el equipo (base + cada bola)
            ;(visio-agent (calcula-visio-total pos-base bolles-propies mapa unitats))

            ; llamamos al agente para que decida lo que hacer (le pasamos solo lo que puede ver, la pintura que le queda y la lista de sus bolas)
            ;(accions (agent-xyz999 visio-agent pintura-total bolles-propies))   
          ) 

        (format t "Torn de ~A. L'agente ha decidit fer: ~A~%" torn-actual accions)
        (format t "Prem una tecla y Enter per aplicar accions i canviar de torn...\n")
        (read) ; El programa se para aquí hasta que escribas algo y pulses Enter

        ; aplicamos las acciones y actualizamos el estado 
        (let* ( (siguiente-turno (cond ((eq torn-actual 'e1) 'e2)
                                       (t 'e1))) 
                (resultado-aplicar-acciones (aplicar-acciones accions unitats pintura-total mapa))
                
                (nuevas-unidades (car resultado-aplicar-acciones))
                (nueva-pintura (car (cdr resultado-aplicar-acciones)))

                (nou-estat (list (nth 0 estat)
                                 nuevas-unidades
                                 (cond ( (eq torn-actual 'e1) nueva-pintura ) 
                                       ( t (nth 2 estat) ))
                                 (cond ( (eq torn-actual 'e2) nueva-pintura ) 
                                       ( t (nth 3 estat) ))
                                 siguiente-turno))                    
              )
            
            "SI ALGUIEN HA GANADO SE TIENE QUE PARAR EL JUEGO "

            "SI NO SEGUIMOS JUGANDO"
            (dibuixa-mapa nou-estat)    ; redibujamos el mapa para ver la accion
            (juga-torn nou-estat)       ; recursividad
        )
    )
)

"====================================================================="
" DEMANAR ACCIONES"
"====================================================================="

"PIDE ACCIONES A CADA UNIDAD DE LA LISTA Y LAS JUNTA EN UNA UNICA LISTA"
(defun demanar-accions-equip (unitats-equip mapa pintura unitats-globals)
    (cond (  (null unitats-equip) nil  )
          (  t 
                (let* ( (unidad-actual (car unitats-equip))
                        (posicion (car unidad-actual))

                        ; necesitamos saber si es la base o una bola porque tienen rangos de vision distintos
                        (es-base (member 'base unidad-actual))
                        (rango-vision (cond (es-base base_visio)
                                            (t bolla_visio)))

                        ; calculamos la vision de solo esta unidad
                        (visio-bruta (visio-mapa posicion rango-vision mapa))
                        (visio-neta (insertar-bolles-en-visio visio-bruta unitats-globals))

                        ; llamamos al agente diciendole quien es, que ve y cuanta pintura tiene
                        (accions-unidad (agent-xyz999 unidad-actual visio-neta pintura))
                      )
                    
                    ; juntamos las acciones de esta unidad con las que devuelvan las demas 
                    (append accions-unidad
                            (demanar-accions-equip (cdr unitats-equip) mapa pintura unitats-globals))
                )  )
    )
)

"====================================================================="
" APLICAR ACCIONES"
"====================================================================="

"ACTUALIZA LA POSICION DE UNA BOLA EN LA LISTA DE UNIDADES"
(defun actualizar-posicio-bola (pos-antigua pos-nueva unitats)
    (cond ( (null unitats) nil )
          ; si la posicion coincide con la de la unidad actual 
          ( (equal pos-antigua (caar unitats))  
            ; creamos una unidad nueva con la pos-nueva y mantenemos el resto de datos de la unidad iguales
            (cons (append (list pos-nueva) (cdr (car unitats)))
                  (cdr unitats)) )
          ; si no es esta bola la dejamos como estaba y seguimos mirando
          ( t (cons (car unitats)
                    (actualizar-posicio-bola pos-antigua pos-nueva (cdr unitats))) )
    )
)

"EJECUTA LAS ORDENES DEL AGENTE Y DEVUELVE LA LISTA DE UNIDADES ACTUALIZADA"
(defun aplicar-acciones (accions unidades pintura mapa)
    (cond ( (null accions) (list unidades pintura) )    ; cuando ya heyamos mirado todas las acciones devolvemos la lista de unidades (ya estaran actualizadas) y la pintura (en segun que acciones va cambiando)
          ( t (let* ( (accio (car accions))             ; cogemos la primera accion de la lista (mou (7 7) (7 8))
                      (tipo (car accio))                ; tipo de accion (mou)
                      (params (cdr accio))              ; ((7 7) (7 8))
                    )
                (cond ( (and (eq tipo 'mou)                                                     ; si la accion es mover una bola
                             (<= (dist-sq (car params) (car (cdr params))) bolla_moviment)                     ; la distancia a la que se quiere mover esta permitida
                             (not (member 'aigua (trobar-casella (car (cdr params)) mapa))))    ; y no se quiere mover encima de una casilla que sea agua
                        ; si todo es legal
                        ; calculamos la nueva lista de unidades tras el movimiento
                        (let ( (nueva-lista (actualizar-posicio-bola (car params) (car (cdr params)) unidades))
                             )
                            ; aplicamos la siguiente accion
                            (aplicar-acciones (cdr accions) nueva-lista pintura mapa)
                        ) )
                      ;"SI LA ACCION NO ES LEGAL O NO ES DE TIPO 'MOU POR AHORA LA IGNORAMOS"
                      ( t (aplicar-acciones (cdr accions) unidades pintura mapa ) )
                )
              ) )
    )
)

"====================================================================="
" COMPROBAR ACCIONES LEGALES"
"====================================================================="

"BUSCA UNA CASILLA EN EL MAPA A PARTIR DE SUS COORDENADAS"
(defun trobar-casella (coordenadas mapa)
    (cond (  (null mapa) nil  )
          ; si encuentra algo lo devuelve 
          (  (trobar-casella-fila coordenadas (car mapa))  )
          ; si no busca en la siguiente fila
          (  t (trobar-casella coordenadas (cdr mapa))  )
    )
)

(defun trobar-casella-fila (coordenadas fila)
    (cond (  (null fila) nil  )
          ; si las coordenadas son iguales devolvemos la casilla entera
          (  (equal coordenadas (caar fila)) (car fila)  )
          ; si no miramos la siguiente casilla de la fila
          (  t (trobar-casella-fila coordenadas (cdr fila))  )
    )
)

"====================================================================="
" VISIÓN "
"====================================================================="

; el agente no recibe el mapa entero, hay que pasarle solo las casillas que puede ver

"CALCULA LA DISTANCIA AL CUADRADO ENTRE DOS PUNTOS"
(defun dist-sq (p1 p2)
    (+ (expt (- (car p1) (car p2)) 2) 
       (expt (- (cadr p1) (cadr p2)) 2))
)

"MIRA QUE CASILLAS DE UNA FILA ESTÁN A MENOS DE rang DE pos"
(defun visio-fila (pos rang fila)
    (cond (  (null fila) nil  )
          ; sila casilla actual esta dentro del rango
          ; miramos que distancia hay entre las coordenadas de la primera casilla (caar) y la posición
          ; si es menor está en rango
          (  (<= (dist-sq pos (caar fila)) rang)
             ; si esta en rango conservamso esa casilla y seguimos mirando 
             (cons (car fila) (visio-fila pos rang (cdr fila))) )
          ; si no no la conservamos y seguimos mirando 
          (  t (visio-fila pos rang (cdr fila))  )
    )
)

"MIRA QUE CASILLAS DEL MAPA ESTÁN A MENOS DE rang DE pos"
(defun visio-mapa (pos rang mapa)
    (cond (  (null mapa) nil  )
          ; juntamos
          (  t (append (visio-fila pos rang (car mapa))     ; las casillas que se ven des de la primera fila
                       (visio-mapa pos rang (cdr mapa))) )  ; y las casillas que se ven des del resto de filas
    )
)

"SUMA LA VISIÓN DE TODAS LAS BOLAS (se le pasan solo las del equipo)"
(defun visio-llista-bolles (bolles mapa)
    (cond (  (null bolles) nil  )
          ; juntamos               lista (x y)
          (  t (append (visio-mapa (caar bolles) bolla_visio mapa)  ; las casillas que ve la primera bola 
                       (visio-llista-bolles (cdr bolles) mapa))  )  ; con las casillas que ven las demas bolas 
    )
)

"JUNTA LA VISIÓN DE LA BASE Y DE LAS BOLAS"
(defun calcula-visio-total (pos-base bolles-equip mapa unitats-globals)
                        ; juntamos
    ; si lo hacemos solo asi y la vision de las bolas y la base coinciden, la lista que se devuelve tiene casillas repetidas
    (let* ( (visio-duplicats (append (visio-mapa pos-base base_visio mapa)       ; las casillas que ve la base
                                     (visio-llista-bolles bolles-equip mapa)))   ; las casillas que ven todas las bolas
            ; por eso hacemos esto, para eliminar las casillas repetidas
            (visio-neta (eliminar-duplicats visio-duplicats)) ; devolvemos la vision sin repeticiones
          ) 
        (insertar-bolles-en-visio visio-neta unitats-globals) ; devolvemos esto
    )
)

"A LA LISTA DE VISION (casillas que puede ver el agente por ranog)
LE TENEMOS QUE INSERTAR LA POSICION DE LAS BOLAS QUE PUEDE VER
(si no el agente veria el terreno pero no las bolas que hay encima)"
(defun insertar-bolles-en-visio (vision-terreno unitats)
    (cond (  (null vision-terreno) nil  ) ; si no hay mas casillas que mirar fin
          (  t
             (let* ( (casilla-actual (car vision-terreno))
                     (coordenadas-casilla (car casilla-actual)) ; extraemos (x y) de la casilla
                     ; buscamos si hay una bola en estas coordenadas
                     (bola-encontrada (buscar-bola-en-coordenadas coordenadas-casilla unitats))
                   )
                (cond ( bola-encontrada ; si se ha encontrado una bola en las coordenadas (es algo que no sea nil)
                        ; creamos una nueva casilla que las combine
                        ; ((x y) terra r) + ((x y) e1 r (r)) = ((x y) terra r bolla e1 r (r))
                        (cons (append casilla-actual (list 'bolla (nth 1 bola-encontrada) (nth 2 bola-encontrada) (nth 3 bola-encontrada)))
                              (insertar-bolles-en-visio (cdr vision-terreno) unitats)) )
                      ; si no se ha encontrado una bola en esas coordenadas 
                      ( t (cons casilla-actual (insertar-bolles-en-visio (cdr vision-terreno) unitats)) ) ; dejamos la casilla como estaba y seguimos mirando
                )
             )
          )

    )
)

"BUSCA UNA BOLA POR COORDENADAS EN LA LISTA DE UNIDADES"
(defun buscar-bola-en-coordenadas (coords unitats)
    (cond ( (null unitats) nil )
          ( (equal coords (caar unitats)) (car unitats) ) ; devolvemos la unidad que tiene las coordenadas que buscamos
          ( t (buscar-bola-en-coordenadas coords (cdr unitats)) )
    )
)

"RECORRE LA LISTA DE VISION Y SOLO CONSERVA LAS CASILLAS QUE NO ESTNA REPETIDAS"
(defun eliminar-duplicats (llista)
    (cond (  (null llista) nil  )
          ; si la casilla actual ya existe en el resto de la lista
          (  (existeix-casella (car llista) (cdr llista)) 
             (eliminar-duplicats (cdr llista))  ) ; la saltamos
          ; si no existe, la conservamos y seguimos limpiando el resto
          (  t (cons (car llista) (eliminar-duplicats (cdr llista)))  )
    )
)

"COMPRUEVA SI UNA CASILLA (elem) YA EXISTE DENTRO DE UNA LISTA"
(defun existeix-casella (elem llista)
    (cond (  (null llista) nil  )
          ; comparamos la posición (car elem) con la posición de la primera casilla de la lista
          (  (equal (car elem) (caar llista)) t  )
          (  t (existeix-casella elem (cdr llista))  )
    )
)

"BUSCA LA BASE DEL EQUIPO EN EL MAPA"
(defun trobar-base (equip mapa)
    (cond (  (null mapa) nil  )
          ; si trobar-base-fila equip devuelve algo que no sea nil es que ha encontrado la casilla de la base
          ; como no hay resultado (cond (condicion resultado)) se devuelve el valor de la condicion (la casilla)
          (  (trobar-base-fila equip (car mapa))  )
          (  t (trobar-base equip (cdr mapa))  )
    )
)

(defun trobar-base-fila (equip fila)
    (cond (  (null fila) nil  )
          ; si la casilla (car fila) es una base y pertenece al equipo 
          ; devolvemos la casilla de la base
          (  (and (member 'base (car fila)) (member equip (car fila))) (car fila)  )
          ; si no buscamos en la siguiente casilla 
          (  t (trobar-base-fila equip (cdr fila))  )
    )
)

"FILTRA LAS BOLAS DEL EQUIPO ACTUAL EN LA LISTA DE UNIDADES"
(defun filtrar-bolles-equip (equip unitats)
    ; unitats: (  ((x1 y1) e1) ((x2 y2) e2) ...  )
    ; (car unitats) -> ((x1 y1) e1)
    ; (cdr (car unitats)) -> (e1)
    ; (car (cdr (car unitats))) -> e1

    (cond (  (null unitats) nil  )
          ; Si la bola es del equipo
          (  (eq equip (car (cdr (car unitats)))) 
             ; la conservamso y seguimos mirando el resto de bolas
             (cons (car unitats) (filtrar-bolles-equip equip (cdr unitats)))  )
          ; si no, la descartamos y miramos el resto de bolas
          (  t (filtrar-bolles-equip equip (cdr unitats))  )
    )
)

"====================================================================="
" LABORATORIS "
"====================================================================="

"CUENTA CUANTOS LABORATORIOS TIENE UN EQUIPO EN EL MAPA"
(defun comptar-laboratoris (equip mapa)
    (cond (  (null mapa) 0  )
          (  t (+ (comptar-labs-fila equip (car mapa))
                  (comptar-laboratoris equip (cdr mapa)))  )
    )
)

"CUENTA LOS LABORATORIOS DE UN EQUIPO EN UNA FILA"
(defun comptar-labs-fila (equip fila)
    (cond (  (null fila) 0  )
          ; si la casilla es un lab y es del equipo se suma 1
          (  (and (member 'lab (car fila)) (member equip (car fila)))
             (+ 1 (comptar-labs-fila equip (cdr fila)))  )
          ; si no no sumamos y seguimos mirando la fila
          (  t (comptar-labs-fila equip (cdr fila)))
    )
)