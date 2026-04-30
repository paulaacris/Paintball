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

(setq tamaño_mapa 19)

(setq max_rondas 200)

"TO DO"
"EN aplicar accion PINTAR SI ES UN LABORATORIO PONERLE A LA CASILLA EL NOMBRE DEL EQUIPO -> EN DIBUJAR SI EL LABORATORIO TIENE EQUIPO REDONDEARLO"

"LÓGIBA BASE (AGENTE Y CONTROLADOR)"
"EN DECIDIR PINTAR SOLO PINTAMOS SI NO ESTA YA PINTADA DE NUESTRO COLOR"
"EN MOURE SI ESTA A TIRO NO NOS SEGUIMOS ACERCANDO A ELLA"
"¿MEJORAR LÓGICA DE MOVIMIENTO?"

; ESTADO
; (mapa unitats pintura-e1 pintura-e2 turno num-ronda)
    ; unitats: todas las bolas activas del juego ((x y) equipo color (colores pintada) tr-moure tr-pintar)
    ; pintura: numero del 0 all 200
    ; torn: e1 / e2

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
            (estat-inicial (list mapa-convertido '(((15 4) e1 r (r g) 0 0) ((10 10) e1 g (g) 0 0) ((12 10) e2 r (r b) 0 0)) 200 200 'e1 1))
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

"FUNCIÓN QUE GESTIONA EL FLUJO DE CADA TURNO"
(defun juga-torn (estat)
    (let* ( (mapa (car estat))                  
            (unitats-velles (car (cdr estat)))  
            (pintura-e1 (car (cddr estat)))     
            (pintura-e2 (car (cdddr estat)))    
            (torn-actual (car (cddddr estat)))  
            (num-ronda (nth 5 estat))           ; Extraemos la ronda
          )
        
        ; --- COMPROBAMOS EL LÍMITE DE RONDAS ANTES DE HACER NADA ---
        (cond ( (>= num-ronda max_rondas)
                (format t "~%=====================================~%")
                (format t "¡LÍMITE DE ~A RONDAS ALCANZADO!~%" max_rondas)
                (format t "=====================================~%")
                
                ; Calculamos y anunciamos el ganador
                (let ((ganador (determinar-guanyador unitats-velles pintura-e1 pintura-e2)))
                    (format t "¡EL GANADOR ES EL EQUIPO ~A!~%" ganador)
                    (format t "=====================================~%~%")
                )
                nil ) ; Devolvemos nil y TERMINAMOS la función aquí
              
              ; --- SI NO HA TERMINADO, JUGAMOS EL TURNO ---
              ( t 
                (let* ( ; actualizamos los tiempos de recuperacion 
                        (unitats (actualitzar-temps-recuperacio unitats-velles torn-actual))
                    
                        ; pintura que tiene el equipo de quien es el turno 
                        (pintura-base (cond ((eq torn-actual 'e1) pintura-e1)
                                            (t pintura-e2)))
                        
                        ; al inicio del turno sumamos 2 de pintura al equipo +1 por laboratorio que tengan
                        (ingresos (+ 2 (comptar-laboratoris torn-actual mapa)))
                        (pintura-total (+ pintura-base ingresos))

                        ; necesitamos saber la posicion de la base del equipo que juega 
                        (casella-base (trobar-base torn-actual mapa))   
                        (pos-base (car casella-base))                   
                        
                        ; y las bolas que le pertenecen 
                        (bolles-propies (filtrar-bolles-equip torn-actual unitats)) 

                        ; juntamos la base y las bolas en una sola lista 
                        (totes-les-unitats (cons casella-base bolles-propies))

                        ; le preguntamos, a cada unidad por separado con su propia vision, lo que quiere hacer
                        (accions (demanar-accions-equip totes-les-unitats mapa pintura-total unitats))
                      ) 

                    (format t "Ronda: ~A | Torn de ~A. L'agente ha decidit fer: ~A~%" num-ronda torn-actual accions)
                    (format t "ESTADO DE MIS BOLAS (pos equipo color pintura tr-moure tr-pinta):~%")
                    (format t " ~A ~%" bolles-propies) 
                    (format t "Prem una tecla y Enter per aplicar accions i canviar de torn...\n")
                    (read) ; El programa se para aquí hasta que escribas algo y pulses Enter

                    ; aplicamos las acciones y actualizamos el estado 
                    (let* ( (siguiente-turno (cond ((eq torn-actual 'e1) 'e2)
                                                   (t 'e1))) 
                            (resultado-aplicar-acciones (aplicar-acciones accions unitats pintura-total mapa))
                            
                            (nuevas-unidades (car resultado-aplicar-acciones))
                            (nueva-pintura (car (cdr resultado-aplicar-acciones)))
                            (nuevo-mapa (caddr resultado-aplicar-acciones))

                            ; eliminamos las bolas muertas
                            (unidades-vidas (matar-bolas nuevas-unidades))

                            (nou-estat (list nuevo-mapa
                                             unidades-vidas
                                             (cond ( (eq torn-actual 'e1) nueva-pintura ) 
                                                   ( t (nth 2 estat) ))
                                             (cond ( (eq torn-actual 'e2) nueva-pintura ) 
                                                   ( t (nth 3 estat) ))
                                             siguiente-turno
                                             ; --- AÑADIMOS LA RONDA ACTUALIZADA AL NUEVO ESTADO ---
                                             (cond ( (eq torn-actual 'e2) (+ num-ronda 1) )
                                                   ( t num-ronda ))
                                       ))                    
                          ) 
                        
                        "SI ALGUIEN HA GANADO SE TIENE QUE PARAR EL JUEGO "
                        (let ((ganador-base (guanyador nuevo-mapa))) ; Usamos nuevo-mapa actualizado
                            (cond ( ganador-base  
                                    (dibuixa-mapa nou-estat) ; Dibujamos para ver la base pintada
                                    (format t "~%=====================================~%")
                                    (format t "¡LA BASE HA SIDO DESTRUIDA!~%")
                                    (format t "¡HA GANADO ~A!~%" ganador-base)
                                    (format t "=====================================~%")
                                    (break "") ) ; Aquí sí detiene la ejecución porque no hay nada después
                                  
                                  ; SI NADIE HA GANADO, SEGUIMOS JUGANDO
                                  ( t 
                                    (dibuixa-mapa nou-estat)    
                                    (juga-torn nou-estat) )       
                            )
                        )
                    )
                ) 
              ) 
        )
    ) 
)

"ELIMINA DE LA LISTA LAS BOLAS QUE TENGAN LOS TRES COLORES (R G B)"
(defun matar-bolas (unitats)
    (cond ( (null unitats) nil )
          ( t 
            (let* ( (bola (car unitats))
                    (colores-pintada (nth 3 bola)) )
                
                ; Si NO tiene los 3 colores, la conservamos
                (cond ( (not (and (member 'r colores-pintada)
                                  (member 'g colores-pintada)
                                  (member 'b colores-pintada)))
                        (cons bola (matar-bolas (cdr unitats))) )
                      
                      ; Si tiene los 3 colores, la ignoramos (muere)
                      ( t (matar-bolas (cdr unitats)) )
                )
            )
          )
    )
)

(defun guanyador (mapa)
    (let* ( (base-e1 (trobar-base 'e1 mapa))
            (base-e2 (trobar-base 'e2 mapa))

            (colores-e1 (car (last base-e1)))
            (colores-e2 (car (last base-e2)))
          ) 

        (cond ( (and (member 'r colores-e1) (member 'b colores-e1) (member 'g colores-e1))
                'e1)
              ( (and (member 'r colores-e2) (member 'b colores-e2) (member 'g colores-e2))
                'e2)
              ( t nil)
        )
    )
)

"DETERMINA EL GANADOR POR TIEMPO SIGUIENDO LAS REGLAS"
(defun determinar-guanyador (unitats pintura-e1 pintura-e2)
    (let* ( ; Filtramos y contamos cuántas bolas vivas tiene cada equipo
            (bolles-e1 (length (filtrar-bolles-equip 'e1 unitats)))
            (bolles-e2 (length (filtrar-bolles-equip 'e2 unitats))) 
          )
        
        (cond 
            ; REGLA 1: Gana quien tiene más bolas vivas
            ( (> bolles-e1 bolles-e2) 'e1 )
            ( (< bolles-e1 bolles-e2) 'e2 )
            
            ; REGLA 2: Si hay empate de bolas, gana quien tiene más pintura
            ( (> pintura-e1 pintura-e2) 'e1 )
            ( (< pintura-e1 pintura-e2) 'e2 )
            
            ; REGLA 3: Si tienen mismas bolas y misma pintura, cara o cruz (aleatorio)
            ; (random 2) devuelve 0 o 1
            ( (= (random 2) 0) 'e1 )
            ( t 'e2 )
        )
    )
)

"BAJA TODOS LOS TIEMPOS DE RECUPERACIÓN A LAS BOLAS DEL EQUIPO"
(defun actualitzar-temps-recuperacio (unitats torn)
    (cond ((null unitats) nil)
          (t (let* ((unitat (car unitats))
                    (pos (nth 0 unitat))
                    (equipo (nth 1 unitat))
                    (color (nth 2 unitat))
                    (color-pintada (nth 3 unitat))
                    (tr-moure (nth 4 unitat))
                    (tr-pintar (nth 5 unitat))
                   )

                    ; si la bola es del equipo que esta jugando ahora 
                    (cond (  (eq torn equipo)  
                             ; cambiamos la bola con una con los tr -1 
                             (cons (list pos equipo color color-pintada (max 0 (- tr-moure 1)) (max 0 (- tr-pintar 1))) 
                                   (actualitzar-temps-recuperacio (cdr unitats ) torn))  )
                          ; si no se queda igual
                          (  t (cons unitat 
                                     (actualitzar-temps-recuperacio (cdr unitats) torn))  )
                    )
             )
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
                            (demanar-accions-equip (cdr unitats-equip) mapa pintura unitats-globals)) ; aplicar de una en una no todas a la vez 
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

"unidades: lista de todas las unidades 
funcion que sustituye la unidad que tiene las coordenadas (coordenadas) por bola (unidad completa)"
(defun actualizar-lista-unidades (coordenadas bola unidades)
    (cond (  (null unidades) nil  )
          ; si encontramos la bola que queremos cambiar 
          (  (equal (caar unidades) coordenadas)
             (cons bola (cdr unidades))  )
          ; si no es esta lo dejamos igual y seguimos miranod
          (  t (cons (car unidades) (actualizar-lista-unidades coordenadas bola (cdr unidades))))
    )
)

"EJECUTA LAS ORDENES DEL AGENTE Y DEVUELVE LA LISTA DE UNIDADES ACTUALIZADA"
(defun aplicar-acciones (accions unidades pintura mapa)
    (cond ( (null accions) (list unidades pintura mapa) )    ; cuando ya heyamos mirado todas las acciones devolvemos la lista de unidades (ya estaran actualizadas) y la pintura (en segun que acciones va cambiando)
          ( t (let* ( (accio (car accions))             ; cogemos la primera accion de la lista (mou (7 7) (7 8))
                      (tipo (car accio))                ; tipo de accion (mou)
                      (params (cdr accio))              ; ((7 7) (7 8)) / en crear bola ('r (7 7) (7 8))
                      ; buscamos la bola (por sus coordenadas) a la que le queremos aplicar la accion
                      ; bola que queremos mover 
                      (bola (buscar-unidad (car params) unidades)) ; unidad completa
                      ; bola que queremos pintar
                      ; la bola que queremos pintar es la correspondiente al segundo parametro (no al primero como la de mover)
                      (bola-pintar (buscar-unidad (cadr params) unidades))

                    )
                (cond ; -- ACCION MOU --
                      ( (and (eq tipo 'mou)                                                     ; si la accion es mover una bola
                             bola                                                               ; si bola no es nil
                             (< (nth 4 bola) 1)                                                 ; si el tiempo de recuperacion de moverse es < 1
                             (<= (dist-sq (car params) (car (cdr params))) bolla_moviment)      ; la distancia a la que se quiere mover esta permitida
                             (dins-mapa (car (cdr params)))
                             (not (member 'aigua (trobar-casella (car (cdr params)) mapa)))    ; y no se quiere mover encima de una casilla que sea agua
                             (not (buscar-unidad (car (cdr params)) unidades))) ; Comprobamos que no haya ninguna unidad en la casilla destino
                        ; SI TODO ES LEGAL
                        (let* ( (incremento (cond ( (es-diagonal (car params) (car (cdr params))) 1.42)
                                                  ( t 1)))
                                ; actualizamos la bola con 
                                (bola-actualizada (list (cadr params)       ; nueva pos
                                                        (nth 1 bola)        ; equipo
                                                        (nth 2 bola)        ; colro
                                                        (nth 3 bola)        ; colores pintada
                                                        (+ (nth 4 bola) incremento)  ; tr_moure + 1 / 1.42
                                                        (nth 5 bola)))      ; tr_pintar igual
                              
                                ; sustituimos la bola vieja por la nueva
                                (nueva-lista (actualizar-lista-unidades (car params) bola-actualizada unidades))
                              ) 
                            ; seguimos con el resto de acciones
                            (aplicar-acciones (cdr accions) nueva-lista pintura mapa)
                        )  )

                     ; -- ACCION CREAR-BOLLA --
                     ( (and (eq tipo 'crear-bolla)                                      ; si la accion es crear una bola
                            (>= pintura 50)                                             ; si pintura >= 50
                            (<= (dist-sq (nth 1 params) (nth 2 params)) base_creacio)   ; si la distancia a la que quiere crear la bola esta permitida
                            (dins-mapa (nth 2 params))
                            (not (member 'aigua (trobar-casella (nth 2 params) mapa))) ; y no quiere crear una bola encima de agua
                            (not (buscar-unidad (car (cdr params)) unidades)))
                        ; SI TODO ES LEGAL 
                        (let* ( (color-bola (nth 0 params))
                                (pos-bola (nth 2 params))
                                (casilla-base (trobar-casella (nth 1 params) mapa)) ; buscamos la casilla de la base para sacar el equipo
                                (equipo (nth 4 casilla-base)) ; ((x y) terra g base e1)

                                ; creamos la nueva bola siguiendo la estructura ((x y) equipo color (colores pintada) tr-moure tr-pintar)
                                (nueva-bola (list pos-bola equipo color-bola (list color-bola) 0 0))

                                ; añadimos la bola nueva a la lista de unidades inicial
                                (nuevas-unidades (cons nueva-bola unidades))
                                ; restamos el coste de la pintura
                                (nueva-pintura (- pintura 50))
                              ) ;(break "jascb")
                            ; seguimos con el resto de acciones con la lista y la pintura actualizadas 
                            (aplicar-acciones (cdr accions) nuevas-unidades nueva-pintura mapa)
                        ) 
                     )

                       ; -- ACCION PINTA -- 
                      ( (and (eq tipo 'pinta) ; si la accion es pintar una bola
                             bola ; si bola no es nil
                             (< (nth 5 bola) 1) ; si el tiempo de recuperacion de pintar es < 1
                             (<= (dist-sq (car params) (car (cdr params))) bolla_pintar) ; la distancia a la que quiere pintar esta permitida 
                             (dins-mapa (car (cdr params)))
                             (not (member 'aigua (trobar-casella (car (cdr params)) mapa)))) ; no quiere pintar una casilla que sea agua 
                        ; SI TODO ES LEGAL 
                        (let* ( (pos-objetivo (car (cdr params)))
                                (color-a-pintar (nth 2 bola))
                                (casilla-objetivo (trobar-casella pos-objetivo mapa))
                              )

                                  ; SE EL OBJETIVO ES UNA BASE
                            (cond ( (member 'base casilla-objetivo)
                                    (let* ( ; obtenemos la lista de colores que tiene al final
                                            ; (terra g base e1 (r g))
                                            (colores-actuales (car (last casilla-objetivo)))
                                            ; añadimos el nuevo color si no estaba 
                                            (nuevos-colores (adjoin color-a-pintar colores-actuales))
                                            ;; Creamos la nueva casilla sustituyendo la vieja lista de colores
                                            ;; Quitamos el último elemento (lista vieja) y añadimos la nueva
                                            (nueva-casilla (append (reverse (cdr (reverse casilla-objetivo))) 
                                                                    (list nuevos-colores)))
                                            ;; Actualizamos el mapa con la casilla nueva
                                            (nuevo-mapa (actualizar-mapa pos-objetivo nueva-casilla mapa))
                                            ;; Actualizamos el TR de la bola que dispara (nth 5)
                                            (bola-act (list (nth 0 bola) (nth 1 bola) (nth 2 bola) 
                                                            (nth 3 bola) (nth 4 bola) (+ (nth 5 bola) 3)))
                                            (nueva-lista-unidades (actualizar-lista-unidades (car params) bola-act unidades))
                                          ) 
                                        (aplicar-acciones (cdr accions) nueva-lista-unidades pintura nuevo-mapa)
                                    ) )

                                  ; SI ES UN LAB
                                  ;( (member 'lab casilla-objetivo)
                                    ; si es un lab en vez de añadir los colores añadimos el equipo de la bola al final
                                   ; nil )

                                  ; SI ES UNA BOLA  
                                  (t (let* ( (color-a-pintar (nth 2 bola)) ; color de la bola que dispara 
                                (colores-victima (nth 3 bola-pintar)) ; colores de los que ya esta pintada la bola a la que vamos a disparar
                                (nuevos-colores-victima (adjoin color-a-pintar colores-victima)) ; adjoin añade un elemento a una lista si no estaba ya  
                                ; actualizamos la bola que lanza
                                (bola-actualizada (list (nth 0 bola)          ; pos igual
                                                        (nth 1 bola)          ; equipo
                                                        (nth 2 bola)          ; colro
                                                        (nth 3 bola)          ; colores pintada
                                                        (nth 4 bola)          ; tr_moure igual
                                                        (+ (nth 5 bola) 3)))  ; tr_pintar + 3
                                ; actualizamos la bola que recibe 
                                (bola-actualizada2 (list (nth 0 bola-pintar)    ; pos igual
                                                         (nth 1 bola-pintar)    ; equipo
                                                         (nth 2 bola-pintar)    ; colro
                                                         nuevos-colores-victima ; unico que cambia (no se como)
                                                         (nth 4 bola-pintar)    ; tr_moure igual
                                                         (nth 5 bola-pintar)))  ; tr_pintar + 3
                                ; sustituimos ambas bolas en la lsita
                                (lista-temp (actualizar-lista-unidades (car params) bola-actualizada unidades))
                                (nueva-lista (actualizar-lista-unidades (car (cdr params)) bola-actualizada2 lista-temp))
                              ) 
                            ; seguimos con el resto de acciones
                            (aplicar-acciones (cdr accions) nueva-lista pintura mapa) 
                        ) ) )
                            
                            )

                        )
                        
                        
                      ; SI LA ACCION NO ES LEGAL O NO ES DEl TIPO LA IGNORAMOS
                      ( t (aplicar-acciones (cdr accions) unidades pintura mapa ) )
                )
                
              ) 
          )
    )
)

"ACTUALIZA UNA COLUMNA DENTRO DE UNA FILA"
(defun actualizar-columna (x nueva-casilla fila)
  (cond ((null fila) nil)
        ((= x 0) (cons nueva-casilla (cdr fila))) ;; Hemos llegado a la X, la cambiamos
        (t (cons (car fila) 
                 (actualizar-columna (- x 1) nueva-casilla (cdr fila)))))
)

"ACTUALIZA UNA FILA DENTRO DEL MAPA"
(defun actualizar-mapa (pos nueva-casilla mapa)
  (let ((x (car pos))
        (y (cadr pos)))
    (cond ((null mapa) nil)
          ((= y 0) ;; Hemos llegado a la fila Y
           (cons (actualizar-columna x nueva-casilla (car mapa)) 
                 (cdr mapa)))
          (t (cons (car mapa) 
                   (actualizar-mapa (list x (- y 1)) nueva-casilla (cdr mapa))))))
)


(defun buscar-unidad (pos unidades)
    (cond (  (null unidades) nil  )
          (  (equal pos (caar unidades)) (car unidades)  )
          (  t (buscar-unidad pos (cdr unidades))  )
    )
)

(defun es-diagonal (pos1 pos2)
    (let ( (x1 (car pos1))
           (y1 (car (cdr pos1)))       
           (x2 (car pos2)) 
           (y2 (car (cdr pos2)))
         )
        
        ; es diagonal si cambian la x y la y a la vez
        (and (not (= x1 x2))
             (not (= y1 y2)))
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

"COMPRUEBA SI UNA POSICION ESTA DENTRO DEL MAPA"
(defun dins-mapa (pos)
    (let ( (x (car pos))
           (y (cadr pos)) )
        (and (>= x 0)
             (<= x tamaño_mapa)
             (>= y 0)
             (<= y tamaño_mapa))
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