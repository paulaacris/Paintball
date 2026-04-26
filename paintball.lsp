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
"LAS BOLAS QUE SE CREAN Y SE ELIMINAN SE TIENEN QUE PONER Y QUITAR DEL MAPA"

" CARCAR ARCHIVOS "
(load 'paintball/grafics)
(load 'paintball/agent-abc123)
; (load 'agent-xyz999)

"====================================================================="
" SETUP "
"====================================================================="
; LEE EL ARCHIVO .MAP Y DEVUELVE EL CONTENIDO
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
            (estat-inicial (list mapa-convertido '(((0 0) e1 r (r)) ((1 1) e2 r (r)) ((19 19) e2 r (r))) 200 200 'e1))
          )

        ; dibujamos el mapa  
        (dibuixa-mapa estat-inicial) ; Le pasa el mapa a grafics.lsp   

        (format t "--- ESTADO INICIAL CREADO ---~%")
        (format t "Turno de: ~A | Pintura restante de E1: ~A | Pintura restante de E2: ~A | Bolas: ~A" (nth 4 estat-inicial) (nth 2 estat-inicial) (nth 3 estat-inicial) (nth 1 estat-inicial))
        (format t "\n-----------------------------~%")
        (read)

        ; empezamos a jugar
        (jugar estat-inicial)
    )
)

(defun jugar (estat)
    (format t "\nCOMENÇA LA PARTIDA!\n~%")
    (juga-torn estat)
)

"FUNCIÓN QUE JESTIONA EL FLUJO DE CADA TURNO"
(defun juga-torn (estat)
    (let* ( (mapa (nth 0 estat))            ; mapa
            (unitats (nth 1 estat))         ; lista de bolas
            (torn-actual (nth 4 estat))     ; turno
        
            ; necesitamos saber la posicion de la base del equipo que juega 
            (casella-base (trobar-base torn-actual mapa))   ; casilla entera de la base
            (pos-base (car casella-base))                   ; coordenadas (x y) de la base
            ; y las bolas que le pertenecen 
            (bolles-propies (filtrar-bolles-equip torn-actual unitats)) ; lista de bolas que pertenecen al equipo (((x y) e1) ((x y) e1))

            ; calculamos que ve el equipo (base + cada bola)
            (visio-agent (calcula-visio-total pos-base bolles-propies mapa unitats))

            ; llamamos al agente para que decida lo que hacer (le pasamos solo lo que puede ver)
            (accions (agent-xyz999 visio-agent))  
               
          ) 

        (format t "Torn de ~A. L'agente ha decidit fer: ~A~%" torn-actual accions)
        (format t "Prem una tecla y Enter per aplicar accions i canviar de torn...\n")
        (read) ; El programa se para aquí hasta que escribas algo y pulses Enter

        ; cremaos un estado nuevo con los cambios realizados 
        "de momento solo cambia el turno"
        (let* ( (siguiente-turno (cond ((eq torn-actual 'e1) 'e2)
                                       (t 'e1))) 
                (nou-estat (list (nth 0 estat)
                                 (nth 1 estat)
                                 (nth 2 estat)
                                 (nth 3 estat)
                                 siguiente-turno))                    
              )
            
            "SI ALGUIEN HA GANADO SE TIENE QUE PARAR EL JUEGO "

            "SI NO SEGUIMOS JUGANDO"
            ; recursividad
            (juga-torn nou-estat)
        )
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