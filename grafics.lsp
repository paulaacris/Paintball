;; =====================================================================
;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Data: 03/05/2025.
;; Estudiants: Paula Cristina Morro Negre, Matilde Navarro Robles.
;; Professor: Miquel Cabot.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; =====================================================================

"====================================================================="
" VARIABLES "
"====================================================================="

(setq mida 15) ; medida de cada casilla
(setq modo 1) ; 0: cada casilla de su color, 1: toda la tierra es gris

; color e1 -> blanco
; color e2 -> negro

"DEVUELVE EL COLOR DE PANTALLA ASOCIADO A UN IDENTIFICADOR DE PINTURA"
(defun aplicar-color (c)
    (cond ( (eq c 'r) (color 222 98 98) )           ; rojo
          ( (eq c 'g) (color 145 213 109) )         ; verde
          ( (eq c 'b) (color 12 177 199) )          ; azul
          ( (eq c 'agua) (color 28 0 122) )         ; agua
          ( (eq c 'tierra) (color 208 208 208) )    ; gris
          ( (eq c 'e1) (color 0 0 0) )              ; negro
          ( (eq c 'e2) (color 255 255 255) )        ; blanco
          ( t (color 0 0 0) )                       ; negro por defecto
    )
)

"====================================================================="
" DIBUJO DEL MAPA PRINCIPAL "
"====================================================================="

"LIMPIA LA PANTALLA Y EMPIEZA EL DIBUJO DEL MAPA COMPLETO"
(defun dibuixa-mapa (estat)
    (let ( (mapa (car estat))
           (unitats (car (cdr estat)))
         )

        (cls)                           ; Limpia la pantalla gráfica
        (dibuixa-files (reverse mapa))  ; Dibuja el mapa (con reverse para que no lo dibuje al reves)
        (dibuixa-bolles unitats)        ; pintamos las bolas actuales encima del mapa
    ) 
) 

;; =====================================================================
;; DIBUJO DE LA BASE DEL MAPA
;; =====================================================================

"RECORRE EL MAPA Y LLAMA AL DIBUJADO COLUMNA POR COLUMNA"
(defun dibuixa-files (mapa &optional (x 0))
    (cond (  (null mapa) t  )                        ; Si no hay más filas, termina
          (  t (dibuixa-columnes (car mapa) 0 x)     ; Dibuja las columnas de la fila x empezando en 0
               (dibuixa-files (cdr mapa) (+ x 1)))   ; pasamos a la Siguiente fila (x+1)
    )
)

"RECORRE LAS COLUMNAS DE UNA FILA Y DIBUJA CADA CASILLA INDIVIDUAL"
(defun dibuixa-columnes (columna y x)
    (cond (  (null columna) t  )                               ; Si la columna termina, vuelve atrás
          (  t (dibuixa-una-casella (car columna) y x)         ; Pinta la casilla actual actual
               (dibuixa-columnes (cdr columna) (+ y 1) x)  )   ; Siguiente columna (y+1)
    )
)

"PINTA UNA CASILLA DEL TABLERO (segun su color o no dependiendo del modo) Y EL LABORATORIO O LA BASE SI HAY"
(defun dibuixa-una-casella (casilla x y)   
    (let* (  (posx (* x mida))      ; posicion del cuadrado
             (posy (* y mida))  ) 
    
          ; DIBUJAR EL SUELO 
          (cond ((= modo 0)
                    ;; Decidimos el color de relleno
                    (cond (  (member 'r casilla) (aplicar-color 'r)  )    
                          (  (member 'g casilla) (aplicar-color 'g)  )  
                          (  (member 'b casilla) (aplicar-color 'b)  )  
                          (  (member 'aigua casilla) (aplicar-color 'agua)  ) 
                          (  t (aplicar-color 'tierra)  )                    
                    )
                )
                ((= modo 1)
                    (cond (  (member 'aigua casilla) (aplicar-color 'agua)  )
                          (  t (aplicar-color 'tierra)  )
                    )
                )
          )

          (quadrat_ple mida posx posy) ; dibujamos la casilla de suelo
          (color 0 0 0) ; cambiamos a color negro 

          ; DIBUJAR LABORATORIOS Y BASES POR ENCIMA DEL SUELO 
          (cond 
            ;; -- BASE (Dibuixem una quadrat negre) -- 
            (  (member 'base casilla)
                (quadrat_ple mida posx posy)

                ; dibujar los colores de los que esta pintada la base
                (let* ( (ultimo (car (last casilla)))
                        ; solo es una lista de colores si el último elemento ES una lista
                        (colores-recibidos (cond ( (listp ultimo) ultimo )
                                                 ( t nil ))) )  ; si no hay lista, no hay colores
                    (cond (  (member 'r colores-recibidos) (aplicar-color 'r) (quadrat_ple 3 (+ posx 4) (+ posy 4)))  )
                    (cond (  (member 'g colores-recibidos) (aplicar-color 'g) (quadrat_ple 3 (+ posx 8) (+ posy 4)))  )
                    (cond (  (member 'b colores-recibidos) (aplicar-color 'b) (quadrat_ple 3 (+ posx 8) (+ posy 8)))  )
                )

                ; elegimos el color del contorno de la bola 
                (cond (  (member 'e1 casilla) (aplicar-color 'e1)  )        
                      (  (member 'e2 casilla) (aplicar-color 'e2)  )   
                )

                ; dibujamos el contorno de la base para saber de que equipo es 
                (quadrat_borde (- mida 4) (+ posx 2) (+ posy 2))
                (quadrat_borde (- mida 2) (+ posx 1) (+ posy 1))
            
            )
            
            ;; -- LABORATORI (Dibuixem una X) --
            ((member 'lab casilla)
                (move (+ posx 4) (+ posy 4))
                (drawrel 7 7)
                (move (+ posx 11) (+ posy 4))
                (drawrel -7 7)
                
                ; si está conquistado, dibujamos el contorno del equipo
                (cond (  (member 'e1 casilla) (aplicar-color 'e1) 
                                              (quadrat_borde (- mida 4) (+ posx 2) (+ posy 2))
                                              (quadrat_borde (- mida 2) (+ posx 1) (+ posy 1))  )        
                      (  (member 'e2 casilla) (aplicar-color 'e2)
                                              (quadrat_borde (- mida 4) (+ posx 2) (+ posy 2))
                                              (quadrat_borde (- mida 2) (+ posx 1) (+ posy 1))  )   
                )
                
                )
          )
        
        (color 0 0 0) ; cambiamos a color negro 
    )
)   

;; =====================================================================
;; DIBUJO DE LAS BOLAS
;; =====================================================================

"RECORRE LA LISTA DE UNIDADES Y LAS PINTA"
(defun dibuixa-bolles (unitats)
    (cond (  (null unitats) t  )
          (  t (dibuixa-una-bolla (car unitats))
               (dibuixa-bolles (cdr unitats))  )
    )
)

"PINTA UNA BOLA ESPECÍFICA CON SU COLOR, CONTORNO Y MANCHAS"
(defun dibuixa-una-bolla (unitat)
    (let* ( (pos (car unitat))
            (color-bola (nth 2 unitat))
            (torn (nth 1 unitat))
            (colores-pintada (nth 3 unitat))

            (mitat (round (/ mida 2))) ; redondeamos porque como la division 15/2 no es entera luego da error al dibujar
            ; los circulos se empiezan a pintar des del centro de la casilla
            ; calculamos la posición a pintar en la pantalla 
            (centro-x (+ (* (car pos) mida) mitat))
            ; la y tiene que estar invertida para que no si dibuje abajo si va arriba
            (centro-y (+ (* (- tamaño_mapa (cadr pos)) mida) mitat))
            (radio 6)  ; Un radio de 6 queda bien dentro de una casilla de 15
          )
        
        ; elegimos el color de la bola 
        (aplicar-color color-bola)
        
        ; dibujamos la bola (circulo) con las manchas
        (dibuixa-manxes colores-pintada centro-x centro-y radio)

        ; elegimos el color del contorno de la bola 
        (cond (  (eq torn 'e1) (aplicar-color 'e1)  )        
              (  (eq torn 'e2) (aplicar-color 'e2)  ) 
        )

        ; dibujamos el contorno de la bola para saber de que equipo es 
        (cercle centro-x centro-y (+ radio 2) 20)
        (cercle centro-x centro-y (+ radio 1) 20)

        (color 0 0 0) ; volvemos al negro 
    )
)

"PINTA CIRCULOS CONCENTRICOS PARA LAS MANCHAS DE PINTURA"
(defun dibuixa-manxes (llista-colors x y radi)
    (cond ( (null llista-colors) t ) ; Si no hay más colores, terminamos
          ( (<= radi 0) t )          ; Si el radio es 0 o menos, no cabe nada más
          ( t 
            (aplicar-color (car llista-colors))    ; Ponemos el color actual
            (cercle-ple x y radi 20)               ; Dibujamos el círculo
            ; Llamada recursiva con el siguiente color y un radio más pequeño
            (dibuixa-manxes (cdr llista-colors) x y (- radi 2)) )
    )
) 

"====================================================================="
" DIBUJO DE FORMAS BÁSICAS "
"====================================================================="

"DIBUJA UN CUADRADO RELLENO"
(defun quadrat_ple (x posx posy)
    (cond (  (<= x 0) t  )
          ( t (move posx posy)
              (quadrat x)
              (quadrat_ple (- x 4) (+ posx 2) (+ posy 2))  )
    )
)

"DIBUJA EL CONTORNO DEL CUADRADO"
(defun quadrat (medida)
    (drawrel 0 medida)
    (drawrel medida 0)
    (drawrel 0 (- medida))
    (drawrel (- medida) 0)
)

"DIBUJA EL CONTORNO DEL CUADRADO (para saber de que equipo es cada base)"
(defun quadrat_borde (mida x y)
    (move x y)
    (draw (+ x mida) y)
    (draw (+ x mida) (+ y mida))
    (draw x (+ y mida))
    (draw x y)
)

"DIBUJA UN CIRCULO RELLENO"
(defun cercle-ple (x y radi segments)
    (cond (  (<= radi 0) (move x y) (draw x y)  )           ; El centro
          (  t (cercle x y radi segments)                   ; Dibuja el aro exterior
               (cercle-ple x y (- radi 0.5) segments)  )    ; Dibuja un aro un poco más pequeño
    )
)

"DIBUJA EL CONTORNO DEL CIRCULO"
(defun cercle (x y radi segments) 
    (mover (+ x radi) y)                    ; empezamos en el primer punto del circulo
    (cercle2 x y radi (/ 360 segments) 0)
)

(defun cercle2 (x y radi pas angle)
    (cond (  (< angle 360)
             ; calculamos el siguiente punto de la circunferencia
             (drawr (+ x (* radi (cos (radians (+ angle pas)))))
                    (+ y (* radi (sin (radians (+ angle pas))))))
             (cercle2 x y radi pas (+ angle pas))  )
          (  t t  )
    )
)

(defun mover (x y)
    (move (round x) (round y))
)

(defun drawr (x y)
    (draw (round x) (round y))
)

"FUNCIÓN PARA PASAR DE GRADOS A RADIANES"
(defun radians (graus)
    (/ (* graus (* 2 pi)) 360)
)