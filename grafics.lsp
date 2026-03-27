;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

;;; =====================================================================
;;; MÓDULO GRÁFICO - DIBUJO DEL MAPA
;;; =====================================================================

;; DIBUJA TODO EL MAPA
(defun dibuixa-mapa (mapa)
    (cls)                       ; Limpia la pantalla gráfica
    (dibuixa-files mapa 0 modo)      ; Empieza el ciclo en la fila x=0
)         

;; RECORRE LAS FILAS (x)
(defun dibuixa-files (mapa x modo)
    (cond (  (null mapa) t  )                           ; Si no hay más filas, termina
        (  t (dibuixa-columnes (car mapa) 0 x modo)     ; Dibuja las columnas de la fila x empezando en 0
             (dibuixa-files (cdr mapa) (+ x 1) modo))   ; pasamos a la Siguiente fila (x+1)
    )
)

;; RECORRE LAS COLUMNAS (y)
(defun dibuixa-columnes (columna y x modo)
    (cond (  (null columna) t  )                                    ; Si la columna termina, vuelve atrás
          (  t (dibuixa-una-casella (car columna) y x modo)         ; Pinta la casilla actual actual
               (dibuixa-columnes (cdr columna) (+ y 1) x modo)  )   ; Siguiente columna (y+1)
    )
)

;; PINTA UNA CASILLA DEL TABLERO SEGÚN SU COLOR
(defun dibuixa-una-casella (casilla x y modo)   
    (let* (  (mida 15)               ; Tamaño de cada casilla
            (posx (* x mida))       ; posicion del cuadrado
            (posy (* y mida))  ) 
    
          (cond ((= modo 0)
                    ;; Decidimos el color de relleno
                    (cond (  (member 'r casilla) (color 222 98 98)  )   ; Rojo
                          (  (member 'g casilla) (color 145 213 109)  )   ; Verde
                          (  (member 'b casilla) (color 12 177 199)  )   ; Azul
                          (  (member 'aigua casilla) (color 28 0 122)  ) ; Negro (Agua)
                          (  t (color 208 208 208)  )                 ; Gris (Tierra vacía)
                    )
                )
                ((= modo 1)
                    (cond (  (member 'aigua casilla) (color 28 0 122)  )
                          (  t (color 208 208 208)  )
                    )
                )
          )
        
        (quadrat_ple mida posx posy) ; Llama a tu función de dibujar líneas
        (color 0 0 0)
    )
)                

;; DIBUJA UN CUADRADO RELLENO
(defun quadrat_ple (x posx posy)
    (cond (  (<= x 0) t  )
          ( t (move posx posy)
              (quadrat x)
              (quadrat_ple (- x 4) (+ posx 2) (+ posy 2))  )
    )
)

;; DIBUJA EL CONTORNO DEL CUADRADO
(defun quadrat (mida)
    (drawrel 0 mida)
    (drawrel mida 0)
    (drawrel 0 (- mida))
    (drawrel (- mida) 0)
)

;; ESTA ES LA FUNCIÓN QUE LLAMARÁ EL CONTROLADOR EN CADA TURNO
(defun pinta (estat)
  ;; 1. Limpiar pantalla
  ;; 2. Llamar a dibuixa-mapa
  ;; 3. Llamar a dibuixa-unitats
  ;; 4. Llamar a dibuixa-marcador (puntos, turno, etc.)
)

;; DIBUJA UN SOLO CUADRADO (Casilla de tierra, agua, base o lab)
(defun dibuixa-casella (x y tipus color)
  ;; Aquí usarás los comandos de XLISP: (color ...), (move ...), (quadrat ...)
)

;; DIBUJA LAS BOLITAS DE PINTURA
(defun dibuixa-unitats (llista-bolles)
  ;; Recorre la lista de unidades y dibuja un círculo o punto en su posición
)

;;; ESTO CREO QUE NO 
;; DIBUJA EL TEXTO CON LA PUNTUACIÓN Y EL TURNO
(defun dibuixa-marcador (punts-roig punts-verd torn)
  ;; Usa (move ...) y (princ ...) para escribir en la ventana gráfica
)