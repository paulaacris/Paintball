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

(setq nom_mapa "paintball/maps/prueba.map")

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

;; ESTO BUSCARÁ LAS COORDENADAS DE LAS BASES (e1, e2) Y LABS EN EL MAPA
(defun trobar-elements (mapa)
)

;; ESTO CREARÁ LA LISTA GIGANTE CON EL "ESTADO 0" (MAPA, PUNTOS, UNIDADES...)
(defun inicialitzar-estat (nom-mapa modo)
)

"====================================================================="
" BUCLE DE JUEGO "
"====================================================================="
;; ESTO ES EL PUNTO DE ENTRADA QUE TÚ ESCRIBES EN LA CONSOLA
(defun inici ()
    ;; Aquí llamarás a inicialitzar-estat y luego a la función 'juga'
    
    ;; DIBUJAMOS EL MAPA INICIAL
    (dibuixa-mapa (carregar-dades-mapa nom_mapa)) ; Le pasa el mapa a grafics.lsp
    t 
)

;; ESTA ES LA FUNCIÓN RECURSIVA QUE SE LLAMA A SÍ MISMA 1500 VECES
(defun juga (estat torn)
  ;; 1. ¿Ha ganado alguien o es el turno 1500? -> Finalizar
  ;; 2. ¿A quién le toca? -> Llamar a su Agente
  ;; 3. ¿Qué quiere hacer el agente? -> Validar y actualizar estado
  ;; 4. Dibujar el mapa -> Llamar a 'pinta' (de grafics.lsp)
  ;; 5. Siguiente turno -> (juga nou-estat (+ torn 1))
)

"====================================================================="
" REGLAS Y LÓGICA "
"====================================================================="
;; ESTO COMPROBARÁ SI UNA ACCIÓN (MOVER, PINTAR) ES LEGAL SEGÚN LAS REGLAS
(defun validar-accio (accio unitat estat)
)

;; ESTO CREARÁ UNA COPIA DEL ESTADO CON LOS CAMBIOS (NUEVA POSICIÓN, COLOR...)
(defun actualitzar-estat (estat accions-agent)
  ;; Aquí usarás 'posa-matriu' para cambiar colores de casillas
)

;; ESTO MIRARÁ SI ALGUNA BASE TIENE LOS 3 COLORES Y DEBE EXPLOTAR
(defun comprovar-guanyador (estat)
)

;; ESTO RESTARÁ 1 AL TIEMPO DE RECUPERACIÓN (Tr) DE TODAS LAS BOLLAS
(defun actualitzar-temps-recuperacio (llista-unitats)
)