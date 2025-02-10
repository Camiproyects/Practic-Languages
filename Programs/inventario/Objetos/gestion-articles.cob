       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTION-ARTICLES.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.
      ****************************************************************
      *                GESTIÓN DE ARTUCULOS 
      *
      * Descripcion: Manejo de articulos mediantes el archivo indexado 
      *              Se utilizan operaciones CRUD basadas en         *
      *              la clave NUMDOC.                                *
      *                                                              *
      * Autor: ANDRES CAMILO LAGUNA BERNAL                           *
      * Fecha: 07-02-2025                                            *
      ****************************************************************
       DATE-WRITTEN.07-02-2025.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-ART ASSIGN TO "../Articulos/articulos.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODART
               ALTERNATE KEY IS CODBAR
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-ART.
       01 USUART.
           05 CODART.
              07 CATERG.
                 09 CP-NOMBRE   PIC X(10).
                 09 CP-DESCRI   PIC X(50).
              07 SUBCAT.
                 09 SC-NOMBRE   PIC X(10).
                 09 SC-DESCRI   PIC X(50).
           05 CODUNI         PIC X(5).
           05 CODBAR         PIC X(48).
           05 DESCRI         PIC X(500).
           05 STOCK          PIC 9(10).
           05 CANTID         PIC X(10).
           05 PRECOM         PIC 9(10)V99.
           05 PREVEN         PIC 9(10)V99.
           05 FECHA          PIC 9(8).
       
       WORKING-STORAGE SECTION.
       01 WS-FS              PIC XX.
       01 LIM                PIC X.
       01 WS-OPTION          PIC X.
       
       LINKAGE SECTION.
       01 PASAR.
           07 T-NUMDOC       PIC 9(10).
           07 T-NOMAPE       PIC X(65).

       PROCEDURE DIVISION USING PASAR.
       INCIO.
           PERFORM CLEAR-SCREEN.
           IF WS-FS NOT = "00"
              DISPLAY "Error al abrir el archivo. FS = " WS-FS
              STOP RUN
           END-IF.
           PERFORM MENU-CRUD-U UNTIL WS-OPCION = "Q".
           CLOSE ARCHIVO-ART.
           DISPLAY "PROGRAMA TERMINADO :p".
           STOP RUN."

       MENU-CRUD-U.
           PERFORM CLEAR-SCREEN.
           DISPLAY "-------------------------------" LINE 3 POSITION 20.
           DISPLAY "GESTION DE INVENTARIO  "         LINE 4 POSITION 20.
           DISPLAY "-------------------------------" LINE 5 POSITION 20.
           DISPLAY " 1->  ARTICULOS  "               LINE 6 POSITION 20.
           DISPLAY " 2->  CATEGORIASY SUBCATEGORIAS" LINE 7 POSITION 20.
           DISPLAY "  Q -> Salir"                    LINE 8 POSITION 20.
           DISPLAY "Seleccione una opción:"         LINE 17 POSITION 20.
           ACCEPT WS-OPCION LINE 17 POSITION 41.
           EVALUATE WS-OPCION
               WHEN "1"
                    PERFORM MENU-CRUD-A
               WHEN "2"
                    PERFORM MANU-CRUD-C
               WHEN "Q"
                    DISPLAY "SALIENDING ...."
           END-EVALUATE.

       MENU-CRUD-C.
           PERFORM CLEAR-SCREEN.



       MENU-CRUD-A.
           PERFORM CLEAR-SCREEN.
           DISPLAY "-------------------------------" LINE 3 POSITION 20.
           DISPLAY "  MENU DE ARTICULOS INDEXADOS  " LINE 4 POSITION 20.
           DISPLAY "-------------------------------" LINE 5 POSITION 20.
           DISPLAY "  C -> Crear articulo"           LINE 7 POSITION 20.
           DISPLAY "  R -> Leer articulo"            LINE 9 POSITION 20.
           DISPLAY "  U -> Actualizar articulo"     LINE 11 POSITION 20.
           DISPLAY "  D -> Eliminar articulo"       LINE 13 POSITION 20.
           DISPLAY "  Q -> Salir"                   LINE 15 POSITION 20.
           DISPLAY "Seleccione una opción:"         LINE 17 POSITION 20.
           ACCEPT WS-OPCION LINE 17 POSITION 41.
           EVALUATE WS-OPCION
               WHEN "C" 
                    PERFORM OPERACION-CREAR
               WHEN "R" 
                    PERFORM OPERACION-LEER
               WHEN "U" 
                    PERFORM OPERACION-ACTUALIZAR
               WHEN "D" 
                    PERFORM OPERACION-ELIMINAR
               WHEN "Q" 
                    DISPLAY "Saliendo..."
               WHEN OTHER 
                    DISPLAY "Opción no válida."
           END-EVALUATE.

       OPERACION-CREAR.
           PERFORM CLEAR-SCREEN.
           DISPLAY "-------------------------------" LINE 3 POSITION 20.
           DISPLAY "  CREAR ARTICULO               " LINE 4 POSITION 20.
           DISPLAY "-------------------------------" LINE 5 POSITION 20.
           DISPLAY "--- CATEGORIA --- "              LINE 6 POSITION 20.
           ACCEPT CP-NOMBRE                          LINE 7 POSITION 40.
           READ ARCHIVO-ART 
             RECORD KEY CODART
             INVALID KEY 
               DISPLAY "NUEVA CATREGORIA " LINE 8 POSITION 20
               DISPLAY "DESCRIPCION DE LA CATEGORIA"LINE 9 POSITION 20
               ACCEPT CP-DESCRI LINE 9 POSITION 40
               DISPLAY "NOMBRA UNA SUBCATEGORIA" LINE 10 POSITION 20
               ACCEPT SC-NOMBRE LINE 10 POSITION 40
               DISPLAY "DESCRIPCION SUB CATEGORIA" LINE 11 POSITION 20
               ACCEPT SC-DESCRI LINE 11 POSITION 40
           STRING CP-NOMBRE CP-DESCRI SC-NOMBRE SC-DESCRI INTO CODART 
               DISPLAY "DESCRIPCION DEC"