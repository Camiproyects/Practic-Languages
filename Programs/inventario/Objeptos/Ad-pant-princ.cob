           IDENTIFICATION DIVISION.
           PROGRAM-ID. Ad-pant-princ.
           AUTHOR. ANDRES CAMILO LAGUNA BERNAL.
      ****************************************************************
      *                GESTIÓN USUARIOS                              *
      *                                                              *
      * Descripción: Menu de Administrador para el manejo de usuarios 
      *              articulos                                 *
      *                                                              *
      * Autor: ANDRES CAMILO LAGUNA BERNAL                           *
      * Fecha: 02-01-2025                                            *
      ****************************************************************
       DATE-WRITTEN. "03-01-2025".
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 LIM           PIC XX.
       01 WS-OPCION     PIC X.
       01 GES-USU       PIC X(30).
       01 GES-ART       PIC X(30).

       LINKAGE SECTION.
       01 NUMDOC        PIC 9(10).
       01 NOMAPE        PIC X(65).
       

       PROCEDURE DIVISION USING NUMDOC , NOMAPE.
       INICIO.
           PERFORM CLEAR-SCREEN.
           PERFORM MENU-ADMIN UNTIL WS-OPCION = "Q".

       MENU-ADMIN.
           MOVE "gestion-user"                       TO GES-USU.
           MOVE "gestion-articles"                   TO GES-ART.
           PERFORM CLEAR-SCREEN.
           DISPLAY "------------------------------" LINE 3  POSITION 20.
           DISPLAY "  BIENBENIDO "                  LINE 4  POSITION 20.
           DISPLAY NOMAPE                           LINE 4  POSITION 40.
           DISPLAY "------------------------------" LINE 5  POSITION 20.
           DISPLAY "  1. -> ADMN USUARIOS"          LINE 7  POSITION 20.
           DISPLAY "  2. -> ADMN ARTICULOS"         LINE 9  POSITION 20.
           DISPLAY "  Q -> Salir"                   LINE 11 POSITION 20.
           DISPLAY NUMDOC                           LINE 13 POSITION 20.
           DISPLAY "Seleccione una opción:"         LINE 17 POSITION 20.
           ACCEPT WS-OPCION                         LINE 17 POSITION 41.
           EVALUATE WS-OPCION
               WHEN 1
               PERFORM CLEAR-SCREEN
               DISPLAY "ENTRAS"
                 CALL   GES-USU USING NUMDOC
                 END-CALL
               WHEN 2 
               CALL  GES-ART USING NUMDOC
                 END-CALL
               WHEN OTHER
                  DISPLAY "ERROR"
           END-EVALUATE.
           

       CLEAR-SCREEN.
           MOVE " " TO LIM.
           DISPLAY LIM LINE 1 POSITION 1 ERASE EOS.
           