           IDENTIFICATION DIVISION.
           PROGRAM-ID. pant_princ.
           AUTHOR. ANDRES CAMILO LAGUNA BERNAL.
      ****************************************************************
      *                GESTIÓN USUARIOS                              *
      *                                                              *
      * Descripción: Manejo de usuarios mediante archivo indexado.   *
      *              Se utilizan operaciones CRUD basadas en         *
      *              la clave NUMDOC.                                *
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

       PROCEDURE DIVISION.
       INICIO.
           PERFORM CLEAR-SCREEN.
           PERFORM MENU-ADMIN UNTIL WS-OPCION = "Q".

       MENU-ADMIN.
           PERFORM CLEAR-SCREEN.
           DISPLAY "------------------------------" LINE 3  POSITION 20.
           DISPLAY "  MENU DE AMINISTRADOR        " LINE 4  POSITION 20.
           DISPLAY "------------------------------" LINE 5  POSITION 20.
           DISPLAY "  1. -> ADMN USUARIOS"          LINE 7  POSITION 20.
           DISPLAY "  2. -> ADMN ARTICULOS"         LINE 9  POSITION 20.
           DISPLAY "  Q -> Salir"                   LINE 11 POSITION 20.
           DISPLAY "Seleccione una opción:"         LINE 17 POSITION 20.
           ACCEPT WS-OPCION LINE 17 POSITION 41.
           EVALUATE WS-OPCION
               WHEN 1
               PERFORM CLEAR-SCREEN
               DISPLAY "ENTRAS"
                 CALL "gestion_user"     USING NUMDOC
                 END-CALL
               WHEN 2 
               CALL "gestion_articles"   USING NUMDOC
                 END-CALL
               WHEN OTHER
                  DISPLAY "ERROR"
           END-EVALUATE.
           
           


       CLEAR-SCREEN.
           MOVE " " TO LIM.
           DISPLAY LIM LINE 1 POSITION 1 ERASE EOS.
           