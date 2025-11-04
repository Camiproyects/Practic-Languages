       IDENTIFICATION DIVISION.
       PROGRAM-ID. tabla.
      ****************************************************************
      *    PROGRAMA QUE MUESTRA LA TABLA DE MULTIPLICAR DE UN NÚMERO *
      *    INGRESADO POR EL USUARIO.                                 *
      *    Date: 2025-01-23                                          *
      *    Author: Andres Camilo Laguna Bernal                      *
      ****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DATOS-VARIADOS.
           03 VARIABLES.
               07 NUMTAB PIC 99 VALUE 0.
               07 RESULT PIC 9(4).
               07 CONT PIC 99 VALUE 0.
               07 MAX PIC 99 VALUE 10.
               07 LIM PIC XX.
               07 STO PIC X(9).

       PROCEDURE DIVISION.
       INICIO.
           PERFORM CLEAN_SCREEN.
           DISPLAY '--------------------------------------------------'.
           DISPLAY '--- POR FAVOR INTRODUZCA EL NÚMERO DE LA TABLA ---'.
           DISPLAY '--- PARA SALIR, DIGITE (00) ----------------------'.
           DISPLAY '--------------------------------------------------'.

           PERFORM UNTIL NUMTAB = 00
              ACCEPT NUMTAB
              IF NUMTAB NOT = 00 THEN
                  PERFORM MOSTRAR
              END-IF
           END-PERFORM.

           DISPLAY '--- GRACIAS ;p ------------------------------------'
           LINE 4 POSITION 20.
           STOP RUN.

       MOSTRAR.
           MOVE 0 TO CONT.
           PERFORM UNTIL CONT >= MAX
               ADD 1 TO CONT
               COMPUTE RESULT = CONT * NUMTAB
               DISPLAY CONT ' * ' NUMTAB ' = ' RESULT
           END-PERFORM.
           PERFORM PAUSA.
           PERFORM CLEAN_SCREEN.

       CLEAN_SCREEN.
           MOVE ' ' TO LIM.
           DISPLAY LIM LINE 1 POSITION 1 ERASE EOS.

       PAUSA.
           ACCEPT STO LINE 1 POSITION 40.
           MOVE ' ' TO STO.
