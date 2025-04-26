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
               05 NUMTAB PIC 99 VALUE 0.
               05 RESULT PIC 9(4).
               05 CONT PIC 99 VALUE 0.
               05 MAX PIC 99 VALUE 10.

       PROCEDURE DIVISION.
       INICIO.
           DISPLAY '--------------------------------------------------'.
           DISPLAY '--- POR FAVOR INTRODUZCA EL NÚMERO DE LA TABLA ---'.
           DISPLAY '--- PARA SALIR, DIGITE (99) ----------------------'.
           DISPLAY '--------------------------------------------------'.

           PERFORM UNTIL NUMTAB = 99
              ACCEPT NUMTAB
              IF NUMTAB NOT = 99 THEN
                  PERFORM MOSTRAR
              END-IF
           END-PERFORM.

           DISPLAY '--- GRACIAS ;p ------------------------------------'.
           STOP RUN.

       MOSTRAR.
           MOVE 0 TO CONT.
           PERFORM UNTIL CONT >= MAX
               ADD 1 TO CONT
               COMPUTE RESULT = CONT * NUMTAB
               DISPLAY CONT ' * ' NUMTAB ' = ' RESULT
           END-PERFORM.
