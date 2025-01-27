       IDENTIFICATION DIVISION.
       PROGRAM-ID. tabla.
      ****************************************************************
      *    PROGRAMA QUE MUESTRA LA TABLA DE MULTIPLICAR DE UN NUMERO *
      *    INGRESADO POR EL USUARIO.                                 *
      *    Date: 2025-01-23                                          *
      *    Author: Andres Camilo Laguna _Bernal                      *
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
            DISPLAY SPACES LINE 1 POSITION 1 ERASE
            DISPLAY '--------------------------------------------------'
      -       LINE 1 POSITION 20
            DISPLAY '--- PORFAVOR INTRODUSCA EL NUMERO DE LA TBALA ----'
      -       LINE 2 POSITION 20
            DISPLAY '--- RECUERDA QUE PARA SALIR DIGITA (99) ----------'
      -       LINE 3 POSITION 20
            DISPLAY '--------------------------------------------------'
      -       LINE 4 POSITION 20
            PERFORM UNTIL NUMTAB = 99
               ACCEPT NUMTAB
               IF NUMTAB = 99
                   EXIT PERFORM
               END-IF
               PERFORM MOSTRAR
           END-PERFORM
           DISPLAY SPACES LINE 1 POSITION 1 ERASE
           DISPLAY '--- GRACIAS ;p ------------------------------------' LINE 10 POSITION 20
           STOP RUN.
       
       MOSTRAR.
           MOVE 0 TO CONT
           PERFORM UNTIL CONT >= MAX
               ADD 1 TO CONT
               COMPUTE RESULT = CONT * NUMTAB
               DISPLAY CONT ' * ' NUMTAB ' = ' RESULT
           END-PERFORM.