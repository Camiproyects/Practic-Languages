       IDENTIFICATION DIVISION.
       PROGRAM-ID. inicio_sesion.
       AUTHOR. "ANDRES CAMILO LAGUNA BERNAL".

      ****************************************************************
      *                INICIO DE SESION                              *
      *                                                              *
      * Descripción: Manejo de usuarios mediante archivo indexado.   *
      *              Se utilizan operaciones CRUD basadas en         *
      *              la clave NUMDOC.                                *
      *                                                              *
      * Autor: ANDRES CAMILO LAGUNA BERNAL                           *
      * Fecha: 04-01-2025                                            *
      ****************************************************************
       DATE-WRITTEN. "03-01-2025".

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-CLIENTES ASSIGN TO "../Usuarios/usuarios.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS NUMDOC
               ALTERNATE RECORD KEY IS CODUNI WITH DUPLICATES
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  ARCHIVO-CLIENTES.
       01  USUDATA.
           05 NOMAPE   PIC X(65).
           05 TIPDOC   PIC X(02).
           05 NUMDOC   PIC 9(10).
           05 MONTPA   PIC 9(10).
           05 NUMCON   PIC 9(10).
           05 CORREO   PIC X(30).
           05 CARGO    PIC X.
           05 DETALL   PIC X(65).
           05 FECREG   PIC 9(08).
           05 REDOND   PIC 9(10).
           05 CODUNI   PIC 9(4).

       WORKING-STORAGE SECTION.
       01 WS-FS         PIC XX.
       01 WS-CL         PIC X.
       01 LIM           PIC X.
       01 WS-OPCION     PIC X.
       01 WS-DATO-JSON  PIC X(500).
       01 WS-CLAVE      PIC 9(10).
       01 PANT-AD       PIC X(30).
       01 PANT-EM       PIC X(30).

       01 TEMP-USUDATA.
           05 T-NOMAPE   PIC X(65).
           05 T-TIPDOC   PIC X(02).
           05 T-NUMDOC   PIC 9(10).
           05 T-MONTPA   PIC 9(10).
           05 T-NUMCON   PIC 9(10).
           05 T-CORREO   PIC X(30).
           05 T-CARGO    PIC X.
           05 T-DETALL   PIC X(65).
           05 T-FECREG   PIC 9(08).
           05 T-CODUNI   PIC 9(4).

       PROCEDURE DIVISION.
       INICIO.
           PERFORM CLEAR-SCREEN.
           OPEN INPUT ARCHIVO-CLIENTES.
           IF WS-FS NOT = "00"
              DISPLAY "Error al abrir el archivo. FS = " WS-FS
              STOP RUN
           END-IF.
           PERFORM INI-SEC UNTIL WS-OPCION = "Q".
           DISPLAY "PROGRAMA TERMINADO :p".
           STOP RUN.

       INI-SEC.
           PERFORM CLEAR-SCREEN.
           MOVE "Em-pant-princ"    TO PANT-EM.
           MOVE "Ad-pant-princ"       TO PANT-AD.
           DISPLAY "-------------------------------" LINE 3 POSITION 20.
           DISPLAY "  INICIO DE SESION             " LINE 4 POSITION 20.
           DISPLAY "-------------------------------" LINE 5 POSITION 20.
           DISPLAY "INSERTA CEDULA DE CIUDADANIA   " LINE 6 POSITION 20.
           DISPLAY "INSERTA NUMERO DE EMPLEADO     " LINE 7 POSITION 20.
           ACCEPT T-NUMDOC LINE 6 POSITION 55.
           ACCEPT T-CODUNI LINE 7 POSITION 55.
           MOVE T-NUMDOC TO NUMDOC.
            READ ARCHIVO-CLIENTES RECORD KEY NUMDOC
                INVALID KEY 
                DISPLAY "Usuario no encontrado." LINE 9 POSITION 50
                NOT INVALID KEY
                IF T-CODUNI = CODUNI
                    CLOSE ARCHIVO-CLIENTES
                    DISPLAY "ENTRASTE" LINE 9 POSITION 50
                    PERFORM PAUSA
                    IF CARGO = "2"
                        CALL PANT-AD USING NUMDOC , NOMAPE
                        END-CALL
                    ELSE
                        CALL PANT-EM USING NUMDOC , NOMAPE
                        END-CALL
                    END-IF
                ELSE 
                DISPLAY "CREDENCIALES ERRONEAS" LINE 9 POSITION 50
            END-READ.
           PERFORM PAUSA.

       CLEAR-SCREEN.
           MOVE " " TO LIM.
           DISPLAY LIM LINE 1 POSITION 1 ERASE EOS.


       PAUSA.
           DISPLAY "Presione ENTER para continuar...".
           ACCEPT WS-OPCION.
           WINDOW-CREATE.
