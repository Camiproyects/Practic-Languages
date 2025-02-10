       IDENTIFICATION DIVISION.
       PROGRAM-ID. traspaso-info.
       AUTHOR ANDRES CAMILO LAGUNA BERNAL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE CONTROL.
           SELECT ARCHIVO-BASE ASSIGN TO "placli.csv"
               ORGANIZATION IS SEQUENTIAL.

           SELECT ARCHIVO-SALIDA ASSIGN "data-user.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE DYNAMIC
               RECORD KEY IS NUMDOC
               FILE STATUS IS WS-FS.
       
       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-BASE.
       01 REGISTRO.
           05 REGIST     PIC X(291).

       FD ARCHIVO-SALIDA.
       01 USUDATA.
           05 NUMDOC     PIC 9(10).
           05 RAZSOC     PIC X(50).
           05 DIRECT     PIC X(50).
           05 CIUDAD     PIC X(20).
           05 CONTAC     PIC X(30).
           05 TEL-1      PIC 9(10).
           05 TEL-2      PIC 9(10).
           05 CORREO     PIC X(100).

       WORKING-STORAGE SECTION.
       01 MANAGE-DATA.
           05 WS-FS      PIC XX.
           05 WS-LIMIT   PIC X.
           05 WS-CONTA   PIC X(10).
           05 WS-OPTI    PIC XX.
           05 LIM        PIC X.


       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARCHIVO-BASE.
           OPEN OUTPUT ARCHIVO-SALIDA.

           PERFORM CLEAR-SCREEN.

           DISPLAY "PROCESANDO DATOS...... :p".

           PERFORM MOVE-DATA UNTIL WS-OPTI = "Q"

       CLEAR-SCREEN.
           MOVE " " TO LIM.
           DISPLAY LIM LINE 1 POSITION 1 ERASE EOS.
       
       
       