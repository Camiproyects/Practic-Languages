       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSULTA-DATA-USER.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.
       DATE-WRITTEN. 11-02-2025.
      *****************************************************************
      *    DESCRIPCION: Programa para consultar datos de 
      *                 usuarios almacenados en un  
      *                 archivo indexado.                
      *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-SALIDA ASSIGN TO "data-user.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE DYNAMIC
               RECORD KEY IS NUMDOC
               FILE STATUS IS WS-FS-SALIDA.
       
       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-SALIDA.
       01 USUDATA.
           05 NUMDOC     PIC 9(13).
           05 RAZSOC     PIC X(50).
           05 DIRECT     PIC X(50).
           05 CIUDAD     PIC X(20).
           05 CONTAC     PIC X(30).
           05 TEL-1      PIC X(10).
           05 TEL-2      PIC X(10).
           05 CORREO     PIC X(100).
       
       WORKING-STORAGE SECTION.
       01 MANAGE-DATA.
           05 WS-FS-SALIDA   PIC XX.
           05 WS-OPCION      PIC X VALUE "R".
           05 LIM            PIC X(80) VALUE SPACES.
           05 T-NUMDOC       PIC X(13).
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM CLEAR-SCREEN.
           OPEN I-O ARCHIVO-SALIDA.
           IF WS-FS-SALIDA NOT = "00"
             DISPLAY "Error al abrir el archivo. FS = " WS-FS-SALIDA
             STOP RUN
           END-IF.
           
           PERFORM UNTIL WS-OPCION = "Q"
             PERFORM CLEAR-SCREEN
             DISPLAY "DIGITE EL NUMERO DE CEDULA:" LINE 5 POSITION 20
             ACCEPT T-NUMDOC                LINE 5 POSITION 55
             MOVE T-NUMDOC TO NUMDOC
             PERFORM CONSULT-DATA
             DISPLAY "R -> CONSULTAR OTRO CLIENTE"  LINE 20 POSITION 20
             DISPLAY "Q -> SALIR"                   LINE 21 POSITION 20
             ACCEPT WS-OPCION              LINE 21 POSITION 45

           END-PERFORM.
           
           CLOSE ARCHIVO-SALIDA.
           PERFORM CLEAR-SCREEN.
           DISPLAY "--- ADIOS ---"          LINE 15 POSITION 30.
           STOP RUN.
       
       CONSULT-DATA.
           READ ARCHIVO-SALIDA 
             RECORD KEY NUMDOC 
             INVALID KEY  
               DISPLAY "CLIENTE NO ENCONTRADO"       LINE 7 POSITION 20
             NOT INVALID KEY
               DISPLAY "--------------------------" LINE 5  POSITION 20
               DISPLAY "NUMERO DE DOCUMENTO: "      LINE 6  POSITION 20
               DISPLAY NUMDOC                       LINE 6  POSITION 45
               DISPLAY "RAZON SOCIAL: "             LINE 7  POSITION 20
               DISPLAY RAZSOC                       LINE 7  POSITION 45
               DISPLAY "DIRECCION: "                LINE 8  POSITION 20
               DISPLAY DIRECT                       LINE 8  POSITION 45
               DISPLAY "CIUDAD: "                   LINE 9  POSITION 20
               DISPLAY CIUDAD                       LINE 9  POSITION 45
               DISPLAY "CONTACTO: "                 LINE 10 POSITION 20
               DISPLAY CONTAC                       LINE 10 POSITION 45
               DISPLAY "TELEFONO #1: "              LINE 11 POSITION 20
               DISPLAY TEL-1                        LINE 11 POSITION 45
               DISPLAY "TELEFONO #2: "              LINE 12 POSITION 20
               DISPLAY TEL-2                        LINE 12 POSITION 45
               DISPLAY "CORREO: "                   LINE 13 POSITION 20
               DISPLAY CORREO                       LINE 13 POSITION 45
           END-READ.
           EXIT.
       
       CLEAR-SCREEN.
           DISPLAY LIM LINE 1 POSITION 1 ERASE EOS.
           EXIT.
