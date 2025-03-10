       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRASPASO-INFO.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-BASE ASSIGN TO "placli.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS.

           SELECT ARCHIVO-SALIDA ASSIGN TO "data-user.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE DYNAMIC
               RECORD KEY IS NUMDOC
               FILE STATUS IS WS-FS-SALIDA.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-BASE.
       01 REGISTRO       PIC X(291).

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
       01 WS-DATA.
           05 WS-FS          PIC XX.
           05 WS-PER         PIC XX.
           05 WS-FS-SALIDA   PIC XX.
           05 WS-OPTI        PIC X     VALUE SPACE.
           05 LIN            PIC 9(5)  VALUE 0.
           05 CONTADOR       PIC 9(10) VALUE 0.
           05 ERROR-COUNT    PIC 9(10) VALUE 0.
           05 T-CONTADOR     PIC X(10).
           05 T-ERROR-COUNT  PIC X(10).

       01 WS-MSG           PIC X(80) VALUE SPACES.
       01 HEADER           PIC X(291) VALUE SPACES.

       PROCEDURE DIVISION.
       INICIO.
           MOVE 0 TO CONTADOR.
           MOVE "Y" TO WS-PER.
           OPEN INPUT ARCHIVO-BASE.
           IF WS-FS NOT = "00"
             STRING "ERROR ABRIENDO ARCHIVO BASE: " DELIMITED BY SIZE
               WS-FS DELIMITED BY SIZE
               INTO WS-MSG
             DISPLAY WS-MSG
             STOP RUN
           END-IF.
           
           PERFORM UNTIL WS-PER = "Q"
             IF WS-PER = "Y"
               PERFORM ABRIR
             ELSE
               PERFORM LEER
             END-IF
           END-PERFORM.
           
       ABRIR.
           OPEN OUTPUT ARCHIVO-SALIDA.
           IF WS-FS-SALIDA NOT = "00"
             STRING "Error al abrir el archivo " DELIMITED BY SIZE
               INTO WS-MSG
               DISPLAY WS-MSG
             STOP RUN
           END-IF.
       
           MOVE "NUMDOC" TO NUMDOC.
           MOVE "RAZSOC" TO RAZSOC.
           MOVE "DIRECT" TO DIRECT.
           MOVE "CIUDAD" TO CIUDAD.
           MOVE "CONTAC" TO CONTAC.
           MOVE "TEL-1" TO TEL-1.
           MOVE "TEL-2" TO TEL-2.
           MOVE "CORREO" TO CORREO.

           WRITE USUDATA.
           IF WS-FS-SALIDA NOT = "00"
             STRING "Error al escribir en el archivo." DELIMITED BY SIZE
               INTO WS-MSG
             DISPLAY WS-MSG
           END-IF.
           MOVE "E" TO WS-PER.
           CLOSE ARCHIVO-SALIDA.
           
           
       LEER.
           OPEN I-O ARCHIVO-SALIDA.
           IF WS-FS-SALIDA NOT = "00" AND WS-FS-SALIDA NOT = "05"
             STRING "ERROR ABRIENDO ARCHIVO SALIDA: " DELIMITED BY SIZE
               WS-FS-SALIDA DELIMITED BY SIZE
               INTO WS-MSG
             DISPLAY WS-MSG
             CLOSE ARCHIVO-BASE
             STOP RUN
           END-IF.
       
           READ ARCHIVO-BASE INTO HEADER
             AT END 
              STRING "Archivo vacío ." DELIMITED BY SIZE
              INTO WS-MSG
              DISPLAY WS-MSG
              CLOSE ARCHIVO-BASE
              CLOSE ARCHIVO-SALIDA
              STOP RUN
           END-READ.
 
           STRING "PROCESANDO DATOS..." DELIMITED BY SIZE
                  INTO WS-MSG
           DISPLAY WS-MSG LINE 2 POSITION 10.
           
           STRING "REGISTROS CON ERROR:" DELIMITED BY SIZE
                  INTO WS-MSG
           DISPLAY WS-MSG LINE 3 POSITION 10.
           
           MOVE 10 TO LIN.

           PERFORM UNTIL WS-OPTI = "Q"
             READ ARCHIVO-BASE INTO REGISTRO
               AT END MOVE "Q" TO WS-OPTI
               NOT AT END 
                 ADD 1 TO CONTADOR
                 PERFORM PROCESAR-REGISTRO
                 PERFORM VER
             END-READ
           END-PERFORM.
           
           CLOSE ARCHIVO-BASE.
           CLOSE ARCHIVO-SALIDA.
           STOP RUN.

       PROCESAR-REGISTRO.
           UNSTRING REGISTRO DELIMITED BY ';'
             INTO NUMDOC, RAZSOC, DIRECT, 
                    CIUDAD, CONTAC, TEL-1, TEL-2, CORREO.
           IF NUMDOC = ZEROS OR RAZSOC = SPACES OR DIRECT = SPACES OR 
              CIUDAD = SPACES OR TEL-1 = SPACES OR CORREO = SPACES
              ADD 1 TO ERROR-COUNT
           ELSE
              WRITE USUDATA
           END-IF.
           EXIT.

       VER.
           ADD 1 TO LIN.
           
           MOVE CONTADOR TO  T-CONTADOR.
           MOVE ERROR-COUNT TO  T-ERROR-COUNT.
           DISPLAY T-CONTADOR         LINE 2 POSITION 50.
           DISPLAY T-ERROR-COUNT      LINE 3 POSITION 50.
           DISPLAY "*"              LINE LIN POSITION 1.
           DISPLAY "*"              LINE LIN POSITION 14.
           DISPLAY "*"              LINE LIN POSITION 30.
           DISPLAY "*"              LINE LIN POSITION 40.
           DISPLAY "*"              LINE LIN POSITION 50.
           DISPLAY NUMDOC           LINE LIN POSITION 2.
           DISPLAY RAZSOC           LINE LIN POSITION 15.
           DISPLAY CIUDAD           LINE LIN POSITION 31.
           DISPLAY CONTAC           LINE LIN POSITION 41.
           DISPLAY TEL-1            LINE LIN POSITION 51.
           IF LIN > 20 
              MOVE 10 TO LIN
           END-IF.
           EXIT.



