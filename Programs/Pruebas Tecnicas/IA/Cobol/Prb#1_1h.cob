       IDENTIFICATION DIVISION.
       PROGRAM-ID Procesamiento_de_datos.
      *PRUEBA 2 — COBOL (Sistemas Bancarios / Legados – 15 puntos)
      *Rol: Analista de sistemas COBOL / Mantenimiento core bancario
      *Duración sugerida: 60 minutos

      *Problema general:
      *Procesar depósitos y retiros sobre un archivo maestro de cuentas corrientes.

      *Criterios de evaluación (15 puntos):
      *1. (1 pt) Divisiones completas: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE.
      *2. (1 pt) Definición correcta de archivos (FD maestro y transacciones).
      *3. (1 pt) Registros con PIC y niveles correctos.
      *4. (2 pts) Lectura secuencial de archivos.
      *5. (2 pts) Validación de tipo de transacción (D o R).
      *6. (2 pts) Control de errores por saldo insuficiente.
      *7. (1 pt) Actualización de saldo en el maestro.
      *8. (2 pts) Reporte final con cuentas, saldo y transacciones rechazadas.
      *9. (2 pts) Orden lógico y comentarios adecuados.
      *10. (1 pt) Uso correcto de PERFORM o IF...ELSE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT ARCHIVO-MAESTRO ASSIGN TO "Data-Master.dat"
                     ORGANIZATION IS INDEXED
                     ACCESS MODE DYNAMIC
                     RECORD KEY IS NUM-CUE
                     FILE STATUS IS FS-AR.

              SELECT CONTADOR ASSIGN TO "Contador.dat"
                     ORGANIZATION IS SEQUENTIAL
                     ACCESS MODE  IS SEQUENTIAL
                     FILE STATUS  IS FS-CO.

       DATA DIVISION.
       FILE SECTION.
       FD CUENTAS.
           01 REG-CUENTA.
                     05 NUM-CUE   PIC 9(10).
                     05 NOMBRE    PIC X(99).
                     05 SALDO     PIC 9(99).
                     05 CLAVE     PIC 9(4).
       FD CONTADOR.
           01 REG-CONTADOR.
              05 ULT-NUM          PIC 9(8).
              
       WORKING-STORAGE SECTION.
           01 FS-AR     PIC XX.
           01 FS-CO     PIC XX.
           01 NUM-NUE   PIC 9(8).
           01 MANEJO.
                     05 INI       PIC 9 VALUE 0.
                     05 CLE       PIC X VALUE SPACES.

       PROCEDURE DIVISION.
       INICIO.
           PERFORM CLEAR-SCREEN.
           OPEN I-O ARCHIVO-MAESTRO.
           IF FS-AR NOT = "00"
              PERFORM CLEAR-SCREEN
              DISPLAY "Eror al abri el archivo, FS = "  FS-AR
              STOP RUN
           END-IF.

           PERFORM PANTALLA-INI UNTIL INI = "9".
      *    STOP RUN.
       
       PANTALLA-INI.
           DISPLAY "-Porfavor selecione el proceso que desea realizar-"
      -    LINE 4 POSITION 20.
           DISPLAY "1: Para crear cuenta "
      -    LINE 5 POSITION 20.
           DISPLAY "2: Para cambiar clave"
      -    LINE 6 POSITION 20.   
           DISPLAY "3: Insertar saldo    "
      -    LINE 7 POSITION 20.   
           DISPLAY "4: Retirar saldo     "
      -    LINE 8 POSITION 20.
           DISPLAY "9: Salir"
           ACCEPT INI LINE 10 POSITION 20.

           EVALUATE INI
              WHEN 1
                     PERFORM CREAR-CUENTA
              WHEN 2
                     PERFORM CAMBIO-CLAVE
              WHEN 3
                     PERFORM AGR-SALDO
              WHEN 4 
                     PERFORM RET-SALDO
              WHEN 9 
                     DISPLAY "ADIOS ;P"
              WHEN OTHER
                     DISPLAY "Opcion no encontrada"

       CREAR-CUENTA.
           PERFORM CLEAR-SCREEN.
           DISPLAY "Porfavor digita tu nombre COMPLETO: "
      -    LINE 4 POSITION 20.
           ACCEPT NOMBRE LINE 4 POSITION 60.
           DISPLAY "Porfavor crea tu clave (4 NUEMROS): "
      -    LINE 5 POSITION 20.
           ACCEPT CLAVE  LINE 5 POSITION 60.
           
           OPEN I-O CONTADOR.
           IF FS-CO = "35"
              OPEN OUTPUT CONTADOR
              MOVE 00000000 TO ULT-NUM
              WRITE REG-CONTADOR 
              CLOSE CONTADOR
           END IF.
           
           READ CONTADOR
              AT END
                     MOVE 0 TO ULT-NUM
           END-READ.

           ADD 1 TO ULT-NUM.
           MOVE ULT-NUM TO NUM-NUE.

           REWRITE REG-CONTADOR.

           DISPLAY "Tu numero de cuenta es: "NUM-NUE
      -    LINE 7 POSITION 20.

           MOVE NUM-NUE TO NUM-CUE.
           MOVE 0 TO SALDO.

           WRITE REG-CUENTA INVALID KEY 
              PERFORM CLEAR-SCREEN
              DISPLAY "Error: Nuemro de cuenta ya existente prueba de "
      -               "nuevo."
              END-WRITE.
           CLOSE CUENTAS CONTADOR



       CLEAR-SCREEN.
           MOVE " " TO CLE.
           DISPLAY CLE LINE 1 POSITION 1 ERASE EOS.
           EXIT.


           



























