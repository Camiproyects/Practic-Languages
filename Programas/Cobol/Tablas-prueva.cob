       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRUEBA-TABLAS.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.
       DATE-WRITTEN. 2025-01-23.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 NUMTAB PIC 99 VALUE 1.
       01 RESULT PIC 99.
       01 TOP PIC 99 VALUE 10.
       01 CONTADOR PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       INICIO.
           PERFORM PROCNUM
           IF NUMTAB NOT EQUAL 99 THEN
               PERFORM MOSTRAR TOP TIMES
           END-IF
           STOP RUN.
       
       PROCNUM.
           DISPLAY '--------------------------------------------------'.
           DISPLAY '--POR FAVOR INSERTE EL VALOR DE TABLA A CONSULTAR-'.
           DISPLAY '--------------------------------------------------'.
           ACCEPT NUMTAB.
       
       MOSTRAR.
           ADD 1 TO CONTADOR.
           COMPUTE RESULT = NUMTAB * CONTADOR.
           DISPLAY CONTADOR ' * ' NUMTAB ' = ' RESULT.