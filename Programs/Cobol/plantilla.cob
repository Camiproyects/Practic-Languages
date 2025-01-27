       IDENTIFICATION DIVISION.
       PROGRAM-ID. plantilla.
      ****************************************************************
      *    PROGRAMA PARA LA PRACTICA DE MUESTRA DE PANTALLA DE COBOL *
      *    DATE: 2025-01-27                                          *
      *    AUTHOR: Andres Camilo Laguna _Bernal                      *
      ****************************************************************
       DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 DATOS-VARIADOS.
            03 VARIABLES.
                05 COD          PIC 9(6).
                05 DES          PIC X(50).
                05 PRECOM       PIC 9(8)V99.
                05 PREVEN       PIC 9(8)V99.
                05 INV          PIC 9(7)V99.
                05 DAT          PIC X.
                
       PROCEDURE DIVISION.
       INICIO.
           MOVE 0 TO COD.
           PERFORM PANTALLA UNTIL DAT = 'Q'.
           STOP RUN.

       PANTALLA.
           DISPLAY SPACES               LINE 1  POSITION 1 ERASE.
           DISPLAY '1- CODIGO        :' LINE 5  POSITION 20
           ACCEPT COD                      LINE 5  POSITION 40.
           IF COD = 0 THEN
               MOVE 'Q' TO DAT
               GO TO FIN-PANTALLA
           END-IF.
           
           DISPLAY '2- DESCRIPCION   :' LINE 7  POSITION 20
           ACCEPT DES                      LINE 7  POSITION 40.
           
           DISPLAY '3- PRECIO COMPRA :' LINE 9  POSITION 20
           ACCEPT PRECOM                   LINE 9  POSITION 40.
           
           DISPLAY '4- PRECIO VENTA  :' LINE 11 POSITION 20
           ACCEPT PREVEN                   LINE 11 POSITION 40.
           
           DISPLAY '5- INVENTARIO    :' LINE 13 POSITION 20
           ACCEPT INV                      LINE 13 POSITION 40.
           
           DISPLAY '6- DATOS         :' LINE 21 POSITION 20
           ACCEPT DAT                      LINE 21 POSITION 40.
           
           PERFORM GRABA-DATOS.
           
       FIN-PANTALLA.
           EXIT.

       GRABA-DATOS.
           DISPLAY COD          LINE 5  POSITION 40
           DISPLAY DES          LINE 7  POSITION 40
           DISPLAY PRECOM       LINE 9  POSITION 40
           DISPLAY PREVEN       LINE 11 POSITION 40
           DISPLAY INV          LINE 13 POSITION 40
           DISPLAY DAT          LINE 21 POSITION 40.
