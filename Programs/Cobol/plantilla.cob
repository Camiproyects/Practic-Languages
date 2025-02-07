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
                 05 PRECOM       PIC X(11).
                 05 PREVEN       PIC X(11).
                 05 INV          PIC X(10).
                 05 DAT          PIC X.
                 05 COD-EDIT     PIC   ZZZZ,ZZ9.
                 05 PRECOM-EDIT  PIC ZZZ,ZZZ,ZZ9.99.
                 05 PREVEN-EDIT  PIC ZZZ,ZZZ,ZZ9.99.
                 05 INV-EDIT    PIC  ZZ,ZZZ,ZZ9.99.
                 
        PROCEDURE DIVISION.
        INICIO.
            MOVE 0 TO COD.
            PERFORM PANTALLA UNTIL DAT = 'Q'.
            STOP RUN.
 
        PANTALLA.
              MOVE ' ' TO DAT.
              DISPLAY DAT LINE 1 POSITION 1 ERASE EOS.
              DISPLAY '***************************' LINE 4  POSITION 20.            
              DISPLAY '*1- CODIGO        :' LINE 5  POSITION 20.            
              DISPLAY '*2- DESCRIPCION   :' LINE 7  POSITION 20.
              DISPLAY '*3- PRECIO COMPRA :' LINE 9  POSITION 20.
              DISPLAY '*4- PRECIO VENTA  :' LINE 11 POSITION 20.
              DISPLAY '*5- INVENTARIO    :' LINE 13 POSITION 20.
              DISPLAY '**************************:' LINE 14 POSITION 20.
              DISPLAY '6- DATOS(S/NUM)   :' LINE 21 POSITION 20.
              PERFORM TOMA-DATOS.	

        TOMA-DATOS.
              ACCEPT  COD                  LINE 5  POSITION 46 REQUIRED.
               MOVE    COD TO COD-EDIT.
               DISPLAY COD-EDIT            LINE 5  POSITION 46.
               
              ACCEPT  DES                  LINE 7  POSITION 40 REQUIRED.
              
              ACCEPT  PRECOM               LINE 9  POSITION 40 REQUIRED.
               MOVE    PRECOM TO PRECOM-EDIT.
               DISPLAY PRECOM-EDIT         LINE 9  POSITION 40.
               
              ACCEPT  PREVEN               LINE 11 POSITION 40 REQUIRED.
               MOVE    PREVEN TO PREVEN-EDIT.
               DISPLAY PREVEN-EDIT         LINE 11 POSITION 40.
               
              ACCEPT  INV                  LINE 13 POSITION 41 REQUIRED.
               MOVE    INV TO INV-EDIT.
               DISPLAY INV-EDIT            LINE 13 POSITION 41.
               
              ACCEPT DAT                   LINE 21 POSITION 40	.
              
              
              IF DAT = 'S' OR DAT ='s'
                PERFORM PANTALLA
              ELSE
                PERFORM UNOAUNO UNTIL DAT = 'S' OR DAT = 'Q'
              END-IF.
        
        UNOAUNO.
                EVALUATE DAT
                  WHEN 1
                   ACCEPT COD              LINE 5  POSITION 46
                   MOVE 0 TO DAT
                   MOVE    COD TO COD-EDIT
                   DISPLAY COD-EDIT            LINE 5  POSITION 46
                  WHEN 2
                   ACCEPT DES              LINE 7  POSITION 40
                   MOVE 0 TO DAT
                  WHEN 3
                   ACCEPT PRECOM           LINE 9  POSITION 40
                   MOVE 0 TO DAT
                   MOVE    PRECOM TO PRECOM-EDIT
                   DISPLAY PRECOM-EDIT         LINE 9  POSITION 40
                  WHEN 4
                   ACCEPT PREVEN           LINE 11 POSITION 40
                   MOVE 0 TO DAT
                   MOVE    PREVEN TO PREVEN-EDIT
                   DISPLAY PREVEN-EDIT         LINE 11 POSITION 40
                  WHEN 5
                   ACCEPT INV              LINE 13 POSITION 41
                   MOVE 0 TO DAT
                   MOVE    INV TO INV-EDIT
                   DISPLAY INV-EDIT            LINE 13 POSITION 41
                  WHEN 0
                   ACCEPT DAT              LINE 21 POSITION 40
                  WHEN 'S'
                   PERFORM PANTALLA
                  WHEN OTHER
                   ACCEPT DAT              LINE 21 POSITION 40
                   DISPLAY 'ERROR DE DIGITACION DIGITE DE NUEVO'
                                           LINE 20 POSITION 30
                                           MOVE 0 TO DAT
                 END-EVALUATE.

