       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTARIO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ARTICULO.
           05  CODORG   PIC X(13). *> Código original ("CAFCAST10258")
           05  CODOPT PIC X(8).  *> Código optimizado ("01010528")
       
      *> Tablas de conversión de categorías y subcategorías a ID numérico
       01  TABLA-CATEGORIA.
           05  CATEGORIA-NOMBRE OCCURS 5 TIMES INDEXED BY IDX-CAT.
               10  CATDES PIC X(3). *> "CAF", "ELE", etc.
               10  CATID          PIC 9(2). *> 01, 02, etc.
       
       01  TABLA-SUBCATEGORIA.
           05  SUBCATEGORIA-NOMBRE OCCURS 5 TIMES INDEXED BY IDX-SUB.
               10  SUBCATDES PIC X(3).
               10  SUBCATID          PIC 9(2).
       
       PROCEDURE DIVISION.
       BEGIN.
           ACCEPT CATID.

           ACCEPT SUBCATID(1).
           
           ACCEPT TO CODORG.
       
           PERFORM VARYING IDX-CAT FROM 1 BY 1 UNTIL IDX-CAT > 5
               IF CODORG(1:3) = CATDES(IDX-CAT)
                   MOVE CATID(IDX-CAT) TO CODOPT(1:2)
                   EXIT PERFORM
               END-IF
           END-PERFORM.
       
           PERFORM VARYING IDX-SUB FROM 1 BY 1 UNTIL IDX-SUB > 5
               IF CODORG(4:3) = SUBCATDES(IDX-SUB)
                   MOVE SUBCATID(IDX-SUB) TO CODOPT(3:2)
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      
      
           MOVE CODORG(9:4) TO CODOPT(5:4).
       
           DISPLAY "Código original: " CODORG.
           DISPLAY "Código optimizado: " CODOPT.
       
           STOP RUN.
