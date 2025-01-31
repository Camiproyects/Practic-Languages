       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAPTURA-DATOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARTICULOS ASSIGN TO "data/articulos.json"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.		
       FILE SECTION.
       FD ARTICULOS.
       01 ARTICULO-REG.
      *    05 CATX            PIC XX.
      *    05 CATN            PIC 99.
      *    05 CAT  PIC ZZ,99 REVALUE CATX AND CATN.
      *    05 SUBCATX         PIC XXX.
      *    05 SUBCATN         PIC 999.
      *    05 SUBCAT PIC ZZZ,999 REVALUE SUBCATX AND SUBCATN.
      *    05 CODBAR PIC X(20) REVALUE CAT AND SUBCAT.
      *    05 CODINT          PIC X(10).
      *    05 DESCRIPCION     PIC X(50).
      *    05 PRECIO-COMPRA   PIC 9(6)V99.
      *    05 PRECIO-VENTA    PIC 9(6)V99.
      *    05 STOCK           PIC 9(4).
      *    05 CANTIDAD        PIC X(10).
      *    05 FECHA-EXP       PIC X(10).

       WORKING-STORAGE SECTION.
      *01 OPCION PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "1. Agregar Articulo".
           DISPLAY "2. Salir".
           ACCEPT OPCION.
           IF OPCION = 1 THEN
               DISPLAY "Ingresar Código de Barras: "
               ACCEPT CODBAR
               DISPLAY "Ingresar Código Interno: "
               ACCEPT CODINT
               DISPLAY "Ingresar Descripción: "
               ACCEPT DESCRIPCION
               DISPLAY "Ingresar Precio de Compra: "
               ACCEPT PRECIO-COMPRA
               DISPLAY "Ingresar Precio de Venta: "
               ACCEPT PRECIO-VENTA
               DISPLAY "Ingresar Stock: "
               ACCEPT STOCK
               DISPLAY "Ingresar Cantidad: "
               ACCEPT CANTIDAD
               DISPLAY "Ingresar Fecha de Expiración (Opcional): "
               ACCEPT FECHA-EXP

               OPEN OUTPUT ARTICULOS
               WRITE ARTICULO-REG
               CLOSE ARTICULOS

               DISPLAY "Artículo agregado correctamente!"
           END-IF.
           
           STOP RUN.
