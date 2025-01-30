        IDENTIFICATION DIVISION.
        PROGRAM-ID. inventario.
      ****************************************************************
      *    PROGRAMA PARA LA PRACTICA DE MUESTRA DE PANTALLA DE COBOL *
      *    DATE: 2025-01-27                                          *
      *    AUTHOR: Andres Camilo Laguna _Bernal                      *
      ****************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 PRODUCTOS.
           05 COD         PIC 9(6).
           05 NOMBRE      PIC X(50).
           05 DESCRIPCION PIC X(100).
           05 PRECOM      PIC 9(8)V9(2).
           05 PREVEN      PIC 9(8)V9(2).
           05 STOCK       PIC 9(6).
       
       01 PRODUCTO-TEMP.
           05 COD-TEMP         PIC 9(6).
           05 NOMBRE-TEMP      PIC X(50).
           05 DESCRIPCION-TEMP PIC X(100).
           05 PRECOM-TEMP      PIC 9(8)V9(2).
           05 PREVEN-TEMP      PIC 9(8)V9(2).
           05 STOCK-TEMP       PIC 9(6).
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY '**** SISTEMA DE GESTIÓN DE INVENTARIOS ****'.
           PERFORM MENU-PRINCIPAL.
           STOP RUN.
       
       MENU-PRINCIPAL.
           DISPLAY '1. Agregar Producto'.
           DISPLAY '2. Editar Producto'.
           DISPLAY '3. Eliminar Producto'.
           DISPLAY '4. Buscar Producto'.
           DISPLAY '5. Generar Informe'.
           DISPLAY '6. Configurar Alertas'.
           DISPLAY '7. Salir'.
           ACCEPT OPCION.
           EVALUATE OPCION
               WHEN 1 PERFORM AGREGAR-PRODUCTO
               WHEN 2 PERFORM EDITAR-PRODUCTO
               WHEN 3 PERFORM ELIMINAR-PRODUCTO
               WHEN 4 PERFORM BUSCAR-PRODUCTO
               WHEN 5 PERFORM GENERAR-INFORME
               WHEN 6 PERFORM CONFIGURAR-ALERTAS
               WHEN 7 STOP RUN
               WHEN OTHER DISPLAY 'Opción no válida. Intente de nuevo.'
           END-EVALUATE.
       
       AGREGAR-PRODUCTO.
           DISPLAY 'Ingrese el código del producto: '.
           ACCEPT COD-TEMP.
           DISPLAY 'Ingrese el nombre del producto: '.
           ACCEPT NOMBRE-TEMP.
           DISPLAY 'Ingrese la descripción del producto: '.
           ACCEPT DESCRIPCION-TEMP.
           DISPLAY 'Ingrese el precio de compra: '.
           ACCEPT PRECOM-TEMP.
           DISPLAY 'Ingrese el precio de venta: '.
           ACCEPT PREVEN-TEMP.
           DISPLAY 'Ingrese la cantidad en stock: '.
           ACCEPT STOCK-TEMP.
           MOVE COD-TEMP TO COD.
           MOVE NOMBRE-TEMP TO NOMBRE.
           MOVE DESCRIPCION-TEMP TO DESCRIPCION.
           MOVE PRECOM-TEMP TO PRECOM.
           MOVE PREVEN-TEMP TO PREVEN.
           MOVE STOCK-TEMP TO STOCK.
           DISPLAY 'Producto agregado exitosamente!'.
           PERFORM MENU-PRINCIPAL.
       
       EDITAR-PRODUCTO.
           DISPLAY 'Funcionalidad pendiente de implementación.'.
           PERFORM MENU-PRINCIPAL.
       
       ELIMINAR-PRODUCTO.
           DISPLAY 'Funcionalidad pendiente de implementación.'.
           PERFORM MENU-PRINCIPAL.
       
       BUSCAR-PRODUCTO.
           DISPLAY 'Funcionalidad pendiente de implementación.'.
           PERFORM MENU-PRINCIPAL.
       
       GENERAR-INFORME.
           DISPLAY 'Funcionalidad pendiente de implementación.'.
           PERFORM MENU-PRINCIPAL.
       
       CONFIGURAR-ALERTAS.
           DISPLAY 'Funcionalidad pendiente de implementación.'.
           PERFORM MENU-PRINCIPAL.
       
