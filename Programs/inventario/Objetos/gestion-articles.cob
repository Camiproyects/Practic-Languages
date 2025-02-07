       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTION-ARTICLES.
       AUTHOR. ANDRES CAMILO LAGUNA BERNAL.
      ****************************************************************
      *                GESTIÓN DE ARTUCULOS 
      *
      * Descripcion: Manejo de articulos mediantes el archivo indexado 
      *              Se utilizan operaciones CRUD basadas en         *
      *              la clave NUMDOC.                                *
      *                                                              *
      * Autor: ANDRES CAMILO LAGUNA BERNAL                           *
      * Fecha: 07-02-2025                                            *
      ****************************************************************
       DATE-WRITTEN.07-02-2025.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-ART ASSIGN TO "../Articulos/articulos.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS 