       IDENTIFICATION DIVISION.
       PROGRAM-ID. inisec.
        
       ENVIROMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USUARIOS ASSIGN TO "data/usuarios.json"
           ORGANIZATION IS LINE SEQUENTIAL.
        
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        
    	01 DATUSU.
    	   03 PERSIS.
	         05 NUMDOC      PIC 9(10).
	         05 CAR         PIC X(05).
	         05 CODPASS     PIC 9(04).
           03 INFADD.
             05 NOM         PIC X(50).
             05 TIPDOC      PIC XXX.
             05 COR         PIC X(20).
             05 CANPAG      PIC 9(10).
             05 DET         PIC X(50).
           03 PANTALLA.
             05 DAT         PIC XX.
             05 MARFILL     PIC X(50) VALUE ALL '*'.
             05 MAR         PIC X.
             05 SW          PIC X.
       
       PROCEDURE DIVISION.
       INICIO.
           MOVE ' ' TO DAT.
           DISPLAY DAT                  LINE 1  POSITION 1  ERASE EOS.
           DISPLAY MARFILL                        LINE 4  POSITION 15.
           DISPLAY MARFILL                        LINE 6  POSITION 15.
           DISPLAY MARFILL                        LINE 8  POSITION 15.
           DISPLAY '*NUMERO DE DOCUMENTO   :'     LINE 5  POSITION 20.
           DISPLAY '*NUMERO DE EMPLEADO    :'     LINE 7  POSITION 20.
           PERFORM ACPDAT UNTIL SW = 'Q'.
           
           STOP RUN.
       ACPDAT.
           ACCEPT NUMDOC                          LINE 5  POSIITON 40.
           ACCEPT CODPASS                         LINE 5  POSIITON 40.
           OPEN I-O USUARIOS USUARIOS.
           
