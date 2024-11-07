      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 4ALUABC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUINX ASSIGN TO DISK
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ALUINX-MATRICULA.

       DATA DIVISION.
       FILE SECTION.
       FD  ALUINX.
       01  ALUINX-REG.
           03  ALUINX-MATRICULA    PIC 9(8).
           03  ALUINX-NOMBRE       PIC X(30).
           03  ALUINX-GRUPO        PIC X(02).
           03  ALUINX-PROMEDIO     PIC 9(03)V99.


       WORKING-STORAGE SECTION.
       77  BANDERA-SALIR-MENU  PIC 9.
       77  OPCION-MENU         PIC 9.
       77  ALUINX-KEY-EXISTE   PIC 9.
       77  AUTORIZACION        PIC 9(03).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 0 TO BANDERA-SALIR-MENU
           OPEN OUTPUT ALUINX
           CLOSE ALUINX.
           OPEN I-O ALUINX
           PERFORM MOSTRAR-MENU UNTIL BANDERA-SALIR-MENU = 1.
           CLOSE ALUINX
           STOP RUN.

       LEER-ALUINX.
           MOVE 1 TO ALUINX-KEY-EXISTE.
           READ ALUINX INVALID KEY MOVE 0 TO ALUINX-KEY-EXISTE.

       CONSULTA.
           DISPLAY "INGRESA MATRICULA"
           ACCEPT ALUINX-MATRICULA.
           PERFORM LEER-ALUINX.
           IF ALUINX-KEY-EXISTE = 1
               DISPLAY "NOMBRE: " ALUINX-NOMBRE
           ELSE
               DISPLAY "MATRICULA NO EXISTE".

       ALTA.
           DISPLAY "INGRESA MATRICULA"
           ACCEPT ALUINX-MATRICULA
           PERFORM LEER-ALUINX
           IF ALUINX-KEY-EXISTE = 1
               DISPLAY "MATRICULA DUPLICADA"
           ELSE
               DISPLAY "INGRESE EL NOMBRE"
               ACCEPT ALUINX-NOMBRE
               WRITE ALUINX-REG.
               ADD 1 TO AUTORIZACION
               DISPLAY "OPERACION EXITOSA: ", AUTORIZACION.

       BAJA.
           DISPLAY "INGRESA MATRICULA"
           ACCEPT ALUINX-MATRICULA
           PERFORM LEER-ALUINX
           IF ALUINX-KEY-EXISTE = 1
               DELETE ALUINX
           ELSE
               DISPLAY "MATRICULA NO EXISTE".
.

       MODIFICAR.
           DISPLAY "INGRESA LA MATRICULA"
           ACCEPT ALUINX-MATRICULA
           PERFORM LEER-ALUINX.
           IF ALUINX-KEY-EXISTE = 1
               DISPLAY "INGRESE NOMBRE"
               ACCEPT ALUINX-NOMBRE
               REWRITE ALUINX-REG
           ELSE
               DISPLAY "MATRICULA NO EXISTE".

       MOSTRAR-MENU.
           DISPLAY "ALUINX".
           DISPLAY "SELECCIONA UNA OPCION".
           DISPLAY "1. CONSULTAR".
           DISPLAY "2. SALIR".
           ACCEPT OPCION-MENU.
           IF OPCION-MENU = 1
               PERFORM CONSULTA
           ELSE IF OPCION-MENU = 2
               DISPLAY "ADIOS"
               MOVE 1 TO BANDERA-SALIR-MENU.


       END PROGRAM 4ALUABC.
