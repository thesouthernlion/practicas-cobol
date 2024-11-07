      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

           ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS ASSIGN TO DISK.
           SELECT EMPINX ASSIGN TO DISK
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EMPINX-NOMINA.


       DATA DIVISION.
       FILE SECTION.
       FD  EMPINX.
       01  EMPIX-REG.
           03 EMPINX-NOMINA      PIC 9(06).
           03 EMPINX-NOMBRE      PIC X(20).
           03 EMPINX-DPTO        PIC X(03).
           03 EMPINX-SUELDO      PIC S9(05)V99.


       FD  EMPLEADOS.
       01  EMP-REG.
           03  EMP-NOMINA   PIC 9(06).
           03  EMP-NOMBRE   PIC X(20).
           03  EMP-DPTO     PIC X(03).
           03  EMP-PERCEP   PIC 9(05)V99.
           03  EMP-DEDU     PIC 9(05)V99.
           03  EMP-SUELDO   PIC S9(05)V99.

       WORKING-STORAGE SECTION.
       77  WKS-EMPLEADOS-EOF PIC 9 VALUE ZERO.
           88  W88-EXISTE-EMPLEADOS      VALUE 0.
           88  W88-NOEXISTE-EMPLEADOS    VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN I-O EMPINX.
           OPEN INPUT EMPLEADOS.
           PERFORM LEER-EMPLEADOS.
           PERFORM GENERAR-EMPINX UNTIL W88-NOEXISTE-EMPLEADOS.
           CLOSE EMPINX.
           CLOSE EMPLEADOS.
           STOP RUN.

       GENERAR-EMPINX.
           DISPLAY "DATOS".
           DISPLAY EMP-NOMINA.
           DISPLAY EMP-NOMBRE.
           DISPLAY EMP-DPTO
           DISPLAY EMP-SUELDO.

           PERFORM LEER-EMPLEADOS.

       LEER-EMPLEADOS.
           READ EMPLEADOS AT END MOVE 1 TO WKS-EMPLEADOS-EOF.

       END PROGRAM YOUR-PROGRAM-NAME.
