      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3ALUPRO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUMNOS  ASSIGN TO DISK.
           SELECT ALUPRO   ASSIGN TO DISK.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  ALUMNOS.
       01  ALU-REG.
         03 ALU-NOM  PIC X(30).
         03 ALU-MAT  PIC 9(08).
         03 ALU-GPO  PIC X(03).
         03 ALU-CALIFICACIONES OCCURS 6 TIMES.
           05 ALU-CAL    PIC 999V99.
         03 FILLER       PIC XX.
       FD  ALUPRO.
       01  ALUPRO-REG.
         03 ALUPRO-NOM  PIC X(30).
         03 ALUPRO-MAT  PIC 9(08).
         03 ALUPRO-GPO  PIC X(03).
         03 ALUPRO-CALIFICACIONES OCCURS 6 TIMES.
           05 ALUPRO-CAL    PIC 999V99.
         03 ALUPRO-PROM PIC 999V99.
         03 FILLER       PIC XX.

       WORKING-STORAGE SECTION.
       77  WKS-ALUMNOS-EOF PIC 9 VALUE ZERO.
           88  W88-EXISTE-ALUMNOS      VALUE 0.
           88  W88-NOEXISTE-ALUMNOS    VALUE 1.
       01  CONT-ALUMN PIC 999.
       01  PROM-TOTAL PIC 999V99.
       01  SALIDA-DETALLE.
           03  SALIDA-NOMBRE   PIC X(30).
           03  FILLER          PIC X VALUE SPACE.
           03  SALIDA-MAT      PIC 9(08).
           03  FILLER          PIC X VALUE SPACE.
           03  SALIDA-GPO      PIC X(03).
           03  FILLER          PIC X VALUE SPACE.
           03  SALIDA-CALIFICACIONES OCCURS 6 TIMES.
               05 SALIDA-CAL   PIC 999V99.
               05 FILLER       PIC X VALUE SPACE.
           03 SALIDA-PROM      PIC 999V99.
       77  I   PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT ALUMNOS
           OPEN OUTPUT ALUPRO
           PERFORM LEER-ALUMNOS
           PERFORM GENERAR-ALUPRO UNTIL W88-NOEXISTE-ALUMNOS.
           COMPUTE PROM-TOTAL = PROM-TOTAL/CONT-ALUMN.
           DISPLAY "TOTAL DE ALUMNOS:  ", CONT-ALUMN.
           DISPLAY "PROMEDIO TOTAL:    ", PROM-TOTAL.
           CLOSE ALUMNOS
           CLOSE ALUPRO.
           STOP RUN.

       GENERAR-ALUPRO.
           MOVE ALU-REG TO ALUPRO-REG
           COMPUTE ALUPRO-PROM = (ALUPRO-CAL(1) +
           ALUPRO-CAL(2) +
           ALUPRO-CAL(3) +
           ALUPRO-CAL(4) +
           ALUPRO-CAL(5) +
           ALUPRO-CAL(6)) / 6.
           WRITE ALUPRO-REG.
           PERFORM DESPLIEGA-ALUPRO
           ADD 1 TO CONT-ALUMN.
           COMPUTE PROM-TOTAL = PROM-TOTAL + ALUPRO-PROM
           PERFORM LEER-ALUMNOS.

       LEER-ALUMNOS.
           READ ALUMNOS AT END MOVE 1 TO WKS-ALUMNOS-EOF.

       DESPLIEGA-ALUPRO.
           MOVE ALUPRO-NOM             TO SALIDA-NOMBRE.
           MOVE ALUPRO-MAT             TO SALIDA-MAT.
           MOVE ALUPRO-GPO             TO SALIDA-GPO.
           PERFORM LLENA-CAL-SALIDA VARYING I FROM 1 BY 1 UNTIL I > 6.
           MOVE ALUPRO-PROM            TO SALIDA-PROM.
           DISPLAY SALIDA-DETALLE.

       LLENA-CAL-SALIDA.
           MOVE ALUPRO-CAL(I) TO SALIDA-CAL(I).

       END PROGRAM 3ALUPRO.
