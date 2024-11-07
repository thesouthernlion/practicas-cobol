      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 5EMPREP.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPSDO       ASSIGN TO DISK.
           SELECT EMPORD       ASSIGN TO DISK.
           SELECT EMPSORT      ASSIGN TO DISK.
           SELECT REPORTE      ASSIGN TO DISK.


       DATA DIVISION.
       FILE SECTION.
       FD  EMPSDO.
       01  EMPSDO-REG.
           03  EMPSDO-NOMINA       PIC 9(06).
           03  EMPSDO-NOMBRE       PIC X(20).
           03  EMPSDO-DPTO         PIC X(03).
           03  EMPSDO-PERCEP       PIC 9(05)V99.
           03  EMPSDO-DEDU         PIC 9(05)V99.
           03  EMPSDO-SUELDO       PIC S9(05)V99.
           03  FILLER              PIC XX.

       FD  EMPORD.
       01  EMPORD-REG.
           03  EMPORD-NOMINA       PIC 9(06).
           03  EMPORD-NOMBRE       PIC X(20).
           03  EMPORD-DPTO         PIC X(03).
           03  EMPORD-PERCEP       PIC 9(05)V99.
           03  EMPORD-DEDU         PIC 9(05)V99.
           03  EMPORD-SUELDO       PIC S9(05)V99.
           03  FILLER              PIC XX.

       SD  EMPSORT.
       01  EMPSORT-REG.
           03  EMPSORT-NOMINA      PIC 9(06).
           03  EMPSORT-NOMBRE      PIC X(20).
           03  EMPSORT-DPTO        PIC X(03).
           03  EMPSORT-PERCEP      PIC 9(05)V99.
           03  EMPSORT-DEDU        PIC 9(05)V99.
           03  EMPSORT-SUELDO      PIC S9(05)V99.
           03  FILLER              PIC XX.

       FD  REPORTE.
       01  REPORTE-REG         PIC X(132).

       WORKING-STORAGE SECTION.
       01  FECHA.
           03  FECHA-AA            PIC 99.
           03  FECHA-MM            PIC 99.
           03  FECHA-DD            PIC 99.
       01  MESES.
           03  FILLER              PIC X(36) VALUE
           "ENEFEBMARABRMAYJUNJILAGOSEPOCTNOVDIC".
       01  MESES-R REDEFINES MESES.
           03  MESES-OC OCCURS 12 TIMES.
               05  MES                 PIC X(03).
       01  EMPREP.
           03  EMPREP-T1.
               05  FILLER              PIC X(07) VALUE "FECHA: ".
               05  EMPREP-T1-DD        PIC 99.
               05  FILLER              PIC X(01) VALUE "/".
               05  EMPREP-T1-MM        PIC X(03).
               05  FILLER              PIC X(03) VALUE "/20".
               05  EMPREP-T1-AA        PIC 99.
               05  FILLER              PIC X(25) VALUE SPACES.
               05  FILLER              PIC X(14) VALUE "NOMINA".
               05  FILLER              PIC X(20) VALUE SPACES.
               05  FILLER              PIC X(05) VALUE "PAG: ".
               05  EMPREP-T1-PAG       PIC X(02).
           03  EMPREP-T2.
               05  FILLER              PIC X(11) VALUE "EMPREP V1.0".
               05  FILLER              PIC X(25) VALUE SPACES.
               05  FILLER              PIC X(24) VALUE
                   "NOMINAS POR DEPARTAMENTO".
           03  EMPREP-T3.
               05  FILLER              PIC X(6)  VALUE "NOMINA".
               05  FILLER              PIC X(10) VALUE SPACES.
               05  FILLER              PIC X(6)  VALUE "NOMBRE".
               05  FILLER              PIC X(10) VALUE SPACES.
               05  FILLER              PIC X(4)  VALUE "DPTO".
               05  FILLER              PIC X(10) VALUE SPACES.
               05  FILLER              PIC X(13) VALUE "PERCEPCIONES".
               05  FILLER              PIC X(5) VALUE SPACES.
               05  FILLER              PIC X(11) VALUE "DEDUCCIONES".
               05  FILLER              PIC X(6) VALUE SPACES.
               05  FILLER              PIC X(07) VALUE "SALARIO".
           03  EMPREP-DETALLE.
               05  EMPREP-NOMINA       PIC X(06).
               05  FILLER              PIC X(04) VALUE SPACES.
               05  EMPREP-NOMBRE       PIC X(20).
               05  FILLER              PIC X(02) VALUE SPACES.
               05  EMPREP-DPTO         PIC X(03).
               05  FILLER              PIC X(10) VALUE SPACES.
               05  EMPREP-PERCEP       PIC $ZZZ,ZZ9.99.
               05  FILLER              PIC X(8) VALUE SPACES.
               05  EMPREP-DEDU         PIC $ZZZ,ZZ9.99.
               05  FILLER              PIC X(8) VALUE SPACES.
               05  FILLER              PIC X(2) VALUE "$".
      *     05  EMPREP-SUELDO       PIC S9(05)V99.
               05  EMPREP-SUELDO       PIC -ZZZ,ZZ9.99.
           03  EMPREP-DPTO-SUBT.
               05  FILLER              PIC X(9) VALUE "EMP DPTO:".
               05  EMPREP-CONT-SUBT    PIC 99.
               05  FILLER              PIC X(2) VALUE SPACES.
               05  FILLER              PIC X(12) VALUE "PERCEP DPTO:".
      *         05  EMPREP-PERCEP-SUBT  PIC Z(08)V99.
               05  EMPREP-PERCEP-SUBT  PIC $ZZZ,ZZ9.99.
               05  FILLER              PIC X(2) VALUE SPACES.
               05  FILLER              PIC X(11) VALUE "DEDUC DPTO:".
      *         05  EMPREP-DEDU-SUBT    PIC Z(08)V99.
               05  EMPREP-DEDU-SUBT    PIC $ZZZ,ZZ9.99.
               05  FILLER              PIC X(2) VALUE SPACES.
               05  FILLER              PIC X(12) VALUE "SUELDO DPTO:".
      *         05  EMPREP-SUELDO-SUBT  PIC Z(08)V99.
               05  EMPREP-SUELDO-SUBT  PIC $ZZZ,ZZ9.99.
           03  EMPREP-TOT.
               05  FILLER              PIC X(8) VALUE "EMP TOT:".
               05  EMPREP-CONT-TOT     PIC 99.
               05  FILLER              PIC X(3) VALUE SPACES.
               05  FILLER              PIC X(11) VALUE "PERCEP TOT:".
      *         05  EMPREP-PERCEP-TOT   PIC Z(08)V99.
               05  EMPREP-PERCEP-TOT   PIC $ZZZ,ZZ9.99.
               05  FILLER              PIC X(3) VALUE SPACES.
               05  FILLER              PIC X(10) VALUE "DEDUC TOT:".
      *         05  EMPREP-DEDU-TOT     PIC Z(08)V99.
               05  EMPREP-DEDU-TOT     PIC $ZZZ,ZZ9.99.
               05  FILLER              PIC X(3) VALUE SPACES.
               05  FILLER              PIC X(12) VALUE "SUELDO :".
      *         05  EMPREP-SUELDO-TOT   PIC Z(08)V99.
               05  EMPREP-SUELDO-TOT   PIC $ZZZ,ZZ9.99.

       77  EMPORD-EOF      PIC 9 VALUE ZERO.
       77  LINEA-CONT      PIC 99.
       77  MAXIMO-LINEAS   PIC 99 VALUE 9.
       77  PAG-CONT        PIC 99.
       77  DPTO-ANTERIOR   PIC X(03).
       77  EMP-CONT        PIC 9(03).
       77  PERCEP-DPTO     PIC 9(08)V99.
       77  DEDU-DPTO       PIC 9(08)V99.
       77  SUELDO-DPTO     PIC 9(08)V99.
       77  EMP-CONT-TOT    PIC 9(03).
       77  PERCEP-TOT      PIC 9(08)V99.
       77  DEDU-TOT        PIC 9(08)V99.
       77  SUELDO-TOT      PIC 9(08)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-PROCESO.
           PERFORM 300-FIN.
           STOP RUN.

       100-INICIO.
           OPEN OUTPUT REPORTE.


       200-PROCESO.
           PERFORM 230-PROCESO-SORT.
           PERFORM 260-PROCESO-REPORTE.

       230-PROCESO-SORT.
           SORT EMPSORT ON ASCENDING KEY EMPSORT-DPTO EMPSORT-NOMINA
               USING   EMPSDO
               GIVING   EMPORD.

       260-PROCESO-REPORTE.
           OPEN INPUT EMPORD.
           PERFORM 266-LEE-FECHA.
           PERFORM 263-LEE-EMPORD.
           MOVE EMPORD-DPTO TO DPTO-ANTERIOR.
           ADD 1 TO MAXIMO-LINEAS GIVING LINEA-CONT.
           PERFORM 270-GENERA-DETALLE UNTIL EMPORD-EOF = 1.
           PERFORM 273-CORTE-DPTOS.
           PERFORM 276-CORTE-TOTAL.
           DISPLAY "SE GENERO REPORTE ULTIMA INSTRUCCION 276".

       263-LEE-EMPORD.
           READ EMPORD AT END MOVE 1 TO EMPORD-EOF.

       266-LEE-FECHA.
           ACCEPT FECHA FROM DATE.
           MOVE FECHA-DD       TO EMPREP-T1-DD
           MOVE MES(FECHA-MM)  TO EMPREP-T1-MM.
           MOVE FECHA-AA       TO EMPREP-T1-AA.

       270-GENERA-DETALLE.
           IF DPTO-ANTERIOR NOT = EMPORD-DPTO
               PERFORM 273-CORTE-DPTOS.
           IF LINEA-CONT >= MAXIMO-LINEAS
               PERFORM 279-GENERAR-TITULOS.
           MOVE EMPORD-NOMINA  TO EMPREP-NOMINA.
           MOVE EMPORD-NOMBRE  TO EMPREP-NOMBRE.
           MOVE EMPORD-DPTO    TO EMPREP-DPTO.
           MOVE EMPORD-PERCEP  TO EMPREP-PERCEP.
           MOVE EMPORD-DEDU    TO EMPREP-DEDU.
           MOVE EMPORD-SUELDO  TO EMPREP-SUELDO.
           WRITE REPORTE-REG   FROM EMPREP-DETALLE BEFORE 1 LINE.
           ADD 1 TO EMP-CONT.
           ADD 1 TO LINEA-CONT.
           ADD EMPORD-PERCEP   TO PERCEP-DPTO.
           ADD EMPORD-DEDU     TO DEDU-DPTO.
           IF  EMPORD-SUELDO > 0
               ADD EMPORD-SUELDO   TO SUELDO-DPTO.
           PERFORM 263-LEE-EMPORD.

       273-CORTE-DPTOS.
           MOVE    EMP-CONT        TO EMPREP-CONT-SUBT.
           MOVE    PERCEP-DPTO     TO EMPREP-PERCEP-SUBT.
           MOVE    DEDU-DPTO       TO EMPREP-DEDU-SUBT.
           MOVE    SUELDO-DPTO     TO EMPREP-SUELDO-SUBT.
           WRITE   REPORTE-REG     FROM SPACES BEFORE 1 LINE.
           WRITE   REPORTE-REG     FROM EMPREP-DPTO-SUBT BEFORE 2 LINES.
           ADD 1                   TO MAXIMO-LINEAS
                                   GIVING  LINEA-CONT.
           ADD     EMP-CONT        TO EMP-CONT-TOT.
           ADD     PERCEP-DPTO     TO PERCEP-TOT.
           ADD     DEDU-DPTO       TO DEDU-TOT.
           ADD     SUELDO-DPTO     TO SUELDO-TOT.
           MOVE    0 TO EMP-CONT,PERCEP-DPTO,DEDU-DPTO,SUELDO-DPTO.
           MOVE    EMPORD-DPTO     TO DPTO-ANTERIOR.

       276-CORTE-TOTAL.
           MOVE    EMP-CONT-TOT    TO EMPREP-CONT-TOT.
           MOVE    PERCEP-TOT      TO EMPREP-PERCEP-TOT.
           MOVE    DEDU-TOT        TO EMPREP-DEDU-TOT.
           MOVE    SUELDO-TOT      TO EMPREP-SUELDO-TOT.
           WRITE   REPORTE-REG     FROM EMPREP-TOT BEFORE 1 LINE.

       279-GENERAR-TITULOS.
           ADD     1               TO PAG-CONT.
           MOVE    PAG-CONT        TO EMPREP-T1-PAG.
           WRITE   REPORTE-REG     FROM EMPREP-T1 BEFORE PAGE.
           WRITE   REPORTE-REG     FROM EMPREP-T2 BEFORE 3 LINES.
           WRITE   REPORTE-REG     FROM EMPREP-T3 BEFORE 1 LINES
           MOVE    5               TO LINEA-CONT.

       300-FIN.
           CLOSE REPORTE.
           CLOSE EMPORD.

       END PROGRAM 5EMPREP.
