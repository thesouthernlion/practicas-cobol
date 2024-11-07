      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 5ALUREP.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUPRO      ASSIGN TO DISK.
           SELECT ALUORD      ASSIGN TO DISK.
           SELECT ALUSORT     ASSIGN TO DISK.
           SELECT REPORTE      ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.
       FD  ALUPRO.
       01  ALUPRO-REG.
           03  ALUPRO-MAT     PIC 9(08).
           03  ALUPRO-NOM     PIC X(30).
           03  ALUPRO-GPO     PIC X(03).
           03  ALUPRO-CALIFICACIONES  OCCURS 6 TIMES.
               05  ALUPRO-CAL     PIC 999V99.
           03  ALUPRO-PROM    PIC 999V99.
           03  FILLER          PIC XX.

       FD  ALUORD.
       01  ALUORD-REG.
           03  ALUORD-MAT     PIC 9(08).
           03  ALUORD-NOM     PIC X(30).
           03  ALUORD-GPO     PIC X(03).
           03  ALUORD-CALIFICACIONES  OCCURS 6 TIMES.
               05  ALUORD-CAL     PIC 999V99.
           03  ALUORD-PROM    PIC 999V99.
           03  FILLER          PIC XX.

       SD  ALUSORT.
       01  ALUSORT-REG.
           03  ALUSORT-MAT    PIC 9(08).
           03  ALUSORT-NOM    PIC X(30).
           03  ALUSORT-GPO    PIC X(03).
           03  ALUSORT-CALIFICACIONES OCCURS 6 TIMES.
               05  ALUSORT-CAL    PIC 999V99.
           03  ALUSORT-PROM   PIC 999V99.
           03  FILLER          PIC XX.

       FD  REPORTE.
       01  REPORTE-REG         PIC X(132).

       WORKING-STORAGE SECTION.
       01  TITULOS.
           03  DETALLE.
               05  DET-MATRICULA   PIC 9(08).
               05  FILLER          PIC X(04)   VALUE SPACES.
               05  DET-NOMBRE      PIC X(30).
               05  FILLER          PIC X(03) VALUE SPACES.
               05  DET-GRUPO       PIC X(05).
               05  FILLER          PIC X(03) VALUE SPACES.
               05  DET-CAL1        PIC 9(03)V99.
               05  FILLER          PIC X(04) VALUE SPACES.
               05  DET-CAL2        PIC 9(03)V99.
               05  FILLER          PIC X(04) VALUE SPACES.
               05  DET-CAL3        PIC 9(03)V99.
               05  FILLER          PIC X(04) VALUE SPACES.
               05  DET-CAL4        PIC 9(03)V99.
               05  FILLER          PIC X(06) VALUE SPACES.
               05  DET-CAL5        PIC 9(03)V99.
               05  FILLER          PIC X(06) VALUE SPACES.
               05  DET-CAL6        PIC 9(03)V99.
               05  FILLER          PIC X(04) VALUE SPACES.
               05  DET-PROM        PIC 9(03)V99.
               05  FILLER          PIC X(03) VALUE SPACES.

       77  ALUORD-EOF         PIC 9 VALUE ZERO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SORT ALUSORT ON ASCENDING KEY ALUSORT-GPO
                                           ALUSORT-MAT
               USING   ALUPRO
               GIVING  ALUORD.
           OPEN INPUT ALUORD.
           PERFORM LEER-ALUORD.
           IF ALUORD-EOF > 0
               DISPLAY "1".
           CLOSE ALUORD.
           STOP RUN.

       LEER-ALUORD.
           READ ALUORD AT END MOVE 1 TO ALUORD-EOF.

       GENERA-DETALLE.

       END PROGRAM 5ALUREP.
