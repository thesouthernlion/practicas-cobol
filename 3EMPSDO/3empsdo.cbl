      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3EMPSDO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS    ASSIGN TO DISK.
           SELECT EMPSDO      ASSIGN TO DISK.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLEADOS.
       01  EMP-REG.
           03  EMP-NOMINA  PIC 9(06).
           03  EMP-NOMBRE  PIC X(20).
           03  EMP-DPTO    PIC X(03).
           03  EMP-PERCEP  PIC 9(05)V99.
           03  EMP-DEDU    PIC 9(05)V99.
           03 FILLER       PIC XX.

       FD  EMPSDO.
       01  EMPSDO-REG.
           03  EMPSDO-NOMINA   PIC 9(06).
           03  EMPSDO-NOMBRE   PIC X(20).
           03  EMPSDO-DPTO     PIC X(03).
           03  EMPSDO-PERCEP   PIC 9(05)V99.
           03  EMPSDO-DEDU     PIC 9(05)V99.
           03  EMPSDO-SUELDO   PIC S9(05)V99.
           03 FILLER       PIC XX.

       WORKING-STORAGE SECTION.
       77  WKS-EMPLEADOS-EOF PIC 9 VALUE ZERO.
           88  W88-EXISTE-EMPLEADOS      VALUE 0.
           88  W88-NOEXISTE-EMPLEADOS    VALUE 1.
       77  CONT-EMP-LEIDOS         PIC 999.
       77  CONT-EMP-GRABADOS         PIC 999.
       77  SUELDO                  PIC S9(05)V99.
       77  SUELDO-TOTAL-EMPRESA    PIC 9(05)V99.
       77  SUELDO-TOTAL-ADM        PIC 9(05)V99.
       77  SUELDO-TOTAL-CON        PIC 9(05)V99.
       77  SUELDO-TOTAL-MER        PIC 9(05)V99.
       77  SUELDO-TOTAL-SIS        PIC 9(05)V99.
       77  SUELDO-TOTAL-RH         PIC 9(05)V99.
       77  SUELDO-TOTAL-TEC        PIC 9(05)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT EMPLEADOS
           OPEN OUTPUT EMPSDO
           PERFORM LEER-EMPLEADOS
           PERFORM GENERAR-EMPSDO UNTIL W88-NOEXISTE-EMPLEADOS.

           DISPLAY "TOTAL DE EMPLEADOS LEIDOS  : ", CONT-EMP-LEIDOS
           DISPLAY "TOTAL DE EMPLEADOS GRABADOS: ", CONT-EMP-GRABADOS
           DISPLAY "SUELDO TOTAL EMPRESA   :   ", SUELDO-TOTAL-EMPRESA
     *     DISPLAY "SUELDO TOTAL ADM       :   ", SUELDO-TOTAL-ADM
           DISPLAY "SUELDO TOTAL CON       :   ", SUELDO-TOTAL-CON
           DISPLAY "SUELDO TOTAL MER       :   ", SUELDO-TOTAL-MER
           DISPLAY "SUELDO TOTAL SIS       :   ", SUELDO-TOTAL-SIS
           DISPLAY "SUELDO TOTAL RH        :   ", SUELDO-TOTAL-RH
           DISPLAY "SUELDO TOTAL TEC       :   ", SUELDO-TOTAL-TEC

           CLOSE EMPLEADOS
           CLOSE EMPSDO
           STOP RUN.

       GENERAR-EMPSDO.
           MOVE EMP-REG TO EMPSDO-REG
           ADD 1 TO CONT-EMP-LEIDOS.
           COMPUTE SUELDO = EMPSDO-PERCEP - EMPSDO-DEDU.
           MOVE SUELDO TO EMPSDO-SUELDO.
           IF SUELDO > 0
               PERFORM SUMAR-SUELDO.
           WRITE EMPSDO-REG.
           ADD 1 TO CONT-EMP-GRABADOS.
           PERFORM LEER-EMPLEADOS.

       LEER-EMPLEADOS.
           READ EMPLEADOS AT END MOVE 1 TO WKS-EMPLEADOS-EOF.

       SUMAR-SUELDO.
           COMPUTE SUELDO-TOTAL-EMPRESA = SUELDO-TOTAL-EMPRESA + SUELDO.
           IF EMPSDO-DPTO = "ADM"
               COMPUTE SUELDO-TOTAL-ADM = SUELDO-TOTAL-ADM + SUELDO
           ELSE IF EMPSDO-DPTO = "CON"
               COMPUTE SUELDO-TOTAL-CON = SUELDO-TOTAL-CON + SUELDO
           ELSE IF EMPSDO-DPTO = "MER"
               COMPUTE SUELDO-TOTAL-MER = SUELDO-TOTAL-MER + SUELDO
           ELSE IF EMPSDO-DPTO = "SIS"
               COMPUTE SUELDO-TOTAL-SIS = SUELDO-TOTAL-SIS + SUELDO
           ELSE IF EMPSDO-DPTO = "RH"
               COMPUTE SUELDO-TOTAL-RH = SUELDO-TOTAL-RH + SUELDO
           ELSE IF EMPSDO-DPTO = "TEC"
               COMPUTE SUELDO-TOTAL-TEC = SUELDO-TOTAL-TEC + SUELDO.

       END PROGRAM 3EMPSDO.
