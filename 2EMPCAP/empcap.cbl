      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPCAP.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLEADOS ASSIGN TO DISK.
      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLEADOS.
       01  EMP-REG.
           03  EMP-NOMINA PIC 9(06).
           03  EMP-NOMBRE PIC X(20).
           03  EMP-DPTO   PIC X(03).
           03  EMP-PERCEP PIC 9(05)V99.
           03  EMP-DEDU   PIC 9(05)V99.
           03  FILLER     PIC XX.

       WORKING-STORAGE SECTION.
       77  BANDERA-DPTO PIC 9.
       77  WKS-RESP PIC X(02).
           88  W88-NO VALUE "NO".
       77  DPTOS-VALIDOS PIC XXX.
           88  DPTO-OK VALUE "ADM", "CON", "MER", "SIS", "RH", "TEC".
       01  CONT-EMP    PIC 999.
       01  CONT-PERCEP PIC 9(05)V99.
       01  CONT-DEDU   PIC 9(05)V99.
      *****CONTADORES DE DEPARTAMENTO
       01  CONT-DPTOS.
           03  CONT-DPTO-OC OCCURS 6 TIMES.
               05  CONT-DPTO PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT EMPLEADOS.
           MOVE "SI" TO WKS-RESP
           PERFORM CAPTURA-EMPLEADOS UNTIL W88-NO.

           DISPLAY "TOTAL EMPLEADOS     : ", CONT-EMP.
           DISPLAY "EMPLEADOS EN ADM    : ", CONT-DPTO(1)
           DISPLAY "EMPLEADOS EN CON    : ", CONT-DPTO(2)
           DISPLAY "EMPLEADOS EN MER    : ", CONT-DPTO(3)
           DISPLAY "EMPLEADOS EN SIS    : ", CONT-DPTO(4)
           DISPLAY "EMPLEADOS EN RH     : ", CONT-DPTO(5)
           DISPLAY "EMPLEADOS EN TEC    : ", CONT-DPTO(6)
           DISPLAY "TOTAL PERCEPCIONES  : ", CONT-PERCEP
           DISPLAY "TOTAL DEDUCCIONES   : ", CONT-DEDU
           CLOSE EMPLEADOS.
           STOP RUN.

       CAPTURA-EMPLEADOS.
           MOVE 0 TO BANDERA-DPTO.
           DISPLAY "DAME NOMINA: "
           ACCEPT EMP-NOMINA

           DISPLAY "DAME NOMBRE: "
           ACCEPT EMP-NOMBRE

           PERFORM VALIDACION-DPTOS UNTIL BANDERA-DPTO = 1

           DISPLAY "DAME PERCEPCIONES"
           ACCEPT EMP-PERCEP

           DISPLAY "DAME DEDUCCIONES"
           ACCEPT EMP-DEDU.
           WRITE EMP-REG.
      *****CONTADOR EMPLEADOS
           ADD 1 TO CONT-EMP.
      *****CONTADOR DEPARTAMENTOS
           IF      EMP-DPTO = "ADM" ADD 1 TO CONT-DPTO(1)
           ELSE IF EMP-DPTO = "CON" ADD 1 TO CONT-DPTO(2)
           ELSE IF EMP-DPTO = "MER" ADD 1 TO CONT-DPTO(3)
           ELSE IF EMP-DPTO = "SIS" ADD 1 TO CONT-DPTO(4)
           ELSE IF EMP-DPTO = "RH"  ADD 1 TO CONT-DPTO(5)
           ELSE IF EMP-DPTO = "TEC" ADD 1 TO CONT-DPTO(6).

      *****CONTADOR PERCEPCIONES
           ADD EMP-PERCEP  TO CONT-PERCEP.
      *****CONTADOR DEDUCCIONES
           ADD EMP-DEDU    TO CONT-DEDU.
           DISPLAY "DESEAS SEGUIR CAPTURANDO EMPLEADOS: ".
           ACCEPT WKS-RESP.

       VALIDACION-DPTOS.
           DISPLAY "DAME DEPARTAMENTO"
           ACCEPT EMP-DPTO.
           MOVE EMP-DPTO TO DPTOS-VALIDOS.
           IF NOT DPTO-OK
               DISPLAY "INGRESA UN GRUPO VALIDO"
               MOVE 0 TO BANDERA-DPTO
           ELSE
               MOVE 1 TO BANDERA-DPTO.
       END PROGRAM EMPCAP.
