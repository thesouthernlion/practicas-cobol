      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 4EMPABC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPINX ASSIGN TO DISK
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS EMPINX-NOMINA.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPINX.
       01  EMPINX-REG.
           03  EMPINX-NOMINA   PIC 9(06).
           03  EMPINX-NOMBRE   PIC X(20).
           03  EMPINX-DPTO     PIC X(03).
           03  EMPINX-SUELDO   PIC S9(05)V99.


       WORKING-STORAGE SECTION.
       77  BANDERA-SALIR-MENU  PIC 9.
       77  OPCION-MENU         PIC 9.
       77  EMPINX-KEY-EXISTE   PIC 9.
       77  AUTORIZACION        PIC 9(03).
       77  WKS-RESP PIC X(02).
           88  W88-NO VALUE "NO".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE 0 TO BANDERA-SALIR-MENU.
           OPEN I-O EMPINX.
           PERFORM MOSTRAR-MENU UNTIL BANDERA-SALIR-MENU = 1.
           CLOSE EMPINX
           STOP RUN.

      *******************************************************************
      *                                LEER
      *******************************************************************
       LEER-EMPINX.
           MOVE 1 TO EMPINX-KEY-EXISTE.
           READ EMPINX INVALID KEY MOVE 0 TO EMPINX-KEY-EXISTE.
      *******************************************************************
      *                             ALTA
      *******************************************************************
       ALTA.
           DISPLAY "INGRESA NOMINA"
           ACCEPT EMPINX-NOMINA
           PERFORM LEER-EMPINX
           IF EMPINX-KEY-EXISTE = 1
               DISPLAY "EMPLEADO YA EXISTE"
           ELSE
               DISPLAY "INGRESE EL NOMBRE"
               ACCEPT EMPINX-NOMBRE
               DISPLAY "INGRESE EL DEPARTAMENTO"
               ACCEPT EMPINX-DPTO
               WRITE EMPINX-REG.
               ADD 1 TO AUTORIZACION
               DISPLAY "OPERACION EXITOSA: ", AUTORIZACION.

      *******************************************************************
      *                             BAJA
      *******************************************************************
       BAJA.
           DISPLAY "INGRESA NOMINA"
           ACCEPT EMPINX-NOMINA
           PERFORM LEER-EMPINX
           IF EMPINX-KEY-EXISTE = 1
               DISPLAY "NOMINA:        ", EMPINX-NOMBRE
               DISPLAY "NOMBRE:        ", EMPINX-NOMINA
               DISPLAY "DEPARTAMENTO:  ", EMPINX-DPTO
               DISPLAY "QUIERES DAR DE BAJA?"
               ACCEPT WKS-RESP
               IF NOT W88-NO
                   DELETE EMPINX
                   ADD 1 TO AUTORIZACION
                   DISPLAY "OPERACION EXITOSA: ", AUTORIZACION
                ELSE
                   DISPLAY "BAJA CANCELADA"
           ELSE
               DISPLAY "EMPLEADO NO EXISTE".

      *******************************************************************
      *                           CONSULTA
      *******************************************************************
       CONSULTA.
           DISPLAY "INGRESA NOMINA"
           ACCEPT EMPINX-NOMINA.
           PERFORM LEER-EMPINX.
           IF EMPINX-KEY-EXISTE = 1
               DISPLAY "NOMINA:        ", EMPINX-NOMINA
               DISPLAY "NOMBRE:        ", EMPINX-NOMBRE
               DISPLAY "DEPARTAMENTO:  ", EMPINX-DPTO
           ELSE
               DISPLAY "NOMINA NO EXISTE".

      *******************************************************************
      *                            MODIFICAR
      *******************************************************************
       MODIFICAR.
           DISPLAY "INGRESA LA NOMINA"
           ACCEPT EMPINX-NOMINA
           PERFORM LEER-EMPINX.
           IF EMPINX-KEY-EXISTE = 1
               DISPLAY "NOMINA:        ", EMPINX-NOMINA
               DISPLAY "NOMBRE:        ", EMPINX-NOMBRE
               DISPLAY "DEPARTAMENTO:  ", EMPINX-DPTO

               DISPLAY "DESEA MODIFICAR?"
               ACCEPT WKS-RESP
               IF NOT W88-NO
                   DISPLAY "INGRESE NOMBRE"
                   ACCEPT EMPINX-NOMBRE
                   DISPLAY "INGRESE DEPARTAMENTO"
                   ACCEPT EMPINX-DPTO
                   REWRITE EMPINX-REG
                   ADD 1 TO AUTORIZACION
                   DISPLAY "OPERACION EXITOSA: ", AUTORIZACION
               ELSE
                   DISPLAY "MODIFICACION CANCELADA"
           ELSE
               DISPLAY "NOMINA NO EXISTE".

      *******************************************************************
      *                               MENU
      *******************************************************************
       MOSTRAR-MENU.
           MOVE "NO" TO WKS-RESP.
           MOVE 0 TO OPCION-MENU.
           DISPLAY "------------".
           DISPLAY "   EMPIX".
           DISPLAY "------------".
           DISPLAY "SELECCIONA UNA OPCION".
           DISPLAY "1. ALTA.".
           DISPLAY "2. BAJA.".
           DISPLAY "3. CONSULTA.".
           DISPLAY "4. MODIFICACION.".
           DISPLAY "5. SALIR."
           ACCEPT OPCION-MENU.
           IF OPCION-MENU = 1
               PERFORM ALTA
           ELSE IF OPCION-MENU = 2
               PERFORM BAJA
           ELSE IF OPCION-MENU = 3
               PERFORM CONSULTA
           ELSE IF OPCION-MENU = 4
               PERFORM MODIFICAR
           ELSE IF OPCION-MENU = 5
               DISPLAY "ADIOS"
               MOVE 1 TO BANDERA-SALIR-MENU.


       END PROGRAM 4EMPABC.
