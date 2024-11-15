      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 6EMPNOM.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PERCEPCION-DEDUCCION ASSIGN TO DISK.
           SELECT EMPSORT              ASSIGN TO DISK.
           SELECT REPORTE              ASSIGN TO DISK.
           SELECT EMPINX               ASSIGN TO DISK
           ORGANIZATION IS INDEXED ACCESS MODE IS DYNAMIC
           RECORD KEY IS EMPINX-NOMINA.

       DATA DIVISION.
       FILE SECTION.
       FD  PERCEPCION-DEDUCCION.
       01  PERCEPCION-DEDUCCION-REG.
           03  PERCEPCION-DEDUCCION-NOMINA     PIC 9(06).
           03  PERCEPCION-DEDUCCION-CLAVE      PIC XX.
           03  PERCEPCION-DEDUCCION-IMPORTE    PIC 9(08).
           03  FILLER                          PIC X(02).
       FD  EMPINX.
       01  EMPINX-REG.
           03  EMPINX-NOMINA                   PIC 9(06).
           03  EMPINX-NOMBRE                   PIC X(20).
           03  EMPINX-DPTO                     PIC X(03).
           03  EMPINX-SUELDO                   PIC S9(05)V99.
       SD  EMPSORT.
       01  EMPSORT-REG.
           03  EMPSORT-NOMINA                  PIC 9(06).
           03  EMPSORT-DPTO                    PIC X(04).
           03  EMPSORT-NOMBRE                  PIC X(20).
           03  EMPSORT-CLAVE                   PIC XX.
           03  EMPSORT-IMPORTE                 PIC 9(08).


       FD  REPORTE.
       01  REPORTE-REG                         PIC X(132).

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
       01  DETALLES-REPORTE.
           03  TITULO-1.
               05  FILLER                      PIC X(07) VALUE
                   "FECHA: ".
               05  T1-DD                       PIC 99.
               05  FILLER                      PIC X(01) VALUE "/".
               05  T1-MM                       PIC X(03).
               05  FILLER                      PIC X(03) VALUE "/20".
               05  T1-AA                       PIC 99.
               05  FILLER                      PIC X(25) VALUE SPACES.
               05  FILLER                      PIC X(14) VALUE "NOMINA".
               05  FILLER                      PIC X(20) VALUE SPACES.
               05  FILLER                      PIC X(05) VALUE "PAG: ".
               05  T1-PAG                      PIC X(02).
           03  TITULO-2.
               05  FILLER                      PIC X(11) VALUE
                   "EMPNOM V1.0".
               05  FILLER                      PIC X(25) VALUE SPACES.
               05  FILLER                      PIC X(24) VALUE
                   "NOMINAS POR DEPARTAMENTO".
           03  TITULO-3.
               05  FILLER                      PIC X(6)  VALUE "NOMINA".
               05  FILLER                      PIC X(10) VALUE SPACES.
               05  FILLER                      PIC X(6)  VALUE "NOMBRE".
               05  FILLER                      PIC X(10) VALUE SPACES.
               05  FILLER                      PIC X(4)  VALUE "DPTO".
               05  FILLER                      PIC X(10) VALUE SPACES.
               05  FILLER                      PIC X(8)  VALUE
                   "CONCEPTO".
               05  FILLER                      PIC X(10) VALUE SPACES.
               05  FILLER                      PIC X(13) VALUE
                   "PERCEPCIONES".
               05  FILLER                      PIC X(5) VALUE SPACES.
               05  FILLER                      PIC X(11) VALUE
                   "DEDUCCIONES".
               05  FILLER                      PIC X(6) VALUE SPACES.
               05  FILLER                      PIC X(07) VALUE
                   "SALARIO".
           03  DETALLE.
               05  DETALLE-NOMINA              PIC X(06).
               05  FILLER                      PIC X(04) VALUE SPACES.
               05  DETALLE-NOMBRE              PIC X(20).
               05  FILLER                      PIC X(02) VALUE SPACES.
               05  DETALLE-DPTO                PIC X(04).
               05  FILLER                      PIC X(02) VALUE SPACES.
               05  DETALLE-CLAVE               PIC X(04).
               05  FILLER                      PIC X(20) VALUE SPACES.
               05  DETALLE-PERCEPCION          PIC $ZZZ,ZZ9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  DETALLE-DEDUCCION           PIC $ZZZ,ZZ9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  DETALLE-SUELDO              PIC $$$,$$9.99.
           03  DETALLE-SUBTOTAL-NOMINA.
               05  FILLER                      PIC X(15) VALUE
                   "SUBTOTAL NOMINA".
               05  FILLER                      PIC X(02) VALUE SPACES.
               05  FILLER                      PIC X(15) VALUE
                   "PERCEPCIONES: ".
               05  DET-SUBTOT-NOMINA-PERCEP    PIC $ZZZ,ZZ9.99.
               05  FILLER                      PIC X(02) VALUE SPACES.
               05  FILLER                      PIC X(13) VALUE
                   "DEDUCCIONES: ".
               05  DET-SUBTOT-NOMINA-DEDUC     PIC $ZZZ,ZZ9.99.
               05  FILLER                      PIC X(02) VALUE SPACES.
               05  FILLER                      PIC X(08) VALUE
                   "SUELDO: ".
               05  DET-SUBTOT-NOMINA-SUELDO    PIC $(08).99-.
           03  DET-SUBOTOTAL-DPTO.
               05  FILLER                      PIC X(13) VALUE
                   "SUBTOTAL DPTO".
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(15) VALUE
                   "EMPLEADOS: ".
               05  DET-EMPLEADOS-DPTO          PIC X(04).
               05  FILLER                      PIC X(15) VALUE
                   "PERCEPCIONES: ".
               05  DET-SUBTOT-DPTO-PERCEP      PIC $ZZZ,ZZ9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(13) VALUE
                   "DEDUCCIONES: ".
               05  DET-SUBTOT-DPTO-DEDUC       PIC $ZZZ,ZZ9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(08) VALUE
                   "SUELDO: ".
               05  DET-SUBTOT-DPTO-SUELDO      PIC ZZZ,ZZ9.99-.
           03  DET-TOTAL-EMPRESA.
               05  FILLER                      PIC X(16) VALUE
                   "SUBTOTAL EMPRESA".
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(15) VALUE
                   "EMPLEADOS: ".
               05  DET-EMPLEADOS-EMPRESA       PIC X(04).
               05  FILLER                      PIC X(15) VALUE
                   "PERCEPCIONES: ".
               05  DET-TOTAL-EMPRESA-PERCEP    PIC $ZZZ,ZZ9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(13) VALUE
                   "DEDUCCIONES: ".
               05  DET-TOTAL-EMPRESA-DEDUC     PIC $ZZZ,ZZ9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(08) VALUE
                   "SUELDO: ".
               05  DET-TOTAL-EMPRESA-SUELDO    PIC -$ZZZ,ZZ9.99.

       77  BANDERA-UTLIMO-TITULO           PIC 9.
       77  BANDERA-PRIMER-TITULO           PIC 9.
       77  REPORTE-CLEANER                 PIC X(132) VALUE " ".
       77  BANDERA-IMPRIME-NOMBRE          PIC 9.
       77  CLEANER                         PIC $ZZZ,ZZ9.99 VALUE ZERO.
       77  BANDERA-CLAVE                   PIC 9 VALUE 6.
       77  PERCEPCION-DEDUCCION-EOF        PIC 9.
       77  EMPSORT-EOF                     PIC 9.
       77  EXISTE-NOMINA                   PIC 9.
       77  MAXIMO-LINEAS                   PIC 99 VALUE 15.
       77  CONT-LINEAS                     PIC 99.
       77  NOMINA-ANTERIOR                 PIC 9(06).
       77  DPTO-ANTERIOR                   PIC X(04).
       77  CONTADOR-REGISTROS-NOMINA       PIC 9(03).
       77  CONTADOR-EMPLEADOS-DPTO         PIC 9(03).
       77  CONTADOR-EMPLEADOS-EMPRESA      PIC 9(03).
       77  CONTADOR-PAGINAS                PIC 9(03).

       77  SUBTOTAL-PERCEPCION-NOMINA      PIC 9(08).
       77  SUBTOTAL-DEDUCCION-NOMINA       PIC 9(08).
       77  SUBTOTAL-SUELDO-NOMINA          PIC S9(08).

       77  SUBTOTAL-PERCEPCION-DPTO        PIC 9(08).
       77  SUBTOTAL-DEDUCCION-DPTO         PIC 9(08).
       77  SUBTOTAL-SUELDO-DPTO            PIC 9(08).

       77  TOTAL-PERCEPCION-EMPRESA        PIC 9(08).
       77  TOTAL-DEDUCCION-EMPRESA         PIC 9(08).
       77  TOTAL-SUELDO-EMPRESA            PIC 9(08).


       PROCEDURE DIVISION .
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-PROCESO.

       GENERAR-EMPSORT SECTION.
           PERFORM 230-LEER-PERCEPCION-DEDUCCION.
           PERFORM 260-LLENA-EMPSORT UNTIL PERCEPCION-DEDUCCION-EOF = 1.
           GO TO FIN-GENERA-EMPSORT.

       FIN-GENERA-EMPSORT.

       GENERAR-REPORTE SECTION.
           PERFORM 236-LEER-EMPSORT
           MOVE EMPSORT-NOMINA TO NOMINA-ANTERIOR.
           MOVE EMPSORT-DPTO    TO DPTO-ANTERIOR.
           MOVE 1 TO BANDERA-IMPRIME-NOMBRE.
           MOVE 1 TO BANDERA-PRIMER-TITULO.

           ADD 1 TO MAXIMO-LINEAS  GIVING CONT-LINEAS.
           PERFORM 270-GENERA-DETALLE UNTIL EMPSORT-EOF = 1.

           PERFORM 273-CORTE-NOMINA.
           PERFORM 274-CORTE-DPTO
           PERFORM 300-FIN.
           STOP RUN.

       100-INICIO.
           DISPLAY "INICIO"
           OPEN I-O EMPINX.
           OPEN OUTPUT REPORTE.
           OPEN INPUT PERCEPCION-DEDUCCION.

       200-PROCESO.
           SORT EMPSORT ON  ASCENDING KEY
           EMPSORT-DPTO EMPSORT-NOMINA EMPSORT-CLAVE
               INPUT PROCEDURE GENERAR-EMPSORT
               OUTPUT PROCEDURE GENERAR-REPORTE.

       230-LEER-PERCEPCION-DEDUCCION.
           READ PERCEPCION-DEDUCCION
           AT END MOVE 1 TO PERCEPCION-DEDUCCION-EOF.

       233-LEER-INDEXADO.
           MOVE 1 TO EXISTE-NOMINA.
           READ EMPINX INVALID KEY MOVE 0 TO EXISTE-NOMINA.

       236-LEER-EMPSORT.
           RETURN EMPSORT AT END MOVE 1 TO EMPSORT-EOF.

       240-OBTENER-NOMBRE-DPTO-EMPINX.
           MOVE EMPINX-NOMBRE  TO EMPSORT-NOMBRE
           MOVE EMPINX-DPTO    TO EMPSORT-DPTO.

      *     IF EMPINX-NOMINA > 0
      *         MOVE 0 TO EXISTE-NOMINA.

      *     IF EXISTE-NOMINA = 1
      *         MOVE EMPINX-NOMBRE  TO EMPSORT-NOMBRE
      *         MOVE EMPINX-DPTO    TO EMPSORT-DPTO
      *     ELSE
      *         MOVE " "            TO EMPSORT-NOMBRE.
      *         MOVE "ZERO"         TO EMPSORT-DPTO.
      *1.	Si en el archivo de Percepciones/Deducciones la n�mina es cero se debe poner departamento �Zero�
      *2.	Si no existe la n�mina en el archivo maestro se debe poner departamento �Zero�


       260-LLENA-EMPSORT.
      *     DISPLAY PERCEPCION-DEDUCCION-NOMINA.
           MOVE PERCEPCION-DEDUCCION-NOMINA  TO EMPSORT-NOMINA.
           MOVE PERCEPCION-DEDUCCION-CLAVE   TO EMPSORT-CLAVE.
           MOVE PERCEPCION-DEDUCCION-IMPORTE TO EMPSORT-IMPORTE.
           MOVE PERCEPCION-DEDUCCION-NOMINA  TO EMPINX-NOMINA.
           PERFORM 233-LEER-INDEXADO.
           PERFORM 240-OBTENER-NOMBRE-DPTO-EMPINX.
           RELEASE EMPSORT-REG.
           PERFORM 230-LEER-PERCEPCION-DEDUCCION.

       270-GENERA-DETALLE.
           DISPLAY "DEPARTAMENTO: ",
               EMPSORT-DPTO, " - " ,
               "DPTO ANT: ", DPTO-ANTERIOR, " - ",
               "NOMINA: ", EMPSORT-NOMINA, " - ",
               "NOMINA ANTERIOR: ", NOMINA-ANTERIOR, " - ",
               "CONT LINEAS: ", CONT-LINEAS.
           IF CONT-LINEAS >= MAXIMO-LINEAS
               PERFORM 276-ESCRIBIR-TITULOS.
           IF NOMINA-ANTERIOR NOT = EMPSORT-NOMINA
               PERFORM 273-CORTE-NOMINA.
      **********************CONDICION NOMBRE NOMINA*********************
           IF BANDERA-IMPRIME-NOMBRE = 1
               MOVE EMPSORT-NOMINA TO DETALLE-NOMINA
               MOVE EMPSORT-NOMBRE TO DETALLE-NOMBRE
           ELSE
               MOVE " " TO DETALLE-NOMINA
               MOVE " " TO DETALLE-NOMBRE.

           PERFORM 280-PASAR-CLAVES-IMPORTES.
      *     DISPLAY "CLAVE: ",EMPSORT-CLAVE.

      *************************CONDICION IMPORTES **********************
           IF EMPSORT-CLAVE <= 06 THEN
               DISPLAY "IMPORTE A PERCEPCION"
               ADD EMPSORT-IMPORTE TO SUBTOTAL-PERCEPCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-PERCEPCION
               MOVE CLEANER TO DETALLE-DEDUCCION
           ELSE
               DISPLAY "IMPORTE A DEDUCCION"
               ADD EMPSORT-IMPORTE TO SUBTOTAL-DEDUCCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-DEDUCCION
               MOVE CLEANER TO DETALLE-PERCEPCION.
      ******************************************************************

           ADD 1 TO CONT-LINEAS.
           ADD 1 TO CONTADOR-REGISTROS-NOMINA.

           WRITE REPORTE-REG FROM DETALLE BEFORE 2 LINE.

           MOVE EMPSORT-NOMINA TO NOMINA-ANTERIOR.

           PERFORM 236-LEER-EMPSORT.



       273-CORTE-NOMINA.
           DISPLAY "ENTRA CORTE NOMINA"

           ADD 1 TO CONTADOR-EMPLEADOS-DPTO.
           ADD 1 TO CONTADOR-EMPLEADOS-EMPRESA.

           ADD SUBTOTAL-PERCEPCION-NOMINA TO SUBTOTAL-PERCEPCION-DPTO.
           ADD SUBTOTAL-DEDUCCION-NOMINA  TO SUBTOTAL-DEDUCCION-DPTO.

           MOVE SUBTOTAL-PERCEPCION-NOMINA TO DET-SUBTOT-NOMINA-PERCEP.
           MOVE SUBTOTAL-DEDUCCION-NOMINA  TO DET-SUBTOT-NOMINA-DEDUC.

           COMPUTE SUBTOTAL-SUELDO-NOMINA =
               SUBTOTAL-PERCEPCION-NOMINA - SUBTOTAL-DEDUCCION-NOMINA.
           MOVE SUBTOTAL-SUELDO-NOMINA TO DET-SUBTOT-NOMINA-SUELDO.

           WRITE REPORTE-REG FROM DETALLE-SUBTOTAL-NOMINA
               BEFORE 1 LINES.


           IF DPTO-ANTERIOR NOT = EMPSORT-DPTO THEN
               PERFORM 274-CORTE-DPTO

           MOVE 1 TO BANDERA-IMPRIME-NOMBRE.
           MOVE 0 TO SUBTOTAL-PERCEPCION-NOMINA.
           MOVE 0 TO SUBTOTAL-DEDUCCION-NOMINA.
           MOVE EMPSORT-DPTO TO DPTO-ANTERIOR.

       274-CORTE-DPTO.
           DISPLAY "CORTE DPTO".
           ADD SUBTOTAL-PERCEPCION-DPTO    TO TOTAL-PERCEPCION-EMPRESA.
           ADD SUBTOTAL-DEDUCCION-DPTO     TO TOTAL-DEDUCCION-EMPRESA.
           ADD SUBTOTAL-SUELDO-DPTO        TO TOTAL-SUELDO-EMPRESA.

           MOVE SUBTOTAL-PERCEPCION-DPTO   TO DET-SUBTOT-DPTO-PERCEP.
           MOVE SUBTOTAL-DEDUCCION-DPTO    TO DET-SUBTOT-DPTO-DEDUC.
           MOVE SUBTOTAL-SUELDO-DPTO       TO DET-SUBTOT-DPTO-SUELDO.

           MOVE CONTADOR-EMPLEADOS-DPTO    TO DET-EMPLEADOS-DPTO.

           WRITE REPORTE-REG FROM DET-SUBOTOTAL-DPTO BEFORE 1 LINE.

           ADD 1 TO MAXIMO-LINEAS GIVING CONT-LINEAS.
           MOVE 0 TO SUBTOTAL-PERCEPCION-DPTO, SUBTOTAL-DEDUCCION-DPTO,
               SUBTOTAL-SUELDO-DPTO, CONTADOR-EMPLEADOS-DPTO.
           PERFORM 276-ESCRIBIR-TITULOS.

       276-ESCRIBIR-TITULOS.
           ADD 1 TO CONTADOR-PAGINAS.
           WRITE REPORTE-REG FROM TITULO-1 BEFORE PAGE.
           WRITE REPORTE-REG FROM TITULO-2 BEFORE 1 LINES.
           WRITE REPORTE-REG FROM TITULO-3 BEFORE 1 LINES.
           MOVE 3 TO CONT-LINEAS.

       279-PASAR-NOMBRES-NOMINA-DPTO.
           IF CONTADOR-REGISTROS-NOMINA = 1
               MOVE EMPSORT-NOMINA TO DETALLE-NOMINA
               MOVE EMPSORT-NOMBRE TO DETALLE-NOMBRE
           ELSE
               MOVE " " TO DETALLE-NOMINA
               MOVE " " TO DETALLE-NOMBRE.

       280-PASAR-CLAVES-IMPORTES.


       300-FIN.
           DISPLAY "FIN DEL PROGRAMA"
           CLOSE EMPINX.
           CLOSE PERCEPCION-DEDUCCION.
           CLOSE REPORTE.



       END PROGRAM 6EMPNOM.
