      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 6EMPNOMV3.

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
               05  T1-PAG                      PIC X(03).
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
               05  FILLER                      PIC X(12) VALUE
                   "DEPARTAMENTO".
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
               05  DETALLE-DPTO                PIC X(20).
               05  FILLER                      PIC X(02) VALUE SPACES.
               05  DETALLE-CLAVE               PIC X(13).
               05  FILLER                      PIC X(10) VALUE SPACES.
               05  DETALLE-PERCEPCION          PIC $$$,$$9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  DETALLE-DEDUCCION           PIC $$$,$$9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  DETALLE-SUELDO              PIC $$$,$$9.99.
           03  DETALLE-SUBTOTAL-NOMINA.
               05  FILLER                      PIC X(15) VALUE
                   "SUBTOTAL NOMINA".
               05  FILLER                      PIC X(30) VALUE SPACES.
               05  FILLER                      PIC X(15) VALUE
                   "PERCEPCIONES: ".
               05  DET-SUBTOT-NOMINA-PERCEP    PIC $$$,$$9.99.
               05  FILLER                      PIC X(02) VALUE SPACES.
               05  FILLER                      PIC X(13) VALUE
                   "DEDUCCIONES: ".
               05  DET-SUBTOT-NOMINA-DEDUC     PIC $$$,$$9.99.
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
               05  DET-SUBTOT-DPTO-PERCEP      PIC $$$,$$9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(13) VALUE
                   "DEDUCCIONES: ".
               05  DET-SUBTOT-DPTO-DEDUC       PIC $$$,$$9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(08) VALUE
                   "SUELDO: ".
               05  DET-SUBTOT-DPTO-SUELDO      PIC $$$,$$9.99.
           03  DET-TOTAL-EMPRESA.
               05  FILLER                      PIC X(16) VALUE
                   "SUBTOTAL EMPRESA".
               05  FILLER                      PIC X(05) VALUE SPACES.
               05  FILLER                      PIC X(15) VALUE
                   "EMPLEADOS: ".
               05  DET-EMPLEADOS-EMPRESA       PIC X(04).
               05  FILLER                      PIC X(15) VALUE
                   "PERCEPCIONES: ".
               05  DET-TOTAL-EMPRESA-PERCEP    PIC $$$,$$9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(13) VALUE
                   "DEDUCCIONES: ".
               05  DET-TOTAL-EMPRESA-DEDUC     PIC $$$,$$9.99.
               05  FILLER                      PIC X(08) VALUE SPACES.
               05  FILLER                      PIC X(08) VALUE
                   "SUELDO: ".
               05  DET-TOTAL-EMPRESA-SUELDO    PIC $$$,$$9.99.


       01  NOMINA-CLEANER.
           03 FILLER                       PIC X(06) VALUE
               "------".
       01  NOMBRE-CLEANER.
           03 FILLER                       PIC X(20) VALUE
               "--------------------".
       01  DPTO-CLEANER.
           03 FILLER                       PIC X(20) VALUE
               "--------------------".
       01  CLAVE-CLEANER.
           03 FILLER                       PIC X(13) VALUE
               "-------------".

      * 77  BANDERA-IMPRIMIR-TITULOS        PIC 9.
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
           PERFORM 284-IMPRIMIR-FECHA.
           PERFORM 236-LEER-EMPSORT
           MOVE EMPSORT-NOMINA TO NOMINA-ANTERIOR.
           MOVE EMPSORT-DPTO    TO DPTO-ANTERIOR.
           MOVE 1 TO BANDERA-IMPRIME-NOMBRE.
           ADD 1 TO MAXIMO-LINEAS  GIVING CONT-LINEAS.
           MOVE 1 TO CONTADOR-PAGINAS.
           PERFORM 290-PRINTER UNTIL EMPSORT-EOF = 1.
           PERFORM 294-IMPRIMIR-SUBTOTAL-EMPLEADO.
           PERFORM 295-IMPRIMIR-SUBTOTAL-DPTO.
           PERFORM 296-IMPRIMIR-TOTAL-EMPRESA.
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
      *1.    Si en el archivo de Percepciones/Deducciones la nómina es cero se debe poner departamento “Zero”
      *2.    Si no existe la nómina en el archivo maestro se debe poner departamento “Zero”


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



       280-PASAR-CLAVES-IMPORTES.
           IF EMPSORT-CLAVE = 01 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-PERCEPCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-PERCEPCION
               MOVE CLEANER TO DETALLE-DEDUCCION
               MOVE "SUELDO" TO DETALLE-CLAVE
           ELSE IF  EMPSORT-CLAVE = 02 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-PERCEPCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-PERCEPCION
               MOVE CLEANER TO DETALLE-DEDUCCION
               MOVE "BONO" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 03 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-PERCEPCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-PERCEPCION
               MOVE CLEANER TO DETALLE-DEDUCCION
               MOVE "PUNTUALIDAD" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 04 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-PERCEPCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-PERCEPCION
               MOVE CLEANER TO DETALLE-DEDUCCION
               MOVE "PRODUCTIVIDAD" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 05 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-PERCEPCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-PERCEPCION
               MOVE CLEANER TO DETALLE-DEDUCCION
               MOVE "VALES" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 06 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-PERCEPCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-PERCEPCION
               MOVE CLEANER TO DETALLE-DEDUCCION
               MOVE "HORAS EXTRAS" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 11 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-DEDUCCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-DEDUCCION
               MOVE CLEANER TO DETALLE-PERCEPCION
               MOVE "IMPUESTOS" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 12 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-DEDUCCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-DEDUCCION
               MOVE CLEANER TO DETALLE-PERCEPCION
               MOVE "IMSS" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 13 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-DEDUCCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-DEDUCCION
               MOVE CLEANER TO DETALLE-PERCEPCION
               MOVE "FALTAS" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 14 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-DEDUCCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-DEDUCCION
               MOVE CLEANER TO DETALLE-PERCEPCION
               MOVE "PAGO PRESTAMO" TO DETALLE-CLAVE
           ELSE IF EMPSORT-CLAVE = 15 THEN
               ADD EMPSORT-IMPORTE TO SUBTOTAL-DEDUCCION-NOMINA
               MOVE EMPSORT-IMPORTE TO DETALLE-DEDUCCION
               MOVE CLEANER TO DETALLE-PERCEPCION
               MOVE "PRESTAMO" TO DETALLE-CLAVE
           ELSE
               MOVE CLEANER TO DETALLE-PERCEPCION
               MOVE CLEANER TO DETALLE-DEDUCCION
               MOVE "INVALIDO" TO DETALLE-CLAVE.

       281-EXEPCION-NOMINA-ZERO.
           MOVE "ZERO" TO DETALLE-DPTO.
           MOVE NOMINA-CLEANER TO DETALLE-NOMINA.
           MOVE NOMBRE-CLEANER TO DETALLE-NOMBRE.
           MOVE CLAVE-CLEANER TO DETALLE-CLAVE.
           MOVE CLEANER TO DETALLE-PERCEPCION.
           MOVE CLEANER TO DETALLE-DEDUCCION.

           WRITE REPORTE-REG FROM DETALLE BEFORE 1 LINES.
           ADD 1 TO CONT-LINEAS.


       283-EXPECIONES.
           MOVE EMPSORT-NOMINA TO EMPINX-NOMINA.
           PERFORM 233-LEER-INDEXADO.
           IF EXISTE-NOMINA = 0 OR EMPSORT-NOMINA <= 0 THEN
               PERFORM 281-EXEPCION-NOMINA-ZERO.

       284-IMPRIMIR-FECHA.
           ACCEPT FECHA FROM DATE.
           MOVE FECHA-DD       TO T1-DD.
           MOVE MES(FECHA-MM)  TO T1-MM.
           MOVE FECHA-AA       TO T1-AA.

       290-PRINTER.
           PERFORM 283-EXPECIONES.
           IF CONT-LINEAS >= MAXIMO-LINEAS THEN
      *        corte de pagina
               PERFORM 292-IMPRIMIR-TITULOS
               MOVE 1 TO BANDERA-IMPRIME-NOMBRE.
           IF NOMINA-ANTERIOR NOT = EMPSORT-NOMINA THEN
      *        corte de nomina
               PERFORM 294-IMPRIMIR-SUBTOTAL-EMPLEADO
               MOVE 1 TO BANDERA-IMPRIME-NOMBRE.
           IF DPTO-ANTERIOR NOT = EMPSORT-DPTO THEN
      *        corte de departamento
               PERFORM 295-IMPRIMIR-SUBTOTAL-DPTO
               MOVE 1 TO BANDERA-IMPRIME-NOMBRE.

           PERFORM 291-IMPRIMIR-EMPLEADO.


           MOVE EMPSORT-DPTO   TO DPTO-ANTERIOR.
           MOVE EMPSORT-NOMINA TO NOMINA-ANTERIOR.
           MOVE 0 TO BANDERA-IMPRIME-NOMBRE.
           PERFORM 236-LEER-EMPSORT.

       291-IMPRIMIR-EMPLEADO.
           PERFORM 293-PASAR-CLAVES-DPTOS.
           PERFORM 280-PASAR-CLAVES-IMPORTES.

      *     IF EMPSORT-CLAVE <= 06 THEN
      *         ADD EMPSORT-IMPORTE TO SUBTOTAL-PERCEPCION-NOMINA
      *         MOVE EMPSORT-IMPORTE TO DETALLE-PERCEPCION
      *         MOVE CLEANER TO DETALLE-PERCEPCION
      *     ELSE
      *         ADD EMPSORT-IMPORTE TO SUBTOTAL-DEDUCCION-NOMINA
      *         MOVE EMPSORT-IMPORTE TO DETALLE-DEDUCCION
      *         MOVE CLEANER TO DETALLE-PERCEPCION.
           IF BANDERA-IMPRIME-NOMBRE = 0 THEN
               MOVE NOMINA-CLEANER TO DETALLE-NOMINA
               MOVE NOMBRE-CLEANER TO DETALLE-NOMBRE
               MOVE DPTO-CLEANER   TO DETALLE-DPTO
           ELSE
               MOVE EMPSORT-NOMINA TO DETALLE-NOMINA
               MOVE EMPSORT-NOMBRE TO DETALLE-NOMBRE.

           WRITE REPORTE-REG FROM DETALLE BEFORE 1 LINES.
           ADD 1 TO CONT-LINEAS.


       292-IMPRIMIR-TITULOS.
           MOVE CONTADOR-PAGINAS TO T1-PAG.
           WRITE REPORTE-REG FROM TITULO-1 BEFORE PAGE.
           WRITE REPORTE-REG FROM TITULO-2 BEFORE 1 LINES.
           WRITE REPORTE-REG FROM TITULO-3 BEFORE 1 LINES.

           MOVE 0 TO CONT-LINEAS.
           ADD 1 TO CONTADOR-PAGINAS.
           ADD 3 TO CONT-LINEAS.

       293-PASAR-CLAVES-DPTOS.
           IF EMPSORT-DPTO = "ADM"
               MOVE "ADMINISTRACION" TO DETALLE-DPTO
           ELSE IF EMPSORT-DPTO = "CON"
               MOVE "CONTABILIDAD" TO DETALLE-DPTO
           ELSE IF EMPSORT-DPTO = "MER"
               MOVE "MERCADOTECNIA" TO DETALLE-DPTO
           ELSE IF EMPSORT-DPTO = "RH"
               MOVE "RECURSOS HUMANOS" TO DETALLE-DPTO
           ELSE IF EMPSORT-DPTO = "SIS"
               MOVE "SISTEMAS" TO DETALLE-DPTO
           ELSE IF EMPSORT-DPTO = "TEC"
               MOVE "TECNOLOGIA" TO DETALLE-DPTO
           ELSE
               MOVE "ZERO" TO DETALLE-DPTO.

       294-IMPRIMIR-SUBTOTAL-EMPLEADO.
           ADD 1 TO CONTADOR-EMPLEADOS-DPTO.
           ADD 1 TO CONTADOR-EMPLEADOS-EMPRESA.

           MOVE SUBTOTAL-PERCEPCION-NOMINA TO DET-SUBTOT-NOMINA-PERCEP.
           MOVE SUBTOTAL-DEDUCCION-NOMINA TO DET-SUBTOT-NOMINA-DEDUC.

           ADD SUBTOTAL-PERCEPCION-NOMINA TO SUBTOTAL-PERCEPCION-DPTO.
           ADD SUBTOTAL-DEDUCCION-NOMINA TO SUBTOTAL-DEDUCCION-DPTO.

           COMPUTE SUBTOTAL-SUELDO-NOMINA = SUBTOTAL-PERCEPCION-NOMINA
                   - SUBTOTAL-DEDUCCION-NOMINA.

           MOVE SUBTOTAL-SUELDO-NOMINA TO DET-SUBTOT-NOMINA-SUELDO.

           IF SUBTOTAL-SUELDO-NOMINA > 0 THEN
               ADD SUBTOTAL-SUELDO-NOMINA TO SUBTOTAL-SUELDO-DPTO
               ADD SUBTOTAL-SUELDO-NOMINA TO TOTAL-SUELDO-EMPRESA.

           WRITE REPORTE-REG
               FROM DETALLE-SUBTOTAL-NOMINA BEFORE 1 LINES.

           DISPLAY EMPSORT-NOMINA.
           PERFORM 297-MODIFICAR-INDEXADO-SUELDO.
           PERFORM 298-CONSULTA-INDEXADO-SUELDO.


           MOVE 0 TO SUBTOTAL-PERCEPCION-NOMINA.
           MOVE 0 TO SUBTOTAL-DEDUCCION-NOMINA.
           MOVE 0 TO SUBTOTAL-SUELDO-NOMINA.
           ADD 1 TO CONT-LINEAS.

       295-IMPRIMIR-SUBTOTAL-DPTO.
           ADD SUBTOTAL-PERCEPCION-DPTO TO TOTAL-PERCEPCION-EMPRESA.
           ADD SUBTOTAL-DEDUCCION-DPTO TO TOTAL-DEDUCCION-EMPRESA.


           MOVE SUBTOTAL-PERCEPCION-DPTO TO DET-SUBTOT-DPTO-PERCEP.
           MOVE SUBTOTAL-DEDUCCION-DPTO TO DET-SUBTOT-DPTO-DEDUC.
           MOVE SUBTOTAL-SUELDO-DPTO TO DET-SUBTOT-DPTO-SUELDO.

           MOVE CONTADOR-EMPLEADOS-DPTO    TO DET-EMPLEADOS-DPTO.

           WRITE REPORTE-REG
               FROM DET-SUBOTOTAL-DPTO BEFORE 1 LINES.
           MOVE 0 TO SUBTOTAL-PERCEPCION-DPTO.
           MOVE 0 TO SUBTOTAL-DEDUCCION-DPTO.
           MOVE 0 TO SUBTOTAL-SUELDO-DPTO.
           MOVE 0 TO CONTADOR-EMPLEADOS-DPTO.
           IF EMPSORT-EOF NOT = 1 THEN
               PERFORM 292-IMPRIMIR-TITULOS.

       296-IMPRIMIR-TOTAL-EMPRESA.
           MOVE TOTAL-PERCEPCION-EMPRESA TO DET-TOTAL-EMPRESA-PERCEP.
           MOVE TOTAL-DEDUCCION-EMPRESA TO DET-TOTAL-EMPRESA-DEDUC.
           MOVE TOTAL-SUELDO-EMPRESA    TO DET-TOTAL-EMPRESA-SUELDO.
           MOVE CONTADOR-EMPLEADOS-EMPRESA TO DET-EMPLEADOS-EMPRESA.

           WRITE REPORTE-REG FROM DET-TOTAL-EMPRESA BEFORE 1 LINE.

       297-MODIFICAR-INDEXADO-SUELDO.
           MOVE NOMINA-ANTERIOR TO EMPINX-NOMINA
           PERFORM 233-LEER-INDEXADO.
           IF EXISTE-NOMINA = 1
               MOVE SUBTOTAL-SUELDO-NOMINA TO EMPINX-SUELDO
               REWRITE EMPINX-REG
               DISPLAY "NOMINA ", " ", EMPSORT-NOMINA, " ",
                       EMPINX-NOMINA, " MODIFICADA"
           ELSE
               DISPLAY "NO EXISTE NOMINA - NO MODIFICAR".

       298-CONSULTA-INDEXADO-SUELDO.
           MOVE NOMINA-ANTERIOR TO EMPINX-NOMINA.
           PERFORM 233-LEER-INDEXADO.
           IF EXISTE-NOMINA = 1
               DISPLAY "NOMINA: ", EMPINX-NOMINA
               DISPLAY "NOMBRE: ", EMPINX-NOMBRE
               DISPLAY "DEPARTAMENTO: ", EMPINX-DPTO
               DISPLAY " SUELDO: ", EMPINX-SUELDO
           ELSE
               DISPLAY EMPINX-NOMINA, " "
                       EMPSORT-NOMINA, " "
                       , " NO EXISTE NOMINA - NO MOSTRAR".

       300-FIN.
           DISPLAY "FIN DEL PROGRAMA"
           CLOSE EMPINX.
           CLOSE PERCEPCION-DEDUCCION.
           CLOSE REPORTE.



       END PROGRAM 6EMPNOMV3.
