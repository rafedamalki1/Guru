/* D:\DELAD\PRO9\GURU\XDYNTEMPOK.P*/
   DEFINE TEMP-TABLE custtemp NO-UNDO LIKE customer.
   /*
   DEFINE INPUT PARAMETER ttbufh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER orgtab AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER temptab AS CHARACTER NO-UNDO.
   */
   DEFINE VARIABLE ttbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgtab AS CHARACTER NO-UNDO.
   DEFINE VARIABLE temptab AS CHARACTER NO-UNDO.
   ttbufh = TEMP-TABLE custtemp:DEFAULT-BUFFER-HANDLE.
ASSIGN
   orgtab = "customer"
   temptab = "custtemp".
   DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
   DEFINE VARIABLE tthorg AS HANDLE NO-UNDO.
   DEFINE VARIABLE appqueh AS HANDLE NO-UNDO.
   DEFINE VARIABLE apptthtemp AS HANDLE. 
   /*DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.   */

   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valdkommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE radant AS INTEGER NO-UNDO.
   DEFINE VARIABLE num AS INTEGER NO-UNDO.
   DEFINE VARIABLE extratemptabh AS HANDLE NO-UNDO.
   DELETE WIDGET-POOL "dynpool" NO-ERROR.
   CREATE WIDGET-POOL "dynpool" PERSISTENT.
   


   DEFINE VARIABLE hQcusttemp AS HANDLE     NO-UNDO.
   DEFINE VARIABLE hField AS HANDLE     NO-UNDO.


   radant = 20.
/*   CREATE BUFFER apptthtemp FOR TABLE ttbufh IN WIDGET-POOL "dynpool".*/
   
   /*CREATE TEMP-TABLE apptthtemp IN WIDGET-POOL "dynpool".
   apptthtemp:CREATE-LIKE(ttbufh).
   apptthtemp:TEMP-TABLE-PREPARE(temptab). 
   */

   /* populating the temp-table*/
   CREATE BUFFER orgbufh FOR TABLE orgtab IN WIDGET-POOL "dynpool".
   IF ttbufh:DBNAME NE "PROGRESST" THEN RETURN.  
   
   CREATE QUERY appqueh IN WIDGET-POOL "dynpool".

   appqueh:SET-BUFFERS(orgbufh).
   kommandoquery = "FOR EACH " + orgbufh:TABLE + " NO-LOCK ".
   valdkommandoquery = kommandoquery.
   appqueh:QUERY-PREPARE(valdkommandoquery).
   appqueh:QUERY-OPEN().
   appqueh:GET-FIRST(NO-LOCK).
   DO WHILE appqueh:QUERY-OFF-END = FALSE AND num < radant:
      ttbufh:BUFFER-CREATE().
      ttbufh:BUFFER-COPY(orgbufh) NO-ERROR.
      appqueh:GET-NEXT(NO-LOCK).
   END.            
   
  
  FOR EACH custtemp:
      DISP custtemp. 
  END.
  
/*
  /*displaying the "name" field from the temp-table*/
  CREATE QUERY hQcusttemp.
  hQcusttemp:SET-BUFFERS(ttbufh).
  hQcusttemp:QUERY-PREPARE('for each ' + temptab). /* temptab = "custtemp" */
  hQcusttemp:QUERY-OPEN().
  hQcusttemp:GET-FIRST(NO-LOCK).
   DO WHILE hQcusttemp:QUERY-OFF-END = FALSE:
      hField = ttbufh:BUFFER-FIELD('name').
      DISPLAY hField:BUFFER-VALUE() FORMAT "x(32)" .
      hQcusttemp:GET-NEXT(NO-LOCK).
   END.            

  DELETE OBJECT hQcusttemp NO-ERROR.
  
  */
