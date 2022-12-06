
/*------------------------------------------------------------------------
    File        : MarkstationDefDyn.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Nov 02 10:38:21 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE bermarkstbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE marksqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE Markstbloblog AS LOGICAL NO-UNDO.
   RUN FINNSTABELL.P (INPUT "MARKSTATIONIBEREDNING", OUTPUT Markstbloblog).
   IF Markstbloblog = FALSE THEN RETURN.
   CREATE WIDGET-POOL "MarkFriDynTable" NO-ERROR.
   CREATE BUFFER bermarkstbuffh FOR TABLE "MARKSTATIONIBEREDNING" IN WIDGET-POOL "MarkFriDynTable".
   bermarkstbuffh:FIND-FIRST("WHERE AONR = " + QUOTER(MarkStaonrvar) + " AND OMRADE = " + QUOTER(MarkStomradevar), NO-LOCK) NO-ERROR.
   IF bermarkstbuffh:AVAILABLE THEN.
   ELSE RETURN.
   kommandoquery = "FOR EACH " + bermarkstbuffh:TABLE + " WHERE AONR = " + QUOTER(MarkStaonrvar) + " AND OMRADE = " + QUOTER(MarkStomradevar) + " NO-LOCK". 
   
   RUN CreateCustomQueryMarkST(INPUT bermarkstbuffh,INPUT kommandoquery,OUTPUT marksqh).