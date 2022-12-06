
/*------------------------------------------------------------------------
    File        : BerbildDefDyn.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Nov 02 10:38:21 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE berbildbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE berbildqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE Berbildbloblog AS LOGICAL NO-UNDO.
   RUN FINNSTABELL.P (INPUT "BILDERIBEREDNING", OUTPUT Berbildbloblog).
   IF Berbildbloblog = FALSE THEN RETURN.
   CREATE WIDGET-POOL "BerbildFriDynTable" NO-ERROR.
   CREATE BUFFER berbildbuffh FOR TABLE "BILDERIBEREDNING" IN WIDGET-POOL "BerbildFriDynTable".
   berbildbuffh:FIND-FIRST("WHERE BERNR = " + STRING(Berbildbernr) + " AND OMRADE = " + QUOTER(Berbildomradevar), NO-LOCK) NO-ERROR.
   IF berbildbuffh:AVAILABLE THEN.
   ELSE RETURN.
   kommandoquery = "FOR EACH " + berbildbuffh:TABLE + " WHERE BERNR = " + STRING(Berbildbernr) + " AND OMRADE = " + QUOTER(Berbildomradevar) + " NO-LOCK". 
   
   RUN CreateCustomQueryBerbild(INPUT berbildbuffh,INPUT kommandoquery,OUTPUT berbildqh).