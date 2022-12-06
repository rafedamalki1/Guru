
/*------------------------------------------------------------------------
    File        : KalkiBerSchApp.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Sep 05 13:09:11 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
{KalkylimportTT.I}
DEFINE BUFFER KALKNUMBUF FOR KALKNUM.
DEFINE INPUT PARAMETER TABLE FOR KalkylimportTT. 
DEFINE VARIABLE ksid LIKE KALKNUM.KLOGSUBID NO-UNDO.
DEFINE VARIABLE numnummer AS INTEGER NO-UNDO.
DEFINE VARIABLE numsubnummer AS INTEGER NO-UNDO.
DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
FIND FIRST KalkylimportTT  WHERE NO-LOCK NO-ERROR.
IF NOT AVAILABLE KalkylimportTT THEN RETURN.
FIND FIRST KALKHUV  WHERE KALKHUV.KALKNR = KalkylimportTT.KALKNR AND KALKHUV.OMRADE = KalkylimportTT.OMRADE  NO-LOCK NO-ERROR.
FIND LAST KALKNUMbuf WHERE KALKNUMbuf.KALKNR = KALKHUV.KALKNR AND KALKNUMbuf.OMRADE = KALKHUV.OMRADE USE-INDEX NUM
NO-LOCK NO-ERROR.
IF AVAILABLE KALKNUMbuf THEN numnummer = KALKNUMbuf.NUM.
ELSE numnummer = 0.

FOR EACH KalkylimportTT WHERE NO-LOCK:
   DO TRANSACTION:
      numnummer = numnummer + 1.
      CREATE KALKNUM. 
      BUFFER-COPY KalkylimportTT EXCEPT KalkylimportTT.NUM  TO KALKNUM.
         
      ASSIGN
      KALKNUM.NUM        = numnummer
      KALKNUM.KALKNR     = KALKHUV.KALKNR
      KALKNUM.OMRADE     = KALKHUV.OMRADE
      KALKNUM.TYPKALK    = KALKHUV.TYPKALK.
   END.   
END.
EMPTY TEMP-TABLE KalkylimportTT NO-ERROR.
RELEASE KALKNUM NO-ERROR.
RELEASE KALKNUMbuf NO-ERROR.
   