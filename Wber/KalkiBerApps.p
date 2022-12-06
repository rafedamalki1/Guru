
/*------------------------------------------------------------------------
    File        : KalkiBerApps.p
    Purpose     : 

    Syntax      :sparar om koder

    Description : 

    Author(s)   : 
    Created     : Mon Sep 05 12:20:50 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
{KalkylimportTT.I}

{KALKTEMP2.I}   


DEFINE BUFFER KALKNUMBUF FOR KALKNUM.
DEFINE INPUT  PARAMETER bernrvar AS INTEGER NO-UNDO. 
DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER valnum AS INTEGER NO-UNDO.

DEFINE INPUT PARAMETER TABLE FOR kalk_temp.
EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
FIND FIRST BEREDNING WHERE BEREDNING.BERNR = bernrvar AND BEREDNING.OMRADE = omrvar NO-LOCK NO-ERROR.
FIND FIRST BERKALKOPPLA WHERE BERKALKOPPLA.BERNR = bernrvar AND
BERKALKOPPLA.OMRADE = omrvar NO-LOCK NO-ERROR.
 

RUN gamlakalkspar_UI.

EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
RELEASE KALKNUM NO-ERROR.
/*Anders Olsson Elpool i Umeå AB  23 sep 2019 10:08:32 
funkar inte 

PROCEDURE kalkspar_UI :
   FOR EACH  KALKNUM WHERE KALKNUM.KALKNR = BERKALKOPPLA.KALKNR AND
   KALKNUM.OMRADE = BERKALKOPPLA.OMRADE AND KALKNUM.BERNUM = valnum 
   EXCLUSIVE-LOCK:
      DELETE KALKNUM.
   END.
   FOR EACH kalk_temp WHERE kalk_temp.NUM = valnum NO-LOCK:
      CREATE KalkylimportTT.
      ASSIGN
      KalkylimportTT.TTRECID = RECID(KalkylimportTT) 
      KalkylimportTT.KALKNR = BERKALKOPPLA.KALKNR
      KalkylimportTT.OMRADE = BERKALKOPPLA.OMRADE
      KalkylimportTT.MATRIS = 1
      KalkylimportTT.ARBKOD = kalk_temp.ARBKOD 
      KalkylimportTT.LOPNR = kalk_temp.LOPNR
      KalkylimportTT.ANTAL = kalk_temp.ANTAL
      KalkylimportTT.BERNUM = kalk_temp.NUM.
   END.      
   RUN kalknumskap_UI.
END PROCEDURE.
*/

PROCEDURE gamlakalkspar_UI :
   FOR EACH kalk_temp WHERE kalk_temp.NUM = valnum NO-LOCK:
      FIND FIRST KALKNUM WHERE KALKNUM.KALKNR = BERKALKOPPLA.KALKNR AND
      KALKNUM.OMRADE = BERKALKOPPLA.OMRADE AND KALKNUM.BERNUM = valnum AND
      KALKNUM.ARBKOD = kalk_temp.ARBKOD AND KALKNUM.LOPNR = kalk_temp.LOPNR
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKNUM  THEN DO:
         CREATE KalkylimportTT.
         ASSIGN
         KalkylimportTT.TTRECID = RECID(KalkylimportTT) 
         KalkylimportTT.KALKNR = BERKALKOPPLA.KALKNR
         KalkylimportTT.OMRADE = BERKALKOPPLA.OMRADE
         KalkylimportTT.MATRIS = 1
         KalkylimportTT.ARBKOD = kalk_temp.ARBKOD 
         KalkylimportTT.LOPNR = kalk_temp.LOPNR
         KalkylimportTT.ANTAL = kalk_temp.ANTAL
         KalkylimportTT.BERNUM = kalk_temp.NUM.
      END.      
   END.
   RUN kalknumskap_UI.
END PROCEDURE.

   

PROCEDURE kalknumskap_UI :
   
   DEFINE VARIABLE ksid LIKE KALKNUM.KLOGSUBID NO-UNDO.
   DEFINE VARIABLE numnummer AS INTEGER NO-UNDO.
   DEFINE VARIABLE numsubnummer AS INTEGER NO-UNDO.
   DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
   FIND FIRST KalkylimportTT  WHERE NO-LOCK NO-ERROR.
   FIND FIRST KALKHUV  WHERE KALKHUV.KALKNR = KalkylimportTT.KALKNR AND KALKHUV.OMRADE = KalkylimportTT.OMRADE  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KalkylimportTT THEN RETURN.
   FIND LAST KALKNUMbuf WHERE KALKNUMbuf.KALKNR = KALKHUV.KALKNR AND KALKNUMbuf.OMRADE = KALKHUV.OMRADE USE-INDEX NUM
   NO-LOCK NO-ERROR.
   IF AVAILABLE KALKNUMbuf THEN numnummer = KALKNUMbuf.NUM.
   ELSE numnummer = 0.
   DEFINE VARIABLE numnummerSUB AS INTEGER NO-UNDO.
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
         numnummerSUB = 0.
      END.   
   END.
   EMPTY TEMP-TABLE KalkylimportTT NO-ERROR.
   RELEASE KALKNUMSUB NO-ERROR.
   RELEASE KALKNUM NO-ERROR.
   RELEASE KALKNUMbuf NO-ERROR.
END PROCEDURE.
