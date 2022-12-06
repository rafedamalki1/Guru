
/*------------------------------------------------------------------------
    File        : BERBILDLADDA.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Mon Dec 05 10:21:21 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/


PROCEDURE bildladda_UI :
   DEFINE INPUT  PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      FOR EACH BERBILD NO-LOCK,
      EACH konsttemp WHERE konsttemp.KTYPKOD = BERBILD.KTYPKOD NO-LOCK:
         konsttemp.BILD = "B".  
      END.
   END.
   IF vad = 2 THEN DO:
      FOR EACH BERBILD NO-LOCK,          
      EACH konstvaltemp WHERE konstvaltemp.KVALKOD = BERBILD.KTYPKOD NO-LOCK:
         konstvaltemp.BILD = "B".  
      END.
      
   END.     
END PROCEDURE.