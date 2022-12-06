
/*------------------------------------------------------------------------
    File        : XCOMP116.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Feb 27 10:14:42 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/



PROCEDURE C116_UI :
   /*XCOMPEndast116.p*/
   RUN comp_UI ("CHECK3264.p").
   RUN comp_UI ("SELGALAGERSTATUS.p").
   RUN comp_UI("ONNILAGERSTATUS.p").
END PROCEDURE.

PROCEDURE C112_UI :
   DEFINE INPUT  PARAMETER filnamn  AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER comp112 AS LOGICAL NO-UNDO.
   IF PROVERSION BEGINS "11.2" AND filnamn = "CHECK3264.p" THEN.
   ELSE IF PROVERSION BEGINS "11.2" AND filnamn = "SELGALAGERSTATUS.p" THEN.
   ELSE IF PROVERSION BEGINS "11.2" AND filnamn = "ONNILAGERSTATUS.p" THEN.
   ELSE  comp112 = TRUE.                  
END PROCEDURE.