
/*------------------------------------------------------------------------
    File        : EJVALAONR.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Sep 21 13:30:05 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE ejvalaonr_UI :
   DEFINE INPUT PARAMETER valomrade AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER globomr2 AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER omrnamn AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER klknr AS INTEGER NO-UNDO.
   IF valomrade = "" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = globomr2 
      USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = valomrade AND OMRADETAB.ELVOMRKOD = 0 USE-INDEX OMR NO-LOCK NO-ERROR. 
   END.
   IF NOT AVAILABLE OMRADETAB THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.ELVOMRKOD = 0 USE-INDEX OMR NO-LOCK NO-ERROR. 
   END.
   
   omrnamn = OMRADETAB.NAMN. 
   FIND LAST berbuff WHERE berbuff.OMRADE = OMRADETAB.OMRADE
   USE-INDEX BERNR NO-LOCK NO-ERROR.
   IF AVAILABLE berbuff THEN DO:             
      klknr = berbuff.BERNR + 1.               
   END.   
   ELSE DO:
      klknr = 1.            
   END.       
   
END PROCEDURE.
