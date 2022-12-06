
/*------------------------------------------------------------------------
    File        : ifsanst.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jun 01 14:28:09 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/



RUN ANST_ui (INPUT "", INPUT ""). 




RUN ANST_ui (INPUT "ROBE", INPUT "ROBBEN").
RUN ANST_ui (INPUT "JOAN", INPUT "JONAND").
RUN ANST_ui (INPUT "", INPUT "STNSVE").
RUN ANST_ui (INPUT "", INPUT "STELEN").
RUN ANST_ui (INPUT "", INPUT "PETDAV").
RUN ANST_ui (INPUT "", INPUT "MARBRI").
RUN ANST_ui (INPUT "", INPUT "FREELN").
RUN ANST_ui (INPUT "MATJO", INPUT "MATJOH").



PROCEDURE ANST_UI :
   DEFINE INPUT  PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER Nranst AS CHARACTER NO-UNDO.
   DO TRANSACTION:
      
      FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD = pkod  AND PERSONALTAB.ANSTNR = Nranst EXCLUSIVE-LOCK NO-ERROR.
      
      IF AVAILABLE PERSONALTAB THEN PERSONALTAB.ANSTNR = Nranst.
   END.   
END PROCEDURE.