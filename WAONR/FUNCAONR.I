
/*------------------------------------------------------------------------
    File        : FUNCAONR.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Oct 20 11:32:02 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

FUNCTION brwval RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE leftbrwakt AS LOGICAL NO-UNDO.
   RUN urvalaktiv_UI IN brwproc[{&ARROWS}] (OUTPUT leftbrwakt).
   IF leftbrwakt = TRUE THEN antal_valda =  BRW_AONR:NUM-SELECTED-ROWS.
   ELSE antal_valda = BRW_VAONR:NUM-SELECTED-ROWS. 
   
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Inget " + LC(Guru.Konstanter:gaok) + " är valt!" VIEW-AS ALERT-BOX.
      RETURN TRUE.                
   END.
   IF leftbrwakt = TRUE THEN DO:
      EMPTY TEMP-TABLE valdaaotemp NO-ERROR.       
      RUN sel_over_UI IN brwproc[{&ARROWS}].
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").  
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaaotemp)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
   END.
   ELSE DO:
      /*
      status-ok = BRW_VAONR:SELECT-FOCUSED-ROW() NO-ERROR.       
      FIND FIRST valdaaotemp WHERE ROWID(valdaaotemp) = valdaobh:ROWID NO-LOCK NO-ERROR.
      */
      RUN selectfoc_UI.
   END.
   RETURN FALSE.   /* Function return value. */

END FUNCTION.

FUNCTION checkbrw RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE leftbrwakt AS LOGICAL NO-UNDO.
   RUN urvalaktiv_UI IN brwproc[{&ARROWS}] (OUTPUT leftbrwakt).
   RETURN leftbrwakt.   


END FUNCTION.
