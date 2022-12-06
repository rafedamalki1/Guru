
/*------------------------------------------------------------------------
    File        : KALKYLUPPSUMMAPLAN.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Wed Mar 06 11:53:18 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
PROCEDURE summeringkalkplan_UI :
   DEFINE VARIABLE frikalkvar AS LOGICAL NO-UNDO.
   IF KALKAONR.PLANNR NE ? THEN DO:
      FIND FIRST kalksumsum  WHERE kalksumsum.AONR = KALKAONR.PLANNR AND kalksumsum.DELNR = KALKAONR.ARTAL NO-LOCK NO-ERROR.
         
      IF NOT AVAILABLE kalksumsum THEN DO:
         CREATE kalksumsum. 
      END.   
      
      ASSIGN
      kalksumsum.KTYP = "PN1"
      kalksumsum.AONR = KALKAONR.PLANNR 
      kalksumsum.DELNR = KALKAONR.ARTAL.
   END.
   ELSE DO:
      FIND FIRST kalksumsum  WHERE kalksumsum.AONR = KALKAONR.AONR AND kalksumsum.DELNR = KALKAONR.DELNR AND kalksumsum.KTYP = "PN1" NO-LOCK NO-ERROR.
         
      IF NOT AVAILABLE kalksumsum THEN DO:
         CREATE kalksumsum. 
      END.   
      
      ASSIGN
      kalksumsum.KTYP = "PN1"
      kalksumsum.AONR = KALKAONR.AONR 
      kalksumsum.DELNR = KALKAONR.DELNR.
   END.      
   FIND FIRST GURUDEFAULTS WHERE GURUDEFAULTS.PROGRAM = "KALKYL" AND GURUDEFAULTS.HUVUDINT = KALKAONR.KALKNR AND GURUDEFAULTS.HUVUDCHAR = KALKAONR.OMRADE AND
   GURUDEFAULTS.TOOLKEY = "FriKalkActive"  
   NO-LOCK NO-ERROR.
   IF AVAILABLE GURUDEFAULTS THEN DO:
      IF GURUDEFAULTS.TOOLVALUE = "yes" THEN frikalkvar = TRUE.
      ELSE frikalkvar = FALSE.
   END.
   ELSE frikalkvar = FALSE.
   {KALKSUMKONV.I}   
          
END PROCEDURE.
