
/*------------------------------------------------------------------------
    File        : KALKYLUPPSUMMA.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Wed Mar 06 11:53:18 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
PROCEDURE summeringkalk_UI :
   DEFINE VARIABLE frikalkvar AS LOGICAL NO-UNDO.
   IF uppvaltemp.DELNRKOLL = TRUE THEN DO:
      
      FIND FIRST kalksumsum  WHERE kalksumsum.AONR = valdaao.AONR AND kalksumsum.DELNR = valdaao.DELNR NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST kalksumsum  WHERE kalksumsum.AONR = valdaao.AONR  NO-LOCK NO-ERROR.
   END.   
   IF NOT AVAILABLE kalksumsum THEN DO:
      CREATE kalksumsum. 
   END.   
  
   ASSIGN
   
   kalksumsum.AONR = valdaao.AONR    
   kalksumsum.DELNR = valdaao.DELNR.
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

PROCEDURE kalksummering_UI :
   DEFINE INPUT  PARAMETER kalknr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE frikalkvar AS LOGICAL NO-UNDO.
   FIND FIRST kalksumsum  WHERE kalksumsum.AONR = omr AND kalksumsum.DELNR = kalknr NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kalksumsum THEN DO:
      CREATE kalksumsum. 
   END.   
   DEBUGGER:SET-BREAK().
   ASSIGN
   kalksumsum.AONR = omr
   kalksumsum.DELNR = kalknr.
   FIND FIRST GURUDEFAULTS WHERE GURUDEFAULTS.PROGRAM = "KALKYL" AND GURUDEFAULTS.HUVUDINT = kalknr AND GURUDEFAULTS.HUVUDCHAR = omr AND
   GURUDEFAULTS.TOOLKEY = "FriKalkActive"  
   NO-LOCK NO-ERROR.
   IF AVAILABLE GURUDEFAULTS THEN DO:
      IF GURUDEFAULTS.TOOLVALUE = "yes" THEN frikalkvar = TRUE.
      ELSE frikalkvar = FALSE.
   END.
   ELSE frikalkvar = FALSE.   
   {KALKSUMKONV.I}
  
             
END PROCEDURE.

