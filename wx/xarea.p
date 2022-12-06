/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: G:\PRO9S\WX\XAREA.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2010.04.15 10:16 ELPAO   
     Modified: 2010.04.15 11:41 ELPAO    
     Modified: 
*/
DEFINE VARIABLE barea AS INTEGER NO-UNDO.

/*FOR EACH konstval WHERE konskod = 14  NO-LOCK:
RUN goromDec_UI (INPUT konstval.ktypkod, OUTPUT barea).
   DISP  konstval.ktypkod barea.
   
END.*/
FOR EACH konstval WHERE ktypkod = "cdc" AND bb = "kablar" NO-LOCK:
   RUN goromDec_UI (INPUT konstval.kvalkod, OUTPUT barea).
   DISP  konstval.kvalkod barea.
   
END. 

PROCEDURE goromDec_UI :
   DEFINE INPUT PARAMETER kvarde AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER karea AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.   
   DEFINE VARIABLE vardearray AS CHARACTER EXTENT 50 NO-UNDO.
   DEFINE VARIABLE vlangd AS INTEGER NO-UNDO.
   DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
   ivar = 1.
   vlangd = LENGTH(kvarde).
   DO WHILE ivar <= (vlangd):
      vardearray[ivar] = SUBSTRING(kvarde,1, 1).
      kvarde = SUBSTRING(kvarde,2).
      ivar = ivar + 1.
   END.
   ASSIGN
   kvarde = ""  karea = "" ivar = 1.
   DO WHILE ivar <= vlangd:
      ascivarde = ASC(vardearray[ivar]).      
      /*siffror*/
      IF ascivarde >= 48 AND ascivarde <= 57 THEN karea = karea  + vardearray[ivar].      
      ELSE karea = "".      
      ivar = ivar + 1.      
  END.      
  IF LENGTH(karea) = 4 THEN karea = substring(karea,2).
  IF LENGTH(karea) = 5 THEN karea = substring(karea,3).
END PROCEDURE.
