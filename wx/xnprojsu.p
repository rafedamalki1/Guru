DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE sldatum AS DATE NO-UNDO.
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
DEFINE VARIABLE projrapp AS CHARACTER FORMAT "X(101)" NO-UNDO.  

FOR EACH aonrtidlage WHERE aonrtidlage.idtidlag = "aoupplagt" AND aonrtidlage.datum1 GE 03/31/2003 NO-LOCK:
         
   FIND FIRST AONRTAB WHERE aonrtab.aonr = aonrtidlage.aonr AND aonrtab.delnr = aonrtidlage.delnr NO-LOCK NO-ERROR.   
   IF AVAILABLE aonrtab THEN DO:   
      ASSIGN 
      aonrvar = AONRTAB.AONR
      delnrvar = AONRTAB.DELNR.
      IF MONTH(aonrtidlage.datum1) GE 9 THEN sldatum = DATE(12,31,YEAR(aonrtidlage.datum1) + 1).
      ELSE sldatum = DATE(12,31,YEAR(aonrtidlage.datum1)).
      IF MONTH(aonrtidlage.datum1) = 01 AND DAY(aonrtidlage.datum1) < 5 THEN stdatum = DATE(12,01,YEAR(aonrtidlage.datum1) - 1).
      ELSE IF DAY(aonrtidlage.datum1) < 5 THEN stdatum = DATE(MONTH(aonrtidlage.datum1) - 1,01,YEAR(aonrtidlage.datum1)).
      ELSE stdatum = DATE(MONTH(aonrtidlage.datum1),01,YEAR(aonrtidlage.datum1)).
     
      IF AONRTAB.OMRADE NE "" THEN DO:
         IF LENGTH(AONRTAB.AONR) = 4 THEN aonrvar = aonrvar + "0".
         OUTPUT TO \\beredning1\delad\SERVER\pro9s\EXPORT\nyproj.txt APPEND.            
         ASSIGN projrapp = "".
         ASSIGN
         SUBSTRING(projrapp,1,5) = aonrvar.
         SUBSTRING(projrapp,7,30) = SUBSTRING(AONRTAB.ORT,1,30).
         SUBSTRING(projrapp,38,10) = STRING(stdatum,"9999/99/99").
         IF AONRTAB.FASTAAONR = TRUE THEN .
         ELSE SUBSTRING(projrapp,49,10) = STRING(sldatum,"9999/99/99").                              
         PUT projrapp.   
         PUT SKIP.
         OUTPUT CLOSE.
         OUTPUT TO \\beredning1\delad\SERVER\pro9s\EXPORT\allanyproj.txt APPEND.
         PUT projrapp.   
         PUT SKIP.
         OUTPUT CLOSE.
      END.
         
      
   END.      
END
