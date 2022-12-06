/*BORTTAG AV MTRLKATALOG*/

DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.   

   {muswait.i}   
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND 
   MTRL.LEVKOD = leverant USE-INDEX LEV NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrlq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRL THEN DO:
         DELETE MTRL.         
      END.  
   END.
   REPEAT:      
      DO TRANSACTION:
         GET NEXT mtrlq EXCLUSIVE-LOCK.
         IF AVAILABLE MTRL THEN DO:
            DELETE MTRL.
         END.
         ELSE LEAVE.   
      END.
   END.                  
   CLOSE QUERY mtrlq.
   FIND FIRST FORETAG NO-LOCK NO-ERROR.
   IF FORETAG.FORETAG = "VSYD" OR FORETAG.FORETAG = "VORD" OR
   FORETAG.FORETAG = "VAST"  OR FORETAG.FORETAG = "VOST" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\koll.txt APPEND.
      PUT "Borttag klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF FORETAG.FORETAG = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
      PUT "Borttag klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF FORETAG.FORETAG = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
      PUT "Borttag klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF FORETAG.FORETAG = "BORL" THEN DO:
      OUTPUT TO D:\GURU\PRO9\koll.txt APPEND.
      PUT "Borttag klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF FORETAG.FORETAG = "STRA" THEN DO:
      OUTPUT TO E:\DELAD\PRO9\koll.txt APPEND.
      PUT "Borttag klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.
