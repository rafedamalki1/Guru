/*LADDMTRL.p TAR BORT MTRL FÖR LEVERANTÖR OCH LADDAR MTRL FRÅN .D FIL*/

DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.   
DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{AMERICANEUROPEAN.I}
   {muswait.i}
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND
   MTRL.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrlq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRL THEN DO:
         DELETE MTRL.
         GET NEXT mtrlq EXCLUSIVE-LOCK.
      END.
   END.
   DO WHILE AVAILABLE(MTRL):
      DO TRANSACTION:
         DELETE MTRL.
         GET NEXT mtrlq EXCLUSIVE-LOCK.
      END.
   END.
   CLOSE QUERY mtrlq.
         
   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\mtrl.d convert target "iso8859-1" source "iso8859-1".       
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      SESSION:NUMERIC-FORMAT = "AMERICAN".
      INPUT FROM d:\elpool\delad\pro9\wrk\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
  
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELPA" THEN DO:
      INPUT FROM \\pc112\DELAD\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELTE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\GURU\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GREL"   THEN DO:
      INPUT FROM C:\PRO9\GURU\WTID\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ATS"   THEN DO:
      INPUT FROM C:\DELAD\PRO10\GURU\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "FORS"  THEN DO:      
      INPUT FROM C:\DELAD\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ORBI" THEN DO:
      INPUT FROM C:\delad\pro10\guru\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELKB" THEN DO:
      INPUT FROM C:\delad\pro10\guru\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "KRIN" THEN DO:
      INPUT FROM C:\delad\pro10\guru\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   
   ELSE DO:         
      INPUT FROM mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.         
   IF globforetag = "sund" OR globforetag = "SNAT" OR globforetag = "CORBI" OR globforetag = "Celpa" THEN DO:
      REPEAT: 
         CREATE MTRL.  
         ASSIGN.     
         IMPORT MTRL.  
         IF SUBSTRING(MTRL.ENR,1,1) = "E"  THEN.
         ELSE MTRL.ENR = "E" + MTRL.ENR.
         {MTRLCREATE.I} 
      END.
      INPUT CLOSE.
   END.
   ELSE DO:   
      REPEAT: 
         CREATE MTRL.  
         ASSIGN.     
         IMPORT MTRL.  
         {MTRLCREATE.I} 
      END.
      INPUT CLOSE.
   END.
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\koll.txt APPEND.
      PUT "inläsning mtrl klart. " SUBSTRING(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
      PUT "inläsning mtrl klart. " SUBSTRING(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
      PUT "inläsning mtrl klart. " SUBSTRING(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      OUTPUT TO E:\DELAD\PRO9\koll.txt APPEND.
      PUT "inläsning mtrl klart. " SUBSTRING(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      OUTPUT TO D:\GURU\PRO9\koll.txt APPEND.
      PUT "inläsning mtrl klart. " SUBSTRING(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
      PUT "inläsning mtrl klart. " SUBSTRING(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
{EUROPEANAMERICAN.I}
