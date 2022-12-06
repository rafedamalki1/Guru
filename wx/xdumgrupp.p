/*PROGRAMMET DUMPAR UT EN VALD KONSTGRUPP*/
/* DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.  */
/* DEFINE INPUT PARAMETER valgrupp LIKE KONSTGRUPP.KONSKOD NO-UNDO.  */
   DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valgrupp AS INTEGER NO-UNDO.
   FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
   globforetag = foretag.foretag.
   /*valgrupp = 31.   */
   valgrupp = 9.  
   IF  
   globforetag = "VAST" OR  globforetag = "VSAB" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\konstgr.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "GKAL" THEN DO:      
      OUTPUT TO D:\DELAD\klient\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      OUTPUT TO /u01/guru/wrk/konstgr.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO c:\konstgr.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\ELPOOL\DELAD\PRO9\konstgr.d  convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      OUTPUT TO c:\konstgr.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\DELAD\KLIENT\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "elpa" OR globforetag = "NYLB"  THEN DO:
      OUTPUT TO C:\PRO9\GURU\WTID\konstgr.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\DELAD\KLIENT\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".*/
   END.   
   ELSE DO:
      OUTPUT TO konstgr.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   OPEN QUERY kq FOR EACH KONSTGRUPP WHERE KONSTGRUPP.KONSKOD = valgrupp
   NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KONSTGRUPP):
      EXPORT KONSTGRUPP.
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
   OUTPUT CLOSE.
         
   IF  
   globforetag = "VAST" OR  globforetag = "VSAB" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\bbenamn.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      OUTPUT TO /u01/guru/wrk/bbenamn.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   ELSE IF globforetag = "GKAL" THEN DO:      
      OUTPUT TO D:\DELAD\klient\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO c:\bbenamn.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\ELPOOL\DELAD\PRO9\bbenamn.d  convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      OUTPUT TO c:\bbenamn.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\DELAD\KLIENT\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "elpa" OR globforetag = "NYLB" THEN DO:
      OUTPUT TO C:\PRO9\GURU\WTID\bbenamn.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\DELAD\KLIENT\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".*/
   END.   
   ELSE DO:
      OUTPUT TO bbenamn.d convert target "iso8859-1" source "iso8859-1" .  
   END.   
   OPEN QUERY ordq FOR EACH BBENAMNING WHERE BBENAMNING.KONSKOD = valgrupp NO-LOCK.  
   GET FIRST ordq NO-LOCK.
   DO WHILE AVAILABLE(BBENAMNING):
      EXPORT BBENAMNING.       
      GET NEXT ordq NO-LOCK.
   END.
   CLOSE QUERY ordq.
   OUTPUT CLOSE.

   IF  
   globforetag = "VAST" OR  globforetag = "VSAB" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\edigrupp.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   ELSE IF globforetag = "GKAL" THEN DO:      
      OUTPUT TO D:\DELAD\klient\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      OUTPUT TO /u01/guru/wrk/edigrupp.d convert target "iso8859-1" source "iso8859-1" .  
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO c:\edigrupp.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\ELPOOL\DELAD\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      OUTPUT TO c:\edigrupp.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\DELAD\KLIENT\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "elpa" OR globforetag = "NYLB" THEN DO:
      OUTPUT TO C:\PRO9\GURU\WTID\edigrupp.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\DELAD\KLIENT\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".*/
   END.   
   ELSE DO:
      OUTPUT TO edigrupp.d convert target "iso8859-1" source "iso8859-1" .  
   END.   
   OPEN QUERY ediq FOR EACH EDIGRUPP WHERE EDIGRUPP.KONSKOD = valgrupp NO-LOCK.  
   GET FIRST ediq NO-LOCK.
   DO WHILE AVAILABLE(EDIGRUPP):
      EXPORT EDIGRUPP.       
      GET NEXT ediq NO-LOCK.
   END.
   CLOSE QUERY ediq.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST" OR  globforetag = "VSAB" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\konstru.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      OUTPUT TO /u01/guru/wrk/konstru.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "GKAL" THEN DO:      
      OUTPUT TO D:\DELAD\klient\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO c:\konstru.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\ELPOOL\DELAD\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      OUTPUT TO c:\konstru.d  convert target "iso8859-1" source "iso8859-1".
      OUTPUT TO D:\DELAD\KLIENT\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "elpa" OR globforetag = "NYLB" THEN DO:
      OUTPUT TO C:\PRO9\GURU\WTID\konstru.d  convert target "iso8859-1" source "iso8859-1".      
   END.   
   ELSE DO:
      OUTPUT TO konstru.d convert target "iso8859-1" source "iso8859-1" .
   END.   
   OPEN QUERY friq FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.     
   GET FIRST friq NO-LOCK.
   DO WHILE AVAILABLE(KONSTRUKTION):
      EXPORT KONSTRUKTION.       
      GET NEXT friq NO-LOCK.
   END.
   CLOSE QUERY friq.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST" OR  globforetag = "VSAB" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\konstval.d convert target "iso8859-1" source "iso8859-1" . 
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO /u01/guru/wrk/konstval.d convert target "iso8859-1" source "iso8859-1" . 
   END.
   ELSE IF  globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1" . 
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO c:\konstval.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\ELPOOL\DELAD\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      OUTPUT TO c:\konstval.d  convert target "iso8859-1" source "iso8859-1".
      OUTPUT TO D:\DELAD\KLIENT\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "elpa" OR globforetag = "NYLB" THEN DO:
      OUTPUT TO C:\PRO9\GURU\WTID\konstval.d  convert target "iso8859-1" source "iso8859-1".      
   END.   
   ELSE DO:
      OUTPUT TO konstval.d convert target "iso8859-1" source "iso8859-1" . 
   END.   
   OPEN QUERY berqid FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = valgrupp NO-LOCK.      
   GET FIRST berqid NO-LOCK.
   DO WHILE AVAILABLE(KONSTVAL):       
      EXPORT KONSTVAL.
      GET NEXT berqid NO-LOCK.
   END.
   CLOSE QUERY berqid. 
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST" OR  globforetag = "VSAB" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\mtrlber.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      OUTPUT TO /u01/guru/wrk/mtrlber.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "GKAL" THEN DO:      
      OUTPUT TO D:\DELAD\klient\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO c:\mtrlber.d  convert target "iso8859-1" source "iso8859-1".
     /* OUTPUT TO D:\ELPOOL\DELAD\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      OUTPUT TO c:\mtrlber.d  convert target "iso8859-1" source "iso8859-1".
      OUTPUT TO D:\DELAD\KLIENT\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "elpa" OR globforetag = "NYLB" THEN DO:
      OUTPUT TO C:\PRO9\GURU\WTID\mtrlber.d  convert target "iso8859-1" source "iso8859-1".      
   END.   
   ELSE DO:
      OUTPUT TO mtrlber.d convert target "iso8859-1" source "iso8859-1" .
   END.   
   OPEN QUERY mq FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.     
   GET FIRST mq NO-LOCK.
   DO WHILE AVAILABLE(KONSTRUKTION):
      OPEN QUERY mq2 FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD NO-LOCK.
      GET FIRST mq2 NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         EXPORT MTRLBER.
         GET NEXT mq2 NO-LOCK.
      END.
      CLOSE QUERY mq2.          
      GET NEXT mq NO-LOCK.
   END.
   CLOSE QUERY mq.
   OUTPUT CLOSE.  
   
   IF  
   globforetag = "VAST" OR  globforetag = "VSAB" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\kalkber.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      OUTPUT TO /u01/guru/wrk/kalkber.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "GKAL" THEN DO:      
      OUTPUT TO D:\DELAD\klient\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1" .
   END.
   ELSE IF globforetag = "LULE" THEN DO:
       OUTPUT TO c:\kalkber.d  convert target "iso8859-1" source "iso8859-1".
      /*OUTPUT TO D:\ELPOOL\DELAD\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".*/
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      OUTPUT TO c:\kalkber.d  convert target "iso8859-1" source "iso8859-1".
      OUTPUT TO D:\DELAD\KLIENT\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "elpa" OR globforetag = "NYLB" THEN DO:
      OUTPUT TO C:\PRO9\GURU\WTID\kalkber.d  convert target "iso8859-1" source "iso8859-1".      
   END.   
   ELSE DO:
      OUTPUT TO kalkber.d convert target "iso8859-1" source "iso8859-1" .
   END.   
   OPEN QUERY mq3 FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.     
   GET FIRST mq3 NO-LOCK.
   DO WHILE AVAILABLE(KONSTRUKTION):
      OPEN QUERY kalkq FOR EACH KALKBER WHERE KALKBER.KTYPKOD = KONSTRUKTION.KTYPKOD 
      NO-LOCK.
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKBER):
         EXPORT KALKBER.
         GET NEXT kalkq NO-LOCK.
      END.                              
      CLOSE QUERY kalkq.
      GET NEXT mq3 NO-LOCK.
   END.
   CLOSE QUERY mq3.
   OUTPUT CLOSE.
