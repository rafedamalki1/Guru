/*DUMPGRUP.p PROGRAMMET DUMPAR UT EN VALD KONSTGRUPP*/
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER valgrupp LIKE KONSTGRUPP.KONSKOD NO-UNDO.

/*    DEFINE VARIABLE valgrupp AS INTEGER NO-UNDO.      */
/*    valgrupp = 33.                                    */
{AMERICANEUROPEAN.I}
   IF  globforetag = "VAST"  THEN DO:
      prognamn = "e:\delad\pro9\guru\".
      /*prognamn = "C:\PRO10\GURU\".*/
   END.      
   ELSE IF globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"   THEN DO:
      prognamn = "C:\PRO10\GURU\".
   END.
   ELSE IF  globforetag = "GKAL" THEN DO:
      prognamn = "D:\DELAD\klient\PRO9\".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      prognamn = "d:\elpool\delad\pro9\wrk\".      
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:                 
      prognamn = "D:\DELAD\PRO10\".
   END.
   ELSE IF globforetag = "NOSS"  THEN DO:      
      prognamn = "C:\DELAD\PRO10\". 
   END.        
   ELSE IF globforetag = "FORS"  THEN DO:      
      prognamn = "C:\DELAD\PRO9\". 
   END.
   ELSE IF globforetag = "ROJO"  THEN DO:      
      prognamn = "C:\elpool\delad\pro10s\". 
   END.
   ELSE IF globforetag = "POFO"  THEN DO:      
      prognamn = "C:\elpool\delad\pro10s\". 
   END.
    
   ELSE IF globforetag = "ELPA"  THEN DO:
      prognamn = "C:\Pro10\GURU\WTID\". 
   END.
   MESSAGE "Konstruktionsgruppen läggs i : " prognamn
   VIEW-AS ALERT-BOX.
   prognamn2 = prognamn + "konstgr.d". 
   OUTPUT TO VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   
   OPEN QUERY kq FOR EACH KONSTGRUPP WHERE KONSTGRUPP.KONSKOD = valgrupp
   NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KONSTGRUPP):
      EXPORT KONSTGRUPP.
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
   OUTPUT CLOSE.
   
   prognamn2 = prognamn + "bbenamn.d". 
   OUTPUT TO VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".     
     
   OPEN QUERY ordq FOR EACH BBENAMNING WHERE BBENAMNING.KONSKOD = valgrupp NO-LOCK.  
   GET FIRST ordq NO-LOCK.
   DO WHILE AVAILABLE(BBENAMNING):
      EXPORT BBENAMNING.       
      GET NEXT ordq NO-LOCK.
   END.
   CLOSE QUERY ordq.
   OUTPUT CLOSE.

   prognamn2 = prognamn + "edigrupp.d". 
   OUTPUT TO VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
      
   OPEN QUERY ediq FOR EACH EDIGRUPP WHERE EDIGRUPP.KONSKOD = valgrupp NO-LOCK.  
   GET FIRST ediq NO-LOCK.
   DO WHILE AVAILABLE(EDIGRUPP):
      EXPORT EDIGRUPP.       
      GET NEXT ediq NO-LOCK.
   END.
   CLOSE QUERY ediq.
   OUTPUT CLOSE.
   
   prognamn2 = prognamn + "konstru.d". 
   OUTPUT TO VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".      
   OPEN QUERY friq FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.     
   GET FIRST friq NO-LOCK.
   DO WHILE AVAILABLE(KONSTRUKTION):
      EXPORT KONSTRUKTION.       
      GET NEXT friq NO-LOCK.
   END.
   CLOSE QUERY friq.
   OUTPUT CLOSE.
   
   prognamn2 = prognamn + "konstval.d". 
   OUTPUT TO VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".      
   OPEN QUERY berqid FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = valgrupp NO-LOCK.      
   GET FIRST berqid NO-LOCK.
   DO WHILE AVAILABLE(KONSTVAL):       
      EXPORT KONSTVAL.
      GET NEXT berqid NO-LOCK.
   END.
   CLOSE QUERY berqid. 
   OUTPUT CLOSE.
   
   prognamn2 = prognamn + "mtrlber.d". 
   OUTPUT TO VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".   
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
   
   prognamn2 = prognamn + "kalkber.d". 
   OUTPUT TO VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".      
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
{EUROPEANAMERICAN.I}