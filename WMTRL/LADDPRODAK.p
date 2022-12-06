/*LADDPRODAK.p LADDAR PRODAKER .D FILER*/
DEFINE VARIABLE rakn AS INTEGER NO-UNDO.

FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
{AMERICANEUROPEAN.I}
SESSION:NUMERIC-FORMAT = "AMERICAN".
   {muswait.i}   
   FOR EACH PRODAKER EXCLUSIVE-LOCK:
      DELETE PRODAKER.
   END.
   IF  
   Guru.Konstanter:globforetag = "VAST"  OR 
    Guru.Konstanter:globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\prodaker.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF Guru.Konstanter:globforetag = "OPPU" OR Guru.Konstanter:globforetag = "VELK" OR Guru.Konstanter:globforetag = "ELCO" OR Guru.Konstanter:globforetag = "SKOG" OR Guru.Konstanter:globforetag = "GREL" OR Guru.Konstanter:globforetag = "ETSA" THEN DO:
      INPUT FROM C:\PRO9\GURU\WTID\prodaker.d convert target "iso8859-1" source "iso8859-1".
   END.      
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\prodaker.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF Guru.Konstanter:globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\prodaker.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF Guru.Konstanter:globforetag = "STRA" THEN DO:      
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF Guru.Konstanter:globforetag = "ALTE" THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF Guru.Konstanter:globforetag = "KRIN" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF Guru.Konstanter:globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\GURU\WTID\prodaker.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF Guru.Konstanter:globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF Guru.Konstanter:globforetag = "REJI" THEN DO:      
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF Guru.Konstanter:globforetag = "kekr" THEN DO:      
      INPUT FROM C:\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF Guru.Konstanter:globforetag = "wsp" THEN DO:      
      INPUT FROM C:\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF Guru.Konstanter:globforetag = "GULL" THEN DO:      
      INPUT FROM C:\DELAD\prodaker.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM prodaker.d convert target "iso8859-1" source "iso8859-1".
   END.
    
/* borde också ha sökväg 
OR Guru.Konstanter:globforetag = "TRAS" OR Guru.Konstanter:globforetag = "LECA" 
OR Guru.Konstanter:globforetag = "BHEL" OR Guru.Konstanter:globforetag = "GETB" OR Guru.Konstanter:globforetag = "PICA" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "PINN"
OR Guru.Konstanter:globforetag = "VEKA" OR Guru.Konstanter:globforetag = "TECT" THEN Guru.Konstanter:varforetypval[5] = 1. */
   
   REPEAT:
      CREATE PRODAKER.
      ASSIGN.
      IMPORT PRODAKER.     
     
   END.
   INPUT CLOSE.
   
   {EUROPEANAMERICAN.I}
