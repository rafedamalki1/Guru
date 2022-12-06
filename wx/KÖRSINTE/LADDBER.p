/*LADDBER.p LADDAR BEREDNING FRÅN .D FILER*/
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE expimphdh AS HANDLE NO-UNDO.
{muswait.i}
{AMERICANEUROPEAN.I}
IF  
globforetag = "VAST"  THEN DO:
   INPUT FROM e:\delad\pro9\guru\berednin.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\berednin.d convert target "iso8859-1" source "iso8859-1".
   END.
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berednin.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\berednin.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM berednin.d convert target "iso8859-1" source "iso8859-1".
   END.
END.
REPEAT:
   CREATE BEREDNING.
   ASSIGN.
   IMPORT BEREDNING.
END.
INPUT CLOSE.

IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\berval.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berval.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\berval.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\berval.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM berval.d convert target "iso8859-1" source "iso8859-1".
   END.      
END.
REPEAT:
   CREATE BERVAL.
   ASSIGN.
   IMPORT BERVAL.
END.
INPUT CLOSE.

IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\berord.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berord.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\berord.d convert target "iso8859-1" source "iso8859-1". 
   END.
  
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\berord.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM berord.d convert target "iso8859-1" source "iso8859-1".
   END.      
END.
REPEAT:
   CREATE BERORD.
   ASSIGN.
   IMPORT BERORD.
END.
INPUT CLOSE.

IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\frikort.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\frikort.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\frikort.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\frikort.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM frikort.d convert target "iso8859-1" source "iso8859-1".
   END.      
END.
REPEAT:
   CREATE FRIKORT.
   ASSIGN.
   IMPORT FRIKORT.
END.
INPUT CLOSE.

IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\berid.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berid.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\berid.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\berid.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM berid.d convert target "iso8859-1" source "iso8859-1".
   END.      
END.
REPEAT:
   CREATE BERID.
   ASSIGN.
   IMPORT BERID.
END.
INPUT CLOSE.

IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\berkalk.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berkalk.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\berkalk.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\berkalk.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM berkalk.d convert target "iso8859-1" source "iso8859-1".
   END.      
END.
REPEAT:
   CREATE BERKALK.
   ASSIGN.
   IMPORT BERKALK.
END.
INPUT CLOSE.
   
IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\bermtrl.d convert target "iso8859-1" source "iso8859-1". 
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\bermtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\bermtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\bermtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM bermtrl.d convert target "iso8859-1" source "iso8859-1".
   END.      
END.   
REPEAT: 
   CREATE BERMTRL.  
   ASSIGN.     
   IMPORT BERMTRL.  
END.
INPUT CLOSE.   
    
IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\berlink.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berlink.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\berlink.d convert target "iso8859-1" source "iso8859-1". 
   END.

   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\berlink.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM berlink.d convert target "iso8859-1" source "iso8859-1".
   END.      
END.
REPEAT:
   CREATE BERLINKAB.
   ASSIGN.
   IMPORT BERLINKAB.
END.
INPUT CLOSE.

IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\kskydd.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\kskydd.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\kskydd.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM kskydd.d convert target "iso8859-1" source "iso8859-1".
   END.      
END.
REPEAT:
   CREATE KSKYDD.
   ASSIGN.
   IMPORT KSKYDD.
END.
INPUT CLOSE.

IF  
globforetag = "VAST"   THEN DO:
   INPUT FROM e:\delad\pro9\guru\berupp.d convert target "iso8859-1" source "iso8859-1".
END.
ELSE DO:
   IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berupp.d convert target "iso8859-1" source "iso8859-1". 
   END.
   IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\berupp.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      INPUT FROM berupp.d convert target "iso8859-1" source "iso8859-1".
   END.
END.
REPEAT:
   CREATE BERUPP.
   ASSIGN.
   IMPORT BERUPP.
END.
INPUT CLOSE.

/*HD HÄR*/
/*
RUN FINNSTABELL.P (INPUT "HDKALK", OUTPUT bloblog).
IF bloblog = TRUE THEN DO:
   RUN EXPIMPHD.P PERSISTENT SET expimphdh.
   RUN hdladd_UI (INPUT INTEGER(valaonr), INPUT valomrade).
   IF VALID-HANDLE(expimphdh) THEN DELETE PROCEDURE expimphdh NO-ERROR.   
END. 
*/
{EUROPEANAMERICAN.I}
