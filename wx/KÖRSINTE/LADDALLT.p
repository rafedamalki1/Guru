/*LADDALLT.p PROGRAMMET LADDAR BEREDNINGSUPPLÄGGET*/
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.

   {muswait.i}
     GLOBFORETAG = "ELPA".
     {AMERICANEUROPEAN.I}
   SESSION:NUMERIC-FORMAT = "EUROPEAN".
   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE KONSTGRUPP.
      ASSIGN.
      IMPORT KONSTGRUPP.
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE BBENAMNING.
      ASSIGN.
      IMPORT BBENAMNING.
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE EDIGRUPP.
      ASSIGN.
      IMPORT EDIGRUPP.
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE KONSTRUKTION.
      ASSIGN.
      IMPORT KONSTRUKTION.
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE KONSTVAL.
      ASSIGN.
      IMPORT KONSTVAL.
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE MTRLBER.
      ASSIGN.
      IMPORT MTRLBER.
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE BERSTOLP.
      ASSIGN.
      IMPORT BERSTOLP.
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE BERSKAP.
      ASSIGN.
      IMPORT BERSKAP.
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\kalkber.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE KALKBER.  
      ASSIGN.     
      IMPORT KALKBER.  
   END.
   INPUT CLOSE.
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\koll.txt APPEND.
      PUT "Laddning av allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
      PUT "Laddning av allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF  globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
      PUT "Laddning av allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      OUTPUT TO E:\DELAD\PRO9\koll.txt APPEND.
      PUT "Laddning av allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
      PUT "Laddning av allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      OUTPUT TO G:\DELAD\PRO9\koll.txt APPEND.
      PUT "Laddning av allt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
{EUROPEANAMERICAN.I}