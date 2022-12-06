/*LADDKALK.p LADDAR KALKYLUPPLÄGG FRÅN .D FILER*/
DEFINE VARIABLE rakn AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
{AMERICANEUROPEAN.I}
SESSION:NUMERIC-FORMAT = "AMERICAN".
   {muswait.i}      
   
   
   IF    globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      prognamn = "e:\delad\pro9\guru\".
   END.
   ELSE IF globforetag = "VELK"   OR globforetag = "GETB"  OR globforetag = "BHEL"   OR globforetag = "GREL" OR  globforetag = "KEWA"  THEN DO:
      prognamn = "C:\PRO10\GURU\WTID\".      
   END.
   ELSE IF  globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"  THEN DO:
      prognamn = "C:\PRO10\GURU\WTID\".
   END.
   ELSE IF  globforetag = "GKAL" THEN DO:
      prognamn = "D:\DELAD\klient\PRO9\".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      prognamn = "d:\elpool\delad\pro9\wrk\".      
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:                 
      prognamn = "D:\DELAD\KLIENT\PRO10\GURU\WTID\".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      prognamn = "D:\DELAD\PRO9\GURU\".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      prognamn = "D:\GURU\PRO9\GURU\WTID\".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      prognamn = "D:\ELPOOL\DELAD\PRO9\". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:      
      prognamn = "E:\DELAD\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS"  THEN DO:      
      prognamn = "C:\DELAD\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "ELKB" OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" 
   OR globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      prognamn = "C:\DELAD\PRO10\GURU\WTID\". 
   END.
   ELSE IF globforetag = "tras" THEN DO:      
      prognamn = "C:\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      prognamn = "C:\ELPOOL\DELAD\PRO10\GURU\WTID\".       
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      prognamn = "D:\SHARED\elpool\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      prognamn = "C:\ELPOOL\DELAD\PRO9\GURU\WTID\". 
   END.   
   ELSE IF globforetag = "ELPA"  THEN DO:
      prognamn = "C:\Pro10\GURU\WTID\". 
   END.   
   /*ELSE DO:
      INPUT FROM p1.d convert target "iso8859-1" source "iso8859-1".
   END.*/
   prognamn2 = prognamn + "p1.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE P1.
      ASSIGN.
      IMPORT P1.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "p2.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE P2.
      ASSIGN.
      IMPORT P2.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "p3.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE P3.
      ASSIGN.
      IMPORT P3.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "lop1.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE LOP1.
      ASSIGN.
      IMPORT LOP1.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "lop2.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE LOP2.
      ASSIGN.
      IMPORT LOP2.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "lop3.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE LOP3.
      ASSIGN.
      IMPORT LOP3.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "ebrpris.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE EBRPRIS.
      ASSIGN.
      IMPORT EBRPRIS.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "sebrpris.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE SEBRPRIS.
      ASSIGN.
      IMPORT SEBRPRIS.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalkbef.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKBEF.
      ASSIGN.
      IMPORT KALKBEF.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "frekvens.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE FREKVENS.
      ASSIGN.
      IMPORT FREKVENS.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "p5.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE P5.
      ASSIGN.
      IMPORT P5.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "lop5.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE LOP5.
      ASSIGN.
      IMPORT LOP5.           
   END.
   INPUT CLOSE.
   
   
   
   
  
   
   /*IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\p1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"   OR globforetag = "GETB"  OR globforetag = "BHEL"   OR globforetag = "GREL" OR  globforetag = "KEWA"  THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1".      
   END.
   ELSE IF  globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"  THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF  globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\p1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\p1.d convert target "iso8859-1" source "iso8859-1".      
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
                 
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\p1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:      
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS"  THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELKB" OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" 
   OR globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1".       
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\p1.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM p1.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE P1.
      ASSIGN.
      IMPORT P1.     
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"   OR 
      globforetag = "GETB"  OR globforetag = "BHEL"  OR globforetag = "GREL" OR  globforetag = "KEWA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU" 
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS"  THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELKB"  OR globforetag = "ORBI" OR globforetag = "SKOK"  OR globforetag = "HANA" OR globforetag = "ATS" OR 
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:      
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\p2.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM p2.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE P2.
      ASSIGN.
      IMPORT P2.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"  OR 
      globforetag = "GETB"  OR globforetag = "BHEL" OR globforetag = "GREL" OR  globforetag = "KEWA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU" 
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA" THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
     /* INPUT FROM C:\PRO10\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS" THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELKB"  OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO"  OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA" THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\p3.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM p3.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE P3.
      ASSIGN.
      IMPORT P3.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
  
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF  globforetag = "VELK"   
      OR globforetag = "GETB"   OR globforetag = "BHEL" 
        OR globforetag = "GREL" OR  globforetag = "KEWA"    THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU" 
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"  THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS"  THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELKB"  OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR 
   globforetag = "JSBF" OR  globforetag = "PICA"  OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\lop1.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM lop1.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE LOP1.
      ASSIGN.
      IMPORT LOP1.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"   OR globforetag = "GETB"  OR 
   globforetag = "BHEL"  OR globforetag = "GREL" OR  globforetag = "KEWA"    THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"  OR globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA"  OR globforetag = "LECA"  THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS" THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELKB"  OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.

   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\lop2.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM lop2.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE LOP2.
      ASSIGN.
      IMPORT LOP2.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"    OR globforetag = "GETB"  OR 
   globforetag = "BHEL" OR globforetag = "GREL" OR  globforetag = "KEWA"     THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU" 
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA" THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS" THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELKB"   OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF  globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\lop3.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM lop3.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE LOP3.
      ASSIGN.
      IMPORT LOP3.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
 
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF  globforetag = "VELK"   OR 
      globforetag = "GETB"   OR globforetag = "BHEL"
        OR globforetag = "GREL"  OR  globforetag = "KEWA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS"  THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ELKB"  OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR 
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF  globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\ebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM ebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE EBRPRIS.
      ASSIGN.
      IMPORT EBRPRIS.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"    OR globforetag = "GETB"  OR 
   globforetag = "BHEL"    OR globforetag = "GREL" OR  globforetag = "KEWA"     THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR  globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA"  OR globforetag = "LECA"  THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS"  THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "elkb"  OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR 
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\sebrpris.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM sebrpris.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE SEBRPRIS.
      ASSIGN.
      IMPORT SEBRPRIS.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST" OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"    OR globforetag = "GETB"  OR 
   globforetag = "BHEL"  OR globforetag = "GREL" OR  globforetag = "KEWA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
      OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA" THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS" THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "elkb"  OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR 
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF  globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\kalkbef.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM kalkbef.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE KALKBEF.
      ASSIGN.
      IMPORT KALKBEF.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.   
  
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF  globforetag = "VELK"   OR globforetag = "GETB"  OR 
   globforetag = "BHEL"   OR globforetag = "GREL" OR  globforetag = "KEWA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
   OR globforetag = "NKON"  OR globforetag = "ETSA" OR globforetag = "LECA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
     /*) INPUT FROM C:\PRO10\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS" THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "elkb"  OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR 
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE  IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\frekvens.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM frekvens.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE FREKVENS.
      ASSIGN.
      IMPORT FREKVENS.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"  OR globforetag = "GETB"  OR 
   globforetag = "BHEL"    OR globforetag = "GREL" OR  globforetag = "KEWA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF  globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"  OR globforetag = "OPPU" 
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"   THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS" THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "elkb"   OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR 
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\p5.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM p5.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE P5.
      ASSIGN.
      IMPORT P5.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   IF  
   globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      INPUT FROM e:\delad\pro9\guru\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "VELK"    OR globforetag = "GETB"  OR 
   globforetag = "BHEL"  OR globforetag = "GREL" OR  globforetag = "KEWA"    THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF  globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"    OR globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"    THEN DO:
      INPUT FROM C:\PRO10\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      /*INPUT FROM C:\PRO10\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1".*/
      INPUT FROM d:\elpool\delad\pro9\wrk\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO10\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      INPUT FROM D:\DELAD\PRO9\GURU\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS" THEN DO:      
      INPUT FROM C:\DELAD\PRO9\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "elkb"  OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" OR 
   globforetag = "JSBF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      INPUT FROM C:\DELAD\PRO10\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF  globforetag = "tras" THEN DO:      
      INPUT FROM C:\PRO9\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      INPUT FROM D:\SHARED\elpool\PRO9\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO9\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.
   /*ELSE IF globforetag = "PICA"  THEN DO:
      INPUT FROM d:\ELPOOL\DELAD\PRO10\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.*/
   ELSE IF globforetag = "ELPA"  THEN DO:
      INPUT FROM C:\Pro10\GURU\WTID\lop5.d convert target "iso8859-1" source "iso8859-1". 
   END.   
   ELSE DO:
      INPUT FROM lop5.d convert target "iso8859-1" source "iso8859-1".
   END.
   rakn = 0.
   REPEAT:
      CREATE LOP5.
      ASSIGN.
      IMPORT LOP5.      
      rakn = rakn + 1.
   END.
   INPUT CLOSE.
   
   
   FIND FIRST FORETAG NO-LOCK NO-ERROR.
   IF FORETAG.FORETAG = "VSYD" OR FORETAG.FORETAG = "VORD" OR
   FORETAG.FORETAG = "VAST"  OR FORETAG.FORETAG = "VOST" THEN DO:
      OUTPUT TO e:\delad\pro9\guru\koll.txt APPEND.
      PUT "Inläsning kalkylupplägg klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF FORETAG.FORETAG = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
      PUT "Inläsning kalkylupplägg klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF FORETAG.FORETAG = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
      PUT "Inläsning kalkylupplägg klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.*/
  {EUROPEANAMERICAN.I}
   
