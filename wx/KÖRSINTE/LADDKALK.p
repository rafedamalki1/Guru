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
   ELSE IF globforetag = "skek"  THEN DO:
      prognamn = "C:\PRO10\".      
   END.
   ELSE IF globforetag = "VELK"    OR globforetag = "BHEL"   OR globforetag = "GREL"  THEN DO:
      prognamn = "C:\PRO10\GURU\WTID\".      
   END.
   ELSE IF globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
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
   OR globforetag = "JSBF" OR globforetag = "KRAF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC"
   OR globforetag = "POLA" OR globforetag = "KNOR" OR globforetag = "Kewa" OR globforetag = "NAEK" OR globforetag = "YSEN" OR globforetag = "POMA" OR  globforetag = "OVIK" OR  globforetag = "TECT"  THEN DO:      
      prognamn = "C:\DELAD\PRO10\GURU\WTID\". 
   END.
   ELSE IF globforetag = "SKEK" THEN DO:      
      prognamn = "C:\DELAD\PRO10\GURU\WTID\". 
   END. 
   ELSE IF globforetag = "tras" THEN DO:      
      prognamn = "C:\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" OR globforetag = "getb" THEN DO:
      prognamn = "C:\ELPOOL\DELAD\PRO10\GURU\WTID\".       
   END.
   ELSE IF globforetag = "CTECT" THEN DO:      
      prognamn = "D:\SHARED\elpool\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      /*prognamn = "C:\Pro10\GURU\".*/
      prognamn = "C:\ELPOOL\DELAD\PRO9\GURU\WTID\". 
   END.   
   ELSE IF globforetag = "CKRAF"  THEN DO:
      prognamn = "C:\Pro10\GURU\". 
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
   
   
   
   
  
  {EUROPEANAMERICAN.I}
   
