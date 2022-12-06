/*LADDKALK.p LADDAR KALKYLUPPLÄGG FRÅN .D FILER*/
DEFINE VARIABLE rakn AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER  NO-UNDO.

{AMERICANEUROPEAN.I}
SESSION:NUMERIC-FORMAT = "AMERICAN".
   {muswait.i}      
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.   
   
   IF FORETAG.FORETAG = "ats"   THEN DO:
      prognamn = "C:\DELAD\ATS\".            
   END.
   ELSE IF FORETAG.FORETAG = "afco"  THEN DO:
      prognamn = "C:\DELAD\ÅF\".            
   END.
   
   prognamn2 = prognamn + "berkalknum.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE BERKALKOPPLA.
      ASSIGN.
      IMPORT BERKALKOPPLA.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalkaonr.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKAONR.
      ASSIGN.
      IMPORT KALKAONR.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalkegnapriser.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKEGNAPRISER.
      ASSIGN.
      IMPORT KALKEGNAPRISER.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalkfaktorer.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKFAKTORER.
      ASSIGN.
      IMPORT KALKFAKTORER.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalkhuv.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKHUV.
      ASSIGN.
      IMPORT KALKHUV.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalkmtrl.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKMTRL.
      ASSIGN.
      IMPORT KALKMTRL.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalknum.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKNUM.
      ASSIGN.
      IMPORT KALKNUM.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalknumanvegen.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKNUMANVEGEN.
      ASSIGN.
      IMPORT KALKNUMANVEGEN.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalknumanvegensum.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKNUMANVEGENSUB.
      ASSIGN.
      IMPORT KALKNUMANVEGENSUB.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalknumsub.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKNUMSUB.
      ASSIGN.
      IMPORT KALKNUMSUB.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "kalkyltidlage.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE KALKYLTIDLAGE.
      ASSIGN.
      IMPORT KALKYLTIDLAGE.           
   END.
   INPUT CLOSE.
   
  
  {EUROPEANAMERICAN.I}
   
