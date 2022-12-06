/*DUMPEGENK.P DUMPAR FASTANM för egenkontroll */

{AMERICANEUROPEAN.I}
   {muswait.i}
   FIND FIRST FORETAG  WHERE NO-LOCK NO-ERROR.      
   IF FORETAG.FORETAG = "KRIN"   THEN DO:
      OUTPUT TO C:\delad\KRIN\fastanm.d convert target "iso8859-1" source "iso8859-1".
   
      OPEN QUERY egenq FOR EACH FASTANM WHERE FASTANM.PROGRAM BEGINS "EGENK" NO-LOCK.   
      GET FIRST egenq NO-LOCK.
      DO WHILE AVAILABLE(FASTANM):
         EXPORT FASTANM.
         GET NEXT egenq NO-LOCK.
      END.                  
      CLOSE QUERY egenq.
      OUTPUT CLOSE.
   END.     
{EUROPEANAMERICAN.I}