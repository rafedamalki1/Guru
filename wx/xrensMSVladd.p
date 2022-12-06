/*xrensMSV2.P*/
/*manuellt :
tidslagen
avtal*/

INPUT FROM c:\tidregit.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE TIDREGITAB.
   ASSIGN.
   IMPORT TIDREGITAB.           
END.
INPUT CLOSE.

INPUT FROM c:\flextid.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE FLEXTID.
   ASSIGN.
   IMPORT FLEXTID.           
END.
INPUT CLOSE.
INPUT FROM c:\flexdag.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE FLEXDAG.
   ASSIGN.
   IMPORT FLEXDAG.           
END.
INPUT CLOSE.

INPUT FROM c:\flbet.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE FLBET.
   ASSIGN.
   IMPORT FLBET.           
END.
INPUT CLOSE.

INPUT FROM c:\flexsald.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE FLEXSALDO.
   ASSIGN.
   IMPORT FLEXSALDO.           
END.
INPUT CLOSE.
INPUT FROM c:\sumtidda.d  convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE SUMTIDDAG.
   ASSIGN.
   IMPORT SUMTIDDAG.           
END.
INPUT CLOSE.
INPUT FROM ny1.d convert target "iso8859-1" source "iso8859-1". 
REPEAT:
   CREATE VECKOARBAV.
   ASSIGN.
   IMPORT VECKOARBAV.           
END.
INPUT CLOSE.



/*FOR EACH personaltab NO-LOCK:
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   IF JURPERS.VIJUDID =  "svab" THEN DO:
      /* TIDREGITAB */
      OUTPUT TO c:\tidregit.d  convert target "iso8859-1" source "iso8859-1".
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = personaltab.PERSONALKOD AND TIDREGITAB.DATUM GE 10/01/2009 NO-LOCK. 
      DO TRANSACTION:      
         GET FIRST tidq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB):
            EXPORT TIDREGITAB.
            GET NEXT tidq NO-LOCK.
         END.
         CLOSE QUERY tidq.
         OUTPUT CLOSE.
      END.
      /* FLEXTID */
      OUTPUT TO c:\flextid.d  convert target "iso8859-1" source "iso8859-1".
      OPEN QUERY flexq FOR EACH FLEXTID WHERE FLEXTID.PERSONALKOD = personaltab.PERSONALKOD AND FLEXTID.DATUM GE 10/01/2009 NO-LOCK. 
      DO TRANSACTION:      
         GET FIRST flexq NO-LOCK.
         DO WHILE AVAILABLE(FLEXTID):
            EXPORT FLEXTID.
            GET NEXT flexq NO-LOCK.
         END.
         CLOSE QUERY flexq.
         OUTPUT CLOSE.
      END.
      /* FLEXDAG */
      OUTPUT TO c:\flexdag.d  convert target "iso8859-1" source "iso8859-1".
      OPEN QUERY fldq FOR EACH FLEXDAG WHERE FLEXDAG.PERSONALKOD = personaltab.PERSONALKOD AND FLEXDAG.DATUM GE 10/01/2009 NO-LOCK. 
      DO TRANSACTION:      
         GET FIRST fldq NO-LOCK.
         DO WHILE AVAILABLE(FLEXDAG):
            EXPORT FLEXDAG.
            GET NEXT fldq NO-LOCK.
         END.
         CLOSE QUERY fldq.
         OUTPUT CLOSE.
      END.
      /* FLBET */
      OUTPUT TO c:\flbet.d  convert target "iso8859-1" source "iso8859-1".
      OPEN QUERY flbq FOR EACH FLBET WHERE FLBET.PERSONALKOD = personaltab.PERSONALKOD AND FLBET.DATUM GE 10/01/2009 NO-LOCK. 
      DO TRANSACTION:      
         GET FIRST flbq NO-LOCK.
         DO WHILE AVAILABLE(FLBET):
            EXPORT FLBET.
            GET NEXT flbq NO-LOCK.
         END.
         CLOSE QUERY flbq.
         OUTPUT CLOSE.
      END.

      /* FLBET */
      OPEN QUERY flbq FOR EACH FLBET WHERE FLBET.PERSONALKOD = personaltab.PERSONALKOD AND FLBET.DATUM GE 10/01/2009 NO-LOCK. 
      DO TRANSACTION:       
         GET FIRST flbq EXCLUSIVE-LOCK.
         IF AVAILABLE FLBET THEN DELETE FLBET.    
      END.
      REPEAT:  
         DO TRANSACTION:
            GET NEXT flbq EXCLUSIVE-LOCK.
            IF AVAILABLE FLBET THEN DELETE FLBET.    
            ELSE LEAVE.      
         END.         
      END.   
      CLOSE QUERY flbq.
      /* FLEXSALDO */
      OUTPUT TO c:\flexsald.d  convert target "iso8859-1" source "iso8859-1".
      OPEN QUERY fsalq FOR EACH FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = personaltab.PERSONALKOD  NO-LOCK. 
      DO TRANSACTION:      
         GET FIRST fsalq NO-LOCK.
         DO WHILE AVAILABLE(FLEXSALDO):
            EXPORT FLEXSALDO.
            GET NEXT fsalq NO-LOCK.
         END.
         CLOSE QUERY fsalq.
         OUTPUT CLOSE.
      END.

      /* SUMTIDDAG */
      OUTPUT TO c:\sumtidda.d  convert target "iso8859-1" source "iso8859-1".
      OPEN QUERY sumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.PERSONALKOD = personaltab.PERSONALKOD AND SUMTIDDAG.DATUM GE 10/01/2009 NO-LOCK. 
      DO TRANSACTION:      
         GET FIRST sumq NO-LOCK.
         DO WHILE AVAILABLE(SUMTIDDAG):
            EXPORT SUMTIDDAG.
            GET NEXT sumq NO-LOCK.
         END.
         CLOSE QUERY sumq.
         OUTPUT CLOSE.
      END.
      /* VECKOARBAV */
      OUTPUT TO c:\ny1.d  convert target "iso8859-1" source "iso8859-1".
      OPEN QUERY vaavq FOR EACH VECKOARBAV WHERE VECKOARBAV.PERSONALKOD = personaltab.PERSONALKOD AND VECKOARBAV.VECKONUMMER GE 200941 NO-LOCK. 
      DO TRANSACTION:      
         GET FIRST vaavq NO-LOCK.
         DO WHILE AVAILABLE(VECKOARBAV):
            EXPORT VECKOARBAV.
            GET NEXT vaavq NO-LOCK.
         END.
         CLOSE QUERY vaavq.
         OUTPUT CLOSE.
      END.
      
   END.
END. */


 


