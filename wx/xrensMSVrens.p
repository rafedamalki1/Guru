/*xrensMSV2.P*/
/*manuellt :
tidslagen
avtal*/
FOR EACH personaltab WHERE personaltab.AKTIV = TRUE NO-LOCK:
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   IF JURPERS.VIJUDID =  "svab" THEN DO:
      /* TIDREGITAB */

      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = personaltab.PERSONALKOD AND TIDREGITAB.DATUM GE 10/01/2009 NO-LOCK. 
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         DO TRANSACTION:         
            FIND CURRENT TIDREGITAB EXCLUSIVE-LOCK.
            DELETE TIDREGITAB.
         END.
         GET NEXT tidq NO-LOCK.
      END.
      CLOSE QUERY tidq.

      /* FLEXTID */
      
      OPEN QUERY flexq FOR EACH FLEXTID WHERE FLEXTID.PERSONALKOD = personaltab.PERSONALKOD AND FLEXTID.DATUM GE 10/01/2009 NO-LOCK. 
      
      GET FIRST flexq NO-LOCK.
      DO WHILE AVAILABLE(FLEXTID):
         DO TRANSACTION:         
            FIND CURRENT FLEXTID EXCLUSIVE-LOCK.
            DELETE FLEXTID.
         END.        
         GET NEXT flexq NO-LOCK.
      END.
      CLOSE QUERY flexq.
      /* FLEXDAG */
      
      OPEN QUERY fldq FOR EACH FLEXDAG WHERE FLEXDAG.PERSONALKOD = personaltab.PERSONALKOD AND FLEXDAG.DATUM GE 10/01/2009 NO-LOCK. 
      
      GET FIRST fldq NO-LOCK.
      DO WHILE AVAILABLE(FLEXDAG):
         DO TRANSACTION:         
            FIND CURRENT FLEXDAG EXCLUSIVE-LOCK.
            DELETE FLEXDAG.
         END.                 
         GET NEXT fldq NO-LOCK.
      END.
      CLOSE QUERY fldq.
      
      
      /* FLBET */      
      /*OPEN QUERY flbq FOR EACH FLBET WHERE FLBET.PERSONALKOD = personaltab.PERSONALKOD AND FLBET.DATUM GE 10/01/2009 NO-LOCK. 
      
      GET FIRST flbq NO-LOCK.
      DO WHILE AVAILABLE(FLBET):
         DO TRANSACTION:         
            FIND CURRENT FLBET EXCLUSIVE-LOCK.
            DELETE FLBET.
         END.                          
         GET NEXT flbq NO-LOCK.
      END.
      CLOSE QUERY flbq.*/
      
      
      /*/* FLEXSALDO */
      
      OPEN QUERY fsalq FOR EACH FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = personaltab.PERSONALKOD  NO-LOCK. 
      GET FIRST fsalq NO-LOCK.
      DO WHILE AVAILABLE(FLEXSALDO):
         DO TRANSACTION:         
            FIND CURRENT FLEXSALDO EXCLUSIVE-LOCK.
            DELETE FLEXSALDO.
         END.                          
         
         GET NEXT fsalq NO-LOCK.
      END.
      CLOSE QUERY fsalq.*/
      
      

      /* SUMTIDDAG */
      
      /*OPEN QUERY sumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.PERSONALKOD = personaltab.PERSONALKOD AND SUMTIDDAG.DATUM GE 10/01/2009 NO-LOCK. 
      
      GET FIRST sumq NO-LOCK.
      DO WHILE AVAILABLE(SUMTIDDAG):
         DO TRANSACTION:         
            FIND CURRENT SUMTIDDAG EXCLUSIVE-LOCK.
            DELETE SUMTIDDAG.
         END.                          
         
         GET NEXT sumq NO-LOCK.
      END.
      CLOSE QUERY sumq. 
      
   
      /* VECKOARBAV */
      
      OPEN QUERY vaavq FOR EACH VECKOARBAV WHERE VECKOARBAV.PERSONALKOD = personaltab.PERSONALKOD AND VECKOARBAV.VECKONUMMER GE 200941 NO-LOCK. 
      
      GET FIRST vaavq NO-LOCK.
      DO WHILE AVAILABLE(VECKOARBAV):
         DO TRANSACTION:         
            FIND CURRENT VECKOARBAV EXCLUSIVE-LOCK.
            DELETE VECKOARBAV.
         END.                                   
         GET NEXT vaavq NO-LOCK.
      END.
      CLOSE QUERY vaavq.*/
      
   
      
   END.
END.


 


