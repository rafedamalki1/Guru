DEFINE VARIABLE vem AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgtabh AS HANDLE NO-UNDO.
CREATE BUFFER orgtabh FOR TABLE "PERSONALTAB".
vem = "eb". 

PROCEDURE l1_UI :
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DO TRANSACTION:
   REPEAT :
      IF i > 10000 THEN LEAVE.
      
      FIND FIRST PERSONALTAB WHERE personalkod = vem EXCLUSIVE-LOCK .
      
      i = i + 1.
   END.
   END.
   MESSAGE "klar"
   VIEW-AS ALERT-BOX.
END PROCEDURE.

PROCEDURE l3_UI :
   FIND FIRST PERSONALTAB WHERE personalkod = vem EXCLUSIVE-LOCK .
    PERSONALTAB.FORNAMN =  PERSONALTAB.FORNAMN +  PERSONALTAB.FORNAMN.
   RUN l2_UI.
END PROCEDURE. 
 
PROCEDURE l2_UI :
  
   IF LOCKED(PERSONALTAB) THEN   MESSAGE "l�st"
   VIEW-AS ALERT-BOX.
   MESSAGE LOCKED(PERSONALTAB)
   VIEW-AS ALERT-BOX.
END PROCEDURE. 

PROCEDURE l4_UI :
   MESSAGE LOCKED(PERSONALTAB)
   VIEW-AS ALERT-BOX.
  /*
   PERSONALTAB.fornamn = "Elis".
   PERSONALTAB.EFTERNAMN = "Berglund".
   */
END PROCEDURE.

PROCEDURE l5_UI :
   MESSAGE PERSONALTAB.FORNAMN PERSONALTAB.EFTERNAMN
   VIEW-AS ALERT-BOX.
   
END PROCEDURE.