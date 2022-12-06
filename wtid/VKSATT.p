/*VKSATT.P*/
{LESAMMAN.I}  
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER vknummer AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE INPUT PARAMETER regdatum AS DATE NO-UNDO.
RUN sammut_UI (INPUT 1).
IF globforetag = "LULE" OR globforetag = "MISV" OR globforetag = "ELPA" THEN DO:      
   IF vknummer = "" THEN DO:
      vknummer = "L" + STRING(TODAY,"99999999").
      IF globforetag = "LULE"  THEN DO:      
         OPEN QUERY vsatt FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM > 03/31/2005 AND TIDREGITAB.DATUM <= vkdatum AND
         SUBSTRING(TIDREGITAB.VECKOKORD,1,9) NE "" AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = " "  USE-INDEX KOLL NO-LOCK. 
      END.
      IF globforetag = "MISV"  THEN DO:      
         OPEN QUERY vsatt FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM > 12/31/2012 AND TIDREGITAB.DATUM <= vkdatum AND
         SUBSTRING(TIDREGITAB.VECKOKORD,1,9) NE "" AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = " "  USE-INDEX KOLL NO-LOCK. 
      END.      
      IF globforetag = "ELPA" THEN DO:      
         OPEN QUERY vsatt FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM > 12/31/2004 AND TIDREGITAB.DATUM <= vkdatum AND
         SUBSTRING(TIDREGITAB.VECKOKORD,1,9) NE "" AND SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = " "  USE-INDEX KOLL NO-LOCK. 
      END.
      GET FIRST vsatt NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         DO TRANSACTION:   
            GET CURRENT vsatt EXCLUSIVE-LOCK.
            /*forfärdig*/
            IF TIDREGITAB.GODKAND = "F" THEN regdatum = regdatum.
            ELSE IF TIDREGITAB.GODKAND = " " THEN regdatum = regdatum.
            /*ELSE ASSIGN TIDREGITAB.VECKOKORD = vknummer.*/
            ELSE ASSIGN SUBSTRING(TIDREGITAB.VECKOKORD,10,9) = vknummer.
         END.
         GET NEXT vsatt NO-LOCK.
      END.
      CLOSE QUERY vsatt.
   END.
   ELSE DO:
      RUN wsatt_UI.
   END.
END.
ELSE DO:
   DO TRANSACTION:
      CREATE INTERNFAKTKOLL.
      ASSIGN
      INTERNFAKTKOLL.VECKOK = FALSE  
      INTERNFAKTKOLL.VDATUM = vkdatum 
      INTERNFAKTKOLL.VECKOKORD = vknummer.
   END.        
   RUN wsatt_UI.
END.
RUN sammut_UI (INPUT 2).

PROCEDURE wsatt_UI:
   IF gvisatidpermanad = TRUE THEN DO:
      OPEN QUERY vsatt FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM <= vkdatum AND
      TIDREGITAB.VECKOKORD = " "  NO-LOCK.
   END.
   ELSE DO:
      OPEN QUERY vsatt FOR EACH TIDREGITAB WHERE TIDREGITAB.DATUM <= regdatum AND
      TIDREGITAB.VECKOKORD = " "  USE-INDEX KOLL NO-LOCK.
   END.
   GET FIRST vsatt NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      DO TRANSACTION:   
         GET CURRENT vsatt EXCLUSIVE-LOCK.
         /*forfärdig*/         
         IF globforetag = "SUND" AND TIDREGITAB.GODKAND = "F" THEN regdatum = regdatum.
         ELSE IF globforetag = "SNAT" AND TIDREGITAB.GODKAND = "F" THEN regdatum = regdatum.
         ELSE IF globforetag = "MISV" AND TIDREGITAB.GODKAND = "F" THEN regdatum = regdatum.
         ELSE IF globforetag = "GKAL" AND TIDREGITAB.GODKAND = "F" THEN regdatum = regdatum.         
         ELSE IF globforetag = "LULE" AND TIDREGITAB.GODKAND = "F" THEN regdatum = regdatum.         
         ELSE IF TIDREGITAB.GODKAND = " " THEN regdatum = regdatum.
         ELSE ASSIGN TIDREGITAB.VECKOKORD = vknummer.
      END.
      GET NEXT vsatt NO-LOCK.
   END.
   CLOSE QUERY vsatt.
   IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
      OPEN QUERY fsatt FOR EACH TIDFEL WHERE TIDFEL.DATUM <= vkdatum AND
      TIDFEL.FELKORD = " "  NO-LOCK.
      GET FIRST fsatt NO-LOCK.
      DO WHILE AVAILABLE(TIDFEL):
         DO TRANSACTION:
            GET CURRENT fsatt EXCLUSIVE-LOCK.
            IF TIDFEL.SKICKA = FALSE THEN regdatum = regdatum.
            ELSE DO:
               ASSIGN 
               TIDFEL.SKICKA = FALSE
               TIDFEL.FELKORD = vknummer.
            END.   
         END.
         GET NEXT fsatt NO-LOCK.
      END.               
   END.          
   CLOSE QUERY fsatt.
END PROCEDURE.


