/*TIDSTOPP.P*/

DEFINE TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.  /*fel sök text*/
DEFINE INPUT PARAMETER tidanvandare AS CHARACTER NO-UNDO. /*globanv*/
DEFINE OUTPUT PARAMETER lasanvandare AS CHARACTER NO-UNDO. /*ANVPER.PERSONALKOD*/
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR felmeddtemp.
 
IF vadgora = 1 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ANVPER THEN DO TRANSACTION:
      /*SKALL VARA FEL VÄNT PÅ GRUND AV INDEX*/
      CREATE ANVPER.
      ASSIGN
      ANVPER.PERSONALKOD = tidanvandare
      ANVPER.ANVANDARE = pkod.
   END.
   ELSE DO:      
      lasanvandare = ANVPER.PERSONALKOD.
      IF tidanvandare = lasanvandare THEN DO:
         CREATE felmeddtemp.
         felmeddtemp.FELMEDD = "Registreringar för " + pkod + " ändras av dig " + 
                                ANVPER.PERSONALKOD + ". Vill du låsa upp?".
      END.
      ELSE DO:   
         CREATE felmeddtemp.
         felmeddtemp.FELMEDD = "Registreringar för " + pkod + " ändras av användare " + 
                                ANVPER.PERSONALKOD + ". Ändring ej möjlig." +  
                                " Använd - Lås upp -  inne i Tid för att återställa".
      END.                          
   END.
END.

IF vadgora = 2 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod AND 
   ANVPER.PERSONALKOD = tidanvandare
   NO-LOCK NO-ERROR.
   IF AVAILABLE ANVPER THEN DO TRANSACTION:
      FIND CURRENT ANVPER EXCLUSIVE-LOCK.
      DELETE ANVPER.
   END.
END.
IF vadgora = 3 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod NO-LOCK NO-ERROR.
   IF AVAILABLE ANVPER THEN DO TRANSACTION:
      lasanvandare = ANVPER.PERSONALKOD.
      /* avvakta med upplåsning från flexen
      IF tidanvandare = lasanvandare THEN DO:
         CREATE felmeddtemp.
         felmeddtemp.FELMEDD = "Registreringar för " + pkod + " ändras av dig " + 
                                ANVPER.PERSONALKOD + ". Vill du låsa upp?".
      END.
      ELSE DO:*/         
      CREATE felmeddtemp.
      felmeddtemp.FELMEDD = "Registreringar för " + pkod + " ändras av användare " + 
                             ANVPER.PERSONALKOD + ". Flextid ej möjlig."  + 
                             " Använd - Lås upp - inne i Tid för att återställa".
     /*END.*/                           
   END.
END.
IF vadgora = 4 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ANVPER THEN DO TRANSACTION:
      /*SKALL VARA FEL VÄNT PÅ GRUND AV INDEX*/
      CREATE ANVPER.
      ASSIGN
      ANVPER.PERSONALKOD = tidanvandare
      ANVPER.ANVANDARE = pkod.
   END.
   ELSE DO:
      lasanvandare = ANVPER.PERSONALKOD.
      IF ANVPER.PERSONALKOD = tidanvandare THEN RETURN.
      CREATE felmeddtemp.
      felmeddtemp.FELMEDD = "Faktura planen används just nu av " + ANVPER.PERSONALKOD + ". Ändring ej möjlig.".
   END.
END.
IF vadgora = 5 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod AND 
   ANVPER.PERSONALKOD = tidanvandare
   NO-LOCK NO-ERROR.
   IF AVAILABLE ANVPER THEN DO TRANSACTION:
      FIND CURRENT ANVPER EXCLUSIVE-LOCK.
      DELETE ANVPER.
   END.
END.
IF vadgora = 6 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod AND 
   ANVPER.PERSONALKOD NE tidanvandare
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ANVPER THEN DO TRANSACTION:
      /*SKALL VARA FEL VÄNT PÅ GRUND AV INDEX*/
      CREATE ANVPER.
      ASSIGN
      ANVPER.PERSONALKOD = tidanvandare
      ANVPER.ANVANDARE = pkod.
   END.
   ELSE DO:
      lasanvandare = ANVPER.PERSONALKOD.
      IF tidanvandare = lasanvandare THEN DO:
         CREATE felmeddtemp.
         felmeddtemp.FELMEDD = "Registreringar för " + pkod + " ändras av dig " + 
                                ANVPER.PERSONALKOD + ". Vill du låsa upp?".
      END.
      ELSE DO:
         CREATE felmeddtemp.
         felmeddtemp.FELMEDD = "Registreringar för " + pkod + " ändras av användare " + 
                                ANVPER.PERSONALKOD + ". Ändring ej möjlig." + 
                                " Använd - Lås upp - inne i Tid för att återställa".
     END.                           
   END.
END.
IF vadgora = 7 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod AND 
   ANVPER.PERSONALKOD = tidanvandare
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ANVPER THEN DO TRANSACTION:
      
   END.
   ELSE DO:
      CREATE felmeddtemp.
      felmeddtemp.FELMEDD = "Faktureringen gick fel vid förra tillfället kontakta Elpool 090-184540.".
   END.
END.
IF vadgora = 8 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod AND 
   ANVPER.PERSONALKOD = tidanvandare
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ANVPER THEN DO TRANSACTION:
      CREATE ANVPER.
      ASSIGN
      ANVPER.PERSONALKOD = tidanvandare
      ANVPER.ANVANDARE = pkod.
   END.
  
END.
IF vadgora = 9 THEN DO:
   FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = pkod NO-LOCK NO-ERROR.
   IF  AVAILABLE ANVPER THEN DO TRANSACTION:
      lasanvandare = ANVPER.PERSONALKOD.
      CREATE felmeddtemp.
      felmeddtemp.FELMEDD = "Registreringar för " + pkod + " ändras av användare " + 
                             ANVPER.PERSONALKOD + ". Ändring ej möjlig.". 
                             
   END.
END.
