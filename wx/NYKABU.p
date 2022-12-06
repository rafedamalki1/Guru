/*NYKABU.P PROGRAMMET HÄMTAR MATERIEL DÄR EJ NÅGON KOMBINATION AV FÄLT ÄR UPPLAGD

KÖRS EJ
*/
{KONVALTEMP.I}
{LISTMTRL.I}      
   
DEFINE INPUT PARAMETER valnum LIKE BERVAL.NUM NO-UNDO.
DEFINE INPUT PARAMETER valskapnum LIKE BERVAL.SKAPNUM NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR kon_val.
DEFINE OUTPUT PARAMETER TABLE FOR list_mtrl.

DEFINE QUERY mtrlq FOR MTRLBER.

/*HUVUDPROGRAM*/   
   FIND FIRST kon_val WHERE kon_val.NUM = valnum AND kon_val.SKAPNUM = valskapnum 
   NO-LOCK NO-ERROR. 
   RUN val_UI.   
/*SLUT HUVUDPROGRAM*/   
   
PROCEDURE val_UI.   
   IF kon_val.F2 NE "" THEN DO: 
      OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = kon_val.F1 AND
      MTRLBER.F1 = kon_val.F2 AND MTRLBER.F2 = " " AND MTRLBER.F3 = " " AND
      MTRLBER.F4 = " " AND MTRLBER.F5 = " " USE-INDEX KOD NO-LOCK. 
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         RUN skapa_UI.
         GET NEXT mtrlq NO-LOCK. 
      END.
      CLOSE QUERY mtrlq.      
   END.     
   IF kon_val.F3 NE "" THEN DO: 
      OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = kon_val.F1 AND
      MTRLBER.F2 = kon_val.F3 AND MTRLBER.F1 = " " AND MTRLBER.F3 = " " AND
      MTRLBER.F4 = " " AND MTRLBER.F5 = " " USE-INDEX KOD NO-LOCK. 
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         RUN skapa_UI.
         GET NEXT mtrlq NO-LOCK. 
      END.
      CLOSE QUERY mtrlq.      
   END.   
   IF kon_val.F4 NE "" THEN DO: 
      OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = kon_val.F1 AND
      MTRLBER.F3 = kon_val.F4 AND MTRLBER.F1 = " " AND MTRLBER.F2 = " " AND
      MTRLBER.F4 = " " AND MTRLBER.F5 = " " USE-INDEX KOD NO-LOCK. 
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         RUN skapa_UI.
         GET NEXT mtrlq NO-LOCK. 
      END.
      CLOSE QUERY mtrlq.      
   END.   
   IF kon_val.F5 NE "" THEN DO: 
      OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = kon_val.F1 AND
      MTRLBER.F4 = kon_val.F5 AND MTRLBER.F1 = " " AND MTRLBER.F2 = " " AND
      MTRLBER.F3 = " " AND MTRLBER.F5 = " " USE-INDEX KOD NO-LOCK. 
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         RUN skapa_UI.
         GET NEXT mtrlq NO-LOCK. 
      END.
      CLOSE QUERY mtrlq.      
   END.   
   IF kon_val.F6 NE "" THEN DO: 
      OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = kon_val.F1 AND
      MTRLBER.F5 = kon_val.F6 AND MTRLBER.F1 = " " AND MTRLBER.F2 = " " AND
      MTRLBER.F3 = " " AND MTRLBER.F4 = " " USE-INDEX KOD NO-LOCK. 
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRLBER):
         RUN skapa_UI.
         GET NEXT mtrlq NO-LOCK. 
      END.
      CLOSE QUERY mtrlq.      
   END.         
END PROCEDURE.  
   
PROCEDURE skapa_UI :   
      IF kon_val.GRUPP = 0 THEN DO:
         FIND FIRST list_mtrl WHERE list_mtrl.ENR = MTRLBER.ENR AND
         list_mtrl.LEVKOD = MTRLBER.LEVKOD AND list_mtrl.NUM = kon_val.NUM
         AND list_mtrl.SKAPNUM = kon_val.SKAPNUM NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST list_mtrl WHERE list_mtrl.ENR = MTRLBER.ENR AND
         list_mtrl.LEVKOD = MTRLBER.LEVKOD AND list_mtrl.NUM = kon_val.NUM
         NO-LOCK NO-ERROR.
      END.   
      IF NOT AVAILABLE list_mtrl THEN DO:
         CREATE list_mtrl.
         ASSIGN 
         list_mtrl.NUM = kon_val.NUM
         list_mtrl.ENR = MTRLBER.ENR
         list_mtrl.BENAMNING = MTRLBER.BENAMNING
         list_mtrl.ENHET = LC(MTRLBER.ENHET)
         list_mtrl.PRIS = MTRLBER.PRIS
         list_mtrl.ANTAL = MTRLBER.ANTAL
         list_mtrl.LEVKOD = MTRLBER.LEVKOD
         list_mtrl.LINKAB = MTRLBER.LINKAB
         list_mtrl.MODUL = MTRLBER.MODUL
         list_mtrl.SKAPNUM = kon_val.SKAPNUM
         list_mtrl.TYPBER = MTRLBER.TYPBER
         list_mtrl.DIAMETER = MTRLBER.DIAMETER
         list_mtrl.SATS = MTRLBER.SATS.
      END.
      ELSE DO:
         list_mtrl.ANTAL = list_mtrl.ANTAL + MTRLBER.ANTAL.   
      END.      
END PROCEDURE.      
