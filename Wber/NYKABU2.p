/*NYKABU2.P PROGRAMMET HÄMTAR KALK DÄR EJ NÅGON KOMBINATION AV FÄLT ÄR UPPLAGD
 hämta kalkkoder vi komplettering
*/

{STARTFORAPP.I}
{KONVALTEMP.I}
{KALKTEMP2.I}  
{KALKKAT20G.I} 
/*Anders Olsson Elpool i Umeå AB  19 jan 2018 16:14:10*/
DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.
DEFINE INPUT PARAMETER valnum LIKE BERVAL.NUM NO-UNDO.
DEFINE INPUT PARAMETER valskapnum LIKE BERVAL.SKAPNUM NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR kon_val.
DEFINE OUTPUT PARAMETER TABLE FOR kalk_temp. 
DEFINE VARIABLE LocalKalknumHandle AS HANDLE NO-UNDO.

DEFINE QUERY kalkq FOR KALKBER.
FIND FIRST kon_val WHERE kon_val.NUM = valnum AND kon_val.SKAPNUM = valskapnum 
NO-LOCK NO-ERROR. 
   
/*HUVUDPROGRAM*/   

   CREATE sparakon_val.
   BUFFER-COPY kon_val TO sparakon_val.
   RUN KalkyliBerAnd.P PERSISTENT SET LocalKalknumHandle.
   RUN komplettering_UI IN LocalKalknumHandle (INPUT INTEGER(valaonr),INPUT valomrade,INPUT valnum,INPUT TABLE sparakon_val, OUTPUT TABLE kalk_temp APPEND).

IF VALID-HANDLE(LocalKalknumHandle) THEN DO:
   RUN avs_UI IN LocalKalknumHandle.
   DELETE PROCEDURE LocalKalknumHandle NO-ERROR.
END. 
/*
 
   RUN val_UI.
  */ 
/*SLUT HUVUDPROGRAM*/   
   
PROCEDURE val_UI.      
   IF kon_val.F2 NE "" THEN DO: 
      OPEN QUERY kalkq FOR EACH KALKBER WHERE KALKBER.KTYPKOD = kon_val.F1 AND
      KALKBER.F1 = kon_val.F2 AND KALKBER.F2 = " " AND KALKBER.F3 = " " AND
      KALKBER.F4 = " " AND KALKBER.F5 = " " USE-INDEX AR NO-LOCK. 
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKBER):
         RUN skapa2_UI.
         GET NEXT kalkq NO-LOCK. 
      END.
      CLOSE QUERY kalkq.      
   END.     
   IF kon_val.F3 NE "" THEN DO: 
      OPEN QUERY kalkq FOR EACH KALKBER WHERE KALKBER.KTYPKOD = kon_val.F1 AND
      KALKBER.F2 = kon_val.F3 AND KALKBER.F1 = " " AND KALKBER.F3 = " " AND
      KALKBER.F4 = " " AND KALKBER.F5 = " " USE-INDEX AR NO-LOCK. 
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKBER):
         RUN skapa2_UI.
         GET NEXT kalkq NO-LOCK. 
      END.
      CLOSE QUERY kalkq.      
   END.   
   IF kon_val.F4 NE "" THEN DO: 
      OPEN QUERY kalkq FOR EACH KALKBER WHERE KALKBER.KTYPKOD = kon_val.F1 AND
      KALKBER.F3 = kon_val.F4 AND KALKBER.F1 = " " AND KALKBER.F2 = " " AND
      KALKBER.F4 = " " AND KALKBER.F5 = " " USE-INDEX AR NO-LOCK. 
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKBER):
         RUN skapa2_UI.
         GET NEXT kalkq NO-LOCK. 
      END.
      CLOSE QUERY kalkq.      
   END.   
   IF kon_val.F5 NE "" THEN DO: 
      OPEN QUERY kalkq FOR EACH KALKBER WHERE KALKBER.KTYPKOD = kon_val.F1 AND
      KALKBER.F4 = kon_val.F5 AND KALKBER.F1 = " " AND KALKBER.F2 = " " AND
      KALKBER.F3 = " " AND KALKBER.F5 = " " USE-INDEX AR NO-LOCK. 
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKBER):
         RUN skapa2_UI.
         GET NEXT kalkq NO-LOCK. 
      END.
      CLOSE QUERY kalkq.      
   END.   
   IF kon_val.F6 NE "" THEN DO: 
      OPEN QUERY kalkq FOR EACH KALKBER WHERE KALKBER.KTYPKOD = kon_val.F1 AND
      KALKBER.F5 = kon_val.F6 AND KALKBER.F1 = " " AND KALKBER.F2 = " " AND
      KALKBER.F3 = " " AND KALKBER.F4 = " " USE-INDEX AR NO-LOCK. 
      GET FIRST kalkq NO-LOCK.
      DO WHILE AVAILABLE(KALKBER):
         RUN skapa2_UI.
         GET NEXT kalkq NO-LOCK. 
      END.
      CLOSE QUERY kalkq.      
   END.  
END PROCEDURE.  
   


PROCEDURE skapa2_UI :   
   FIND FIRST kalk_temp WHERE kalk_temp.NUM = kon_val.NUM AND 
   kalk_temp.ARBKOD = KALKBER.ARBKOD AND kalk_temp.LOPNR = KALKBER.LOPNR
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kalk_temp THEN DO:
      CREATE kalk_temp.
      ASSIGN 
      kalk_temp.NUM = kon_val.NUM
      kalk_temp.ARBKOD = KALKBER.ARBKOD
      kalk_temp.LOPNR = KALKBER.LOPNR      
      kalk_temp.BENAMNING = KALKBER.BENAMNING
      kalk_temp.ENHET = KALKBER.ENHET      
      kalk_temp.ANTAL = KALKBER.ANTAL.
   END.
       
END PROCEDURE.
