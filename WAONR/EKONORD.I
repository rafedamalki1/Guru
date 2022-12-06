   /*EKONORD_UI*/
   FIND FIRST VKORN WHERE VKORN.VECKOK = TRUE
   USE-INDEX VKORN NO-LOCK NO-ERROR.
   IF NOT AVAILABLE VKORN THEN DO:
      RUN NORDEKO.P.
   END.   
   ELSE DO:
      IF SEL_UPP = "ekonomi koll natt" THEN musz = musz.
      ELSE MESSAGE "Eko-löne-sammanst. pågår. Denna lista går ej att ta fram nu."
      VIEW-AS ALERT-BOX.               
      RETURN.          
   END.
   FIND FIRST EKRAPPRESULT NO-LOCK NO-ERROR.
   IF AVAILABLE EKRAPPRESULT THEN DO:
      IF SEL_UPP = "ekonomi koll natt" THEN musz = musz.
      ELSE
      MESSAGE "Senaste eko-löne-sammanst. är ej överläst till ekonomisystemet."
      VIEW-AS ALERT-BOX.  
   END.
   /*
   FIND FIRST VECKONATT WHERE SUBSTRING(VECKONATT.DAG_AR,1,3) = "DAG" 
   USE-INDEX NATT NO-LOCK NO-ERROR.
   DO TRANSACTION:
      CREATE tidut.  
      CREATE tidut.
      ASSIGN tidut.UT = str2.
      CREATE tidut.
      CREATE tidut. 
      CREATE tidut.            
      ASSIGN
      SUBSTRING(tidut.UT,1) = "SENASTE VECKÖRNINGEN GJORDES VECKA :"
      SUBSTRING(tidut.UT,38) = SUBSTRING(VECKONATT.VECKOKORD,2,3).
      CREATE tidut.   
      SUBSTRING(tidut.UT,1) = "EKONOMISYSTEMETS KONTON".           
      CREATE tidut.                       
   END.
   FOR EACH eko USE-INDEX ORG NO-LOCK:   
      FIND FIRST eko23 WHERE eko23.EDEBKRED = eko.EDEBKRED AND 
      eko23.EORG = eko.EORG AND 
      eko23.EVERDATUM = eko.EVERDATUM AND
      eko23.EPROJEKT = eko.EPROJEKT AND
      eko23.DELNR = eko.DELNR AND
      eko23.EKOSTNADSSLAG = eko.EKOSTNADSSLAG
      USE-INDEX PERSORG EXCLUSIVE-LOCK NO-ERROR.  
      IF NOT AVAILABLE eko23 THEN DO:
         CREATE eko23.
      END.    
      ASSIGN            
      eko23.EDEBKRED = eko.EDEBKRED
      eko23.ENY = TRUE 
      eko23.EORG = eko.EORG  
      eko23.EPROJEKT = eko.EPROJEKT 
      eko23.DELNR = eko.DELNR 
      eko23.EVERDATUM = eko.EVERDATUM
      eko23.EKOSTNADSSLAG = eko.EKOSTNADSSLAG
      eko23.EANTAL = eko23.EANTAL + eko.EANTAL  
      eko23.EBELOPP = eko23.EBELOPP + eko.EBELOPP + eko.ELONBELOPP       
      eko23.ELONTILLANTAL = eko23.ELONTILLANTAL + eko.ELONTILLANTAL  
      eko23.ELONBELOPP = eko23.ELONBELOPP + eko.ELONBELOPP.            
   END.
   DO TRANSACTION:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1,6) = "MÅNAD"
      SUBSTRING(tidut.UT,8,6) = "OMRÅDE"
      SUBSTRING(tidut.UT,15,6) = "AONR"
      SUBSTRING(tidut.UT,22,5) = "KONTO"
      SUBSTRING(tidut.UT,28,10) = "BELOPP".
      CREATE tidut.                 
      ASSIGN                                        
      SUBSTRING(tidut.UT,1) = "======.======.======.=====.==========".
   END.              
   FOR EACH eko23 USE-INDEX PERSORG NO-LOCK:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1,6) = SUBSTRING(eko23.EVERDATUM,1,6)
      SUBSTRING(tidut.UT,8,3) = SUBSTRING(eko23.EORG,1,3)
      SUBSTRING(tidut.UT,15,6) = SUBSTRING(eko23.EPROJEKT,1,6)
      SUBSTRING(tidut.UT,22,4) = SUBSTRING(eko23.EKOSTNADSSLAG,1,4)                                                                
      SUBSTRING(tidut.UT,28) = SUBSTRING(STRING(eko23.EBELOPP,">>>>>>>>>9"),1,10).
   END.    
   CREATE tidut.
   ASSIGN tidut.UT = str2.
   */ 
