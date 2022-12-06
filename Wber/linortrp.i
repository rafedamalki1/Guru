/*LINORTRP.I*/   
   OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND 
   BERLINKAB.OMRADE = valomrade AND BERLINKAB.DATUM = datvar AND BERLINKAB.KORTKOD = ?
   AND BERLINKAB.UPPLAG NE ? USE-INDEX DATUM NO-LOCK.
   GET FIRST linq NO-LOCK.
   DO WHILE AVAILABLE(BERLINKAB):
      CREATE lin_upp.
      ASSIGN
      lin_upp.ENR = BERLINKAB.ENR
      lin_upp.BENAMNING = BERLINKAB.BENAMNING
      lin_upp.ENHET = BERLINKAB.ENHET
      lin_upp.PRIS = BERLINKAB.PRIS
      lin_upp.LEVKOD = BERLINKAB.LEVKOD
      lin_upp.METER = BERLINKAB.METER
      lin_upp.TOTMETER = BERLINKAB.TOTMETER
      lin_upp.UPPLAG = BERLINKAB.UPPLAG.
      IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
         IF BERLINKAB.LEVKOD = "12" OR BERLINKAB.LEVKOD = "13"  THEN lin_upp.LEVKOD = "16".
      END.
      GET NEXT linq NO-LOCK.
   END.          
   CLOSE QUERY linq.
   OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND 
   BERLINKAB.OMRADE = valomrade AND BERLINKAB.DATUM = datvar AND BERLINKAB.KORTKOD = ?
   AND BERLINKAB.TOTMETER = 0 USE-INDEX DATUM NO-LOCK.
   GET FIRST linq NO-LOCK.
   DO WHILE AVAILABLE(BERLINKAB):
      IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
         IF BERLINKAB.LEVKOD = "12"  OR BERLINKAB.LEVKOD = "13"  THEN DO:
            FIND FIRST lin_upp WHERE lin_upp.ENR = BERLINKAB.ENR AND
            lin_upp.LEVKOD = "16" NO-LOCK NO-ERROR.
         END.         
         ELSE DO:
            FIND FIRST lin_upp WHERE lin_upp.ENR = BERLINKAB.ENR AND
            lin_upp.LEVKOD = BERLINKAB.LEVKOD NO-LOCK NO-ERROR.
         END.
      END.
      ELSE DO:
         FIND FIRST lin_upp WHERE lin_upp.ENR = BERLINKAB.ENR AND
         lin_upp.LEVKOD = BERLINKAB.LEVKOD NO-LOCK NO-ERROR.
      END.      
      IF AVAILABLE lin_upp THEN DO:
         musz = musz.
      END.
      ELSE DO:
         CREATE mtrl_temp.
         ASSIGN  
         mtrl_temp.NUM = BERLINKAB.NUM1
         mtrl_temp.ENR = BERLINKAB.ENR     
         mtrl_temp.BENAMNING = BERLINKAB.BENAMNING
         mtrl_temp.ENHET = BERLINKAB.ENHET
         mtrl_temp.ANTAL = BERLINKAB.METER * BERLINKAB.LEDARE
         mtrl_temp.PRIS = BERLINKAB.PRIS 
         mtrl_temp.TOTPRIS = BERLINKAB.PRIS * mtrl_temp.ANTAL      
         mtrl_temp.LEVKOD = BERLINKAB.LEVKOD.
         IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
            IF BERLINKAB.LEVKOD = "12" OR BERLINKAB.LEVKOD = "13" THEN mtrl_temp.LEVKOD = "16".
         END.
      END.   
      GET NEXT linq NO-LOCK.
   END.          
   CLOSE QUERY linq.
