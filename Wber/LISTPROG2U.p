/*LISTPROG2U.P*/
DEFINE VARIABLE datvar LIKE BERMTRL.DATUM NO-UNDO.
{KONVALTEMP.I}         

DEFINE TEMP-TABLE mtrl_temp 
   {MTRLTEMPTT.I}
   
DEFINE TEMP-TABLE lin_upp   
   FIELD METER LIKE BERLINKAB.METER      
   FIELD ENR LIKE BERLINKAB.ENR         
   FIELD BENAMNING LIKE BERLINKAB.BENAMNING 
   FIELD PRIS LIKE BERLINKAB.PRIS
   FIELD ENHET LIKE BERLINKAB.ENHET   
   FIELD TOTMETER LIKE BERLINKAB.TOTMETER
   FIELD UPPLAG LIKE BERLINKAB.UPPLAG 
   FIELD LEVKOD LIKE BERLINKAB.LEVKOD
   FIELD TOTPRIS LIKE BERMTRL.PRIS         
   INDEX ENR ENR ASCENDING.     
   
DEFINE TEMP-TABLE lin_temp  
   FIELD NUM1 LIKE BERLINKAB.NUM1
   FIELD NUM2 LIKE BERLINKAB.NUM2 
   FIELD METER LIKE BERLINKAB.METER              
   FIELD BENAMNING LIKE BERLINKAB.BENAMNING       
   INDEX NUM NUM1 NUM2 ASCENDING.   

DEFINE BUFFER linbuff FOR BERLINKAB.
   
DEFINE INPUT PARAMETER TABLE FOR kon_val.

DEFINE OUTPUT PARAMETER TABLE FOR mtrl_temp.
DEFINE OUTPUT PARAMETER TABLE FOR lin_upp.
DEFINE OUTPUT PARAMETER TABLE FOR lin_temp.                 
   
   FOR EACH kon_val:   
      FIND LAST BERMTRL WHERE BERMTRL.AONR = kon_val.BERAONR AND 
      BERMTRL.OMRADE = kon_val.OMRADE AND BERMTRL.INKOP = FALSE 
      USE-INDEX DATUM NO-LOCK NO-ERROR.
      IF AVAILABLE BERMTRL THEN DO:
         datvar = BERMTRL.DATUM.
      END.     
      ELSE DO:
         datvar = TODAY.
      END.
      IF kon_val.GRUPP NE 1000 THEN DO: 
         OPEN QUERY mtrlprisq FOR EACH BERMTRL WHERE BERMTRL.AONR = kon_val.BERAONR AND
         BERMTRL.OMRADE = kon_val.OMRADE AND BERMTRL.INKOP = FALSE
         AND BERMTRL.DATUM = datvar AND BERMTRL.NUM = kon_val.NUM USE-INDEX DATUM NO-LOCK.
         GET FIRST mtrlprisq NO-LOCK.
         DO WHILE AVAILABLE(BERMTRL):      
            CREATE mtrl_temp.
            ASSIGN  
            mtrl_temp.NUM = kon_val.ORD
            mtrl_temp.ENR = BERMTRL.ENR     
            mtrl_temp.BENAMNING = BERMTRL.BENAMNING
            mtrl_temp.ENHET = BERMTRL.ENHET
            mtrl_temp.ANTAL = BERMTRL.ANTAL
            mtrl_temp.PRIS = BERMTRL.PRIS 
            mtrl_temp.TOTPRIS = BERMTRL.PRIS * BERMTRL.ANTAL      
            mtrl_temp.LEVKOD = BERMTRL.LEVKOD
            mtrl_temp.PAR = BERMTRL.PAR
            mtrl_temp.MTRLTEXT = BERMTRL.MTRLTEXT.         
            GET NEXT mtrlprisq NO-LOCK.
         END.    
         CLOSE QUERY mtrlprisq.   
      END.
      ELSE DO:          
         OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = kon_val.BERAONR AND 
         BERLINKAB.OMRADE = kon_val.OMRADE AND BERLINKAB.DATUM = datvar AND BERLINKAB.KORTKOD = ?
         AND BERLINKAB.UPPLAG = INTEGER(STRING(kon_val.F2)) USE-INDEX INKOP NO-LOCK.
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
            lin_upp.UPPLAG = BERLINKAB.UPPLAG
            lin_upp.TOTPRIS = BERLINKAB.PRIS * BERLINKAB.TOTMETER.
            GET NEXT linq NO-LOCK.
         END.          
         CLOSE QUERY linq.
         OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = kon_val.BERAONR AND 
         BERLINKAB.OMRADE = kon_val.OMRADE AND BERLINKAB.DATUM = datvar AND BERLINKAB.KORTKOD = ?
         AND BERLINKAB.TOTMETER = 0 AND BERLINKAB.NUM1 = kon_val.NUM AND BERLINKAB.NUM2 = kon_val.NUM USE-INDEX INKOP NO-LOCK.
         GET FIRST linq NO-LOCK.
         DO WHILE AVAILABLE(BERLINKAB):
            FIND FIRST lin_upp WHERE lin_upp.ENR = BERLINKAB.ENR AND
            lin_upp.LEVKOD = BERLINKAB.LEVKOD NO-LOCK NO-ERROR.
            IF AVAILABLE lin_upp THEN DO:
               datvar = datvar.
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
            END.   
            CREATE lin_temp.
            ASSIGN  
            lin_temp.NUM1 = BERLINKAB.NUM1
            lin_temp.NUM2 = BERLINKAB.NUM2
            lin_temp.BENAMNING = BERLINKAB.BENAMNING         
            lin_temp.METER = BERLINKAB.METER.
            GET NEXT linq NO-LOCK.
         END.          
         CLOSE QUERY linq.  
         
      END.
      /* Lena testar att lägga in samma som i inköp med val av linor och kablar per upplag*/
      /*  
      OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = kon_val.BERAONR AND
      BERLINKAB.OMRADE = kon_val.OMRADE AND BERLINKAB.DATUM = datvar AND BERLINKAB.KORTKOD = ?
      AND BERLINKAB.TOTMETER = 0 AND BERLINKAB.NUM1 = kon_val.NUM AND 
      BERLINKAB.NUM2 = kon_val.NUM NO-LOCK.
      GET FIRST linq NO-LOCK.
      DO WHILE AVAILABLE(BERLINKAB):
         FIND FIRST linbuff WHERE linbuff.AONR = kon_val.BERAONR AND
         linbuff.OMRADE = kon_val.OMRADE AND linbuff.DATUM = datvar AND linbuff.KORTKOD = ?
         AND linbuff.UPPLAG NE ? AND linbuff.ENR = BERLINKAB.ENR AND
         linbuff.LEVKOD = BERLINKAB.LEVKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE linbuff THEN DO:
            CREATE mtrl_temp.
            ASSIGN
            mtrl_temp.NUM = kon_val.ORD
            mtrl_temp.ENR = BERLINKAB.ENR
            mtrl_temp.BENAMNING = BERLINKAB.BENAMNING
            mtrl_temp.ENHET = BERLINKAB.ENHET
            mtrl_temp.ANTAL = BERLINKAB.METER * BERLINKAB.LEDARE
            mtrl_temp.PRIS = BERLINKAB.PRIS
            mtrl_temp.TOTPRIS = BERLINKAB.PRIS * mtrl_temp.ANTAL
            mtrl_temp.LEVKOD = BERLINKAB.LEVKOD.
            CREATE lin_temp.
            ASSIGN
            lin_temp.NUM1 = kon_val.ORD
            lin_temp.NUM2 = kon_val.ORD
            lin_temp.BENAMNING = BERLINKAB.BENAMNING
            lin_temp.METER = BERLINKAB.METER.
         END.      
         GET NEXT linq NO-LOCK.
      END.
      CLOSE QUERY linq.*/
   END.
         
