   /*NETTOMARK.I*/

   IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
      /*nettopris beredning inköp*/         
      EMPTY TEMP-TABLE elist_mtrl NO-ERROR.       
      FOR EACH list_mtrl WHERE list_mtrl.num = valnum NO-LOCK:
         FIND FIRST elist_mtrl WHERE elist_mtrl.ENR = list_mtrl.ENR AND elist_mtrl.LEVKOD = list_mtrl.LEVKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE elist_mtrl THEN DO:         
            FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl.LEVKOD AND kmtrltemp.ENR = list_mtrl.ENR AND kmtrltemp.KALKNR = 0
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kmtrltemp THEN DO:
               CREATE elist_mtrl.
               BUFFER-COPY list_mtrl TO elist_mtrl.
            END.
         END.
      END.
      RUN nettomark_UI IN nettoh (INPUT TABLE elist_mtrl, OUTPUT TABLE kmtrltemp APPEND).            
      EMPTY TEMP-TABLE elist_mtrl NO-ERROR. 
   END.
   
   
   
   
