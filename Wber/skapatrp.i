/*SKAPATRP.I*/   
   ASSIGN
   sumpris = 0
   sumantal = 0.
 
   IF delbest = FALSE THEN DO:   
      OPEN QUERY ordq FOR EACH BERORD WHERE BERORD.AONR = valaonr
      AND BERORD.OMRADE = valomrade USE-INDEX ORD NO-LOCK.
      GET FIRST ordq NO-LOCK.
      DO WHILE AVAILABLE(BERORD):      
         CREATE ord_temp.  
         ASSIGN
         ord_temp.NUM = BERORD.NUM  
         ord_temp.ORD = BERORD.ORD.     
         GET NEXT ordq NO-LOCK.
      END.
      CLOSE QUERY ordq.
      OPEN QUERY upplq FOR EACH BERVAL WHERE BERVAL.AONR = valaonr
      AND BERVAL.OMRADE = valomrade AND BERVAL.KSKAP = FALSE USE-INDEX OMR NO-LOCK.
      GET FIRST upplq NO-LOCK.
      DO WHILE AVAILABLE(BERVAL):
         CREATE uppl_temp.  
         ASSIGN
         uppl_temp.NUM = BERVAL.NUM  
         uppl_temp.F1 = BERVAL.KTYPKOD  
         uppl_temp.F2 = BERVAL.F2
         uppl_temp.F3 = BERVAL.F3
         uppl_temp.F4 = BERVAL.F4
         uppl_temp.F5 = BERVAL.F5
         uppl_temp.F6 = BERVAL.F6   
         uppl_temp.UPPLAG = BERVAL.UPPLAG
         uppl_temp.GRUPP = BERVAL.KONSKOD
         uppl_temp.ANMARK = BERVAL.ANMARK.
         FIND FIRST ord_temp WHERE ord_temp.NUM = uppl_temp.NUM
         USE-INDEX NUM NO-LOCK NO-ERROR.
         IF AVAILABLE ord_temp THEN uppl_temp.ORD = ord_temp.ORD.
         GET NEXT upplq NO-LOCK.
      END.
      CLOSE QUERY upplq.    
      
      
      OPEN QUERY beridq FOR EACH BERID WHERE BERID.AONR = valaonr AND
      BERID.OMRADE = valomrade USE-INDEX OMR NO-LOCK.
      GET FIRST beridq NO-LOCK.
      DO WHILE AVAILABLE(BERID):
         CREATE id_temp.
         ASSIGN
         id_temp.NUM = BERID.NUM            
         id_temp.FORNR = BERID.FORNR 
         id_temp.LINNR = BERID.LINNR 
         id_temp.NATNR = BERID.NATNR  
         id_temp.FRI1 = BERID.FRI1 
         id_temp.FRI2 = BERID.FRI2 
         id_temp.XKORD = BERID.XKORD  
         id_temp.ENDKOMB = BERID.ENDKOMB
         id_temp.FRI3 = BERID.FRI3.      
         FIND FIRST uppl_temp WHERE uppl_temp.NUM = id_temp.NUM USE-INDEX NUM.
         ASSIGN
         id_temp.GRUPP = uppl_temp.GRUPP
         id_temp.ORD = uppl_temp.ORD.
         GET NEXT beridq NO-LOCK.
      END.
      CLOSE QUERY beridq.
        
      OPEN QUERY mtrlprisq FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND
      BERMTRL.OMRADE = valomrade AND BERMTRL.INKOP = FALSE
      AND BERMTRL.DATUM = datvar USE-INDEX DATUM NO-LOCK.
      GET FIRST mtrlprisq NO-LOCK.
      DO WHILE AVAILABLE(BERMTRL):      
         CREATE mtrl_temp.
         ASSIGN  
         mtrl_temp.NUM = BERMTRL.NUM
         mtrl_temp.ENR = BERMTRL.ENR     
         mtrl_temp.BENAMNING = BERMTRL.BENAMNING
         mtrl_temp.ENHET = BERMTRL.ENHET
         mtrl_temp.ANTAL = BERMTRL.ANTAL
         mtrl_temp.PRIS = BERMTRL.PRIS 
         mtrl_temp.TOTPRIS = BERMTRL.PRIS * BERMTRL.ANTAL      
         mtrl_temp.LEVKOD = BERMTRL.LEVKOD
         mtrl_temp.MTRLTEXT = BERMTRL.MTRLTEXT.      
         GET NEXT mtrlprisq NO-LOCK.
      END.    
      CLOSE QUERY mtrlprisq.              
      
      RUN linor_UI.          
   END.
   ELSE DO:
      FOR EACH kon_val:
         IF kon_val.GRUPP NE 1000 THEN DO:         
            FIND FIRST BERORD WHERE BERORD.AONR = valaonr
            AND BERORD.OMRADE = valomrade AND BERORD.NUM = kon_val.NUM USE-INDEX ORD NO-LOCK.
            IF AVAILABLE BERORD THEN DO:         
               CREATE ord_temp.  
               ASSIGN
               ord_temp.NUM = BERORD.NUM  
               ord_temp.ORD = BERORD.ORD.                 
            END.
            FIND FIRST BERVAL WHERE BERVAL.AONR = valaonr
            AND BERVAL.OMRADE = valomrade AND BERVAL.NUM = kon_val.NUM AND BERVAL.KSKAP = FALSE USE-INDEX OMR NO-LOCK.
            IF AVAILABLE BERVAL THEN DO:         
               CREATE uppl_temp.  
               ASSIGN
               uppl_temp.NUM = BERVAL.NUM  
               uppl_temp.F1 = BERVAL.KTYPKOD  
               uppl_temp.F2 = BERVAL.F2
               uppl_temp.F3 = BERVAL.F3
               uppl_temp.F4 = BERVAL.F4
               uppl_temp.F5 = BERVAL.F5
               uppl_temp.F6 = BERVAL.F6   
               uppl_temp.UPPLAG = BERVAL.UPPLAG
               uppl_temp.GRUPP = BERVAL.KONSKOD
               uppl_temp.ANMARK = BERVAL.ANMARK.
               FIND FIRST ord_temp WHERE ord_temp.NUM = uppl_temp.NUM
               USE-INDEX NUM NO-LOCK NO-ERROR.
               IF AVAILABLE ord_temp THEN uppl_temp.ORD = ord_temp.ORD.            
            END.  
            FIND FIRST BERID WHERE BERID.AONR = valaonr AND
            BERID.OMRADE = valomrade AND BERID.NUM = kon_val.NUM USE-INDEX OMR NO-LOCK NO-ERROR.
            IF AVAILABLE BERID THEN DO:         
               CREATE id_temp.
               ASSIGN
               id_temp.NUM = BERID.NUM            
               id_temp.FORNR = BERID.FORNR 
               id_temp.LINNR = BERID.LINNR 
               id_temp.NATNR = BERID.NATNR  
               id_temp.FRI1 = BERID.FRI1 
               id_temp.FRI2 = BERID.FRI2 
               id_temp.XKORD = BERID.XKORD  
               id_temp.ENDKOMB = BERID.ENDKOMB
               id_temp.FRI3 = BERID.FRI3.      
               FIND FIRST uppl_temp WHERE uppl_temp.NUM = id_temp.NUM USE-INDEX NUM.
               ASSIGN
               id_temp.GRUPP = uppl_temp.GRUPP
               id_temp.ORD = uppl_temp.ORD.            
            END.                    
            OPEN QUERY hamtaq FOR EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND BERMTRL.OMRADE = valomrade
            AND BERMTRL.NUM = kon_val.NUM NO-LOCK.
            GET FIRST hamtaq NO-LOCK.
            DO WHILE AVAILABLE(BERMTRL):
               IF BERMTRL.ANTAL > 0 THEN DO: 
                  CREATE mtrl_temp.
                  ASSIGN
                  mtrl_temp.FORNR = BERMTRL.AONR /*BEREDNINGSNUMMER*/
                  mtrl_temp.LINNR = BERMTRL.OMRADE /*OMRÅDE*/
                  mtrl_temp.NUM = BERMTRL.NUM
                  mtrl_temp.ENR = BERMTRL.ENR     
                  mtrl_temp.BENAMNING = BERMTRL.BENAMNING
                  mtrl_temp.ENHET = BERMTRL.ENHET
                  mtrl_temp.ANTAL = BERMTRL.ANTAL
                  mtrl_temp.PRIS = BERMTRL.PRIS 
                  mtrl_temp.TOTPRIS = BERMTRL.PRIS * BERMTRL.ANTAL      
                  mtrl_temp.LEVKOD = BERMTRL.LEVKOD
                  mtrl_temp.MTRLTEXT = BERMTRL.MTRLTEXT.               
               END.
               GET NEXT hamtaq NO-LOCK.
            END.
            CLOSE QUERY hamtaq.                  
         END.
         ELSE DO:
            OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = kon_val.BERAONR AND
            BERLINKAB.OMRADE = kon_val.OMRADE /* NIKLAS 070315 AND BERLINKAB.DATUM = datvar*/ AND BERLINKAB.KORTKOD = ?
            AND BERLINKAB.UPPLAG = INTEGER(STRING(kon_val.F2)) NO-LOCK.
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
               IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
                  IF BERLINKAB.LEVKOD = "12" OR BERLINKAB.LEVKOD = "13"  THEN lin_upp.LEVKOD = "16".
               END.                        
               GET NEXT linq NO-LOCK.
            END.
            CLOSE QUERY linq. 
         END.
      END. 
      FOR EACH kon_val WHERE kon_val.GRUPP NE 1000:
         OPEN QUERY linq FOR EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND 
         BERLINKAB.OMRADE = valomrade AND BERLINKAB.DATUM = datvar AND BERLINKAB.KORTKOD = ?
         AND BERLINKAB.TOTMETER = 0 AND BERLINKAB.NUM1 = kon_val.NUM USE-INDEX DATUM NO-LOCK.
         GET FIRST linq NO-LOCK.
         DO WHILE AVAILABLE(BERLINKAB):
            IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
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
               IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
                  IF BERLINKAB.LEVKOD = "12" OR BERLINKAB.LEVKOD = "13"  THEN mtrl_temp.LEVKOD = "16".
               END.
            END.   
            GET NEXT linq NO-LOCK.
         END.          
         CLOSE QUERY linq.
      END.
   END.   
   FOR EACH mtrl_temp:
      FIND FIRST uppl_temp WHERE uppl_temp.NUM = mtrl_temp.NUM
      USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE uppl_temp THEN DO: 
         ASSIGN
         mtrl_temp.UPPLAG = uppl_temp.UPPLAG
         mtrl_temp.GRUPP = uppl_temp.GRUPP
         mtrl_temp.ORD = uppl_temp.ORD.
      END.
      ELSE DO:
         mtrl_temp.UPPLAG = ?.
      END.
      FIND FIRST id_temp WHERE id_temp.NUM = mtrl_temp.NUM
      USE-INDEX NUM NO-LOCK NO-ERROR.
      IF AVAILABLE id_temp THEN DO:  
         IF id_temp.ENDKOMB = FALSE THEN DO:
            ASSIGN
            mtrl_temp.FORNR = id_temp.FORNR
            mtrl_temp.LINNR = id_temp.LINNR
            mtrl_temp.NATNR = id_temp.NATNR
            mtrl_temp.FRI1 = id_temp.FRI1
            mtrl_temp.FRI2 = id_temp.FRI2        
            mtrl_temp.XKORD = id_temp.XKORD.
         END.
         ELSE DO:
            FIND FIRST idbuff WHERE idbuff.XKORD = id_temp.XKORD AND
            RECID(idbuff) NE RECID(id_temp) NO-LOCK NO-ERROR.
            ASSIGN          
            mtrl_temp.FORNR = idbuff.FORNR
            mtrl_temp.LINNR = idbuff.LINNR
            mtrl_temp.NATNR = idbuff.NATNR
            mtrl_temp.FRI1 = idbuff.FRI1
            mtrl_temp.FRI2 = idbuff.FRI2        
            mtrl_temp.XKORD = idbuff.XKORD.
         END.   
      END.
      ELSE DO:
         mtrl_temp.XKORD = ?.
      END.
      IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
         IF mtrl_temp.LEVKOD = "12" OR mtrl_temp.LEVKOD = "13" THEN mtrl_temp.LEVKOD = "16".
      END.
   END.         
   
   ASSIGN
   sumpris = 0
   sumantal = 0.
   FOR EACH mtrl_temp WHERE mtrl_temp.XKORD NE ? BREAK BY mtrl_temp.ENR BY mtrl_temp.UPPLAG BY mtrl_temp.XKORD BY mtrl_temp.LEVKOD: 
      ACCUMULATE mtrl_temp.TOTPRIS (TOTAL BY mtrl_temp.ENR BY mtrl_temp.UPPLAG BY mtrl_temp.XKORD BY mtrl_temp.LEVKOD). 
      ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.ENR BY mtrl_temp.UPPLAG BY mtrl_temp.XKORD BY mtrl_temp.LEVKOD).       
      IF LAST-OF(mtrl_temp.LEVKOD) THEN DO TRANSACTION:
         CREATE mtrl_temp2.
         ASSIGN                        
         mtrl_temp2.UPPLAG = mtrl_temp.UPPLAG 
         mtrl_temp2.GRUPP = mtrl_temp.GRUPP 
         mtrl_temp2.XKORD = mtrl_temp.XKORD
         mtrl_temp2.ENR = mtrl_temp.ENR
         mtrl_temp2.BENAMNING = mtrl_temp.BENAMNING 
         mtrl_temp2.ENHET = mtrl_temp.ENHET
         mtrl_temp2.PRIS = mtrl_temp.PRIS
         mtrl_temp2.LEVKOD = mtrl_temp.LEVKOD
         mtrl_temp2.KLAR = FALSE
         mtrl_temp2.FORNR = mtrl_temp.FORNR
         mtrl_temp2.LINNR = mtrl_temp.LINNR
         mtrl_temp2.NATNR = mtrl_temp.NATNR
         mtrl_temp2.FRI1 = mtrl_temp.FRI1
         mtrl_temp2.FRI2 = mtrl_temp.FRI2
         mtrl_temp2.ORD = mtrl_temp.ORD
         mtrl_temp2.TOTPRIS = (ACCUM TOTAL mtrl_temp.TOTPRIS) - sumpris                       
         mtrl_temp2.ANTAL = (ACCUM TOTAL mtrl_temp.ANTAL) - sumantal                
         sumpris = ACCUM TOTAL mtrl_temp.TOTPRIS 
         sumantal = ACCUM TOTAL mtrl_temp.ANTAL.                       
      END.     
   END.  
   ASSIGN
   sumpris = 0
   sumantal = 0.    
   FOR EACH mtrl_temp WHERE mtrl_temp.XKORD = ? BREAK BY mtrl_temp.ENR BY mtrl_temp.UPPLAG BY mtrl_temp.NUM BY mtrl_temp.LEVKOD: 
      ACCUMULATE mtrl_temp.TOTPRIS (TOTAL BY mtrl_temp.ENR BY mtrl_temp.UPPLAG BY mtrl_temp.NUM BY mtrl_temp.LEVKOD). 
      ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.ENR BY mtrl_temp.UPPLAG BY mtrl_temp.NUM BY mtrl_temp.LEVKOD).       
      IF LAST-OF(mtrl_temp.LEVKOD) THEN DO TRANSACTION:
         CREATE mtrl_temp2.
         ASSIGN                        
         mtrl_temp2.UPPLAG = mtrl_temp.UPPLAG 
         mtrl_temp2.GRUPP = mtrl_temp.GRUPP 
         mtrl_temp2.XKORD = mtrl_temp.XKORD 
         mtrl_temp2.NUM = mtrl_temp.NUM
         mtrl_temp2.ENR = mtrl_temp.ENR
         mtrl_temp2.BENAMNING = mtrl_temp.BENAMNING 
         mtrl_temp2.ENHET = mtrl_temp.ENHET
         mtrl_temp2.PRIS = mtrl_temp.PRIS
         mtrl_temp2.LEVKOD = mtrl_temp.LEVKOD 
         mtrl_temp2.KLAR = FALSE
         mtrl_temp2.FORNR = mtrl_temp.FORNR
         mtrl_temp2.LINNR = mtrl_temp.LINNR
         mtrl_temp2.NATNR = mtrl_temp.NATNR
         mtrl_temp2.FRI1 = mtrl_temp.FRI1
         mtrl_temp2.FRI2 = mtrl_temp.FRI2
         mtrl_temp2.ORD = mtrl_temp.ORD
         mtrl_temp2.TOTPRIS = (ACCUM TOTAL mtrl_temp.TOTPRIS) - sumpris                       
         mtrl_temp2.ANTAL = (ACCUM TOTAL mtrl_temp.ANTAL) - sumantal                                         
         sumpris = ACCUM TOTAL mtrl_temp.TOTPRIS 
         sumantal = ACCUM TOTAL mtrl_temp.ANTAL.                       
      END.     
   END.   
   FOR EACH mtrl_temp2 WHERE mtrl_temp2.ANTAL = 0:
      DELETE mtrl_temp2.
   END.
   
   IF delbest = FALSE THEN DO:
      RUN skydd_UI.
   END.
   FOR EACH lin_upp:
      lin_upp.KLAR2 = FALSE.
   END.   
   FOR EACH lin_upp WHERE lin_upp.KLAR2 = FALSE:        
      FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = lin_upp.ENR AND
      trp_mtrl.LEVKOD = lin_upp.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ" 
      NO-LOCK NO-ERROR. 
      IF AVAILABLE trp_mtrl THEN DO:  
         enrvar = lin_upp.ENR.                         
         IF trp_mtrl.ANTAL > 0 THEN DO: 
            RUN nedepa2_UI.                                           
         END.          
         ELSE DO: 
            RUN depa2_UI.               
         END.                     
      END.
      ELSE DO:
         FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = lin_upp.ENR AND
         trp_mtrl.LEVKOD NE lin_upp.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ"       
         NO-LOCK NO-ERROR.
         IF AVAILABLE trp_mtrl THEN DO:  
            enrvar = lin_upp.ENR.       
            IF trp_mtrl.ANTAL > 0 THEN DO:            
               RUN nedepa2_UI.
            END.
            ELSE DO:
               RUN depa2_UI.
            END.                            
         END.
         ELSE DO:
            FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = "E" + lin_upp.ENR AND
            trp_mtrl.LEVKOD NE lin_upp.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ"       
            NO-LOCK NO-ERROR.
            IF AVAILABLE trp_mtrl THEN DO:         
               enrvar = "E" + lin_upp.ENR.
               IF trp_mtrl.ANTAL > 0 THEN DO:            
                  RUN nedepa2_UI.
               END.
               ELSE DO:
                  RUN depa2_UI.
               END.                               
            END.
            ELSE DO:
               FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = SUBSTRING(lin_upp.ENR,2,11) AND
               trp_mtrl.LEVKOD NE lin_upp.LEVKOD  AND trp_mtrl.DBEST NE "DEPÅ"       
               NO-LOCK NO-ERROR.
               IF AVAILABLE trp_mtrl THEN DO:         
                  enrvar = SUBSTRING(lin_upp.ENR,2,11).
                  IF trp_mtrl.ANTAL > 0 THEN DO:            
                     RUN nedepa2_UI.
                  END.
                  ELSE DO:
                     RUN depa2_UI.
                  END.                               
               END.
               ELSE DO:
                  DELETE lin_upp.
               END.
            END.
         END.   
      END. 
   END.    
  
   FOR EACH mtrl_temp2:      
      mtrl_temp2.KLAR2 = FALSE.
   END.        
   
   OPEN QUERY mq FOR EACH mtrl_temp2 WHERE mtrl_temp2.KLAR2 = FALSE.
   GET FIRST mq.
   DO WHILE AVAILABLE(mtrl_temp2):
      FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = mtrl_temp2.ENR AND
      trp_mtrl.LEVKOD = mtrl_temp2.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ"       
      NO-LOCK NO-ERROR.
      IF AVAILABLE trp_mtrl THEN DO:                                     
         enrvar = mtrl_temp2.ENR.         
         IF trp_mtrl.ANTAL > 0 THEN DO:
            RUN nedepa_UI.        
         END.          
         ELSE DO: 
            RUN depa_UI.   
         END.                  
      END.
      ELSE DO:       
         FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = mtrl_temp2.ENR AND
         trp_mtrl.LEVKOD NE mtrl_temp2.LEVKOD AND trp_mtrl.LEVKOD NE "0" AND trp_mtrl.DBEST NE "DEPÅ"       
         NO-LOCK NO-ERROR.
         IF AVAILABLE trp_mtrl THEN DO:                    
            enrvar = mtrl_temp2.ENR.
            IF trp_mtrl.ANTAL > 0 THEN DO:                   
               RUN nedepa_UI.                       
            END.
            ELSE DO:
               RUN depa_UI.
            END.                            
         END.
         ELSE DO:
            FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = "E" + mtrl_temp2.ENR AND
            trp_mtrl.LEVKOD NE mtrl_temp2.LEVKOD AND trp_mtrl.DBEST NE "DEPÅ"       
            NO-LOCK NO-ERROR.
            IF AVAILABLE trp_mtrl THEN DO:         
               enrvar = "E" + mtrl_temp2.ENR.
               IF trp_mtrl.ANTAL > 0 THEN DO:            
                  RUN nedepa_UI.
               END.
               ELSE DO:
                  RUN depa_UI.
               END.                            
            END.
            ELSE DO:
               FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = SUBSTRING(mtrl_temp2.ENR,2,11) AND
               trp_mtrl.LEVKOD NE mtrl_temp2.LEVKOD AND trp_mtrl.LEVKOD NE "0" AND trp_mtrl.DBEST NE "DEPÅ"       
               NO-LOCK NO-ERROR.
               IF AVAILABLE trp_mtrl THEN DO:                   
                  enrvar = SUBSTRING(mtrl_temp2.ENR,2,11).        
                  IF trp_mtrl.ANTAL > 0 THEN DO:                      
                     RUN nedepa_UI.                            
                  END.
                  ELSE DO:
                     RUN depa_UI.
                  END.                            
               END.
               ELSE DO:       
                  DELETE mtrl_temp2.
               END.
            END.            
         END.   
      END. 
      GET NEXT mq.  
   END.    
  
   OPEN QUERY gruppq FOR EACH KONSTGRUPP NO-LOCK.
   GET FIRST gruppq NO-LOCK.
   DO WHILE AVAILABLE(KONSTGRUPP):
      CREATE grupp_temp.
      ASSIGN 
      grupp_temp.KONSKOD = KONSTGRUPP.KONSKOD
      grupp_temp.BENAMNING = KONSTGRUPP.BENAMNING
      grupp_temp.ORDNING = KONSTGRUPP.ORDNING.
      GET NEXT gruppq NO-LOCK.
   END.
   CLOSE QUERY gruppq.
   FOR EACH grupp_temp:
      CREATE grupp_temp2.
      ASSIGN 
      grupp_temp2.KONSKOD = grupp_temp.KONSKOD
      grupp_temp2.BENAMNING = grupp_temp.BENAMNING
      grupp_temp2.ORDNING = grupp_temp.ORDNING.  
   END.
   FOR EACH mtrl_temp WHERE mtrl_temp.MTRLTEXT NE " ":
      FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = mtrl_temp.ENR AND
      trp_mtrl.LEVKOD = mtrl_temp.LEVKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE trp_mtrl THEN DO:
         FIND FIRST trp_mtrl WHERE trp_mtrl.ENR = mtrl_temp.ENR AND
         trp_mtrl.LEVKOD NE mtrl_temp.LEVKOD NO-LOCK NO-ERROR.
         IF AVAILABLE trp_mtrl THEN DO:
            mtrl_temp.LEVKOD = trp_mtrl.LEVKOD.
         END.            
      END.
   END.   
