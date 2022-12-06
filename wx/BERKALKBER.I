/*BERKALKBER.I*/
/*OBS! GLÖM EJ ATT ÄNDRA I BERKALK.I BERKALK2I BERKALKBER.I DIRUTFAPPc.P*/
/*KALKBER*/
    totmtrl = 0.   
      IF KALKAONR.AONR NE ? THEN DO:
         FIND FIRST BEREDNING WHERE BEREDNING.AONR = KALKAONR.AONR AND 
         BEREDNING.DELNR = KALKAONR.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE BEREDNING THEN DO:
            RUN LISTPROG.P (INPUT BEREDNING.BERAONR, INPUT BEREDNING.OMRADE, OUTPUT TABLE mtrl_temp, OUTPUT TABLE lin_upp,OUTPUT TABLE lin_temp).
            EMPTY TEMP-TABLE mtrl_temp2 NO-ERROR. 
            FOR EACH mtrl_temp BREAK BY mtrl_temp.ENR: 
               ACCUMULATE mtrl_temp.TOTPRIS (TOTAL BY mtrl_temp.ENR). 
               ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.ENR).       
               IF LAST-OF(mtrl_temp.ENR) THEN DO:
                  CREATE mtrl_temp2.
                  ASSIGN 
                  mtrl_temp2.ENR = mtrl_temp.ENR
                  mtrl_temp2.BENAMNING = mtrl_temp.BENAMNING 
                  mtrl_temp2.ENHET = mtrl_temp.ENHET 
                  mtrl_temp2.PRIS = mtrl_temp.PRIS
                  mtrl_temp2.TOTPRIS = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.TOTPRIS)                       
                  mtrl_temp2.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).                                                        
               END.     
            END.    
            FOR EACH lin_upp:
               FIND FIRST mtrl_temp2 WHERE mtrl_temp2.ENR = lin_upp.ENR NO-LOCK NO-ERROR.
               IF AVAILABLE mtrl_temp2 THEN DO:                      
                  ASSIGN
                  mtrl_temp2.ANTAL = mtrl_temp2.ANTAL + lin_upp.TOTMETER
                  mtrl_temp2.TOTPRIS = mtrl_temp2.TOTPRIS + lin_upp.TOTPRIS.
               END.
               ELSE DO:                    
                  CREATE mtrl_temp2.
                  ASSIGN 
                  mtrl_temp2.ENR = lin_upp.ENR
                  mtrl_temp2.BENAMNING = lin_upp.BENAMNING 
                  mtrl_temp2.ENHET = lin_upp.ENHET 
                  mtrl_temp2.PRIS = lin_upp.PRIS
                  mtrl_temp2.TOTPRIS = lin_upp.TOTPRIS                       
                  mtrl_temp2.ANTAL = lin_upp.TOTMETER.
               END.
            END.
            FOR EACH mtrl_temp2:
               totmtrl = totmtrl + mtrl_temp2.TOTPRIS.
            END.              
         END.
      END.
      IF globforetag = "ELPA" OR globforetag = "GRAN" THEN DO:
         FIND FIRST kalk_temp WHERE kalk_temp.ARBKOD = "SUCC" NO-LOCK NO-ERROR.
         IF AVAILABLE kalk_temp THEN totmtrl = totmtrl + kalk_temp.MATERIEL.
      END.
