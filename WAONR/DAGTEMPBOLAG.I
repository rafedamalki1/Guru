/*DAGTEMPBOLAG.I*/
DEFINE BUFFER dagbuff FOR dagtemp.
IF Guru.Konstanter:varforetypval[18] = 0 OR Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)   THEN.
ELSE DO:
   FOR EACH valdaao WHERE valdaao.omrade = "":
      FOR EACH dagtemp WHERE dagtemp.AONR = valdaao.AONR AND dagtemp.DELNR = valdaao.DELNR:
         FIND FIRST omvtemp WHERE omvtemp.OMRADE = dagtemp.OMRADE NO-LOCK NO-ERROR.
         IF AVAILABLE omvtemp THEN DO:
            FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR. 
            IF NOT AVAILABLE BOLAGSEK THEN DO:
               FOR EACH dagbuff WHERE dagbuff.AONR = dagtemp.AONR AND dagbuff.DELNR = dagtemp.DELNR AND dagbuff.OMRADE = dagtemp.OMRADE:
                  DELETE dagbuff.
               END.   
            END.                                 
         END.
      END.    
   END.
END.      