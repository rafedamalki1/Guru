/*AOTKOLLNW2.I*/
   FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
{AVAFOR.I}

PROCEDURE tidaokoll_UI.   
   FIND FIRST FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.  
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:    
      DEFINE BUFFER tidbuff10 FOR TIDREGITAB.
      DEFINE VARIABLE totarbkort AS DECIMAL NO-UNDO.
      DEFINE VARIABLE maxarbkort AS DECIMAL NO-UNDO.
      DEFINE VARIABLE foremaxarbkort AS DECIMAL NO-UNDO.
      DEFINE VARIABLE atidl AS LOGICAL NO-UNDO.
      DEFINE VARIABLE avarfor AS INTEGER NO-UNDO.
      ASSIGN atidl = FALSE
      totarbkort = 0.                           
      IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
         /*Kalmar kan ta ut arbetstidsförkortning i pengar vid tex sjukdom*/
         FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.LONTILLAGG = "ATL"
         AND YEAR(tidbuff10.DATUM) = YEAR(extratidallt.DATUM) NO-LOCK:
            totarbkort = totarbkort + klock100(tidbuff10.LONTILLANTAL).
         END.                      
         IF extratidallt.LONTILLAGG = "ATL" THEN DO:                     
            regdatum = kolladatumvar.
            persrec = RECID(PERSONALTAB).         
            RUN REGVEC.P.
            RUN SLUTARB.P.                           
            IF tidtot > regtotalt   THEN DO:
               CREATE felmeddtemp.  
               felmeddtemp.felmedd = "Det går bara att ta ut lika många timmar som det finns arbetstid per dag, hör med lönepersonal".
               RETURN.
            END.
            IF PERSONALTAB.OMREGTID = 1 THEN DO:
               CREATE felmeddtemp.  
               felmeddtemp.felmedd = "Arbetstidförkortning spärrad, hör med lönepersonal".
               RETURN.
               
            END.                                 
            ASSIGN atidl = TRUE.
            FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "160"
            AND YEAR(tidbuff10.DATUM) = YEAR(extratidallt.DATUM) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
               totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
            END.               
            
            IF vart = "NYA" THEN DO:         
               totarbkort = totarbkort + klock100(tidtot).
            END.                  
            ELSE DO:            
               FIND FIRST tidbuff10 WHERE RECID(tidbuff10) = extratidallt.RECTIDVIS NO-LOCK NO-ERROR.
               IF AVAILABLE tidbuff10 THEN DO:                  
                  totarbkort = totarbkort + klock100(tidtot) - klock100(tidbuff10.LONTILLANTAL).                  
               END.
            END.
            avarfor = 0.
            IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
               RUN havafor (INPUT PERSONALTAB.PERSONALKOD, OUTPUT avarfor).                        
            END.
            
            /*atkgräns*/
            IF Guru.Konstanter:globforetag = "gkal" AND extratidallt.DATUM GE 01/01/2014 THEN foremaxarbkort = 63.
            ELSE IF Guru.Konstanter:globforetag = "SNAT" AND extratidallt.DATUM GE 01/01/2014 THEN foremaxarbkort = 63.
            ELSE IF Guru.Konstanter:globforetag = "Sund" AND extratidallt.DATUM GE 01/01/2014 THEN foremaxarbkort = 54.
            ELSE IF Guru.Konstanter:globforetag = "LULE" AND extratidallt.DATUM GE 01/01/2014 THEN foremaxarbkort = 63.            
            ELSE foremaxarbkort = 63.
            
            IF avarfor > 0 THEN maxarbkort = avarfor.
            ELSE maxarbkort = foremaxarbkort.           
            IF totarbkort > maxarbkort THEN DO:
               totarbkort = totarbkort - klock100(tidtot).
               CREATE felmeddtemp.  
               felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning, du har tidigare skrivit"  + STRING(totarbkort) + "timmar".
               RETURN.                  
            END.            
         END.         
      END.      
      IF atidl = FALSE AND AONRTAB.AONR = "160" AND AONRTAB.DELNR  = 0 THEN DO:                  
         
         FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "160"  AND tidbuff10.DELNR = 0
         AND YEAR(tidbuff10.DATUM) = YEAR(extratidallt.DATUM) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
            totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
         END.                     
         IF vart = "NYA" THEN DO:         
            totarbkort = totarbkort + klock100(tidtot).
         END.                  
         ELSE DO:            
            FIND FIRST tidbuff10 WHERE RECID(tidbuff10) = extratidallt.RECTIDVIS NO-LOCK NO-ERROR.
            IF AVAILABLE tidbuff10 THEN DO:
               IF tidbuff10.AONR = "160" AND tidbuff10.DELNR = 0 THEN DO:
                  totarbkort = totarbkort + klock100(tidtot) - klock100(tidbuff10.TOTALT).
               END.
               ELSE DO:                  
                  totarbkort = totarbkort + klock100(tidtot).
               END.
            END.
         END.         
         FOR EACH TIDFEL WHERE TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND TIDFEL.AONR = "160" AND TIDFEL.DELNR = 0
         AND TIDFEL.DEBET = TRUE
         AND YEAR(TIDFEL.DATUM) = YEAR(extratidallt.DATUM) AND TIDFEL.TIDLOG = TRUE NO-LOCK:
            totarbkort = totarbkort + klock100(TIDFEL.TOTALT).
         END.            
         FOR EACH TIDFEL WHERE TIDFEL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND TIDFEL.AONR = "160" AND TIDFEL.DELNR = 0
         AND TIDFEL.DEBET = FALSE
         AND YEAR(TIDFEL.DATUM) = YEAR(extratidallt.DATUM) AND TIDFEL.TIDLOG = TRUE NO-LOCK:
            totarbkort = totarbkort - klock100(TIDFEL.TOTALT).
         END.            
         avarfor = 0.
         IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
            RUN havafor (INPUT PERSONALTAB.PERSONALKOD, OUTPUT avarfor).                        
         END.
         /*atkgräns*/           
         IF Guru.Konstanter:globforetag = "gkal" AND extratidallt.DATUM GE 01/01/2014 THEN foremaxarbkort = 63.
         ELSE IF Guru.Konstanter:globforetag = "SNAT" AND extratidallt.DATUM GE 01/01/2014 THEN foremaxarbkort = 63.
         ELSE IF Guru.Konstanter:globforetag = "SUND" AND extratidallt.DATUM GE 01/01/2014 THEN foremaxarbkort = 54.
         ELSE IF Guru.Konstanter:globforetag = "LULE" AND extratidallt.DATUM GE 01/01/2014 THEN foremaxarbkort = 63.
         ELSE foremaxarbkort = 63.
                  
         IF avarfor > 0 THEN maxarbkort = avarfor.
         ELSE maxarbkort = foremaxarbkort.        
         IF totarbkort > maxarbkort THEN DO:
            totarbkort = totarbkort - klock100(tidtot).
            CREATE felmeddtemp.  
            felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning, du har tidigare skrivit"  + STRING(totarbkort) + "timmar".
            RETURN.                  
         END.            
      END.  
      IF Guru.Konstanter:globforetag = "MISV"  THEN DO:    
         IF AONRTAB.AONR = "161" THEN DO:                              
            FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "161"
            AND YEAR(tidbuff10.DATUM) = YEAR(extratidallt.DATUM) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
               totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
            END.             
            IF vart = "NYA" THEN DO:         
               totarbkort = totarbkort + klock100(tidtot).
            END.
            ELSE DO:            
               FIND FIRST tidbuff10 WHERE RECID(tidbuff10) = extratidallt.RECTIDVIS NO-LOCK NO-ERROR.
               IF AVAILABLE tidbuff10 THEN DO:
                  IF tidbuff10.AONR NE "161" THEN DO:
                     totarbkort = totarbkort + klock100(tidtot).
                  END.
               END.
   
            END.
            /*atkgräns*/
            IF extratidallt.DATUM GE 01/01/2004 THEN maxarbkort = 27.            
            IF totarbkort > maxarbkort THEN DO:
               totarbkort = totarbkort - klock100(tidtot).
               CREATE felmeddtemp.  
               felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning, du har tidigare skrivit"  + STRING(totarbkort) + "timmar".
               RETURN.                  
            END.            
         END.
      END.
      /*IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:    
         /*enskild angelägenhet*/
         IF AONRTAB.AONR = "135" THEN DO:                              
            FOR EACH tidbuff10 WHERE tidbuff10.PERSONALKOD = PERSONALTAB.PERSONALKOD AND tidbuff10.AONR = "135"
            AND YEAR(tidbuff10.DATUM) = YEAR(extratidallt.DATUM) AND tidbuff10.TIDLOG = TRUE NO-LOCK:
               totarbkort = totarbkort + klock100(tidbuff10.TOTALT).
            END.             
            IF vart = "NYA" THEN DO:         
               totarbkort = totarbkort + klock100(tidtot).
            END.
            ELSE DO:            
               FIND FIRST tidbuff10 WHERE RECID(tidbuff10) = extratidallt.RECTIDVIS NO-LOCK NO-ERROR.
               IF AVAILABLE tidbuff10 THEN DO:
                  IF tidbuff10.AONR NE "135" THEN DO:
                     totarbkort = totarbkort + klock100(tidtot).
                  END.
               END.
   
            END.            
            /*80  timmar eller 8 dagar (tillfällen)  Ingrid kollar vad som gäller*/
            FIND FIRST RULLPERS WHERE RULLPERS.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
            RULLPERS.STARTDATUM <= extratidallt.DATUM AND RULLPERS.SLUTDATUM >= extratidallt.DATUM NO-LOCK NO-ERROR.
            IF AVAILABLE RULLPERS THEN DO:
               /*snittveckoarbetstid 34 tim? enligt Ingrid*/

            END.
            ELSE IF PERSONALTAB.DELTID = TRUE THEN DO:
               /*FIND FIRST VECKOARBETID WHERE VECKOARBETID.VECKOSCHEMA = vschemat
               USE-INDEX VECKOSCHEMA NO-LOCK NO-ERROR.               
               IF ORDARB.OBKOD NE "" THEN DO:
                  FIND FIRST FLEXREG NO-LOCK NO-ERROR.         
                  IF AVAILABLE FLEXREG THEN DO:
                     sommar = FALSE
                     IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN sommar = TRUE.                  
                     ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN  sommar = TRUE. 
                     ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN  sommar = TRUE.
                     IF sommar = TRUE THEN DO:
   
                     END.
                     ELSE DO:
                     END.
                  END.
               END.*/
            END.
            ELSE DO:
               /*Alla heltidare som ej jobbar skift*/
               ASSIGN maxarbkort = 80.
            END.
            IF totarbkort > maxarbkort THEN DO:
               totarbkort = totarbkort - klock100(tidtot).
               CREATE felmeddtemp.  
               felmeddtemp.felmedd = "Max " + STRING(maxarbkort) + " timmar per år får skrivas på arbetstidförkortning, du har tidigare skrivit"  + STRING(totarbkort) + "timmar".
               RETURN.                  
            END.            
         END.
      END.*/
   END.   
END PROCEDURE.



