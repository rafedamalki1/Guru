/*AOHMT.I*/
      CREATE utsokaonr.          
      BUFFER-COPY AONRTAB TO utsokaonr
      ASSIGN
      utsokaonr.PROJEKTOR = AONRTAB.STARTDAG
      utsokaonr.AONRREC = RECID(AONRTAB)
      utsokaonr.TABORT = TRUE.
      IF utsokaonr.PROJEKTOR = "TUÖ" THEN DO:
         utsokaonr.PROJEKTOR = utsokaonr.PROJEKTOR.
      END.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = utsokaonr.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN utsokaonr.AVDELNINGNR = OMRADETAB.AVDELNINGNR.
      /* utsokaonr.ARBUPPG = AONRTAB.ARBUPPG[1]*/
      FIND FIRST valsoktemp NO-ERROR.
      IF AVAILABLE valsoktemp THEN DO:  
                     
         IF FORETAG.FORETAG = "GKAL" OR FORETAG.FORETAG = "celpa" THEN DO:    
                              
            IF valsoktemp.SOKCHAR[2] = "U210- " + Guru.Konstanter:gaok + " till excel" THEN DO:                         
               IF uppvaltemp.MANUPPDEL  = FALSE THEN DO:   
                                                
                  FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = AONRTAB.AONR AND AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
                  AONRTIDLAGE.IDTIDLAG = "FAKTURA UNDERLAG" NO-LOCK NO-ERROR.
                  IF AVAILABLE AONRTIDLAGE THEN DO: 
                     IF AVAILABLE utsokaonr THEN DO:                                          
                        DELETE utsokaonr.
                     END.
                  END.
                  FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = AONRTAB.AONR AND AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
                  AONRTIDLAGE.IDTIDLAG = "FAKTURA" NO-LOCK NO-ERROR.
                  IF AVAILABLE AONRTIDLAGE THEN DO: 
                     IF AVAILABLE utsokaonr THEN DO:                                          
                        DELETE utsokaonr.
                     END.
                  END.
                  FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = AONRTAB.AONR AND AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
                  AONRTIDLAGE.IDTIDLAG = "Fakturan" NO-LOCK NO-ERROR.
                  IF AVAILABLE AONRTIDLAGE THEN DO: 
                     IF AVAILABLE utsokaonr THEN DO:                                          
                        DELETE utsokaonr.
                     END.
                  END.                  
               END.
               ELSE DO:               
                  FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = AONRTAB.AONR AND AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
                  AONRTIDLAGE.IDTIDLAG = "FAKTURA" NO-LOCK NO-ERROR.
                  IF AVAILABLE AONRTIDLAGE THEN DO: 
                     IF AVAILABLE utsokaonr THEN DO:                                          
                        DELETE utsokaonr.
                     END.
                  END.
                  /*Wenche vill inte ha med Fakturan makulerad */
                  FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = AONRTAB.AONR AND AONRTIDLAGE.DELNR = AONRTAB.DELNR AND
                  AONRTIDLAGE.IDTIDLAG = "Fakturan" NO-LOCK NO-ERROR.
                  IF AVAILABLE AONRTIDLAGE THEN DO: 
                     IF AVAILABLE utsokaonr THEN DO:                                          
                        DELETE utsokaonr.
                     END.
                  END.
               END.
            END.
         END.                  
         
         IF FORETAG.FORETAG = "fors" OR FORETAG.FORETAG = "cvast" OR FORETAG.FORETAG = "elpa" THEN DO:                      
            DEFINE VARIABLE gejbest AS LOGICAL NO-UNDO.
            gejbest = TRUE.
            IF valsoktemp.SOKCHAR[2] = "Gjorda beställningar" THEN DO:                         
               FIND FIRST BEREDNING WHERE BEREDNING.AONR = AONRTAB.AONR  AND BEREDNING.DELNR = AONRTAB.DELNR
               NO-LOCK NO-ERROR.
               IF AVAILABLE BEREDNING THEN DO:
                  FIND FIRST BESTSTAT WHERE BESTSTAT.BERNR = INTEGER(BEREDNING.BERAONR) AND BESTSTAT.OMRADE = BEREDNING.OMRADE 
                  AND BESTSTAT.DATUM GE uppvaltemp.STARTDATUM AND BESTSTAT.DATUM LE uppvaltemp.SLUTDATUM  NO-LOCK NO-ERROR.
                  IF AVAILABLE BESTSTAT THEN DO:  
                     gejbest = FALSE.                                       
                  END.
               END.
               IF gejbest = TRUE THEN DO:                  
                  IF AVAILABLE utsokaonr THEN DO:                                          
                     DELETE utsokaonr.
                  END.
               END.
            END.
         END.

         IF AVAILABLE utsokaonr THEN DO:            
            IF valsoktemp.SOKCHAR[1] = "alla" OR valsoktemp.SOKCHAR[1] = "" OR valsoktemp.SOKCHAR[1] = ? THEN utkonto = utkonto.
            ELSE DO:
               IF AONRTAB.ARBARTKOD = valsoktemp.SOKINT[1] THEN utkonto = utkonto.
               ELSE DO:
                  IF AVAILABLE utsokaonr THEN DO:                                          
                     DELETE utsokaonr.               
                  END.
               END.
            END.
         END.
         IF AVAILABLE utsokaonr THEN DO:            
            IF valsoktemp.SOKCHAR[4] = "alla" OR valsoktemp.SOKCHAR[4] = "" OR valsoktemp.SOKCHAR[4] = ? THEN utkonto = utkonto.
            ELSE DO:
               IF AONRTAB.PKOD = valsoktemp.SOKINT[4] THEN utkonto = utkonto.
               ELSE DO:
                  IF AVAILABLE utsokaonr THEN DO:                                          
                     DELETE utsokaonr.               
                  END.
               END.
            END.
         END.
         
         IF AVAILABLE utsokaonr THEN DO:            
            IF valsoktemp.SOKCHAR[3] = "alla" THEN utkonto = utkonto.
            ELSE IF valsoktemp.SOKCHAR[3] = "" THEN utkonto = utkonto.
            ELSE IF uppvaltemp.AVDNR NE "ALLA" THEN utkonto = utkonto.
            ELSE IF uppvaltemp.OMRADE NE "ALLA" THEN utkonto = utkonto.
            ELSE DO:
               FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = utsokaonr.OMRADE NO-LOCK NO-ERROR.
               IF AVAILABLE OMRADETAB THEN DO:
                  FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
                  IF AVAILABLE AVDELNING THEN DO:
                     IF AVDELNING.POSTANST NE valsoktemp.SOKCHAR[3] THEN DELETE utsokaonr.               
                  END.
                  ELSE DELETE utsokaonr.
               END.
               ELSE IF utsokaonr.OMRADE = "" THEN.
               ELSE DELETE utsokaonr.               
            END.
            
         END.
      END.
      IF AVAILABLE utsokaonr THEN DO:
         IF uppvaltemp.DELNRKOLL = FALSE THEN DO:
            IF utsokaonr.DELNR NE 0 THEN DELETE utsokaonr. 
         END.
      END.
      IF AVAILABLE utsokaonr THEN DO:
         IF uppvaltemp.ENDBEST = TRUE THEN DO:
            IF utsokaonr.OMRADE =  utsokaonr.BESTID THEN DELETE utsokaonr. 
         END.
         ELSE IF uppvaltemp.ENDBEST = FALSE THEN DO:
            IF utsokaonr.OMRADE NE utsokaonr.BESTID THEN DELETE utsokaonr. 
         END.
      END.
      IF AVAILABLE utsokaonr THEN DO:
         IF uppvaltemp.OMRADE NE "ALLA" THEN DO:
            IF utsokaonr.OMRADE NE uppvaltemp.OMRADE THEN  DELETE utsokaonr. 
         END.
      END. 
      IF AVAILABLE utsokaonr THEN DO:
         IF uppvaltemp.BEREDARE NE "ALLA" THEN DO:
            IF utsokaonr.BEREDARE NE uppvaltemp.BEREDARE THEN DELETE utsokaonr. 
         END.
      END.
      IF AVAILABLE utsokaonr THEN DO:
         IF SUBSTRING(uppvaltemp.PROJEKTOR,1,20) NE "ALLA" THEN DO:
            IF utsokaonr.PROJEKTOR NE SUBSTRING(uppvaltemp.PROJEKTOR,1,20) THEN DO:
               DELETE utsokaonr. 
            END.
         END.
      END.
      IF AVAILABLE utsokaonr THEN DO:        
         IF SUBSTRING(uppvaltemp.PROJEKTOR,25,10) NE "" THEN DO:
            IF INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,25,10),"*") = 1 THEN utkonto = utkonto.
            ELSE DO:             
               IF INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,25,10),"*") = 0 THEN DO:
                  FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  AONRTAB.AONR AND
                  AONRKONTKOD.DELNR =  AONRTAB.DELNR AND AONRKONTKOD.K2 = SUBSTRING(uppvaltemp.PROJEKTOR,25,10)
                  NO-LOCK NO-ERROR.
               END.
               ELSE DO:
                  utkonto = SUBSTRING(uppvaltemp.PROJEKTOR,25,INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,25,10),"*") - 1).
                  FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  AONRTAB.AONR AND
                  AONRKONTKOD.DELNR =  AONRTAB.DELNR AND AONRKONTKOD.K2 BEGINS utkonto
                  NO-LOCK NO-ERROR.
               END.
               
               IF NOT AVAILABLE AONRKONTKOD THEN DO:
                  DELETE utsokaonr. 
               END.
            END.
         END.
         IF AVAILABLE utsokaonr THEN DO:            
            IF SUBSTRING(uppvaltemp.PROJEKTOR,35,10) NE "" THEN DO:
               IF INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,35,10),"*") = 1 THEN utkonto = utkonto.
               ELSE DO:                   
                  IF INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,35,10),"*") = 0 THEN DO:
                     FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  AONRTAB.AONR AND
                     AONRKONTKOD.DELNR =  AONRTAB.DELNR AND AONRKONTKOD.K1 = SUBSTRING(uppvaltemp.PROJEKTOR,35,10)
                     NO-LOCK NO-ERROR.
                  END.
                  ELSE DO:
                     utkonto = SUBSTRING(uppvaltemp.PROJEKTOR,35,INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,35,10),"*") - 1).
                     FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  AONRTAB.AONR AND
                     AONRKONTKOD.DELNR =  AONRTAB.DELNR AND AONRKONTKOD.K1 BEGINS utkonto
                     NO-LOCK NO-ERROR.
                  END.                  
                  IF NOT AVAILABLE AONRKONTKOD THEN DO:
                     DELETE utsokaonr. 
                  END.
               END.
            END.
            IF AVAILABLE utsokaonr THEN DO:            
               IF SUBSTRING(uppvaltemp.PROJEKTOR,45,10) NE "" THEN DO:
                  IF INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,45,10),"*") = 1 THEN utkonto = utkonto.
                  ELSE DO:                   
                     IF INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,45,10),"*") = 0 THEN DO:
                        FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  AONRTAB.AONR AND
                        AONRKONTKOD.DELNR =  AONRTAB.DELNR AND AONRKONTKOD.K3 = SUBSTRING(uppvaltemp.PROJEKTOR,45,10)
                        NO-LOCK NO-ERROR.
                     END.
                     ELSE DO:
                        utkonto = SUBSTRING(uppvaltemp.PROJEKTOR,45,INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,45,10),"*") - 1).
                        FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  AONRTAB.AONR AND
                        AONRKONTKOD.DELNR =  AONRTAB.DELNR AND AONRKONTKOD.K3 BEGINS utkonto
                        NO-LOCK NO-ERROR.
                     END.                  
                     IF NOT AVAILABLE AONRKONTKOD THEN DO:
                        DELETE utsokaonr. 
                     END.
                  END.
               END.
               IF AVAILABLE utsokaonr THEN DO:            
                  IF SUBSTRING(uppvaltemp.PROJEKTOR,55,10) NE "" THEN DO:
                     IF INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,55,10),"*") = 1 THEN utkonto = utkonto.
                     ELSE DO:                   
                        IF INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,55,10),"*") = 0 THEN DO:
                           FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  AONRTAB.AONR AND
                           AONRKONTKOD.DELNR =  AONRTAB.DELNR AND AONRKONTKOD.K4 = SUBSTRING(uppvaltemp.PROJEKTOR,55,10)
                           NO-LOCK NO-ERROR.
                        END.
                        ELSE DO:
                           utkonto = SUBSTRING(uppvaltemp.PROJEKTOR,55,INDEX(SUBSTRING(uppvaltemp.PROJEKTOR,55,10),"*") - 1).
                           FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR =  AONRTAB.AONR AND
                           AONRKONTKOD.DELNR =  AONRTAB.DELNR AND AONRKONTKOD.K4 BEGINS utkonto
                           NO-LOCK NO-ERROR.
                        END.                  
                        IF NOT AVAILABLE AONRKONTKOD THEN DO:
                           DELETE utsokaonr. 
                        END.
                     END.
                  END.
               END.            
            END.
         END.
      END.      
      IF AVAILABLE utsokaonr THEN DO:
         IF uppvaltemp.ARBANSVARIG NE "ALLA" THEN DO:
            IF utsokaonr.ARBANSVARIG NE uppvaltemp.ARBANSVARIG THEN DELETE utsokaonr. 
         END.
      END.
      
      IF AVAILABLE utsokaonr  THEN DO:
         IF AVAILABLE valsoktemp  THEN DO:
            IF Guru.Konstanter:globforetag = "SUND" THEN DO:         
               IF valsoktemp.SOKINT[5] = ? OR  valsoktemp.SOKINT[5] = 0 OR valsoktemp.SOKINT[5] = 15 THEN.
               ELSE IF LENGTH(utsokaonr.AONR) = 3 THEN DO:
                  DELETE utsokaonr.      
               END.               
            END.
         END.   
      END.   
