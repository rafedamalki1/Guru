/*AOHMTALLT.P*/
{BOLAGSEKSTART.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED

{GLOBVAR2DEL1.I}
DEFINE QUERY aq FOR AONRTAB.
DEFINE QUERY atidlq FOR AONRTIDLAGE,AONRTAB.
DEFINE VARIABLE utkonto AS CHARACTER NO-UNDO.
{DIRDEF.I}

/*{GLOBVAR2DEL1.I}*/
{SOKDEF.I}
DEFINE INPUT PARAMETER SEL_UPP AS CHARACTER NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR utsokaonr.   
DEFINE INPUT PARAMETER TABLE FOR valsoktemp.   
EMPTY TEMP-TABLE utsokaonr NO-ERROR. 
           
DEFINE VARIABLE valbestomr AS CHARACTER NO-UNDO. 
DEFINE BUFFER ormbuff FOR OMRADETAB.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.   
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
/*Guru.Konstanter:globforetag = FORETAG.FORETAG.*/
EMPTY TEMP-TABLE utsokaonr NO-ERROR. 
FIND FIRST uppvaltemp NO-ERROR.
IF uppvaltemp.AVDNR NE "ALLA" THEN DO:
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = INTEGER(uppvaltemp.AVDNR) NO-LOCK NO-ERROR. 
END.
IF uppvaltemp.OMRADE NE "ALLA" THEN DO:
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = uppvaltemp.OMRADE NO-LOCK NO-ERROR. 
END.
IF uppvaltemp.BESTID NE "ALLA" THEN DO:

   FIND FIRST BESTTAB WHERE BESTTAB.BESTID = uppvaltemp.BESTID NO-LOCK NO-ERROR.    
   IF NOT AVAILABLE BESTTAB THEN DO:
      FIND FIRST ormbuff WHERE ormbuff.OMRADE = uppvaltemp.BESTID NO-LOCK NO-ERROR.
      valbestomr = ormbuff.OMRADE.
   END.   
   ELSE valbestomr = BESTTAB.BESTID.
END.

IF uppvaltemp.TILLFALLFAST = 1 THEN DO:      
   /*ALLA TILLFÄLIGA*/
   IF uppvaltemp.PAAV = 1 THEN DO: 
      /*ALLA PÅGÅENDE*/
      RUN allpa_UI (INPUT FALSE).
      RUN skapaao_UI.
      RUN allpav_UI (INPUT FALSE).
      RUN skapaao_UI.
   END.
   ELSE IF uppvaltemp.PAAV = 2 THEN DO:      
      /*ALLA AVSLUTADE*/
      RUN allav_UI (INPUT FALSE).
      RUN skapaao_UI.
   END.   
   ELSE IF uppvaltemp.PAAV = 3 THEN DO:
      /*ALLA*/
      RUN allpa_UI (INPUT FALSE).
      RUN skapaao_UI.
      RUN allav_UI (INPUT FALSE). 
      RUN skapaao_UI.
   END.
   ELSE IF uppvaltemp.PAAV = 4 THEN DO:
      /*ALLA*/
      RUN alltidlage_UI (INPUT FALSE,INPUT "AOUPPLAGT"). 
      RUN skapaaotidl_UI.
   END.
END.
ELSE IF uppvaltemp.TILLFALLFAST = 2 THEN DO:      
   /*ALLA FASTA*/
   IF uppvaltemp.PAAV = 1 THEN DO: 
      /*ALLA PÅGÅENDE*/
      RUN allpa_UI (INPUT TRUE).
      RUN skapaao_UI.
      RUN allpav_UI (INPUT TRUE).
      RUN skapaao_UI.
   END.
   ELSE IF uppvaltemp.PAAV = 2 THEN DO:      
      /*ALLA AVSLUTADE*/
      RUN allav_UI (INPUT TRUE).
      RUN skapaao_UI.
   END.   
   ELSE IF uppvaltemp.PAAV = 3 THEN DO:
      /*ALLA*/
      RUN allpa_UI (INPUT TRUE).
      RUN skapaao_UI.
      RUN allav_UI (INPUT TRUE). 
      RUN skapaao_UI.
   END.
   ELSE IF uppvaltemp.PAAV = 4 THEN DO:
      /*ALLA*/
      RUN alltidlage_UI (INPUT TRUE,INPUT "AOUPPLAGT"). 
      RUN skapaaotidl_UI.
   END.
END.
ELSE IF uppvaltemp.TILLFALLFAST = 3 THEN DO:      
   /*ALLA*/
   IF uppvaltemp.PAAV = 1 THEN DO: 
      /*ALLA PÅGÅENDE*/
      RUN allpa_UI (INPUT FALSE).
      RUN skapaao_UI.      
      RUN allpa_UI (INPUT TRUE).
      RUN skapaao_UI.
      RUN allpav_UI (INPUT FALSE).
      RUN skapaao_UI.      
      RUN allpav_UI (INPUT TRUE).
      RUN skapaao_UI.     
   END.
   ELSE IF uppvaltemp.PAAV = 2 THEN DO:      
      /*ALLA AVSLUTADE*/
      RUN allav_UI (INPUT FALSE).
      RUN skapaao_UI.
      RUN allav_UI (INPUT TRUE).
      RUN skapaao_UI.
   END.   
   ELSE IF uppvaltemp.PAAV = 3 THEN DO:
      /*ALLA*/
      RUN allpa_UI (INPUT FALSE).
      RUN skapaao_UI.
      RUN allav_UI (INPUT FALSE). 
      RUN skapaao_UI.
      RUN allpa_UI (INPUT TRUE).
      RUN skapaao_UI.
      RUN allav_UI (INPUT TRUE). 
      RUN skapaao_UI.
   END.
   ELSE IF uppvaltemp.PAAV = 4 THEN DO:
      /*ALLA*/
      RUN alltidlage_UI (INPUT FALSE,INPUT "AOUPPLAGT"). 
      RUN skapaaotidl_UI.
      RUN alltidlage_UI (INPUT TRUE,INPUT "AOUPPLAGT"). 
      RUN skapaaotidl_UI.
   END.
END.
RUN avdkoll_UI.
IF uppvaltemp.ANVANDARE NE "" THEN RUN aonrsekkoll_UI (INPUT 1).
PROCEDURE aonrsekkoll_UI :
  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF Guru.Konstanter:varforetypchar[4] = "" AND Guru.Konstanter:varforetypval[18] = 0 THEN DO:
      RETURN.
   END.
   IF uppvaltemp.ANVANDARE = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN RETURN.
   IF Guru.Konstanter:varforetypchar[4] = "ja" THEN DO:
      IF vad = 1 THEN DO:
         FOR EACH utsokaonr:
            IF utsokaonr.OMRADE = "" THEN.
            ELSE DO:
               FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = uppvaltemp.ANVANDARE AND 
               OFFERT.OMRADE = utsokaonr.OMRADE NO-LOCK NO-ERROR.
               IF NOT AVAILABLE OFFERT THEN DO:
                  FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = uppvaltemp.ANVANDARE AND 
                  OFFERT.AONR = utsokaonr.AONR AND OFFERT.DELNR = utsokaonr.DELNR
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE OFFERT THEN DO:
                     DELETE utsokaonr.
                  END.
               END.
            END.   
         END.
      END.
      IF vad = 2 THEN DO:
         FOR EACH evaldaao:
            IF evaldaao.OMRADE = "" THEN.
            ELSE DO: 
               FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = uppvaltemp.ANVANDARE AND 
               OFFERT.OMRADE = evaldaao.OMRADE NO-LOCK NO-ERROR.
               IF NOT AVAILABLE OFFERT THEN DO:
                  FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = uppvaltemp.ANVANDARE AND 
                  OFFERT.AONR = evaldaao.AONR AND OFFERT.DELNR = evaldaao.DELNR
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE OFFERT THEN DO:
                     DELETE evaldaao.
                  END.
               END.
            END.   
         END.
      END.
      RETURN.
   END.
   
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:
      IF vad = 1 THEN DO:
         FOR EACH utsokaonr:
            FIND FIRST omvtemp WHERE omvtemp.OMRADE = utsokaonr.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omvtemp THEN DO:
               FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE = uppvaltemp.ANVANDARE NO-LOCK NO-ERROR. 
               IF NOT AVAILABLE BOLAGSEK THEN DELETE utsokaonr.               
            END.
            ELSE DO:
               IF utsokaonr.OMRADE NE "" THEN DELETE utsokaonr.  
            END.
         END.
      END.
      IF vad = 2 THEN DO:
         FOR EACH evaldaao:
            FIND FIRST omvtemp WHERE omvtemp.OMRADE = evaldaao.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omvtemp THEN DO:
               FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE = uppvaltemp.ANVANDARE NO-LOCK NO-ERROR. 
               IF NOT AVAILABLE BOLAGSEK THEN DELETE evaldaao.               
            END.
            ELSE DO:
               IF evaldaao.OMRADE NE "" THEN DELETE evaldaao.  
            END.
         END.
      END.
      RETURN.
   END.
   
END PROCEDURE.
PROCEDURE avdkoll_UI.
   /*IF uppvaltemp.OMRADE NE "ALLA" THEN RETURN.*/
   IF uppvaltemp.AVDNR = "ALLA" THEN RETURN.
   RELEASE utsokaonr NO-ERROR.
   OPEN QUERY omrq FOR EACH OMRADETAB WHERE OMRADETAB.AVDELNINGNR = AVDELNING.AVDELNINGNR NO-LOCK,
   EACH utsokaonr WHERE utsokaonr.OMRADE = OMRADETAB.OMRADE NO-LOCK.
   GET FIRST omrq NO-LOCK.
   DO WHILE AVAILABLE(utsokaonr):
      utsokaonr.TABORT = FALSE.
      GET NEXT omrq NO-LOCK.
   END.
   FOR EACH utsokaonr WHERE utsokaonr.tabort = TRUE:
      DELETE utsokaonr.
   END.
END PROCEDURE.
PROCEDURE allpa_UI.
   DEFINE INPUT PARAMETER fastaonr AS LOGICAL NO-UNDO.
   IF uppvaltemp.BESTID = "ALLA" THEN DO:
      IF uppvaltemp.FAKTTYP = "ALLA" THEN DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE             
         AONRTAB.FASTAAONR = fastaonr AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 
         NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE             
         AONRTAB.FASTAAONR = fastaonr AND 
         AONRTAB.AONRAVDATUM = 01/01/1991 AND
         AONRTAB.FAKTTYP = uppvaltemp.FAKTTYP
         NO-LOCK.
      END.
   END.
   ELSE DO:      
      IF uppvaltemp.FAKTTYP = "ALLA" THEN DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE 
         AONRTAB.FASTAAONR = fastaonr AND
         AONRTAB.AONRAVDATUM = 01/01/1991 AND
         AONRTAB.BESTID = valbestomr                           
         NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE 
         AONRTAB.FASTAAONR = fastaonr AND
         AONRTAB.AONRAVDATUM = 01/01/1991 AND
         AONRTAB.BESTID = valbestomr AND                        
         AONRTAB.FAKTTYP = uppvaltemp.FAKTTYP
         NO-LOCK.
      END.
   END.  
END PROCEDURE.
PROCEDURE allpav_UI.
   DEFINE INPUT PARAMETER fastaonr AS LOGICAL NO-UNDO.
   IF uppvaltemp.BESTID = "ALLA" THEN DO:
      IF uppvaltemp.FAKTTYP = "ALLA" THEN DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE             
         AONRTAB.FASTAAONR = fastaonr AND 
         AONRTAB.AONRAVDATUM > TODAY 
         NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE             
         AONRTAB.FASTAAONR = fastaonr AND 
         AONRTAB.AONRAVDATUM > TODAY AND
         AONRTAB.FAKTTYP = uppvaltemp.FAKTTYP
         NO-LOCK.
      END.
   END.
   ELSE DO:      
      IF uppvaltemp.FAKTTYP = "ALLA" THEN DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE 
         AONRTAB.FASTAAONR = fastaonr AND
         AONRTAB.AONRAVDATUM > TODAY AND
         AONRTAB.BESTID = valbestomr                           
         NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE 
         AONRTAB.FASTAAONR = fastaonr AND
         AONRTAB.AONRAVDATUM > TODAY AND
         AONRTAB.BESTID = valbestomr AND                        
         AONRTAB.FAKTTYP = uppvaltemp.FAKTTYP
         NO-LOCK.
      END.
   END.  
END PROCEDURE.
PROCEDURE alltidlage_UI :
   DEFINE INPUT PARAMETER fastaonr AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER tlagevar AS CHARACTER NO-UNDO.
   
   IF uppvaltemp.BESTID = "ALLA" THEN DO:
      IF uppvaltemp.FAKTTYP = "ALLA" THEN DO:
         OPEN QUERY atidlq FOR EACH AONRTIDLAGE WHERE AONRTIDLAGE.IDTIDLAG = tlagevar AND AONRTIDLAGE.DATUM1 >= uppvaltemp.AVSLUTSTART AND AONRTIDLAGE.DATUM1 <= uppvaltemp.AVSLUTSLUT NO-LOCK, 
         EACH AONRTAB WHERE   
         AONRTAB.AONR = AONRTIDLAGE.AONR AND
         AONRTAB.DELNR = AONRTIDLAGE.DELNR AND          
         AONRTAB.FASTAAONR = fastaonr           
         NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY atidlq FOR EACH AONRTIDLAGE WHERE AONRTIDLAGE.IDTIDLAG = tlagevar AND AONRTIDLAGE.DATUM1 >= uppvaltemp.AVSLUTSTART AND AONRTIDLAGE.DATUM1 <= uppvaltemp.AVSLUTSLUT NO-LOCK, 
         EACH AONRTAB WHERE             
         AONRTAB.AONR = AONRTIDLAGE.AONR AND
         AONRTAB.DELNR = AONRTIDLAGE.DELNR AND
         AONRTAB.FASTAAONR = fastaonr AND 
         AONRTAB.FAKTTYP = uppvaltemp.FAKTTYP
         NO-LOCK.
      END.
   END.
   ELSE DO:      
      IF uppvaltemp.FAKTTYP = "ALLA" THEN DO:
         OPEN QUERY atidlq FOR EACH AONRTIDLAGE WHERE AONRTIDLAGE.IDTIDLAG = tlagevar AND AONRTIDLAGE.DATUM1 >= uppvaltemp.AVSLUTSTART AND AONRTIDLAGE.DATUM1 <= uppvaltemp.AVSLUTSLUT NO-LOCK, 
         EACH AONRTAB WHERE 
         AONRTAB.AONR = AONRTIDLAGE.AONR AND
         AONRTAB.DELNR = AONRTIDLAGE.DELNR AND
         AONRTAB.FASTAAONR = fastaonr AND
         AONRTAB.BESTID = valbestomr                           
         NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY atidlq FOR EACH AONRTIDLAGE WHERE AONRTIDLAGE.IDTIDLAG = tlagevar AND AONRTIDLAGE.DATUM1 >= uppvaltemp.AVSLUTSTART AND AONRTIDLAGE.DATUM1 <= uppvaltemp.AVSLUTSLUT NO-LOCK, 
         EACH AONRTAB WHERE 
         AONRTAB.AONR = AONRTIDLAGE.AONR AND
         AONRTAB.DELNR = AONRTIDLAGE.DELNR AND
         AONRTAB.FASTAAONR = fastaonr AND
         AONRTAB.BESTID = valbestomr AND                        
         AONRTAB.FAKTTYP = uppvaltemp.FAKTTYP
         NO-LOCK.
      END.
   END.
END PROCEDURE.
PROCEDURE allav_UI.
   DEFINE INPUT PARAMETER fastaonr AS LOGICAL NO-UNDO. 
   IF uppvaltemp.BESTID = "ALLA" THEN DO:
      IF uppvaltemp.FAKTTYP = "ALLA" THEN DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE             
         AONRTAB.FASTAAONR = fastaonr AND 
         AONRTAB.AONRAVDATUM >= uppvaltemp.AVSLUTSTART AND
         AONRTAB.AONRAVDATUM <= uppvaltemp.AVSLUTSLUT  
         NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE             
         AONRTAB.FASTAAONR = fastaonr AND 
         AONRTAB.AONRAVDATUM >= uppvaltemp.AVSLUTSTART AND
         AONRTAB.AONRAVDATUM <= uppvaltemp.AVSLUTSLUT AND
         AONRTAB.FAKTTYP = uppvaltemp.FAKTTYP
         NO-LOCK.
      END.
   END.
   ELSE DO:
      IF uppvaltemp.FAKTTYP = "ALLA" THEN DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE 
         AONRTAB.FASTAAONR = fastaonr AND            
         AONRTAB.AONRAVDATUM >= uppvaltemp.AVSLUTSTART AND
         AONRTAB.AONRAVDATUM <= uppvaltemp.AVSLUTSLUT AND
         AONRTAB.BESTID = valbestomr                           
         NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY aq FOR EACH AONRTAB WHERE 
         AONRTAB.FASTAAONR = fastaonr AND             
         AONRTAB.AONRAVDATUM >= uppvaltemp.AVSLUTSTART AND
         AONRTAB.AONRAVDATUM <= uppvaltemp.AVSLUTSLUT AND
         AONRTAB.BESTID = valbestomr AND                        
         AONRTAB.FAKTTYP = uppvaltemp.FAKTTYP
         NO-LOCK.
      END.
   END.
END PROCEDURE.
PROCEDURE skapaao_UI:
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):
      {AOHMT.I}
      
      GET NEXT aq NO-LOCK.
   END.
END PROCEDURE.   
PROCEDURE skapaaotidl_UI:
   GET FIRST atidlq NO-LOCK.
   DO WHILE AVAILABLE(AONRTAB):         
      {AOHMT.I}
      GET NEXT atidlq NO-LOCK.
   END.
END PROCEDURE.           
