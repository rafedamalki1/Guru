/*PROJRVISAPP.p*/
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
{EXTRADATA.I}
/*{EPERSTEMP.I}*/
{DIRDEF.I}
{VISUTSOKAONR.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{TIDSLAGTEMP.I}
{AOTIDKOPP.I}

/*DEFINE  TEMP-TABLE avtaltemp NO-UNDO
   FIELD AVTALNAMN AS CHARACTER
   FIELD AVTALTYP AS CHARACTER
   FIELD TYP AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX TYP IS PRIMARY TYP ORDNING.*/
CREATE WIDGET-POOL "DynTablepr" NO-ERROR.

FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.   
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).

PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "DynTablepr".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.

PROCEDURE hamtpliggare_UI:
   
   DEFINE OUTPUT PARAMETER TABLE FOR eplutsokaonr .
   
   DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
   DEFINE VARIABLE gplhuvudbuffh AS HANDLE NO-UNDO.   
   DEFINE VARIABLE qString       AS CHARACTER  NO-UNDO.
   
   
   CREATE BUFFER gplhuvudbuffh FOR TABLE "GPLHUVUD" IN WIDGET-POOL "DynTablepr".
   EMPTY TEMP-TABLE eplutsokaonr NO-ERROR. 
  
   qString = "FOR EACH " + gplhuvudbuffh:TABLE .
   RUN CreateCustomQuery(INPUT gplhuvudbuffh,INPUT qString,OUTPUT hQuery).

   /*hQuery:GET-FIRST(NO-LOCK).*/  
   DO WHILE hQuery:QUERY-OFF-END = FALSE:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = gplhuvudbuffh:BUFFER-FIELD("AONRAONR"):BUFFER-VALUE AND AONRTAB.DELNR = gplhuvudbuffh:BUFFER-FIELD("AONRDELNR"):BUFFER-VALUE NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:                        
         CREATE eplutsokaonr.            
         BUFFER-COPY AONRTAB TO eplutsokaonr.
         ASSIGN 
         eplutsokaonr.PLID = gplhuvudbuffh:BUFFER-FIELD("PLID"):BUFFER-VALUE
         eplutsokaonr.IDNR = gplhuvudbuffh:BUFFER-FIELD("IDNR"):BUFFER-VALUE
         eplutsokaonr.AKTIV = gplhuvudbuffh:BUFFER-FIELD("AKTIV"):BUFFER-VALUE.
         IF eplutsokaonr.AONRAVDATUM = 01/01/91 THEN eplutsokaonr.AONRAVDATUM = ?.
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = eplutsokaonr.OMRADE NO-LOCK NO-ERROR.
         IF AVAILABLE OMRADETAB THEN DO:
            eplutsokaonr.OMRADENAMN = OMRADETAB.NAMN.
         END.
      END.          
      hQuery:GET-NEXT(NO-LOCK).
   END.
   DELETE OBJECT gplhuvudbuffh.
   gplhuvudbuffh = ?.   
END PROCEDURE.   

PROCEDURE hamtprojupp_UI:
   DEFINE INPUT PARAMETER TABLE FOR valdaao .
   DEFINE OUTPUT PARAMETER TABLE for evisutsokaonr .
   
   EMPTY TEMP-TABLE evisutsokaonr NO-ERROR. 
   EMPTY TEMP-TABLE visutsokaonr NO-ERROR.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   
   FOR EACH valdaao:
      CREATE visutsokaonr.
      BUFFER-COPY valdaao EXCEPT valdaao.ARBUPPG valdaao.ANM  TO visutsokaonr .
      ASSIGN 
      visutsokaonr.ARBUPPG = valdaao.ARBUPPG[1] 
      visutsokaonr.ANM = valdaao.ANM[1].
      visutsokaonr.ARBUPPG = REPLACE(visutsokaonr.ARBUPPG,CHR(10)," ").
      visutsokaonr.ARBUPPG = REPLACE(visutsokaonr.ARBUPPG,CHR(13)," ").
      visutsokaonr.ARBUPPG = REPLACE(visutsokaonr.ARBUPPG,CHR(9)," ").
      visutsokaonr.ANM = REPLACE(visutsokaonr.ANM,CHR(10)," ").
      visutsokaonr.ANM = REPLACE(visutsokaonr.ANM,CHR(13)," ").
      visutsokaonr.ANM = REPLACE(visutsokaonr.ANM,CHR(9)," ").
   END.
           
   FOR EACH visutsokaonr:
      IF visutsokaonr.AONRAVDATUM = 01/01/91 THEN visutsokaonr.AONRAVDATUM = ?.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = visutsokaonr.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN DO:
         visutsokaonr.OMRADENAMN = OMRADETAB.NAMN.
      END.
      FIND FIRST BESTTAB WHERE BESTTAB.BESTID = visutsokaonr.BESTID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BESTTAB THEN DO:
         visutsokaonr.BESTNAMN = visutsokaonr.OMRADENAMN.
      END.
      ELSE visutsokaonr.BESTNAMN = BESTTAB.BESTNAMN.
      FIND FIRST ARBETSART WHERE ARBETSART.ARBARTKOD =  visutsokaonr.ARBARTKOD NO-LOCK NO-ERROR.
      IF AVAILABLE ARBETSART THEN DO:
         visutsokaonr.ARBARTKODNAMN = ARBETSART.ARBBENAMNING.
      END.
            
      FIND FIRST PRIORITET WHERE PRIORITET.PKOD =  visutsokaonr.PKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PRIORITET THEN DO:
         visutsokaonr.PKODNAMN = PRIORITET.P-BENAMNING.
      END.

      
      FIND FIRST ANLAGGNING WHERE ANLAGGNING.ANLNR =  visutsokaonr.ANLNR NO-LOCK NO-ERROR.
      IF AVAILABLE ANLAGGNING THEN DO:
         visutsokaonr.ANLNRBENAMNING = ANLAGGNING.BENAMNING.
      END.
      
      FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD =  visutsokaonr.PROJEKTOR NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         visutsokaonr.PROJEKTOR = visutsokaonr.PROJEKTOR + "-" + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
      FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD =  visutsokaonr.BEREDARE NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         visutsokaonr.BEREDARE = visutsokaonr.BEREDARE + "-" + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
      FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD =  visutsokaonr.ARBANSVARIG NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         visutsokaonr.ARBANSVARIG = visutsokaonr.ARBANSVARIG + "-" + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.   
      
      FIND AONRTIDLAGE WHERE AONRTIDLAGE.AONR = visutsokaonr.AONR AND AONRTIDLAGE.DELNR = visutsokaonr.DELNR AND AONRTIDLAGE.IDTIDLAG = "AOUPPLAGT" NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTIDLAGE THEN DO:
         visutsokaonr.AOUPPLAGT = AONRTIDLAGE.DATUM1.
      END.   
         
                       
      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "AOREF"                   
      inextradatatemp.HUVUDCH = visutsokaonr.AONR              
      inextradatatemp.HUVUDINT =  visutsokaonr.DELNR.
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:
         ASSIGN
         visutsokaonr.REFKONTAKT = extradatatemp.SOKCHAR[5]
         visutsokaonr.REF = extradatatemp.SOKCHAR[1].
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + visutsokaonr.REF + "," + visutsokaonr.REF.

      END.  
   END.    
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?. 
     
   
   IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
      FOR EACH visutsokaonr,
      EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = visutsokaonr.AONR AND AONRKONTKOD.DELNR = visutsokaonr.DELNR NO-LOCK.
         CREATE evisutsokaonr.
         BUFFER-COPY visutsokaonr TO evisutsokaonr.
         assign
         evisutsokaonr.k1 = AONRKONTKOD.K1
         evisutsokaonr.k2 = AONRKONTKOD.K2
         evisutsokaonr.k3 = AONRKONTKOD.K3
         evisutsokaonr.k4 = AONRKONTKOD.K4
         evisutsokaonr.k5 = AONRKONTKOD.K5
         evisutsokaonr.sats% = AONRKONTKOD.SATS%. 
      END.
   END.
   ELSE DO:
      FOR EACH visutsokaonr:
         CREATE evisutsokaonr.
         BUFFER-COPY visutsokaonr TO evisutsokaonr.
      END.   
   END.   
   
  {GDPRLOGGCLIENT.I}      
END PROCEDURE.

PROCEDURE hamtprojupptidl_UI:
   DEFINE INPUT PARAMETER TABLE FOR valdaao .
   DEFINE INPUT PARAMETER TABLE FOR vtidslagtemp .
   DEFINE INPUT PARAMETER TABLE FOR vejtidslagtemp .
   DEFINE OUTPUT PARAMETER TABLE for evisutsokaonr .
   
   EMPTY TEMP-TABLE evisutsokaonr NO-ERROR. 
   EMPTY TEMP-TABLE visutsokaonr NO-ERROR.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   
   IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   
   FOR EACH valdaao :
      ASSIGN valdaao.UTRYCKNING = FALSE.
   END.
   
   FOR EACH valdaao,
   EACH AONRTIDLAGE WHERE AONRTIDLAGE.AONR = valdaao.AONR AND AONRTIDLAGE.DELNR = valdaao.DELNR:      
      FIND FIRST vtidslagtemp WHERE vtidslagtemp.IDTIDLAG = AONRTIDLAGE.IDTIDLAG NO-LOCK NO-ERROR.
      IF AVAILABLE vtidslagtemp THEN DO:
         ASSIGN valdaao.UTRYCKNING = TRUE.
      END.   
   END.
   FIND FIRST vtidslagtemp NO-LOCK NO-ERROR.
   IF AVAILABLE vtidslagtemp THEN DO:
      FOR EACH valdaao WHERE valdaao.UTRYCKNING = FALSE:
         DELETE valdaao.
      END.
   END.
   FOR EACH valdaao :
      ASSIGN valdaao.UTRYCKNING = FALSE.
   END.
   
   FOR EACH valdaao,
   EACH AONRTIDLAGE WHERE AONRTIDLAGE.AONR = valdaao.AONR AND AONRTIDLAGE.DELNR = valdaao.DELNR:      
      FIND FIRST vejtidslagtemp WHERE vejtidslagtemp.IDTIDLAG = AONRTIDLAGE.IDTIDLAG NO-LOCK NO-ERROR.
      IF AVAILABLE vejtidslagtemp THEN DO:
         ASSIGN valdaao.UTRYCKNING = TRUE.
      END.   
   END.
   FOR EACH valdaao WHERE valdaao.UTRYCKNING = TRUE:
      DELETE valdaao.
   END.
         
   FOR EACH valdaao:      
      CREATE visutsokaonr.
      BUFFER-COPY valdaao EXCEPT valdaao.ARBUPPG valdaao.ANM  TO visutsokaonr .
      ASSIGN 
      visutsokaonr.ARBUPPG = valdaao.ARBUPPG[1] 
      visutsokaonr.ANM = valdaao.ANM[1].
      visutsokaonr.ARBUPPG = REPLACE(visutsokaonr.ARBUPPG,CHR(10)," ").
      visutsokaonr.ARBUPPG = REPLACE(visutsokaonr.ARBUPPG,CHR(13)," ").
      visutsokaonr.ARBUPPG = REPLACE(visutsokaonr.ARBUPPG,CHR(9)," ").
      visutsokaonr.ANM = REPLACE(visutsokaonr.ANM,CHR(10)," ").
      visutsokaonr.ANM = REPLACE(visutsokaonr.ANM,CHR(13)," ").
      visutsokaonr.ANM = REPLACE(visutsokaonr.ANM,CHR(9)," ").
   END.
           
   FOR EACH visutsokaonr:
      IF visutsokaonr.AONRAVDATUM = 01/01/91 THEN visutsokaonr.AONRAVDATUM = ?.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = visutsokaonr.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN DO:
         visutsokaonr.OMRADENAMN = OMRADETAB.NAMN.
      END.
      FIND FIRST BESTTAB WHERE BESTTAB.BESTID = visutsokaonr.BESTID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BESTTAB THEN DO:
         visutsokaonr.BESTNAMN = visutsokaonr.OMRADENAMN.
      END.
      ELSE visutsokaonr.BESTNAMN = BESTTAB.BESTNAMN.
      FIND FIRST ARBETSART WHERE ARBETSART.ARBARTKOD =  visutsokaonr.ARBARTKOD NO-LOCK NO-ERROR.
      IF AVAILABLE ARBETSART THEN DO:
         visutsokaonr.ARBARTKODNAMN = ARBETSART.ARBBENAMNING.
      END.
            
      FIND FIRST PRIORITET WHERE PRIORITET.PKOD =  visutsokaonr.PKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PRIORITET THEN DO:
         visutsokaonr.PKODNAMN = PRIORITET.P-BENAMNING.
      END.
      FIND FIRST ANLAGGNING WHERE ANLAGGNING.ANLNR =  visutsokaonr.ANLNR NO-LOCK NO-ERROR.
      IF AVAILABLE ANLAGGNING THEN DO:
         visutsokaonr.ANLNRBENAMNING = ANLAGGNING.BENAMNING.
      END.
      
      FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD =  visutsokaonr.PROJEKTOR NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         visutsokaonr.PROJEKTOR = visutsokaonr.PROJEKTOR + "-" + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
      FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD =  visutsokaonr.BEREDARE NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         visutsokaonr.BEREDARE = visutsokaonr.BEREDARE + "-" + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
      FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD =  visutsokaonr.ARBANSVARIG NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         visutsokaonr.ARBANSVARIG = visutsokaonr.ARBANSVARIG + "-" + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.   
      
      FIND AONRTIDLAGE WHERE AONRTIDLAGE.AONR = visutsokaonr.AONR AND AONRTIDLAGE.DELNR = visutsokaonr.DELNR AND AONRTIDLAGE.IDTIDLAG = "AOUPPLAGT" NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTIDLAGE THEN DO:
         visutsokaonr.AOUPPLAGT = AONRTIDLAGE.DATUM1.
      END.   
         
                       
      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "AOREF"                   
      inextradatatemp.HUVUDCH = visutsokaonr.AONR              
      inextradatatemp.HUVUDINT =  visutsokaonr.DELNR.
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:
         ASSIGN
         visutsokaonr.REFKONTAKT = extradatatemp.SOKCHAR[5]
         visutsokaonr.REF = extradatatemp.SOKCHAR[1].
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + visutsokaonr.REF + "," + visutsokaonr.REF.
      END.  
   END.    
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?. 

   
         
   
   FOR EACH visutsokaonr,
   EACH AONRTIDLAGE WHERE AONRTIDLAGE.AONR = visutsokaonr.AONR AND AONRTIDLAGE.DELNR = visutsokaonr.DELNR NO-LOCK:
      FIND FIRST vtidslagtemp NO-LOCK NO-ERROR.
      IF AVAILABLE vtidslagtemp THEN DO:
         FIND FIRST vtidslagtemp WHERE vtidslagtemp.IDTIDLAG = AONRTIDLAGE.IDTIDLAG NO-LOCK NO-ERROR.
         IF AVAILABLE vtidslagtemp THEN DO:           
            CREATE evisutsokaonr.
            BUFFER-COPY visutsokaonr TO evisutsokaonr.
            FIND FIRST TIDSLAGEN WHERE TIDSLAGEN.IDTIDLAG = AONRTIDLAGE.IDTIDLAG  NO-LOCK NO-ERROR.
            IF AVAILABLE TIDSLAGEN THEN DO:
               ASSIGN  
               evisutsokaonr.DAT1 = AONRTIDLAGE.DATUM1
               evisutsokaonr.DAT2 = AONRTIDLAGE.DATUM2
               evisutsokaonr.ANVANDARE1 = AONRTIDLAGE.ANVANDARE1
               evisutsokaonr.ANVANDARE2 = AONRTIDLAGE.ANVANDARE2
               evisutsokaonr.AKTIVITET1 = TIDSLAGEN.AKTIVITET1
               evisutsokaonr.AKTIVITET2 = TIDSLAGEN.AKTIVITET2
               evisutsokaonr.IDTIDLAG = AONRTIDLAGE.IDTIDLAG
               evisutsokaonr.TIDLAGE = TIDSLAGEN.TIDLAGE.
            END.
         END.
            
      END.
      ELSE DO:
         FIND FIRST vejtidslagtemp NO-LOCK NO-ERROR.
         IF AVAILABLE vejtidslagtemp THEN DO:
            FIND FIRST evisutsokaonr  WHERE evisutsokaonr.AONR = visutsokaonr.AONR AND evisutsokaonr.DELNR = visutsokaonr.DELNR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE evisutsokaonr THEN DO:            
               CREATE evisutsokaonr.
               BUFFER-COPY visutsokaonr TO evisutsokaonr.
            END.   
         END.      
        
      END.          
   END.
   {GDPRLOGGCLIENT.I}
   
   
   /*FOR EACH visutsokaonr:
      CREATE evisutsokaonr.
      BUFFER-COPY visutsokaonr TO evisutsokaonr.
   END.*/        
END PROCEDURE.

PROCEDURE hamtprojuppalltidl_UI:
   DEFINE INPUT PARAMETER TABLE FOR valdaao .   
   DEFINE OUTPUT PARAMETER TABLE for visutsokaonr .
    
   
   EMPTY TEMP-TABLE visutsokaonr NO-ERROR. 
   OPEN QUERY atq
   FOR EACH valdaao,
   EACH AONRTIDLAGE WHERE AONRTIDLAGE.AONR = valdaao.AONR AND AONRTIDLAGE.DELNR = valdaao.DELNR NO-LOCK, 
   EACH TIDSLAGEN WHERE TIDSLAGEN.IDTIDLAG = AONRTIDLAGE.IDTIDLAG NO-LOCK.
   GET FIRST atq NO-LOCK.
   DO WHILE AVAILABLE(valdaao):
      CREATE visutsokaonr.
      ASSIGN 
      visutsokaonr.AONR       = AONRTIDLAGE.AONR
      visutsokaonr.DELNR      = AONRTIDLAGE.DELNR
      visutsokaonr.ORT       = valdaao.ORT
      visutsokaonr.OMRADE       = valdaao.OMRADE            
      visutsokaonr.DAT1     = AONRTIDLAGE.DATUM1
      visutsokaonr.DAT2     = AONRTIDLAGE.DATUM2
      visutsokaonr.IDTIDLAG   = AONRTIDLAGE.IDTIDLAG
      visutsokaonr.TIDLAGE   = TIDSLAGEN.TIDLAGE
      visutsokaonr.AKTIVITET1 = TIDSLAGEN.AKTIVITET1
      visutsokaonr.AKTIVITET2 = TIDSLAGEN.AKTIVITET2
      visutsokaonr.ANVANDARE1 = AONRTIDLAGE.ANVANDARE1
      visutsokaonr.ANVANDARE2 = AONRTIDLAGE.ANVANDARE2
      visutsokaonr.AONRTIDLREC  = RECID(AONRTIDLAGE).
      BUFFER-COPY TIDSLAGEN TO visutsokaonr.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = visutsokaonr.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE OMRADETAB THEN DO:
         visutsokaonr.OMRADENAMN = OMRADETAB.NAMN.
      END.
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + visutsokaonr.ANVANDARE1 + "," + visutsokaonr.ANVANDARE2.
      GET NEXT atq NO-LOCK.
   END.
   CLOSE QUERY atq.
   {GDPRLOGGCLIENT.I}
END PROCEDURE.


PROCEDURE hamtuppg_UI:   
   DEFINE OUTPUT PARAMETER pkodf AS LOGICAL.
   DEFINE OUTPUT PARAMETER arbartkodf AS LOGICAL.
   DEFINE OUTPUT PARAMETER anlnrf AS LOGICAL.
   EMPTY TEMP-TABLE evisutsokaonr NO-ERROR. 
   EMPTY TEMP-TABLE visutsokaonr NO-ERROR.
   
   FIND FIRST PRIORITET  NO-LOCK NO-ERROR.
   IF AVAILABLE PRIORITET THEN pkodf = TRUE.
   ELSE pkodf = FALSE.
   FIND FIRST ARBETSART  NO-LOCK NO-ERROR.
   IF AVAILABLE ARBETSART THEN arbartkodf = TRUE.
   ELSE arbartkodf = FALSE.
   
   FIND FIRST ANLAGGNING  NO-LOCK NO-ERROR.
   IF AVAILABLE ANLAGGNING THEN anlnrf = TRUE.
   ELSE anlnrf = FALSE.
END PROCEDURE.

PROCEDURE hamttidl_UI:
   
   DEFINE OUTPUT PARAMETER TABLE for tidslagtemp .
   EMPTY TEMP-TABLE tidslagtemp NO-ERROR.
       
   OPEN QUERY tq FOR EACH TIDSLAGEN  NO-LOCK.
   GET FIRST tq NO-LOCK.
   DO WHILE AVAILABLE(TIDSLAGEN):
      CREATE tidslagtemp.
      BUFFER-COPY TIDSLAGEN TO tidslagtemp.   
      GET NEXT tq NO-LOCK.
   END.
   CLOSE QUERY tq.
END PROCEDURE.

PROCEDURE avsProjVis_UI :
   DELETE WIDGET-POOL "DynTablepr" NO-ERROR.
END PROCEDURE.