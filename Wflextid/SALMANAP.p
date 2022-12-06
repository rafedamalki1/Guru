/*SALMANAP.P*/
{TIDUTTTNEW.I}
&Scoped-define NEW NEW
{TIDPERS.I}
{REGVAR.I}
DEFINE TEMP-TABLE fsal
   FIELD PERSONALKOD AS CHARACTER  
   FIELD FORNAMN AS CHARACTER 
   FIELD EFTERNAMN AS CHARACTER 
   FIELD TOTALT AS DECIMAL 
   FIELD AONR AS CHARACTER
   FIELD PRISTYP AS CHARACTER
   FIELD OVERTIDUTTAG AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.

DEFINE TEMP-TABLE flsal
   FIELD PERSONALKOD AS CHARACTER  
   FIELD FORNAMN AS CHARACTER 
   FIELD EFTERNAMN AS CHARACTER 
   FIELD TOTALT AS DECIMAL 
   FIELD AONR AS CHARACTER
   FIELD PRISTYP AS CHARACTER  
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.
   
DEFINE TEMP-TABLE flexsal
   FIELD PERSONALKOD AS CHARACTER  
   FIELD FORNAMN AS CHARACTER 
   FIELD EFTERNAMN AS CHARACTER 
   FIELD FLEX AS DECIMAL
   FIELD RESTID AS DECIMAL
   FIELD SJUKDOM AS DECIMAL
   FIELD BARN AS DECIMAL
   FIELD SEMESTER AS DECIMAL  
   FIELD TOTALT AS DECIMAL
   FIELD PLUS AS DECIMAL FORMAT "->>9.99"  
   FIELD INSAL AS DECIMAL FORMAT "->>9.99" 
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING.
DEFINE INPUT PARAMETER arnr AS INTEGER FORMAT "9999" NO-UNDO.
DEFINE INPUT PARAMETER manadnamn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER manadnr AS INTEGER FORMAT "99" NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE ingsaldo AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE ftot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE mtot AS DECIMAL FORMAT "-999.99" NO-UNDO.
DEFINE VARIABLE isaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.
DEFINE VARIABLE splus AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.
DEFINE VARIABLE personal AS CHARACTER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE dagnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE mantim AS INTEGER NO-UNDO.
DEFINE VARIABLE antpers AS INTEGER NO-UNDO. 
RUN tolk_UI.
{GDPRLOGGCLIENT.I}
PROCEDURE tolk_UI :   
   RUN skapaflex_UI.  
   RUN huvud_UI.  
   RUN skapaut_UI.                     
END PROCEDURE.

PROCEDURE huvud_UI :
     /*HUVUD*/  
   CREATE tidut. 
   ASSIGN
   SUBSTRING(tidut.UT,4) = "Månadssaldo"
   SUBSTRING(tidut.UT,20) = manadnamn
   SUBSTRING(tidut.UT,35) = STRING(arnr)
   SUBSTRING(tidut.UT,60) = STRING(TODAY)
   SUBSTRING(tidut.UT,70) = STRING(TIME,"HH:MM:SS").
   CREATE tidut. 
   IF manadnr = 99 THEN DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,4) = "Årets arbetstid"
      SUBSTRING(tidut.UT,30) = STRING(mantim)
      SUBSTRING(tidut.UT,37) = "timmar".       
   END.
   ELSE DO:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,4) = "Månadens arbetstid"
      SUBSTRING(tidut.UT,30) = STRING(mantim)
      SUBSTRING(tidut.UT,37) = "timmar". 
   END.
   CREATE tidut.
   CREATE tidut.    
   ASSIGN
   SUBSTRING(tidut.UT,1) = "PERSON"
   SUBSTRING(tidut.UT,9) = "FÖRNAMN"
   SUBSTRING(tidut.UT,19) = "EFTERNAMN"
   SUBSTRING(tidut.UT,37) = "REG TID"
   SUBSTRING(tidut.UT,49) = "RESTID"
   SUBSTRING(tidut.UT,56) = "Avdraget"
   SUBSTRING(tidut.UT,67) = "Ej avdraget"
   SUBSTRING(tidut.UT,79) = "SEMESTER"
   SUBSTRING(tidut.UT,94) = "PLUS".   
   str2 =                                                                                                                            
   "=======.=========.===============.=========.==========.==========.==========.==========.==========.==========.".   
   str =                                                                                                                            
   "=======.=========.===============.=========.==========.==========.==========.==========.==========". 
   IF manadnr = 99 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,101) = "IN SALDO".
      CREATE tidut.                  
      ASSIGN
      SUBSTRING(tidut.UT,1) = str2.   
   END.         
   ELSE DO:
      CREATE tidut.                  
      ASSIGN
      SUBSTRING(tidut.UT,1) = str.   
   END.   
END PROCEDURE.

PROCEDURE skapaut_UI :
   FOR EACH flexsal USE-INDEX PERSONALKOD: 
      CREATE tidut. 
      ASSIGN
      SUBSTRING(tidut.UT,1) = flexsal.PERSONALKOD
      SUBSTRING(tidut.UT,9) = flexsal.FORNAMN
      SUBSTRING(tidut.UT,19) = flexsal.EFTERNAMN
      SUBSTRING(tidut.UT,37) = STRING(flexsal.FLEX,">>>9.99")          
      SUBSTRING(tidut.UT,48) = STRING(flexsal.RESTID,">>>9.99")
      SUBSTRING(tidut.UT,59) = STRING(flexsal.SJUKDOM,">>>9.99")
      SUBSTRING(tidut.UT,70) = STRING(flexsal.BARN,">>>9.99")
      SUBSTRING(tidut.UT,80) = STRING(flexsal.SEMESTER,">>>9.99")
      SUBSTRING(tidut.UT,90) = STRING(flexsal.PLUS,"->>>9.99").
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + flexsal.PERSONALKOD.   
      IF manadnr = 99 THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,101) = STRING(flexsal.INSAL,"->>>9.99").
      END.   
   END.       
   IF manadnr = 99 THEN DO:     
      CREATE tidut. 
      ASSIGN 
      tidut.UT = str2.  
   END.   
   ELSE DO:
      CREATE tidut. 
      ASSIGN 
      tidut.UT = str.  
   END.            
END PROCEDURE.
               
PROCEDURE skapaflex_UI :
   antpers = 0.
   FOR EACH tidpers:
      antpers = antpers + 1.
   END.   
   IF manadnr = 99 THEN DO:   /*hela året*/
      OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
      EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      YEAR(TIDREGITAB.DATUM) = arnr AND TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK.
   END.
   ELSE DO: 
      OPEN QUERY flgam FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK,
      EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
      AND YEAR(TIDREGITAB.DATUM) = arnr AND MONTH(TIDREGITAB.DATUM) = manadnr
      AND TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK.   
   END.   
   GET FIRST flgam NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):        
      CREATE fsal.
      ASSIGN
      fsal.PERSONALKOD = tidpers.PERSONALKOD
      fsal.FORNAMN = tidpers.FORNAMN
      fsal.EFTERNAMN = tidpers.EFTERNAMN
      fsal.TOTALT =  DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),1,2)) +
      (DECIMAL(SUBSTRING(STRING(TIDREGITAB.TOTALT,"99.99"),4,2)) / 60)
      fsal.AONR = TIDREGITAB.AONR
      fsal.PRISTYP = TIDREGITAB.PRISTYP
      fsal.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG.    
      GET NEXT flgam NO-LOCK.
   END.              
   CLOSE QUERY flgam.    
   arrhjsum = 0.   
   FOR EACH fsal BREAK BY fsal.PERSONALKOD BY fsal.AONR BY fsal.PRISTYP:    
      ACCUMULATE fsal.TOTALT(TOTAL BY fsal.PERSONALKOD BY fsal.AONR BY fsal.PRISTYP).                
      IF LAST-OF(fsal.PRISTYP) THEN DO:
         CREATE flsal.                    
         ASSIGN          
         flsal.PERSONALKOD = fsal.PERSONALKOD
         flsal.FORNAMN = fsal.FORNAMN
         flsal.EFTERNAMN = fsal.EFTERNAMN
         flsal.AONR = fsal.AONR
         flsal.PRISTYP = fsal.PRISTYP        
         flsal.TOTALT = (ACCUM TOTAL fsal.TOTALT) - arrhjsum                  
         arrhjsum = ACCUM TOTAL fsal.TOTALT.                                                 
      END.
   END.     
   personal = "".
   FOR EACH flsal:                            
      IF flsal.PERSONALKOD NE personal THEN DO:
         FIND FIRST FLEXSALDO WHERE FLEXSALDO.PERSONALKOD = flsal.PERSONALKOD
         USE-INDEX PKOD NO-LOCK.
         IF NOT AVAILABLE FLEXSALDO THEN DO:
            CREATE FLEXSALDO.
            ASSIGN FLEXSALDO.PERSONALKOD = PERSONALTAB.PERSONALKOD.
         END.
         CREATE flexsal.   
         ASSIGN          
         flexsal.PERSONALKOD = flsal.PERSONALKOD
         flexsal.FORNAMN = flsal.FORNAMN
         flexsal.EFTERNAMN = flsal.EFTERNAMN
         flexsal.INSAL = FLEXSALDO.ACCREGTID.                
      END.                       
      IF flsal.PRISTYP = "TOT.PRIS." THEN DO:
         ASSIGN flexsal.FLEX = flsal.TOTALT.
      END.
      ELSE IF flsal.PRISTYP = "RESTID.." THEN DO:
         ASSIGN flexsal.RESTID = flsal.TOTALT.
      END.
      ELSE IF flsal.PRISTYP = "FRÅNVARO." AND ( flsal.AONR = "110" OR flsal.AONR = "120")   THEN DO:
          /* sjuk vård av barn med avdrag gjort*/
         ASSIGN flexsal.SJUKDOM = flexsal.SJUKDOM + flsal.TOTALT.
      END.
      ELSE IF flsal.PRISTYP = "FRÅNVARO." AND ( flsal.AONR = "118" OR flsal.AONR = "130" )  THEN DO:
          /* sjuk vård av barn utan avdrag gjort*/
         ASSIGN flexsal.BARN = flexsal.BARN + flsal.TOTALT.
      END.
      ELSE IF flsal.PRISTYP = "FRÅNVARO." AND flsal.AONR = "150"  THEN DO:
         ASSIGN flexsal.SEMESTER = flsal.TOTALT.
      END.
      personal = flsal.PERSONALKOD.                    
   END.      
   IF manadnr = 99 THEN DO:
      ASSIGN hjdat = DATE(01,01,arnr).
      mantim = 0.
      REPEAT:
         IF hjdat > TODAY  THEN LEAVE.
         IF YEAR(hjdat) > arnr THEN LEAVE.
         FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = hjdat USE-INDEX ODATUM NO-LOCK NO-ERROR.
         IF NOT AVAILABLE OVERAVTAB THEN dagnr = WEEKDAY(hjdat).
         ELSE dagnr = OVERAVTAB.EQDAG.
         IF dagnr = 7 OR dagnr = 1 THEN hjdat = hjdat.
         ELSE DO:
            IF antpers = 1 THEN DO:
               regdatum = hjdat.
               FIND FIRST tidpers NO-ERROR.
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDPERS.PERSONALKOD NO-LOCK NO-ERROR.
               persrec = RECID(PERSONALTAB).
               RUN REGVEC.P.
               RUN SLUTARB.P.
               IF regtotalt = 0 THEN.
               ELSE IF regtotalt = 4 THEN mantim = mantim + 4.
               ELSE mantim = mantim + 8.
            END.            
            ELSE DO:
               IF AVAILABLE OVERAVTAB  THEN DO:
                  IF OVERAVTAB.DAGEQ = "HAL" THEN mantim = mantim + 4. 
                  ELSE mantim = mantim + 8.  
               END.   
               ELSE mantim = mantim + 8.
            END.   
         END.
         hjdat = hjdat + 1.
      END.     
   END.
   ELSE DO:      
      ASSIGN hjdat = DATE(manadnr,01,arnr).
      mantim = 0.
      REPEAT:
         IF MONTH(hjdat) NE manadnr THEN LEAVE.         
         IF hjdat > TODAY  THEN LEAVE.
         FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = hjdat USE-INDEX ODATUM NO-LOCK NO-ERROR.
         IF NOT AVAILABLE OVERAVTAB THEN dagnr = WEEKDAY(hjdat).
         ELSE dagnr = OVERAVTAB.EQDAG.
         IF dagnr = 7 OR dagnr = 1 THEN hjdat = hjdat.
         ELSE DO:
            IF antpers = 1 THEN DO:
               regdatum = hjdat.
               FIND FIRST tidpers NO-ERROR.
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDPERS.PERSONALKOD NO-LOCK NO-ERROR.
               persrec = RECID(PERSONALTAB).
               RUN REGVEC.P.
               RUN SLUTARB.P.
               IF regtotalt = 0 THEN.
               ELSE IF regtotalt = 4 THEN mantim = mantim + 4.
               ELSE mantim = mantim + 8.
            END.
            ELSE DO:
               IF AVAILABLE OVERAVTAB  THEN DO:
                  IF OVERAVTAB.DAGEQ = "HAL" THEN mantim = mantim + 4. 
                  ELSE mantim = mantim + 8.
               END.     
               ELSE mantim = mantim + 8.
            END.
            
         END.        
         hjdat = hjdat + 1.
      END.     
   END.     
   FOR EACH flexsal:
      flexsal.TOTALT = flexsal.FLEX.  /*+ flexsal.SJUKDOM + flexsal.SEMESTER.*/
      flexsal.PLUS = flexsal.TOTALT - mantim.
   END.       
END PROCEDURE.

