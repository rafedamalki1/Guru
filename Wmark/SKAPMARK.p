 /*SKAPMARK.P PROTOKOLL VARDERING*/
{APP.I}


DEFINE VARIABLE valvard AS CHARACTER NO-UNDO.
DEFINE VARIABLE samman AS LOGICAL NO-UNDO.
DEFINE VARIABLE stamp AS LOGICAL NO-UNDO.
DEFINE VARIABLE arrhjsum AS INTEGER NO-UNDO.
DEFINE VARIABLE arrhj1 AS DECIMAL NO-UNDO.     
DEFINE VARIABLE arrhj2 AS INTEGER NO-UNDO.
DEFINE VARIABLE arrhj3 AS INTEGER NO-UNDO.    
DEFINE VARIABLE arrhj4 AS DECIMAL NO-UNDO.   
DEFINE VARIABLE arrhj5 AS INTEGER NO-UNDO.  
DEFINE VARIABLE arrhj6 AS INTEGER NO-UNDO.
DEFINE VARIABLE arrhj7 AS DECIMAL NO-UNDO.   
DEFINE VARIABLE arrhj8 AS INTEGER NO-UNDO.  
DEFINE VARIABLE arrhj9 AS INTEGER NO-UNDO.
DEFINE VARIABLE arrhj10 AS DECIMAL NO-UNDO.   
DEFINE VARIABLE arrhj11 AS INTEGER NO-UNDO.
DEFINE VARIABLE suakertot AS INTEGER NO-UNDO. 
DEFINE VARIABLE sumkabel AS INTEGER NO-UNDO. 
DEFINE VARIABLE sumskogn AS INTEGER NO-UNDO.
DEFINE VARIABLE sumskognpro AS INTEGER NO-UNDO.
DEFINE VARIABLE sumskogb AS INTEGER NO-UNDO.
DEFINE VARIABLE sumakern AS INTEGER NO-UNDO.
DEFINE VARIABLE sumakernpro AS INTEGER NO-UNDO.
DEFINE VARIABLE sumakerb AS INTEGER NO-UNDO.
DEFINE VARIABLE sumkabeln AS INTEGER NO-UNDO.
DEFINE VARIABLE sumkabelnpro AS INTEGER NO-UNDO.
DEFINE VARIABLE sumkabelb AS INTEGER NO-UNDO.
DEFINE VARIABLE sumallan AS INTEGER NO-UNDO.
DEFINE VARIABLE sumallab AS INTEGER NO-UNDO.
DEFINE VARIABLE sumplus AS INTEGER NO-UNDO.
DEFINE VARIABLE sumproc AS INTEGER NO-UNDO.
DEFINE VARIABLE tillagg AS INTEGER NO-UNDO.
DEFINE VARIABLE sumvol AS INTEGER NO-UNDO.
DEFINE VARIABLE sumvol2 AS INTEGER NO-UNDO.
DEFINE VARIABLE totsumma AS INTEGER NO-UNDO.
DEFINE VARIABLE minus AS INTEGER NO-UNDO.
DEFINE VARIABLE moms AS INTEGER NO-UNDO.
DEFINE VARIABLE rottot AS INTEGER NO-UNDO.
DEFINE VARIABLE markarec AS RECID NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE st2 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE str3 AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE fbeteckn LIKE FASTIGHET.BETECKNING NO-UNDO.
DEFINE VARIABLE spfbeteckn LIKE FASTIGHET.BETECKNING NO-UNDO.
DEFINE VARIABLE markant AS INTEGER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
{EXTRADATA.I}
   

DEFINE  VARIABLE TOG_ALLAMA AS LOGICAL INITIAL no 
     LABEL "ALLA MARKÄGARE":L 
     VIEW-AS TOGGLE-BOX
     SIZE 24.25 BY 1 NO-UNDO.
{VARDUT.I}


/* ***************************  Main Block  *************************** */
FIND FIRST uppfoltemp NO-LOCK NO-ERROR.
ASSIGN
 
 
TOG_ALLAMA = uppfoltemp.ALLAMA
valvard = uppfoltemp.VALVARD
samman = uppfoltemp.FORSTA
stamp = uppfoltemp.STAMP.
FOR EACH markval USE-INDEX ORDNING:
   FIND FIRST FASTIGHET WHERE FASTIGHET.BETECKNING = markval.BETECKNING 
   USE-INDEX FAST NO-LOCK NO-ERROR.
   ASSIGN markval.KOMMUN = FASTIGHET.KOMMUN 
   markval.SOCKEN = FASTIGHET.SOCKEN
   markval.PAKER = FASTIGHET.PAKER.
   /*markval.VAKER = FASTIGHET.VAKER.*/       
   FIND FIRST AOVARD WHERE AOVARD.VARDNR = markval.VARDNR USE-INDEX VARDNR NO-LOCK NO-ERROR.
   IF AVAILABLE AOVARD THEN DO:
      ASSIGN markval.AONR = AOVARD.AONR
      markval.DELNR = AOVARD.DELNR.
   END.
   ELSE DO:
      ASSIGN markval.AONR = ?
      markval.DELNR = ?.
   END.     
END.     
EMPTY TEMP-TABLE tidut NO-ERROR. 

str=                                                                    
"========================================================================================".                                                       
         
/*RUN skapain_UI.*/
RUN protokoll_UI.    
/* **********************  Internal Procedures  *********************** */



PROCEDURE protokoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   IF valvard = "" THEN DO:      
      CREATE tidut.
      ASSIGN             
      SUBSTRING(tidut.UT,5) = " M A R K Ä G A R E  ".  
      ASSIGN
      SUBSTRING(tidut.UT,45) = "Utskriftsdatum"
      SUBSTRING(tidut.UT,60) = STRING(TODAY) + " " + STRING(TIME,"HH:MM").
      CREATE tidut.
      ASSIGN             
      SUBSTRING(tidut.UT,5) = "===================".
      CREATE tidut.
      IF AVAILABLE AONRTIDLAGE THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,5) = "Markvärdering klar"
         SUBSTRING(tidut.UT,25) = STRING(AONRTIDLAGE.DATUM2).
      END.
      FIND FIRST markval NO-LOCK NO-ERROR.
      FIND FIRST VARDERING WHERE VARDERING.VARDNR = markval.VARDNR USE-INDEX VARDNR
      NO-LOCK NO-ERROR.      
      
      CREATE tidut.
      IF Guru.Konstanter:varforetypval[5] = 1 THEN DO: 
         CREATE tidut. 
         ASSIGN             
         SUBSTRING(tidut.UT,1) = "Nätägare:".   
         SUBSTRING(tidut.UT,13) = VARDERING.NATAGARE.
         CREATE tidut.
      END.
      markant = 1.
      CREATE tidut.
      ASSIGN             
      SUBSTRING(tidut.UT,1) = str.
      FIND FIRST markval NO-ERROR.      
      IF markval.ORDNING NE 0 THEN DO:         
      
         spfbeteckn = "".
         FOR EACH markval USE-INDEX ORDNING:                      
            ASSIGN            
            markarec = markval.MARKREC
            fbeteckn = markval.BETECKNING.
            IF spfbeteckn = fbeteckn THEN DO:
               FIND LAST tidut NO-ERROR.
               IF AVAILABLE tidut THEN DO:
                 IF SUBSTRING(tidut.UT,1) = str THEN DELETE tidut.
               END.
            END.
            RUN huvudsum_UI.              
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,1) = str.            
            spfbeteckn = fbeteckn.                    
            IF markant = 5 THEN DO:            
               CREATE tidut.
               ASSIGN SUBSTRING(tidut.UT,1) = str.
               ASSIGN SUBSTRING(tidut.UT,132,1) = "$"
               markant = 0.
            END.
            markant = markant + 1.
         END.   
      END.
      ELSE DO:
      
         spfbeteckn = "".
         FOR EACH markval  BY markval.MARKNR BY markval.BETECKNING BY markval.VARDNR:       
            ASSIGN         
            markarec = markval.MARKREC
            fbeteckn = markval.BETECKNING.
            IF spfbeteckn = fbeteckn THEN DO:
               FIND LAST tidut NO-ERROR.
               IF AVAILABLE tidut THEN DO:
                 IF SUBSTRING(tidut.UT,1) = str THEN DELETE tidut.
               END.
            END.
            RUN huvudsum_UI.  
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,1) = str.            
            spfbeteckn = fbeteckn.                    
            IF markant = 6 THEN DO:                
               CREATE tidut.
               ASSIGN SUBSTRING(tidut.UT,1) = str.
               ASSIGN SUBSTRING(tidut.UT,132,1) = "$"
               markant = 0.
            END.
            markant = markant + 1.
         END.   
      END.
   END.   
   
   ASSIGN valvard = "".
END PROCEDURE.

PROCEDURE huvudsum_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   FIND FIRST markval WHERE markval.MARKREC = markarec
   AND markval.BETECKNING = fbeteckn NO-LOCK NO-ERROR.
   IF markval.AONR NE "" THEN DO:
      FIND FIRST AONRTIDLAGE WHERE AONRTIDLAGE.AONR = markval.AONR AND AONRTIDLAGE.DELNR = markval.DELNR
      AND AONRTIDLAGE.IDTIDLAG = "Markvärdering" AND AONRTIDLAGE.DATUM2 NE ? NO-LOCK NO-ERROR.
   END.   
   
   FIND FIRST markval WHERE markval.MARKREC = markarec
   AND markval.BETECKNING = fbeteckn NO-LOCK NO-ERROR.
   FIND FIRST VARDERING WHERE VARDERING.VARDNR = markval.VARDNR USE-INDEX VARDNR
   NO-LOCK NO-ERROR.      
   
   FIND FIRST VARDERING WHERE VARDERING.VARDNR = markval.VARDNR USE-INDEX VARDNR
   NO-LOCK NO-ERROR.      
   CREATE tidut.
   ASSIGN             
   SUBSTRING(tidut.UT,1) = "Markvärdering:"
   SUBSTRING(tidut.UT,20) = STRING(VARDERING.VARDNR)
   SUBSTRING(tidut.UT,30) = VARDERING.BENAMNING.   
   IF markval.AONR NE ? THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,65) = Guru.Konstanter:gaok + ":"  
      SUBSTRING(tidut.UT,70) = markval.AONR
      SUBSTRING(tidut.UT,77) = STRING(markval.DELNR,Guru.Konstanter:varforetypchar[1]). 
   END.   
   CREATE tidut.      
   CREATE tidut.   
   ASSIGN       
   SUBSTRING(tidut.UT,1) = "Beteckning:"
   SUBSTRING(tidut.UT,13) = markval.BETECKNING.   
   CREATE tidut.      
   CREATE tidut.
   ASSIGN       
   SUBSTRING(tidut.UT,1) = "Kommun:"
   SUBSTRING(tidut.UT,13) = markval.KOMMUN
   SUBSTRING(tidut.UT,40) = "Socken:"     
   SUBSTRING(tidut.UT,49) = markval.SOCKEN.  
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN musz = musz.
   ELSE DO:   
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph. 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FASTLOPNR"                   
      inextradatatemp.HUVUDCH = markval.BETECKNING.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN
         SUBSTRING(tidut.UT,65) = "Löpnr:"
         SUBSTRING(tidut.UT,72) =  extradatatemp.SOKCHAR[1].      
      END.
      IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
      edataapph = ?.
   END.
   FIND FIRST FASTIGHET WHERE FASTIGHET.BETECKNING = markval.BETECKNING 
   USE-INDEX FAST NO-LOCK NO-ERROR.
   
   CREATE tidut.
   ASSIGN       
   SUBSTRING(tidut.UT,1) = "Markägarnr:"
   SUBSTRING(tidut.UT,13) = STRING(markval.MARKNR)
   SUBSTRING(tidut.UT,37) = "Ägarandel:".
   
   /*markägare andel*/
   IF Guru.Konstanter:varforetypval[22] = 1  THEN DO:   
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "MARKFASTANDEL"                   
      inextradatatemp.HUVUDINT = markval.MARKNR.                    
      inextradatatemp.HUVUDCH = markval.BETECKNING.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN SUBSTRING(tidut.UT,49) = extradatatemp.SOKCHAR[1].                             
      END.       
      ELSE DO:
         IF markval.PROCENT = 100 THEN SUBSTRING(tidut.UT,49) = "1/1".
         ELSE IF markval.PROCENT = 80 THEN SUBSTRING(tidut.UT,49) = "4/5".         
         ELSE IF markval.PROCENT = 66 THEN SUBSTRING(tidut.UT,49) = "2/3".         
         ELSE IF markval.PROCENT = 60 THEN SUBSTRING(tidut.UT,49) = "3/5".         
         ELSE IF markval.PROCENT = 50 THEN SUBSTRING(tidut.UT,49) = "1/2".         
         ELSE IF markval.PROCENT = 40 THEN SUBSTRING(tidut.UT,49) = "2/5".         
         ELSE IF markval.PROCENT = 33 THEN SUBSTRING(tidut.UT,49) = "1/3".         
         ELSE IF markval.PROCENT = 34 THEN SUBSTRING(tidut.UT,49) = "1/3".   
         ELSE IF markval.PROCENT = 25 THEN SUBSTRING(tidut.UT,49) = "1/4".
         ELSE IF markval.PROCENT = 20 THEN SUBSTRING(tidut.UT,49) = "1/5".
         ELSE IF markval.PROCENT = 10 THEN SUBSTRING(tidut.UT,49) = "1/10".
         ELSE SUBSTRING(tidut.UT,49) = STRING(markval.PROCENT) + "/100".
      END.    
   END.
   ELSE DO:   
      ASSIGN      
      SUBSTRING(tidut.UT,49) = STRING(markval.PROCENT)
      SUBSTRING(tidut.UT,53) = "%".
   END.
   CREATE tidut.
   ASSIGN       
   SUBSTRING(tidut.UT,1) = "Markägare:"
   SUBSTRING(tidut.UT,13) = markval.MARKAGARE.
   CREATE tidut.
   ASSIGN     
   SUBSTRING(tidut.UT,1) = "Persnr:"
   SUBSTRING(tidut.UT,13) = STRING(markval.PERSONNUMMER,"999999-9999").
   IF markval.PNR2 BEGINS "0000" THEN musz = musz.
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,38) = "Make/makas persnr:" 
      SUBSTRING(tidut.UT,49) = STRING(markval.PNR2,"999999-9999").
   END.    
   CREATE tidut.
   ASSIGN       
   SUBSTRING(tidut.UT,1) = "Adress:"
   SUBSTRING(tidut.UT,13) = SUBSTRING(markval.GATUADRESS,1,25)
   SUBSTRING(tidut.UT,40) = "Postnr:"
   SUBSTRING(tidut.UT,49) = STRING(markval.POSTNUMMER,"999 99")
   SUBSTRING(tidut.UT,56) = "Postadress:"
   SUBSTRING(tidut.UT,69) = markval.POSTADRESS.   
   FIND FIRST MARKAGARE WHERE MARKAGARE.MARKNR = markval.MARKNR 
   USE-INDEX MARKNR NO-LOCK NO-ERROR.
   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + STRING(MARKAGARE.MARKNR).
   
   CREATE tidut.
   ASSIGN       
   SUBSTRING(tidut.UT,1) = "Telefon:"
   SUBSTRING(tidut.UT,13) = STRING(MARKAGARE.TELEFON)
   SUBSTRING(tidut.UT,35) = "Telefon arb:"
   SUBSTRING(tidut.UT,49) = STRING(MARKAGARE.TELEFON2).
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN musz = musz.
   ELSE DO:   
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "MARKAG"                   
      inextradatatemp.HUVUDINT = MARKAGARE.MARKNR.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         CREATE tidut.
         ASSIGN       
         SUBSTRING(tidut.UT,1) = "Mobil:"
         SUBSTRING(tidut.UT,13) = extradatatemp.SOKCHAR[1]
         SUBSTRING(tidut.UT,40) = "E-post:"
         SUBSTRING(tidut.UT,49) = extradatatemp.SOKCHAR[2].
      END.         
      IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
      edataapph = ?.
   END.
   CREATE tidut.
   IF SUBSTRING(markval.ANAMN,1,30) NE "" THEN DO:        
      CREATE tidut.
      ASSIGN       
      SUBSTRING(tidut.UT,1) = "Arrendator:"
      SUBSTRING(tidut.UT,13) = SUBSTRING(markval.ANAMN,1,30)
      SUBSTRING(tidut.UT,40) = "Persnr:"
      SUBSTRING(tidut.UT,48) = STRING(markval.APERNR,"999999-9999").   
      CREATE tidut.
      ASSIGN       
      SUBSTRING(tidut.UT,1) = "Adress/tel:"
      SUBSTRING(tidut.UT,13) = markval.AADRESS
      SUBSTRING(tidut.UT,49) = "Postnr:"
      SUBSTRING(tidut.UT,57) = STRING(markval.APONR,"999 99")
      SUBSTRING(tidut.UT,64) = "Postadr:"
      SUBSTRING(tidut.UT,73) = markval.APADRESS.      
   END.   
   Guru.GlobalaVariabler:GDPRtyp = "M". 
   {GDPRLOGGCLIENT.I}
END PROCEDURE.




