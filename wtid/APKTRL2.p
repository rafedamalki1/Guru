/*APKTRL2.P*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstart LIKE TIDREGITAB.START NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt LIKE TIDREGITAB.TOTALT NO-UNDO.      
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE VARIABLE periodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pekodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pertot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE pkodtot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE difftot AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE flslutet LIKE TIDREGITAB.SLUT NO-UNDO.
DEFINE VARIABLE flstop AS INTEGER NO-UNDO.
DEFINE VARIABLE heldag AS DECIMAL FORMAT "9999.99" NO-UNDO.
DEFINE VARIABLE kolltid AS DECIMAL FORMAT "9999.99" NO-UNDO.
DEFINE VARIABLE flexkvst LIKE TIDREGITAB.START NO-UNDO. 
DEFINE VARIABLE hjflexkvst LIKE TIDREGITAB.START NO-UNDO. 
DEFINE VARIABLE flexkvstsp AS DECIMAL NO-UNDO. 
DEFINE VARIABLE flexmoslsp AS DECIMAL NO-UNDO. 
DEFINE VARIABLE flexkvslsp AS DECIMAL NO-UNDO. 
DEFINE VARIABLE flexkvslspar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE flexkvsl  AS DECIMAL NO-UNDO. 
DEFINE VARIABLE flexmosl AS DECIMAL NO-UNDO. 
DEFINE VARIABLE hjstart AS DECIMAL NO-UNDO. 
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE dagtot AS DECIMAL NO-UNDO.
DEFINE VARIABLE ovar AS DECIMAL NO-UNDO.
DEFINE VARIABLE ovman AS DECIMAL NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE BUFFER persbuff FOR PERSONALTAB.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE dispens AS LOGICAL NO-UNDO.
DEFINE VARIABLE dispensm AS LOGICAL NO-UNDO.
DEFINE VARIABLE antsjjuni AS DECIMAL NO-UNDO.
DEFINE VARIABLE sjjuni AS LOGICAL NO-UNDO.
DEFINE VARIABLE kravkomm AS CHARACTER NO-UNDO.
DEFINE VARIABLE aodlak AS CHARACTER NO-UNDO.
DEFINE VARIABLE spafr AS LOGICAL NO-UNDO.
DEFINE VARIABLE frisktot AS DECIMAL NO-UNDO.
DEFINE VARIABLE varngr AS INTEGER NO-UNDO.
DEFINE VARIABLE aratkfrisk AS INTEGER NO-UNDO.
DEFINE VARIABLE ftro AS LOGICAL NO-UNDO.
DEFINE VARIABLE tillit AS LOGICAL NO-UNDO.
DEFINE VARIABLE maxov AS INTEGER NO-UNDO.
DEFINE VARIABLE finnstid AS LOGICAL NO-UNDO.
DEFINE VARIABLE sokartal AS INTEGER NO-UNDO.
DEFINE VARIABLE ater50 AS LOGICAL NO-UNDO.
DEFINE VARIABLE atertim  AS INTEGER NO-UNDO.
DEFINE VARIABLE vilach AS LOGICAL NO-UNDO.
&Scoped-define NEW NEW
{TIDPERS.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}

DEFINE TEMP-TABLE invartemp   
   FIELD GA LIKE ANVANDARE.ANVANDARE 
   FIELD GM AS LOGICAL 
   FIELD SK AS LOGICAL 
   FIELD TI AS RECID 
   FIELD PER AS RECID 
   FIELD PER2 AS RECID 
   FIELD MU AS LOGICAL    
   FIELD REGST LIKE TIDREGITAB.START 
   FIELD REGSU LIKE TIDREGITAB.SLUT 
   FIELD RV AS INTEGER FORMAT "999" 
   FIELD RDAG AS CHARACTER FORMAT "X(3)"         
   FIELD RD AS DATE 
   FIELD RM AS INTEGER FORMAT "99" 
   FIELD RMN AS CHARACTER  
   FIELD REGA AS INTEGER FORMAT "99" 
   FIELD RT LIKE TIDREGITAB.TOTALT       
   FIELD BD AS DATE 
   FIELD AD AS DATE 
   FIELD NY AS DECIMAL 
   FIELD SEK AS INTEGER FORMAT "-9999999" 
   FIELD RSEK AS INTEGER 
   FIELD REGS AS INTEGER 
   FIELD GL LIKE FORETAG.FORETAG. 

DEFINE TEMP-TABLE ltillagg    
   FIELD RDATUM AS DATE
   FIELD LONTILLAGG AS CHARACTER
   FIELD LONTILLANTAL AS DECIMAL. 
DEFINE TEMP-TABLE bilkoll    
   FIELD LONTILLAGG AS CHARACTER
   FIELD LONTILLANTAL AS DECIMAL.
   
 DEFINE TEMP-TABLE sjtemp   
   FIELD DATUM AS DATE
   FIELD AONR AS CHARACTER
   FIELD SJUK AS INTEGER
   INDEX DATUM DATUM DESCENDING . 
   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(90)" NO-UNDO.
{TIDUTTTNEW.I}
DEFINE QUERY tidq FOR TIDREGITAB.

DEFINE INPUT PARAMETER TABLE FOR invartemp.
DEFINE INPUT PARAMETER TABLE FOR tidpers.
DEFINE OUTPUT PARAMETER TABLE FOR tidut.
{EXTRADATA.I}
RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
FUNCTION klock60 RETURNS DECIMAL
   ( INPUT ber100 AS DECIMAL ) :
   RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).   /* Function return value. */
END FUNCTION.
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

ASSIGN   str=                                                                              
"===========================================================================================================".   

FIND FIRST invartemp NO-ERROR.
ASSIGN
 
gvisatidpermanad = invartemp.GM 
skrivut = invartemp.SK   
tidtabrec = invartemp.TI   
persrec = invartemp.PER   
persrec2 = invartemp.PER2  
musz = invartemp.MU      
regstart = invartemp.REGST   
regslut = invartemp.REGSU  
regvnr = invartemp.RV   
regdagnamn = invartemp.RDAG          
regdatum = invartemp.RD   
regmnr = invartemp.RM  
regmannamn = invartemp.RMN  
regar = invartemp.REGA  
regtotalt = invartemp.RT        
bdatum = invartemp.BD  
avdatum = invartemp.AD  
nytid = invartemp.NY 
sekunder = invartemp.SEK 
regstartsek = invartemp.RSEK  
regslutsek = invartemp.REGS .

RUN huvud_UI.

{GDPRLOGGCLIENT.I}
PROCEDURE huvud_UI :   
   CREATE tidut.
   ASSIGN 
   SUBSTRING(tidut.UT,4) = "Avvikelser från arbetsschemat"   
   SUBSTRING(tidut.UT,70) = STRING(TODAY)
   SUBSTRING(tidut.UT,80) = STRING(TIME,"HH:MM:SS").
   CREATE tidut.                        
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Enhet".
   CREATE tidut.                        
   ASSIGN
   SUBSTRING(tidut.UT,7) = "Förnamn"
   SUBSTRING(tidut.UT,23) = "Efternamn"   
   SUBSTRING(tidut.UT,44) = "Vnr"  
   SUBSTRING(tidut.UT,53) = "Dag" 
   SUBSTRING(tidut.UT,57) = "Förklarande text".
   IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = "Datum   ".
   SUBSTRING(tidut.UT,1) = "/Sign".
   CREATE tidut.  
   ASSIGN tidut.UT = str.   
   FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK:     
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tidpers.PERSONALKOD.  
      ASSIGN
      persrec = tidpers.TIDPERSREC
      regdatum = bdatum. 
      FIND PERSONALTAB WHERE RECID(PERSONALTAB) = persrec NO-LOCK NO-ERROR. 
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.                 
      musz = FALSE.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE FLEXAVT THEN DO:
            IF FLEXAVT.FLEXTID = TRUE THEN musz = TRUE.             
         END.
      END.
      IF musz = TRUE THEN DO:
         
         /*FLEXAVTAL*/
         musz = FALSE.
         FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD NO-LOCK NO-ERROR.
         ASSIGN        
         flexmosl = FLEXREG.MOSLUT 
         flexmoslsp = FLEXREG.MOSLUT
         flexkvsl = FLEXREG.KVSLUT
         flexkvslsp = FLEXREG.KVSLUT
         flexkvst = FLEXREG.KVSTART
         flexkvstsp = FLEXREG.KVSTART.                  
         regdatum = bdatum.  
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR Guru.Konstanter:globforetag = "elpa" THEN DO:         
             /* sommartid*/
            /*k- morgonflex till 9 både sommar och vinter , övriga sommar 8.30 vinter 9 -ändras manuellt i FLEXREG*/         
            IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.               
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.               
               flexkvst = FLEXREG.KVSOST.
               flexkvsl = FLEXREG.KVSOSL.            
            END.  
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.               
               flexkvst = FLEXREG.KVSOST.
               flexkvsl = FLEXREG.KVSOSL.            
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.               
               flexkvst = FLEXREG.KVSOST.
               flexkvsl = FLEXREG.KVSOSL.            
            END.
            ELSE DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                                       
            END.  
         END.
         IF Guru.Konstanter:globforetag = "LULE" THEN DO:         
             /* sommartid*/            
            IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:               
               flexkvst = FLEXREG.KVSOST.
               flexkvsl = FLEXREG.KVSOSL.            
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:               
               flexkvst = FLEXREG.KVSOST.
               flexkvsl = FLEXREG.KVSOSL.            
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO:                
               flexkvst = FLEXREG.KVSOST.
               flexkvsl = FLEXREG.KVSOSL.            
            END.            
         END.

         NYDAG:
         REPEAT:                          
            RUN REGDAG.P.
            RUN REGVEC.P.
            RUN SLUTARB.P.
            IF regstart = regslut THEN DO:
               regdatum = regdatum + 1.               
               IF regdatum > avdatum THEN LEAVE NYDAG.
               ELSE NEXT NYDAG.
            END.                       
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:         
               /* sommartid*/
               IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:
                  flexkvst = FLEXREG.KVSOST.
                  flexkvsl = FLEXREG.KVSOSL.
                  IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
                  ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                  
               END.
               ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
                  flexkvst = FLEXREG.KVSOST.
                  flexkvsl = FLEXREG.KVSOSL.
                  IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
                  ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                  
               END.
               ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
                  flexkvst = FLEXREG.KVSOST.
                  flexkvsl = FLEXREG.KVSOSL.
                  IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
                  ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                  
               END.
               ELSE DO:
                  IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
                  ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                  
               END.         
            END.
            IF Guru.Konstanter:globforetag = "LULE" THEN DO:         
                /* sommartid*/            
               IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:               
                  flexkvst = FLEXREG.KVSOST.
                  flexkvsl = FLEXREG.KVSOSL.            
               END.
               ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:               
                  flexkvst = FLEXREG.KVSOST.
                  flexkvsl = FLEXREG.KVSOSL.            
               END.
               ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO:                
                  flexkvst = FLEXREG.KVSOST.
                  flexkvsl = FLEXREG.KVSOSL.            
               END.            
            END.
            FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND 
            OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
            IF AVAILABLE OVERAVTAB THEN DO:
               IF OVERAVTAB.DAGEQ = "HAL"  THEN flexkvst = regslut.               
               IF OVERAVTAB.DAGEQ = "KLA" THEN DO:
                  regdatum = regdatum + 1.
                  IF regdatum > avdatum THEN LEAVE NYDAG.
                  ELSE NEXT NYDAG. 
               END.
            END.
            IF PERSONALTAB.DELTID = TRUE THEN DO:
               FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD NO-LOCK NO-ERROR.
               ASSIGN        
               flexmosl = FLEXREG.MOSLUT 
               flexkvsl = FLEXREG.KVSLUT
               flexkvst = FLEXREG.KVSTART
               flexmoslsp = FLEXREG.MOSLUT.         
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:         
                  /* sommartid*/
                  IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:
                     IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
                     ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                     
                     flexkvst = FLEXREG.KVSOST.
                     flexkvsl = FLEXREG.KVSOSL.
                  END.
                  ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
                     IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
                     ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                     
                     flexkvst = FLEXREG.KVSOST.
                     flexkvsl = FLEXREG.KVSOSL.
                  END.
                  ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
                     IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
                     ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                     
                     flexkvst = FLEXREG.KVSOST.
                     flexkvsl = FLEXREG.KVSOSL.
                  END.
                  ELSE DO:
                     IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
                     ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                     
                  END.
               END.
               IF Guru.Konstanter:globforetag = "LULE" THEN DO:         
                   /* sommartid*/            
                  IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:               
                     flexkvst = FLEXREG.KVSOST.
                     flexkvsl = FLEXREG.KVSOSL.            
                  END.
                  ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:               
                     flexkvst = FLEXREG.KVSOST.
                     flexkvsl = FLEXREG.KVSOSL.            
                  END.
                  ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO:                
                     flexkvst = FLEXREG.KVSOST.
                     flexkvsl = FLEXREG.KVSOSL.            
                  END.            
               END.
               FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING NO-LOCK NO-ERROR.
               IF AVAILABLE ORDARB THEN DO:               
                  sekunder = ORDARB.STOPP1.
                  RUN SEKTIM.P.
                  IF nytid > regslut THEN DO: 
                     flexkvslspar = flexkvsl.
                     nytid = flexkvsl.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regslut.
                     RUN TIMSEK.P.
                     sekunder = seku - ORDARB.STOPP1 + sekunder.
                     RUN SEKTIM.P.
                     flexkvsl = nytid.
                     nytid = flexkvst.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regslut.
                     RUN TIMSEK.P.
                     sekunder = seku - ORDARB.STOPP1 + sekunder.
                     RUN SEKTIM.P.
                     flexkvst = nytid.
                     IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"   THEN DO:
                        flexkvsl = flexkvslspar.                        
                     END.
                  END.   
                  sekunder = ORDARB.START1.
                  RUN SEKTIM.P.               
                  IF nytid < regstart THEN DO:                   
                     nytid = flexmosl.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regstart.
                     RUN TIMSEK.P.
                     sekunder = seku - ORDARB.START1 + sekunder.
                     RUN SEKTIM.P.
                     flexmosl = nytid.                     
                  END.                                 
               END.                    
            END.
            
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.VECKOKORD NE "" 
            USE-INDEX PVKORD NO-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO:
               regdatum = regdatum + 1.
               IF regdatum > avdatum THEN LEAVE NYDAG.
               ELSE NEXT NYDAG.
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "ELPA"  THEN DO:               
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START <= flexmosl AND 
               TIDREGITAB.TIDLOG = TRUE 
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:
                  FIND FIRST TIDREGITAB WHERE
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum AND
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO:
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)     
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = "Registrering saknas A".  
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
                     regdatum = regdatum + 1.  
                     IF regdatum > avdatum THEN LEAVE NYDAG.
                     ELSE NEXT NYDAG.                            
                  END.
                  ELSE DO:
                     IF TIDREGITAB.GODKAND = "" THEN DO:                     
                        /*undanta skiftare som har gått över till flex under månaden*/
                        /*undanta skiftare som har gått över till flex under månaden och skriver övertid före arbetstidens start*/
                        IF TIDREGITAB.START = regstart AND TIDREGITAB.SLUT = regslut THEN.
                        ELSE IF TIDREGITAB.OVERTIDUTTAG = "k" OR TIDREGITAB.OVERTIDUTTAG = "ö"  OR TIDREGITAB.OVERTIDUTTAG = "I" THEN. 
                        ELSE DO:                     
                           CREATE tidut.               
                           ASSIGN
                           SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                           SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                           SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                           SUBSTRING(tidut.UT,44) = STRING(regvnr)
                           SUBSTRING(tidut.UT,53) = regdagnamn  
                           SUBSTRING(tidut.UT,57) = 
                           "Första registrering är klockan " + STRING(TIDREGITAB.START,"99.99").
                           IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                       
                        END.
                     END.   
                  END.
               END.           
                          
               hjflexkvst = flexkvst.
               IF flexkvst > regslut THEN hjflexkvst = regslut.
               FIND FIRST TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.SLUT >= hjflexkvst AND 
               TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:
                  FIND LAST TIDREGITAB WHERE
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum AND
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.                             
                  ELSE DO:
                     IF TIDREGITAB.GODKAND = "" THEN DO:
                        CREATE tidut.      
                        ASSIGN         
                        SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                        SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                        SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                        SUBSTRING(tidut.UT,44) = STRING(regvnr)
                        SUBSTRING(tidut.UT,53) = regdagnamn  
                        SUBSTRING(tidut.UT,57) = 
                        "Sista registreringen är klockan " + STRING(TIDREGITAB.SLUT,"99.99").
                        IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
                     END.                      
                  END.
               END.
               musz = FALSE.
               FIND FIRST TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.OVERTIDUTTAG = "F"
               AND TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.
               ELSE DO: 
                  IF TIDREGITAB.START > flexmosl THEN DO:
                     /*undanta enstaka skift som en flexare har*/
                     IF TIDREGITAB.START = regstart THEN.
                     ELSE DO:
                         flstop = 1.
                         musz = TRUE.
                     END.                                                        
                  END.
                  flslutet = TIDREGITAB.SLUT.               
                  REPEAT:                                            
                     FIND NEXT TIDREGITAB WHERE
                     TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                     TIDREGITAB.DATUM = regdatum AND
                     TIDREGITAB.OVERTIDUTTAG = "F"                  
                     AND TIDREGITAB.TIDLOG = TRUE
                     USE-INDEX PSTART NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE TIDREGITAB THEN LEAVE.
                     ELSE DO:
                        IF flslutet NE TIDREGITAB.START THEN musz = TRUE.                                                                                          
                        IF flslutet < regstart THEN musz = FALSE. /*ÖVERTID*/
                        IF TIDREGITAB.START > regslut THEN musz = FALSE. /*ÖVERTID*/
                        IF TIDREGITAB.GODKAND NE "" THEN musz = FALSE. 
                        flstop = 2.
                        flslutet = TIDREGITAB.SLUT.
                     END.
                  END.
                  IF flslutet < hjflexkvst THEN DO:
                     /*undanta enstaka skift som en flexare har*/
                     IF flslutet = regslut THEN .
                     ELSE DO: 
                        musz = TRUE.
                        flstop = 3.
                     END.   
                  END.
                  IF musz = TRUE THEN DO:                     
                     musz = FALSE.
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = "Ofullständig registrering". /* + STRING(flslutet)  + STRING(flexreg.kvstart) + STRING(FLSTOP).              */
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                           
                  END.
               END.
            END.
            /*LULEflexavtal registrerar bara flex inom flexramen , ej mellan restart och regslut*/
            ELSE IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "CLULE" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START <= regstart AND TIDREGITAB.SLUT GE regstart AND 
               TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:
                  FIND FIRST TIDREGITAB WHERE
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum AND
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO:
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)     
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = "Registrering saknas B".  
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
                     regdatum = regdatum + 1.  
                     IF regdatum > avdatum THEN LEAVE NYDAG.
                     ELSE NEXT NYDAG.                            
                  END.
                  ELSE DO:
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = 
                     "Första registrering är klockan " + STRING(TIDREGITAB.START,"99.99").
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                       
                  END.
               END.                      
               FIND FIRST TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START < regslut AND TIDREGITAB.SLUT >= regslut AND 
               TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:
                  FIND LAST TIDREGITAB WHERE
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum AND
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.                             
                  ELSE DO:
                     CREATE tidut.      
                     ASSIGN         
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = 
                     "Sista registreringen är klockan " + STRING(TIDREGITAB.SLUT,"99.99").
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                   
                  END.
               END.
               musz = FALSE.
               FIND FIRST TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.OVERTIDUTTAG = "F"
               AND TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.
               ELSE DO: 
                  IF TIDREGITAB.START > regstart THEN DO:
                      flstop = 1.
                      musz = TRUE.                              
                  END.
                  flslutet = TIDREGITAB.SLUT.               
                  REPEAT:                                            
                     FIND NEXT TIDREGITAB WHERE
                     TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                     TIDREGITAB.DATUM = regdatum AND
                     TIDREGITAB.OVERTIDUTTAG = "F"                  
                     AND TIDREGITAB.TIDLOG = TRUE
                     USE-INDEX PSTART NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE TIDREGITAB THEN LEAVE.
                     ELSE DO:
                        IF flslutet NE TIDREGITAB.START THEN musz = TRUE.                                                                                          
                        IF flslutet < regstart THEN musz = FALSE. /*ÖVERTID*/
                        IF TIDREGITAB.START > regslut THEN musz = FALSE. /*ÖVERTID*/
                        flstop = 2.
                        flslutet = TIDREGITAB.SLUT.
                     END.
                  END.
                  IF flslutet < regslut THEN DO: 
                     musz = TRUE.
                     flstop = 3.
                  END.
                  IF musz = TRUE THEN DO:
                     musz = FALSE.
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = "Ofullständig registrering". /* + STRING(flslutet)  + STRING(flexreg.kvstart) + STRING(FLSTOP).              */
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
                  END.
               END.
            END.
            /*dddd*/
            RUN dagcheck_UI.
            
            /*NY DAG*/
            flexkvst = flexkvstsp.
            flexmosl = flexmoslsp.
            regdatum = regdatum + 1.
            IF regdatum > avdatum THEN LEAVE NYDAG.
         END.
         /* Kontroller för hela månaden*/
         
         IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR Guru.Konstanter:globforetag = "elpa" THEN DO:         
            OPEN QUERY fq FOR EACH FLEXDAG WHERE FLEXDAG.PERSONALKOD = tidpers.PERSONALKOD
            AND FLEXDAG.DATUM GE bdatum AND FLEXDAG.DATUM LE avdatum AND FLEXDAG.FELOK = FALSE NO-LOCK.
            GET FIRST fq NO-LOCK.
            DO WHILE AVAILABLE (FLEXDAG):
               regdatum = FLEXDAG.DATUM.
               RUN REGDAG.P.
               IF FLEXDAG.FELMED = "Ingen registrering gjord" THEN DO:                  
                  RUN REGVEC.P.
                  RUN SLUTARB.P.
                  IF regstart = regslut THEN DO TRANSACTION:
                     FIND CURRENT FLEXDAG EXCLUSIVE-LOCK.
                     DELETE FLEXDAG.
                  END.
                  ELSE DO:               
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                
                     SUBSTRING(tidut.UT,53) = regdagnamn.
                     IF FLEXDAG.FELMED = "" THEN SUBSTRING(tidut.UT,57) = "Flex ej kontrollerad".
                     ELSE SUBSTRING(tidut.UT,57) = SUBSTRING(FLEXDAG.FELMED,1,20).
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(FLEXDAG.DATUM,"99/99/99").
                  END.   
               END.   
               ELSE DO:               
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                
                  SUBSTRING(tidut.UT,53) = regdagnamn.
                  IF FLEXDAG.FELMED = "" THEN SUBSTRING(tidut.UT,57) = "Flex ej kontrollerad".
                  ELSE SUBSTRING(tidut.UT,57) = SUBSTRING(FLEXDAG.FELMED,1,20).
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(FLEXDAG.DATUM,"99/99/99").
               END.
               
               GET NEXT fq NO-LOCK.
            END.
         END.           
         RUN mancheck_UI.
                          
      END.      
      ELSE DO:  
         /*EJ FLEXAVTAL ccc*/
         ftro = FALSE.
         tillit = FALSE.
         RUN ftro_UI.
         RUN tillit_UI.
         IF ftro = TRUE OR tillit = TRUE THEN DO:
            /*förtroendeavtal Kalmar*/
            regdatum = bdatum.
            NYDAGF:
            REPEAT:                          
               RUN REGDAG.P.
               RUN REGVEC.P.
               RUN SLUTARB.P.
               kolltid = 0.
               FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
               AND TIDREGITAB.PRISTYP = "FRÅNVARO." USE-INDEX PSTART NO-LOCK:
                  kolltid = kolltid + klock100(TIDREGITAB.TOTALT).                     
               END.
               kolltid = klock60(kolltid).                
               IF kolltid > regtotalt THEN DO:
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)     
                  SUBSTRING(tidut.UT,44) = STRING(regvnr)
                  SUBSTRING(tidut.UT,53) = regdagnamn  
                  SUBSTRING(tidut.UT,57) = "Frånvaro får registreras max " + STRING(regtotalt) + "timmar per dag. ".   
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               END.                            
               /*NY DAG*/
               regdatum = regdatum + 1.
               IF regdatum > avdatum THEN LEAVE NYDAGF.
            END.    
         END.   
         ELSE DO:
            regdatum = bdatum.
            NYDAG:
            REPEAT:                          
               RUN REGDAG.P.
               RUN REGVEC.P.
               RUN SLUTARB.P.
               IF regstart = regslut THEN DO:
                  regdatum = regdatum + 1.
                  IF regdatum > avdatum THEN LEAVE NYDAG.
                  ELSE NEXT NYDAG.
               END.                       
               FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND 
               OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
               IF AVAILABLE OVERAVTAB THEN DO:
                  IF OVERAVTAB.DAGEQ = "KLA" THEN DO:
                     regdatum = regdatum + 1.
                     IF regdatum > avdatum THEN LEAVE NYDAG.
                     ELSE NEXT NYDAG. 
                  END.
               END.
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.VECKOKORD NE "" 
               USE-INDEX PVKORD NO-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:
                  regdatum = regdatum + 1.
                  IF regdatum > avdatum THEN LEAVE NYDAG.
                  ELSE NEXT NYDAG.
               END.
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START <= regstart AND 
               TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:
                  FIND FIRST TIDREGITAB WHERE
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum AND
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO:
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)     
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = "Registrering saknas C ". /* + ANSTFORMTAB.KOD + PERSONALTAB.PERSONALKOD + REGVNR.*/  
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
                     regdatum = regdatum + 1.  
                     IF regdatum > avdatum THEN LEAVE NYDAG.
                     ELSE NEXT NYDAG.                            
                  END.
                  ELSE DO:
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = 
                     "Första registrering är klockan " + STRING(TIDREGITAB.START,"99.99") .
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                       
                  END.
               END.                      
               FIND FIRST TIDREGITAB WHERE 
               TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.SLUT >= regslut AND 
               TIDREGITAB.TIDLOG = TRUE
               USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO TRANSACTION:
                  FIND LAST TIDREGITAB WHERE
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum AND
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN persrec = persrec.                             
                  ELSE DO:
                     CREATE tidut.      
                     ASSIGN         
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = 
                     "Sista registreringen är klockan " + STRING(TIDREGITAB.SLUT,"99.99").
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                   
                  END.
               END.            
               
               IF PERSONALTAB.OVERTIDUTTAG = "I" THEN DO:
                  FIND FIRST TIDREGITAB WHERE 
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                  TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.OVERTIDUTTAG = "I" 
                  USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO: 
                     regstartsek = 0.
                  END.
                  ELSE DO: 
                     nytid = TIDREGITAB.TOTALT.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.         
                     REPEAT:                                            
                        FIND NEXT TIDREGITAB WHERE
                        TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                        TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.OVERTIDUTTAG = "I"
                        USE-INDEX PSTART NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE TIDREGITAB THEN LEAVE.
                        ELSE DO:
                           nytid = TIDREGITAB.TOTALT.
                           RUN TIMSEK.P.
                           regstartsek = regstartsek + sekunder.
                        END.
                     END.
                  END.             
                  musz = FALSE.
                  IF AVAILABLE OVERAVTAB THEN DO:            
                     IF PERSONALTAB.DELTID = TRUE AND OVERAVTAB.DAGEQ = "HAL" THEN DO: 
                        FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING NO-LOCK NO-ERROR.
                        IF AVAILABLE ORDARB THEN DO:                              
                           sekunder = ORDARB.START1.
                           RUN SEKTIM.P.
                           hjstart = nytid.
                           IF hjstart NE regstart THEN musz = TRUE.
                        END.
                     END.
                  END.
                  sekunder = regstartsek.
                  RUN SEKTIM.P.                 
                  IF musz = TRUE THEN musz = FALSE.
                  ELSE IF nytid < regtotalt THEN DO:
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = "Ofullständig registrering" + string(nytid) + string(regtotalt).               
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
                  END.
               END.
               ELSE DO:   
                  FIND FIRST TIDREGITAB WHERE 
                  TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND 
                  TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START >= regstart AND 
                  TIDREGITAB.SLUT <= regslut AND TIDREGITAB.TIDLOG = TRUE 
                  USE-INDEX PSTART NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO: 
                     regstartsek = 0.
                  END.
                  ELSE DO: 
                     nytid = TIDREGITAB.TOTALT.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.         
                     REPEAT:                                            
                        FIND NEXT TIDREGITAB WHERE
                        TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                        TIDREGITAB.DATUM = regdatum AND
                        TIDREGITAB.START >= regstart AND TIDREGITAB.SLUT <= regslut
                        AND TIDREGITAB.TIDLOG = TRUE
                        USE-INDEX PSTART NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE TIDREGITAB THEN LEAVE.
                        ELSE DO:
                           nytid = TIDREGITAB.TOTALT.
                           RUN TIMSEK.P.
                           regstartsek = regstartsek + sekunder.
                        END.
                     END.
                  END.             
                  musz = FALSE.
                  IF AVAILABLE OVERAVTAB THEN DO:            
                     IF PERSONALTAB.DELTID = TRUE AND OVERAVTAB.DAGEQ = "HAL" THEN DO: 
                        FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING NO-LOCK NO-ERROR.
                        IF AVAILABLE ORDARB THEN DO:                              
                           sekunder = ORDARB.START1.
                           RUN SEKTIM.P.
                           hjstart = nytid.
                           IF hjstart NE regstart THEN musz = TRUE.
                        END.
                     END.
                  END.
                  sekunder = regstartsek.
                  RUN SEKTIM.P.
                  IF musz = TRUE THEN musz = FALSE.
                  ELSE IF nytid < regtotalt THEN DO:
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn  
                     SUBSTRING(tidut.UT,57) = "Ofullständig registrering" + string(nytid) + string(regtotalt).               
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
                  END.
               END.                  
               RUN dagcheck_UI.
                                           
               /*NY DAG*/
               regdatum = regdatum + 1.
               IF regdatum > avdatum THEN LEAVE NYDAG.
            END.   
         END.          
         RUN mancheck_UI.         
      END. 
   END.      
   FIND LAST tidut NO-LOCK NO-ERROR.   
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE sjjuni_UI :
   IF Guru.Konstanter:globforetag = "GKAL"  OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
      /*6:JUNI -som infaller lör sön ger extra ledig dag , uttages mellan 6juni-31dec pnr 162 = 6:e juni*/
      sjjuni = FALSE.
      IF WEEKDAY(DATE(06,06,YEAR(bdatum))) = 1 OR WEEKDAY(DATE(06,06,YEAR(bdatum))) = 7 THEN sjjuni = TRUE.
      IF sjjuni = TRUE AND avdatum < DATE(06,01,YEAR(bdatum)) THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.TIDLOG = TRUE AND
         TIDREGITAB.AONR = "162"  USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                                                 
            SUBSTRING(tidut.UT,53) = TIDREGITAB.DAG                     
            SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
            SUBSTRING(tidut.UT,57) = Guru.Konstanter:gaok + " " + TIDREGITAB.AONR + " får bara användas efter 6:e juni".
         END.
      END.
      ELSE IF sjjuni = TRUE AND MONTH(bdatum) = 6 THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM < DATE(06,06,YEAR(bdatum)) AND TIDREGITAB.TIDLOG = TRUE AND
         TIDREGITAB.AONR = "162"  USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                                                 
            SUBSTRING(tidut.UT,53) = TIDREGITAB.DAG                     
            SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
            SUBSTRING(tidut.UT,57) = Guru.Konstanter:gaok + " " + TIDREGITAB.AONR + " får bara användas efter 6:e juni".
         END.
      END.
      IF sjjuni = FALSE  THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.TIDLOG = TRUE AND
         TIDREGITAB.AONR = "162"  USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
            SUBSTRING(tidut.UT,53) = TIDREGITAB.DAG                     
            SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
            SUBSTRING(tidut.UT,57) = Guru.Konstanter:gaok + " " + TIDREGITAB.AONR + " får bara användas år när 6:e juni infaller lördag eller söndag".                  
         END.
      END.
      IF sjjuni = TRUE AND avdatum > DATE(06,06,YEAR(bdatum)) THEN DO:
         /*6:e juni får bara tas ut vid ett tillfälle*/
         antsjjuni = 0.
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
         AND TIDREGITAB.AONR = "162"  AND YEAR(TIDREGITAB.DATUM) = YEAR(bdatum) AND TIDREGITAB.DATUM < avdatum AND TIDREGITAB.TIDLOG = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
            AND TIDREGITAB.AONR = "162"  AND YEAR(TIDREGITAB.DATUM) = YEAR(bdatum) AND TIDREGITAB.DATUM < avdatum AND TIDREGITAB.TIDLOG = TRUE NO-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO:
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)  
               SUBSTRING(tidut.UT,53) = TIDREGITAB.DAG                     
               SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
               SUBSTRING(tidut.UT,57) = Guru.Konstanter:gaok + " " + TIDREGITAB.AONR + " får bara användas vid ett tillfälle".                                    
            END.
         END.
      END.           
   END.
   IF Guru.Konstanter:globforetag = "misv" OR Guru.Konstanter:globforetag = "sund" THEN DO:         
      /*6:JUNI -som infaller lör sön ger extra ledig dag , uttages mellan 6juni-31dec pnr 162 = 6:e juni*/
      sjjuni = FALSE.
      IF WEEKDAY(DATE(06,06,YEAR(bdatum))) = 1 OR WEEKDAY(DATE(06,06,YEAR(bdatum))) = 7 THEN sjjuni = TRUE.      
      IF sjjuni = FALSE  THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.TIDLOG = TRUE AND
         TIDREGITAB.AONR = "162"  USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
            SUBSTRING(tidut.UT,53) = TIDREGITAB.DAG                     
            SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
            SUBSTRING(tidut.UT,57) = Guru.Konstanter:gaok + " " + TIDREGITAB.AONR + " får bara användas år när 6:e juni infaller lördag eller söndag".                  
         END.
      END.
      IF sjjuni = TRUE  THEN DO:
         /*6:e juni får bara tas ut vid ett tillfälle*/
         antsjjuni = 0.
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
         AND TIDREGITAB.AONR = "162"  AND YEAR(TIDREGITAB.DATUM) = YEAR(bdatum)  AND TIDREGITAB.TIDLOG = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
            AND TIDREGITAB.AONR = "162"  AND YEAR(TIDREGITAB.DATUM) = YEAR(bdatum)  AND TIDREGITAB.TIDLOG = TRUE NO-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO:
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)  
               SUBSTRING(tidut.UT,53) = TIDREGITAB.DAG                     
               SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
               SUBSTRING(tidut.UT,57) = Guru.Konstanter:gaok + " " + TIDREGITAB.AONR + " får bara användas vid ett tillfälle".                                    
            END.
         END.
      END.           
   END.   
END PROCEDURE.

PROCEDURE paminn_UI :
   FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   IF Guru.Konstanter:globforetag = "CSUND" OR Guru.Konstanter:globforetag = "celpa"  THEN DO:  
      /*Taget ur drift 20150902   efter prat med Jarmo Klint. De måste ha veckovila  antingen på fredag eller måndag*/                            
      IF WEEKDAY(regdatum) = 6 THEN DO:               
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = (regdatum - 1) AND TIDREGITAB.BEREDSKAP  NE "" AND TIDREGITAB.BERANTAL > 1 USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
         IF AVAILABLE TIDREGITAB THEN DO:
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.BEREDSKAP  NE "" AND TIDREGITAB.BERANTAL > 1 USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
            IF NOT AVAILABLE TIDREGITAB  THEN DO:
               FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.PRISTYP NE "FRÅNVARO." USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                    
               IF AVAILABLE TIDREGITAB  THEN DO:
                  FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.LONTILLAGG = "4372" USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                         
                  IF NOT AVAILABLE TIDREGITAB  THEN DO:                              
                     CREATE tidut.
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn.  
                     SUBSTRING(tidut.UT,57) = "Påminnelse!Vid arbete under veckovila skall lart 217 (korsta) registreras som ersättning för arbetstiden.Kontrollera att det är gjort".                        
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                
                  END.
               END.
            END.
         END.
      END.               
   END.
   IF  Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "celpa"  THEN DO:                              
      IF WEEKDAY(regdatum) = 6 THEN DO:               
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = (regdatum - 1) AND TIDREGITAB.BEREDSKAP  NE "" AND TIDREGITAB.BERANTAL > 1 USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
         IF AVAILABLE TIDREGITAB THEN DO:
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.BEREDSKAP  NE "" AND TIDREGITAB.BERANTAL > 1 USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
            IF NOT AVAILABLE TIDREGITAB  THEN DO:
               FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.PRISTYP NE "FRÅNVARO." AND TIDREGITAB.OKOD1 = "" AND TIDREGITAB.OKOD2 = "" AND TIDREGITAB.OKOD3 = "" USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                    
               IF AVAILABLE TIDREGITAB  THEN DO:
                  
                  CREATE tidut.
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                  SUBSTRING(tidut.UT,44) = STRING(regvnr)
                  SUBSTRING(tidut.UT,53) = regdagnamn.  
                  SUBSTRING(tidut.UT,57) = "Varning!Det borde vara veckovila – inte arbete (kontakta HR om annat överenskommet) " +  STRING(regdatum,"99/99/99").                           
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                
               END.   
                  /*Borttaget 20221101 Lena . De får inte arbeta under veckovila om inte företaget kräver det.
                  FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.LONTILLAGG = "190"  USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                         
                  IF NOT AVAILABLE TIDREGITAB  THEN DO:                              
                     CREATE tidut.
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn.  
                     SUBSTRING(tidut.UT,57) = "Påminnelse!Vid arbete under veckovila skall lart 191 (flextid elnät) registreras som ersättning för arbetstiden.Kontrollera att det är gjort".                        
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                
                  END.
               END.*/
            END.
         END.
      END.
      
      /*veckovila 36 TIM HELG  20180515 Ingrid Eriksson 
      1.       Den som registrerar övertid mellan kl. 19.00 lördag och kl. 04.00 söndag (grundschema 7-16)
               eller mellan kl. 20.00 lördag och 05.00 söndag (grundschema 8-17)ska få upp ett meddelande: ”Kom ihåg att du är berättigad till veckovila nästa fredag”
       2.       GURU ska då på samma sätt som när man registrerar beredskap förutsätta att nästkommande fredag är veckovila 
                så att om personen ändå registrerar tid den fredagen får upp påminnelsen om att även registrera löneart 190, Arbete under veckovila.      
      */
      vilach = FALSE.
      IF WEEKDAY(regdatum) = 6 THEN DO:            
         IF ANSTFORMTAB.KOD BEGINS  "K" THEN DO:      
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = (regdatum - 6) AND TIDREGITAB.SLUT  > 19 USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE  TIDREGITAB THEN vilach = TRUE.
         END.
         IF ANSTFORMTAB.KOD BEGINS  "T" THEN DO:      
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = (regdatum - 6) AND TIDREGITAB.SLUT  > 20 USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE  TIDREGITAB THEN vilach = TRUE.
         END.
         IF ANSTFORMTAB.KOD BEGINS  "K" THEN DO:      
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = (regdatum - 5) AND TIDREGITAB.START  < 4 USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE  TIDREGITAB THEN vilach = TRUE.
         END.
         IF ANSTFORMTAB.KOD BEGINS  "T" THEN DO:      
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = (regdatum - 5) AND TIDREGITAB.START < 5  USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE  TIDREGITAB THEN vilach = TRUE.
         END.   
      END.      
         
      IF vilach = TRUE THEN DO:    
         FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.PRISTYP NE "FRÅNVARO." AND TIDREGITAB.OKOD1 = "" AND TIDREGITAB.OKOD2 = "" AND TIDREGITAB.OKOD3 = "" USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                    
         IF AVAILABLE TIDREGITAB  THEN DO:
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
            SUBSTRING(tidut.UT,44) = STRING(regvnr)
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            SUBSTRING(tidut.UT,57) = "Varning!Det borde vara veckovila – inte arbete (kontakta HR om annat överenskommet) " +  STRING(regdatum,"99/99/99").                           
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                
         END.
            /*Borttaget 20221101 Lena . De får inte arbeta under veckovila om inte företaget kräver det.
            FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.LONTILLAGG = "190"  USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                         
            IF NOT AVAILABLE TIDREGITAB  THEN DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Påminnelse!Vid arbete under veckovila skall lart 191 (flextid elnät) registreras som ersättning för arbetstiden.Kontrollera att det är gjort".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
            END.
         END.   */
      END.               
   END.
   IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:                              
      IF WEEKDAY(regdatum) = 6 THEN DO:               
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = (regdatum - 1) AND TIDREGITAB.BEREDSKAP  NE "" AND TIDREGITAB.BERANTAL > 1 USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
         IF AVAILABLE TIDREGITAB THEN DO:
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.BEREDSKAP  NE "" AND TIDREGITAB.BERANTAL > 1 USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
            IF NOT AVAILABLE TIDREGITAB  THEN DO:
               /*DET SKA VARA OK ATT REGISTRERA ÖVERTID INNAN ARBETSTID*/
               FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
               TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.PRISTYP NE "FRÅNVARO." AND TIDREGITAB.OKOD1 = "" AND TIDREGITAB.OKOD2 = "" AND TIDREGITAB.OKOD3 = "" USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                    
               IF AVAILABLE TIDREGITAB  THEN DO:
                  FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
                  TIDREGITAB.DATUM = regdatum  AND ( TIDREGITAB.LONTILLAGG = "4506" OR TIDREGITAB.LONTILLAGG = "4574") USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                         
                  IF NOT AVAILABLE TIDREGITAB  THEN DO:                              
                     CREATE tidut.
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44) = STRING(regvnr)
                     SUBSTRING(tidut.UT,53) = regdagnamn.  
                     SUBSTRING(tidut.UT,57) = "Påminnelse!Vid arbete under veckovila skall lart 215 registreras som ersättning för arbetstiden.Kontrollera att det är gjort".                        
                     IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                
                  END.
               END.
            END.
         END.
      END.               
   END.

END PROCEDURE.

PROCEDURE paminnpark_UI :   
   IF Guru.Konstanter:globforetag = "cLULE" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
      /*borttaget 20131025 Marie-Louise Gabrielsson*/                                        
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND  TIDREGITAB.LONTILLAGG = "930" USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
      IF NOT AVAILABLE TIDREGITAB THEN DO:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND  TIDREGITAB.LONTILLAGG = "931" USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
         IF NOT AVAILABLE TIDREGITAB  THEN DO:               
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
            SUBSTRING(tidut.UT,44) = STRING(regvnr)
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            SUBSTRING(tidut.UT,57) = "Påminnelse! Har du glömt registrera parkering".                        
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").                
         END.
      END.
   END.               
   
END PROCEDURE.

PROCEDURE atkfrisk_UI :   
   /*pausad*/
   IF Guru.Konstanter:globforetag = "Cgkal"   OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
      /*kalmar ska inte längre välja atk. De ska sättas till att alla får ta ut atk i tid vid årsskiftet 20230101 Lena 20220615*/                
      IF PERSONALTAB.BEFATTNING = "INHYRD PERSONAL" OR PERSONALTAB.ANSTALLNING = "ENTREP.AVTAL" OR PERSONALTAB.ANSTALLNING = "Extern konsult"  THEN .      
      ELSE DO:                            
         IF MONTH(bdatum) = 10 THEN DO:      
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            aratkfrisk = 0.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "ATKFRISK"                   
            inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.   
            inextradatatemp.HUVUDINT = YEAR(bdatum) + 1.         
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
            IF AVAILABLE extradatatemp THEN DO:      
               ASSIGN aratkfrisk = extradatatemp.HUVUDINT.         
            END.   
            IF aratkfrisk = year(bdatum) + 1 THEN DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Påminnelse- Du har rapporterat ATK och Friskvårdsval för nästa år. Vill du ändra valen för nästa år gör du det under Kontroll. ".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").
            END.   
            ELSE DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Påminnelse: Kom ihåg att rapportera ATK och Friskvårdsval för nästa år senast i november under Kontroll. ".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").            
            END.                
         END.
         IF MONTH(bdatum) = 11 THEN DO:      
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            aratkfrisk = 0.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "ATKFRISK"                   
            inextradatatemp.HUVUDCH = tidpers.PERSONALKOD. 
            inextradatatemp.HUVUDINT = YEAR(bdatum) + 1.           
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
            IF AVAILABLE extradatatemp THEN DO:      
               ASSIGN aratkfrisk = extradatatemp.HUVUDINT.         
            END.   
            IF aratkfrisk = year(bdatum) + 1 THEN DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Påminnelse- Du har rapporterat ATK och Friskvårdsval för nästa år. Vill du ändra valen för nästa år gör du det under Kontroll. ".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").
            END.   
            ELSE DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Uppdatera ditt ATK och friskvårdsval nedan innan du kan färdigrapportera din tid".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").            
            END.                
         END.
      END.   
   END.
   IF  Guru.Konstanter:globforetag = "sund"  OR Guru.Konstanter:globforetag = "elpa"  THEN DO:                
      /*se även ASFAKTAPP.P*/
      IF PERSONALTAB.ANSTALLNING = "ENTREP.AVTAL" OR PERSONALTAB.BEFATTNING = "Timanställd"  THEN .      
      ELSE DO:                            
         IF MONTH(bdatum) = 10 THEN DO:      
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            aratkfrisk = 0.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "ATKFRISK"                   
            inextradatatemp.HUVUDCH = tidpers.PERSONALKOD. 
            inextradatatemp.HUVUDINT = YEAR(bdatum) + 1.           
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
            IF AVAILABLE extradatatemp THEN DO:      
               ASSIGN aratkfrisk = extradatatemp.HUVUDINT.         
            END.   
            IF aratkfrisk = year(bdatum) + 1 THEN DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Påminnelse- Du har rapporterat ATK-val för nästa år. Vill du ändra valen för nästa år gör du det under Kontroll. ".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").
            END.   
            ELSE DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Uppdatera ditt ATK-val nedan innan du kan färdigrapportera din tid".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").            
            END.                
         END.
      END.   
   END. 
   IF  Guru.Konstanter:globforetag = "misv" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:                
      /*se även ASFAKTAPP.P*/
      IF PERSONALTAB.ANSTALLNING = "ENTREP.AVTAL" OR PERSONALTAB.BEFATTNING = "Timanställd" OR PERSONALTAB.BEFATTNING = "Extern konsult"  THEN .      
      ELSE DO:                            
         IF MONTH(bdatum) = 11 THEN DO:      
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            aratkfrisk = 0.
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "ATKFRISK"                   
            inextradatatemp.HUVUDCH = tidpers.PERSONALKOD. 
            inextradatatemp.HUVUDINT = YEAR(bdatum) + 1.           
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
            IF AVAILABLE extradatatemp THEN DO:      
               ASSIGN aratkfrisk = extradatatemp.HUVUDINT.         
            END.   
            IF aratkfrisk = year(bdatum) + 1 THEN DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Påminnelse- Du har rapporterat ATK-val för nästa år. Vill du ändra valen för nästa år gör du det under Kontroll. ".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").
            END.   
            ELSE DO:
               CREATE tidut.
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Uppdatera ditt ATK-val nedan innan du kan färdigrapportera din tid".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").            
            END.                
         END.
      END.   
   END.               
   
END PROCEDURE.

PROCEDURE beredkoll_UI :      
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:                                         
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND  TIDREGITAB.LONTILLAGG = "324" USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
      IF AVAILABLE TIDREGITAB THEN DO:
         IF PERSONALTAB.BEREDSKAPSAVTAL BEGINS "M" THEN.
         ELSE DO:                 
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
            SUBSTRING(tidut.UT,44) = STRING(regvnr)
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            SUBSTRING(tidut.UT,57) = "Endast personer med beredskapsavtal mätning har rätt till lart " + "324" .                        
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(bdatum,"99/99/99").                
         END.
      END.
   END.                 
END PROCEDURE.

PROCEDURE l302check_UI :      
   DEFINE VARIABLE lant AS INTEGER NO-UNDO.
   IF Guru.Konstanter:globforetag = "misv" THEN DO:
      lant = 0.
      OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.LONTILLAGG = "302"  NO-LOCK.
      GET FIRST toq NO-LOCK.
      DO WHILE AVAILABLE (TIDREGITAB):
         lant = lant + TIDREGITAB.LONTILLANTAL.      
         GET NEXT toq NO-LOCK.
      END.
      IF lant > 2 THEN DO:
         CREATE tidut.               
         ASSIGN
         SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
         SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
         SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
         SUBSTRING(tidut.UT,57) = "Du har registrerat " + STRING(lant) + " st lart 302 denna månad. Max är 2 st/månad".
      END.   
   END.   
END PROCEDURE.

PROCEDURE kollovgod_UI :      
   DEFINE VARIABLE lant AS INTEGER NO-UNDO.
   DEFINE VARIABLE obeordg AS LOGICAL NO-UNDO.
   
   obeordg = FALSE.
   IF Guru.Konstanter:AppSpringSet[1] = "USUNDNAT" THEN  obeordg = TRUE. 
   IF Guru.Konstanter:AppSpringSet[1] = "SUNDNAT" THEN obeordg = TRUE.
   IF Guru.Konstanter:AppSpringSet[1] = "SNEUTBI" THEN  obeordg = TRUE. 
   
   IF obeordg = TRUE THEN DO:      
      EMPTY TEMP-TABLE ltillagg NO-ERROR.
      OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.VECKOKORD = "" NO-LOCK.
      GET FIRST toq NO-LOCK.
      DO WHILE AVAILABLE (TIDREGITAB):
         IF TIDREGITAB.OKOD1 = "" AND TIDREGITAB.OKOD2 = "" AND TIDREGITAB.OKOD3 = "" THEN .
         ELSE DO:
            IF TIDREGITAB.LAGBAS = FALSE THEN DO:
               FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = SUBSTRING(TIDREGITAB.RESMAL,159,6)  NO-LOCK NO-ERROR.
               FIND FIRST persbuff WHERE persbuff.PERSONALKOD = tidpers.PERSONALKOD NO-LOCK NO-ERROR.
               /*de som har AVTALAT BORT ÖVERTID MEN HAR ÖVERTID VID BEREDSKAP ANGER INTE ÖVERTIDBEORDARE */            
               IF persbuff.OVERTIDUTTAG  = "I" THEN.
               ELSE DO: 
                  regdatum = TIDREGITAB.DATUM.
                  RUN REGDAG.P.
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)
                  SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").               
                  SUBSTRING(tidut.UT,53) = regdagnamn.   
                  IF NOT AVAILABLE PERSONALTAB THEN DO:                            
                     SUBSTRING(tidut.UT,57) = "Övertid är ej godkänd. Kontakta övertidsbeordare  " +  SUBSTRING(TIDREGITAB.RESMAL,159,6).
                  END.   
                  ELSE DO:
                     SUBSTRING(tidut.UT,57) = "Övertid är ej godkänd. Kontakta övertidsbeordare " +  SUBSTRING(TIDREGITAB.RESMAL,159,6) + "-" +  PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
                  END.
               END.         
            END.
         END.
               
         GET NEXT toq NO-LOCK.
      END.
         
   END.   
END PROCEDURE.

PROCEDURE kollkompledsaldo_UI:      
   DEFINE VARIABLE acckompvman  AS DECIMAL NO-UNDO.
   DEFINE VARIABLE acckompvmandat AS DATE NO-UNDO.
   DEFINE VARIABLE acckompsenast AS DECIMAL NO-UNDO.
   DEFINE VARIABLE acckompsenasttot AS DECIMAL NO-UNDO.
   DEFINE VARIABLE acckompsenastdat AS DATE NO-UNDO.
   DEFINE VARIABLE komputtag AS DECIMAL NO-UNDO.
   DEFINE VARIABLE antmult AS DECIMAL NO-UNDO.
   DEFINE VARIABLE komplog AS LOGICAL NO-UNDO.
   DEFINE VARIABLE kompapph AS HANDLE NO-UNDO.
   /* kontroll att man inte tar ut mer komledigt än man har arbetat in*/

   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      RUN FINNSTABELL.P (INPUT "KOMPSALDO", OUTPUT komplog).
      IF komplog = TRUE THEN DO:   
         IF NOT VALID-HANDLE(kompapph) THEN RUN KOMPSMANTAB.P PERSISTENT SET kompapph.
         IF VALID-HANDLE(kompapph) THEN DO:
            
            RUN viskompsal IN kompapph (INPUT tidpers.PERSONALKOD, INPUT avdatum, OUTPUT acckompvman, OUTPUT acckompvmandat, OUTPUT acckompsenast,  OUTPUT acckompsenastdat).            
            komputtag = 0.
            OPEN QUERY toUq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND TIDREGITAB.VECKOKORD = ""
            AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.AONR = "170"  NO-LOCK.
            GET FIRST touq NO-LOCK.
            DO WHILE AVAILABLE (TIDREGITAB):
               komputtag = komputtag + klock100(TIDREGITAB.TOTALT).
               GET NEXT touq NO-LOCK. 
            END.   
            antmult = 0.
            OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND TIDREGITAB.VECKOKORD = ""
            AND TIDREGITAB.DATUM LE avdatum AND  TIDREGITAB.OVERTIDUTTAG = "K"  NO-LOCK.
            GET FIRST toq NO-LOCK.
            DO WHILE AVAILABLE (TIDREGITAB):
               IF TIDREGITAB.OKOD1 = "" AND TIDREGITAB.OKOD2 = "" AND TIDREGITAB.OKOD3 = "" THEN.
               ELSE DO:                                 
                  IF TIDREGITAB.OANT1 > 0  THEN DO:
                     FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD1 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
                     IF AVAILABLE  OVERKOD THEN DO:                                
                        antmult = antmult + ((1 + OVERKOD.ERSATTNING) * klock100(TIDREGITAB.OANT1)).
                     END.
                  END.
                  IF TIDREGITAB.OANT2 > 0  THEN DO:
                     FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD2 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
                     IF AVAILABLE  OVERKOD THEN DO:                                
                        antmult = antmult + ((1 + OVERKOD.ERSATTNING) * klock100(TIDREGITAB.OANT2)).
                     END.
                  END.  
                  IF TIDREGITAB.OANT3 > 0  THEN DO:
                     FIND FIRST OVERKOD WHERE OVERKOD.OVERTIDTILL = TIDREGITAB.OKOD3 AND OVERKOD.KOD = ANSTFORMTAB.KOD USE-INDEX OVER NO-LOCK NO-ERROR.
                     IF AVAILABLE  OVERKOD THEN DO:                                
                        antmult = antmult + ((1 + OVERKOD.ERSATTNING) * klock100(TIDREGITAB.OANT3)).
                     END.
                  END. 
               END.
               GET NEXT toq NO-LOCK.
            END.         
           
           
            /*berdskapstolkning när veckvila inträffar under klämdag genererar lart 260 = Fyllnadslön mot ledighet vid beredskap Lena 20190611*/      
            OPEN QUERY tblq FOR EACH TIDREGITAB WHERE
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND TIDREGITAB.VECKOKORD = "" AND  TIDREGITAB.DATUM LE avdatum  AND
            TIDREGITAB.LONTILLAGG = "260"  USE-INDEX PSTART NO-LOCK.   
            GET FIRST tblq NO-LOCK.
            DO WHILE AVAILABLE(TIDREGITAB):
               antmult = antmult +  klock100(TIDREGITAB.LONTILLANTAL).              
               GET NEXT tblq NO-LOCK.      
            END.
            /*berdskapstolkning klämdagar halvdagar genererar lart 260 = Fyllnadslön mot ledighet vid beredksp Lena 20190611*/      
            OPEN QUERY tbq FOR EACH TIDREGITAB WHERE
            TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND TIDREGITAB.VECKOKORD = "" AND  TIDREGITAB.DATUM LE avdatum AND
            TIDREGITAB.BEREDSKAP = "260"  USE-INDEX PSTART NO-LOCK.   
            GET FIRST tbq NO-LOCK.
            DO WHILE AVAILABLE(TIDREGITAB):
               antmult = antmult +  klock100(TIDREGITAB.BERANTAL).               
               GET NEXT tbq NO-LOCK.      
            END.        
          
                                                    
            acckompsenasttot  = klock60( acckompsenast + antmult - komputtag ).              
            IF acckompsenasttot < 0 THEN DO:
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
               SUBSTRING(tidut.UT,57) = "För mycket kompledighet inlagt. Inarb komp: " + STRING(klock100(acckompsenast + antmult),">9.99") + " Uttagen komp: " + STRING(klock100(komputtag),">9.99").

            END.              
         END.
      END.
   END.   
      
END PROCEDURE.



PROCEDURE l778check_UI :      
   DEFINE VARIABLE lant AS INTEGER NO-UNDO.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:      
      EMPTY TEMP-TABLE ltillagg NO-ERROR.
      OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.LONTILLAGG = "778"  NO-LOCK.
      GET FIRST toq NO-LOCK.
      DO WHILE AVAILABLE (TIDREGITAB):
         FIND FIRST ltillagg WHERE ltillagg.RDATUM = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ltillagg THEN DO:
            CREATE ltillagg.
            ASSIGN ltillagg.RDATUM  = TIDREGITAB.DATUM. 
         END.
         ltillagg.LONTILLANTAL  = ltillagg.LONTILLANTAL + TIDREGITAB.LONTILLANTAL.
               
         GET NEXT toq NO-LOCK.
      END.
      FIND FIRST ltillagg WHERE ltillagg.LONTILLANTAL > 1 NO-LOCK NO-ERROR.
      IF AVAILABLE ltillagg THEN DO:      
         CREATE tidut.               
         ASSIGN
         SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
         SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
         SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
         SUBSTRING(tidut.UT,57) = "Du har registrerat " + STRING(ltillagg.LONTILLANTAL) + " st lart 778 " + STRING(ltillagg.RDATUM) + ". Max är 1 st/dag.".
      END.   
   END.   
END PROCEDURE.

PROCEDURE frvtimVECKA_UI :      
   IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                   
      RUN aokollve_UI (INPUT "140").
   END.
END PROCEDURE.


PROCEDURE aokollve_UI :      
   DEFINE INPUT PARAMETER friskao AS CHARACTER NO-UNDO.
   DEFINE VARIABLE  veckkoll AS INTEGER.   
   ASSIGN
   frisktot = 0
   veckkoll = 0.            
   OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
   TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.AONR = friskao
   AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.GODKAND = "" NO-LOCK.
   GET FIRST toq NO-LOCK.
   DO WHILE AVAILABLE (TIDREGITAB):
      
      IF veckkoll = 0 THEN DO:
         
         FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
         tidbuff.veckonummer = TIDREGITAB.VECKONUMMER AND tidbuff.DATUM < bdatum AND tidbuff.DATUM >  DATE(01,01,YEAR(bdatum)) AND tidbuff.AONR = friskao  AND tidbuff.TIDLOG = TRUE AND tidbuff.GODKAND = "" NO-LOCK:
            frisktot = frisktot + klock100(tidbuff.TOTALT).            
         END.
      END.   
      IF veckkoll = TIDREGITAB.VECKONUMMER  OR veckkoll = 0 THEN DO:
         veckkoll = TIDREGITAB.VECKONUMMER.                   
         frisktot = frisktot + klock100(TIDREGITAB.TOTALT).
         IF frisktot > 1.25 THEN LEAVE.
      END.   
      ELSE DO:
         frisktot = klock100(TIDREGITAB.TOTALT).
      END.   
      veckkoll = TIDREGITAB.VECKONUMMER.  
      GET NEXT toq NO-LOCK.
   END.
      
   frisktot = ROUND(frisktot,2).
   
   IF frisktot > 1.25  THEN DO:
      CREATE tidut.               
      ASSIGN
      SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
      SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
      SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
      SUBSTRING(tidut.UT,57) = "Du har skrivit " + STRING(klock60(frisktot),">9.99") + " tim på " + Guru.Konstanter:gaok + " " + friskao + " vecka " + string(veckkoll,"999") + ". Max är 1 tim 15 min/vecka". 
   END.                         
END PROCEDURE.

PROCEDURE frvtimkoll_UI :      
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:             
      IF Guru.Konstanter:globforetag = "ELPA"  THEN RUN aokoll_UI (INPUT "144030").
      RUN aokoll_UI (INPUT "061082").
      RUN aokoll_UI (INPUT "061023").
      RUN aokoll_UI (INPUT "810010").
      RUN aokoll_UI (INPUT "005345").
      RUN aokoll_UI (INPUT "061030").
   END.
END PROCEDURE.

PROCEDURE aokoll_UI :      
   DEFINE INPUT PARAMETER friskao AS CHARACTER NO-UNDO.   
   ASSIGN
   frisktot = 0.            
   OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
   AND TIDREGITAB.DATUM GE DATE(01,01,YEAR(bdatum)) AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.AONR = friskao
   AND TIDREGITAB.TIDLOG = TRUE NO-LOCK.
   GET FIRST toq NO-LOCK.
   DO WHILE AVAILABLE (TIDREGITAB):               
      frisktot = frisktot + klock100(TIDREGITAB.TOTALT).
     
      GET NEXT toq NO-LOCK.
   END.
   IF frisktot > 15 THEN DO:
      CREATE tidut.               
      ASSIGN
      SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
      SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
      SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
      SUBSTRING(tidut.UT,57) = "Du har tidskrivit " + STRING(frisktot,">9.99") + " tim på " + Guru.Konstanter:gaok + " " + friskao + ". Max friskvårdstim. per år är 15 ". 
   END.                         
END PROCEDURE.



PROCEDURE sjuk90_UI :
   DEFINE VARIABLE sjukdat AS DATE NO-UNDO.
   DEFINE VARIABLE sjukjmf AS DATE NO-UNDO.   
   DEFINE VARIABLE sjkoll AS INTEGER NO-UNDO.
   DEFINE VARIABLE avbrott AS LOGICAL NO-UNDO.
   avbrott = FALSE.
   EMPTY TEMP-TABLE sjtemp NO-ERROR.
   
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO: 
      FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
      AND TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND 
      ( TIDREGITAB.AONR = "110" OR TIDREGITAB.AONR = "180" OR TIDREGITAB.AONR = "181" OR TIDREGITAB.AONR = "182" ) 
      AND TIDREGITAB.TIDLOG = TRUE USE-INDEX aonr  NO-LOCK NO-ERROR.
      IF AVAILABLE TIDREGITAB THEN DO:
        
         sjukdat = TIDREGITAB.DATUM  - 100.
         EMPTY TEMP-TABLE sjtemp NO-ERROR. 
         OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
         AND TIDREGITAB.DATUM GE sjukdat AND TIDREGITAB.DATUM LE avdatum  AND TIDREGITAB.TIDLOG = TRUE NO-LOCK.
         GET FIRST toq NO-LOCK.
         DO WHILE AVAILABLE (TIDREGITAB):
            
            FIND FIRST sjtemp WHERE sjtemp.datum = TIDREGITAB.DATUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE sjtemp THEN DO:
               CREATE sjtemp.
               sjtemp.DATUM = TIDREGITAB.DATUM.            
            END.
            IF TIDREGITAB.AONR = "110" OR TIDREGITAB.AONR = "180" OR TIDREGITAB.AONR = "181" OR TIDREGITAB.AONR = "182" THEN DO:
               sjtemp.AONR =  TIDREGITAB.AONR.
            END.   
            GET NEXT toq NO-LOCK.
         END.
      
         FIND FIRST sjtemp  NO-LOCK NO-ERROR.
         IF AVAILABLE sjtemp THEN DO:
            IF sjtemp.DATUM GE bdatum THEN DO:
               sjukjmf = sjtemp.DATUM.
               sjkoll = 1.
               REPEAT:        
                  DEBUGGER:SET-BREAK().         
                  sjukjmf = sjukjmf - 1.
                  IF  sjukjmf < sjukdat THEN LEAVE.  
                  FIND FIRST sjtemp  WHERE sjtemp.datum = sjukjmf NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE sjtemp THEN DO:
                     IF avbrott = FALSE THEN  sjkoll = sjkoll + 1.
                  END.
                  IF AVAILABLE sjtemp THEN DO:
                     IF sjtemp.AONR NE "" THEN DO:
                        sjkoll = sjkoll + 1.
                        avbrott = FALSE.
                     END.
                     ELSE DO:
                        IF sjkoll < 90 THEN sjkoll = 0.                        
                        avbrott = TRUE.   
                     END.
                  END.
               END.               
            END.
            IF sjkoll GE 90 THEN DO:
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
               SUBSTRING(tidut.UT,57) = "Varning! Efter 90 dagar i sträck sjukskrivning bör du söka AFA. Du har " + STRING(sjkoll) + " dagar registrerade.".
            END.
         END.
      END.                   
   END.                          
END PROCEDURE.

PROCEDURE skyliftkoll_UI:      
   dagtot = 0.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
      AND TIDREGITAB.AONR = "005785" USE-INDEX PSTART NO-LOCK:
         FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
         tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = FALSE 
         AND tidbuff.AONR = "005785" AND tidbuff.LONTILLAGG = "SKYL" AND tidbuff.LONTILLANTAL = TIDREGITAB.TOTALT USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidbuff THEN DO:                  
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
            SUBSTRING(tidut.UT,44) = STRING(regvnr)
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            SUBSTRING(tidut.UT,57) = "Varning!Tid skriven på projekt 005785 men ingen Skylift är registerad " + STRING(regdatum,"99/99/99").          
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").          
         END.
      END.
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
      AND TIDREGITAB.AONR = "001831" USE-INDEX PSTART NO-LOCK:
         FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
         tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = FALSE 
         AND tidbuff.AONR = "001831" AND tidbuff.LONTILLAGG = "SKYL" AND tidbuff.LONTILLANTAL = TIDREGITAB.TOTALT USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidbuff THEN DO:                  
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
            SUBSTRING(tidut.UT,44) = STRING(regvnr)
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            SUBSTRING(tidut.UT,57) = "Varning!Tid skriven på projekt 001831  men ingen Skylift är registerad " + STRING(regdatum,"99/99/99").          
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").          
         END.
      END.
   END.    

END PROCEDURE.

PROCEDURE utbkoll_UI:      
   
   dagtot = 0.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND
      TIDREGITAB.AONR = "250" USE-INDEX PSTART NO-LOCK:
          dagtot = dagtot + klock100(TIDREGITAB.TOTALT).
      END.     
      IF dagtot > 8 THEN DO:                   
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = (regdatum + 1)  AND TIDREGITAB.TIDLOG = TRUE AND
         ( TIDREGITAB.AONR = "250" OR TIDREGITAB.AONR = "255")   USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                         
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
            SUBSTRING(tidut.UT,57) = "Vid flerdygnsutb. ersätts max 8 tim/dag. Du har "  + STRING(dagtot) + " tim. Ta bort överstig.".
         END.                   
         ELSE DO:
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = (regdatum -  1)  AND TIDREGITAB.TIDLOG = TRUE AND
             (TIDREGITAB.AONR = "250" OR TIDREGITAB.AONR = "255")   USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO:
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                         
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               SUBSTRING(tidut.UT,57) = "Vid flerdygnsutb. ersätts max 8 tim/dag. Du har "  + STRING(dagtot) + " tim. Ta bort överstig.". 
            END.    
         END.    
      END.  
   END.                 
END PROCEDURE.


PROCEDURE spfrisk_UI :      
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                                 
      /*spärr friskvårdstimmar. då ska det ej gå att ta ut i tid*/      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      dispens = FALSE.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "SPFRISK"                   
      inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.            
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN spafr = extradatatemp.SOKLOG[1].         
      END.   
      ELSE spafr = FALSE.                    
      IF spafr = TRUE THEN DO:            
         IF Guru.Konstanter:globforetag = "GKAL" THEN DO:         
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND
            (TIDREGITAB.AONR = "061082" OR TIDREGITAB.AONR = "061023" OR TIDREGITAB.AONR = "810010" OR TIDREGITAB.AONR = "005345" OR TIDREGITAB.AONR = "061030")
            USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                       
         END.
         IF Guru.Konstanter:globforetag = "elpa" THEN DO:         
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND
            (TIDREGITAB.AONR = "144030" OR TIDREGITAB.AONR = "061023" OR TIDREGITAB.AONR = "810010" OR TIDREGITAB.AONR = "005345" OR TIDREGITAB.AONR = "061030")
            USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                       
         END.
         IF AVAILABLE TIDREGITAB THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                         
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
            SUBSTRING(tidut.UT,57) = "Friskvårdsbidrag uttaget, då går det ej att registrera tid på  " + Guru.Konstanter:gaok + " "  + TIDREGITAB.AONR + ". Kontakta lönepersonal".
         END.                   
         
      END.
   END.                 
END PROCEDURE.

PROCEDURE oblkomm_UI :
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR  Guru.Konstanter:globforetag = "elpa" THEN DO:
      kravkomm = "".                              
      IF  Guru.Konstanter:globforetag = "SNAT" THEN DO:
         /*119 föräldrapenning får man ta ut 60 dagar innan barns födelse*/
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND (TIDREGITAB.AONR = "117" OR TIDREGITAB.AONR = "118" OR TIDREGITAB.AONR = "119"
         OR TIDREGITAB.AONR = "191" OR TIDREGITAB.AONR = "192" OR TIDREGITAB.AONR = "193" OR TIDREGITAB.AONR = "194" ) USE-INDEX PSTART NO-LOCK:                                                                        
            IF TIDREGITAB.RESMAL = "" THEN DO:
               kravkomm = TIDREGITAB.AONR.                  
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Barnets personnummer måste vara ifyllt på " + Guru.Konstanter:gaok + kravkomm.                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               kravkomm = "".                
            END.
         END.
      END.
      ELSE DO:
         /*119 föräldrapenning får man ta ut 60 dagar innan barns födelse*/
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum  AND TIDREGITAB.TIDLOG = TRUE AND (TIDREGITAB.AONR = "117" /*OR TIDREGITAB.AONR = "118"*/ OR TIDREGITAB.AONR = "119" ) USE-INDEX PSTART NO-LOCK:                                                                        
            IF TIDREGITAB.RESMAL = "" THEN DO:
               kravkomm = TIDREGITAB.AONR.                  
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Barnets personnummer måste vara ifyllt på " + Guru.Konstanter:gaok + kravkomm.                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               kravkomm = "".                
            END.
         END.
      END.      
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kravkomm = "".
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.GODKAND = "" 
         AND (TIDREGITAB.AONR = "135" OR TIDREGITAB.AONR = "S85209") USE-INDEX PSTART NO-LOCK:                                                                        
            IF TIDREGITAB.RESMAL = "" THEN DO:
               kravkomm = TIDREGITAB.AONR.                  
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Det är obligatorisk med kommentar på " + Guru.Konstanter:gaok  + " " + kravkomm.                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               kravkomm = "".                
            END.
         END.       
         kravkomm = "".
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.GODKAND = "" AND TIDREGITAB.RESMAL = "" USE-INDEX PSTART NO-LOCK:                                                                                          
            IF TIDREGITAB.AONR = "00003" THEN kravkomm = "00003".
            IF TIDREGITAB.AONR = "21471" AND TIDREGITAB.DELNR = 1 THEN kravkomm = "21471".
            IF TIDREGITAB.AONR = "80014"  THEN kravkomm = "80014".
            IF TIDREGITAB.AONR = "80015"  THEN kravkomm = "80015". 
            IF TIDREGITAB.AONR = "S10019"  THEN kravkomm = "S10019".
            IF TIDREGITAB.AONR = "S15053"  THEN kravkomm = "S15053".
            IF TIDREGITAB.AONR = "S15054"  THEN kravkomm = "S15054".
            IF TIDREGITAB.AONR = "S18001"  THEN kravkomm = "S18001".
            IF TIDREGITAB.AONR = "S18051"  THEN kravkomm = "S18051".
            IF TIDREGITAB.AONR = "S18054"  THEN kravkomm = "S18054".
            IF TIDREGITAB.AONR = "S18057"  THEN kravkomm = "S18057".
            IF TIDREGITAB.AONR = "S20001"  THEN kravkomm = "S20001".
            IF TIDREGITAB.AONR = "S20102"  THEN kravkomm = "S20102".
            IF TIDREGITAB.AONR = "S21001"  THEN kravkomm = "S21001".
            IF TIDREGITAB.AONR = "S22001"  THEN kravkomm = "S22001".
            IF TIDREGITAB.AONR = "S23001"  THEN kravkomm = "S23001".
            IF TIDREGITAB.AONR = "S24001"  THEN kravkomm = "S24001".
            IF TIDREGITAB.AONR = "S25001"  THEN kravkomm = "S25001".
            IF TIDREGITAB.AONR = "S26001"  THEN kravkomm = "S26001".
            IF TIDREGITAB.AONR = "S27001"  THEN kravkomm = "S27001".
            IF TIDREGITAB.AONR = "S29001"  THEN kravkomm = "S29001".
            IF TIDREGITAB.AONR = "S40001"  THEN kravkomm = "S40001".            
            IF TIDREGITAB.AONR = "S41001"  THEN kravkomm = "S41001".            
            IF TIDREGITAB.AONR = "S44001"  THEN kravkomm = "S44001".
            IF TIDREGITAB.AONR = "S81001"  THEN kravkomm = "S81001".
            IF TIDREGITAB.AONR = "S81101"  THEN kravkomm = "S81101".
            IF TIDREGITAB.AONR = "S83001"  THEN kravkomm = "S83001".
            IF TIDREGITAB.AONR = "S85101"  THEN kravkomm = "S85101".
            IF TIDREGITAB.AONR = "S85201"  THEN kravkomm = "S85201".
            IF TIDREGITAB.AONR = "S85301"  THEN kravkomm = "S85301".
            IF TIDREGITAB.AONR = "S83101"  THEN kravkomm = "S83101".
            IF TIDREGITAB.AONR = "S86001"  THEN kravkomm = "S86001".
            IF TIDREGITAB.AONR = "S86002"  THEN kravkomm = "S86002".
            IF TIDREGITAB.AONR = "S86003"  THEN kravkomm = "S86003".
            IF TIDREGITAB.AONR = "S86004"  THEN kravkomm = "S86004".
            IF TIDREGITAB.AONR = "S86020"  THEN kravkomm = "S86020".
            IF TIDREGITAB.AONR = "S86122"  THEN kravkomm = "S86122".
            IF TIDREGITAB.AONR = "S88001"  THEN kravkomm = "S88001".                                 
             
            IF kravkomm NE "" THEN DO:                                 
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.                 
               SUBSTRING(tidut.UT,57) = "Utbildningens och utbildningsföretagets namn är obligatorisk på " + Guru.Konstanter:gaok + " " + kravkomm.                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               kravkomm = "".
            END.                                        
         END.
      END.
      IF Guru.Konstanter:globforetag = "MISV"  THEN DO:
         kravkomm = "".
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.GODKAND = "" AND
         (TIDREGITAB.AONR = "135" OR TIDREGITAB.AONR = "181001" OR TIDREGITAB.AONR = "181002") USE-INDEX PSTART NO-LOCK:                                                                                    
            IF TIDREGITAB.RESMAL = "" THEN DO:
               kravkomm = TIDREGITAB.AONR.                  
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               IF TIDREGITAB.AONR = "181001" THEN SUBSTRING(tidut.UT,57) = "Det är obligatoriskt att ange kursens namn på " + Guru.Konstanter:gaok + " " + kravkomm.  
               ELSE SUBSTRING(tidut.UT,57) = "Det är obligatorisk med kommentar på detta" + Guru.Konstanter:gaok + " " + kravkomm.                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               kravkomm = "".                
            END.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "LULE"  THEN DO:
      kravkomm = "".
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.GODKAND = "" AND
      TIDREGITAB.AONR = "250"  USE-INDEX PSTART NO-LOCK:                                                                                    
         IF TIDREGITAB.RESMAL = "" THEN DO:
            kravkomm = TIDREGITAB.AONR.                  
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
            SUBSTRING(tidut.UT,44) = STRING(regvnr)
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            SUBSTRING(tidut.UT,57) = "Kommentar om vad utbildningen avser är obligatorisk på " + Guru.Konstanter:gaok + " " + kravkomm.                                       
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
            kravkomm = "".                
         END.
      END.
   END.   
                    
END PROCEDURE.

PROCEDURE ftro_UI :      
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "sund"  OR Guru.Konstanter:globforetag = "TECM" THEN DO:                             
      /*förtroendetid skriver endast frånvaro SUNDSVALL
      kalmar skriver fritt tid*/      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      ftro = FALSE.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FORTRO"                   
      inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.            
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
        ftro = extradatatemp.SOKLOG[1].         
      END.   
      ELSE ftro = FALSE.                        
   END.                 
END PROCEDURE.
PROCEDURE tillit_UI :      
   IF Guru.Konstanter:globforetag = "GKAL"  THEN DO:                             
            
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      ftro = FALSE.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "TILLIT"                   
      inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.            
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
        tillit = extradatatemp.SOKLOG[1].         
      END.   
      ELSE tillit = FALSE.                        
   END.                 
END PROCEDURE.

PROCEDURE ftrocheck_UI :      
   IF Guru.Konstanter:globforetag = "csund"  THEN DO:                             
      /*förtroendetid skriver endast frånvaro*/      
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.TIDLOG = TRUE AND  TIDREGITAB.PRISTYP NE "FRÅNVARO." USE-INDEX PSTART NO-LOCK NO-ERROR.                                                                      
      IF AVAILABLE TIDREGITAB THEN DO:                         
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
         SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
         SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
         SUBSTRING(tidut.UT,44) = STRING(regvnr)
         SUBSTRING(tidut.UT,53) = regdagnamn.  
         SUBSTRING(tidut.UT,57) = "Enbart frånvaro skall registreras på personer med förtroendetid".                                           
         IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").                         
      END.                       
   END.                 
END PROCEDURE.

PROCEDURE dagcheck_UI :
   
   IF Guru.Konstanter:globforetag = "GKAL" OR  Guru.Konstanter:globforetag = "elpa" THEN DO:                              
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.AONR = "172" AND TIDREGITAB.START = regstart
      AND TIDREGITAB.SLUT = regslut USE-INDEX PSTART NO-LOCK:                                                                                          
         CREATE tidut.               
         ASSIGN
         SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
         SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
         SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
         SUBSTRING(tidut.UT,44) = STRING(regvnr)
         SUBSTRING(tidut.UT,53) = regdagnamn.  
         SUBSTRING(tidut.UT,57) = "Dubbelcheck:Du har tidskrivit på dygnsvila (172). Är du säker på att " +
         "det inte ska vara veckovila (170)? Om det är fel , byt projekt i tidredovisningen.".                        
         IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                
      END.
   END.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:                              
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG BEGINS "72" USE-INDEX PSTART NO-LOCK:                                    
         IF TIDREGITAB.LONTILLAGG = "726" OR TIDREGITAB.LONTILLAGG = "727" OR TIDREGITAB.LONTILLAGG = "728"  THEN DO:
            IF TIDREGITAB.LONTILLANTAL > 0 THEN DO:                     
               FIND FIRST  tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
               tidbuff.DATUM = regdatum AND tidbuff.TRAKTKOD BEGINS "70" USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE tidbuff THEN DO:                        
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                  SUBSTRING(tidut.UT,44) = STRING(regvnr)
                  SUBSTRING(tidut.UT,53) = regdagnamn.  
                  SUBSTRING(tidut.UT,57) = "Det finns måltidsavdrag men inget resetillägg.Ändra till traktamentszon 1.".                        
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                  
               END.                   
            END.
         END.
      END.
   END.
   IF  Guru.Konstanter:globforetag = "SNAT"  THEN DO:                              
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = "843" AND TIDREGITAB.LONAUTO = FALSE USE-INDEX PSTART NO-LOCK:                                                      
         IF TIDREGITAB.LONTILLANTAL > 0 THEN DO:                     
            FIND FIRST  tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
            tidbuff.DATUM = regdatum AND tidbuff.LONTILLAGG = "778" USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidbuff THEN DO:                        
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Det finns måltidsavdrag men inget resetillägg.Ta bort måltidsavdraget.".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                  
            END.                                  
         END.
      END.
   END.   
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:                              
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = "8249" USE-INDEX PSTART NO-LOCK:                                                      
         IF TIDREGITAB.LONTILLANTAL > 0 THEN DO:                     
            FIND FIRST  tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
            tidbuff.DATUM = regdatum AND tidbuff.TRAKTKOD = "8972" USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidbuff THEN DO:                        
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Det finns måltidsavdrag men inget resetillägg.Ändra till traktamentszon 1.".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                  
            END.                                  
         END.
      END.
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = "7212" USE-INDEX PSTART NO-LOCK:                                                      
         IF TIDREGITAB.LONTILLANTAL > 0 THEN DO:                     
            FIND FIRST  tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
            tidbuff.DATUM = regdatum AND tidbuff.TRAKTKOD NE "" USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidbuff THEN DO:                
               FIND FIRST  tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
               tidbuff.DATUM = regdatum AND tidbuff.LONTILLAGG = "8206" USE-INDEX PSTART NO-LOCK NO-ERROR.
               IF NOT AVAILABLE tidbuff THEN DO:                
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                  SUBSTRING(tidut.UT,44) = STRING(regvnr)
                  SUBSTRING(tidut.UT,53) = regdagnamn.  
                  SUBSTRING(tidut.UT,57) = "Det finns kostförmån men inget traktamente.Ändra till traktamentszon 1.".                        
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                
               END.
            END.                                  
         END.
      END.
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG = "4506" AND TIDREGITAB.LONAUTO = FALSE USE-INDEX PSTART NO-LOCK:                                                      
            IF TIDREGITAB.LONTILLANTAL > 0 THEN DO:                     
               dagtot = 0.
               FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
               tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE NO-LOCK:
                  dagtot = dagtot + klock100(tidbuff.TOTALT).
               END.
               IF (dagtot - TRUNCATE(dagtot,0)) GE 0 AND ( dagtot - TRUNCATE(dagtot,0)) < 0.26 THEN dagtot = TRUNCATE(dagtot,0).
               ELSE IF (dagtot - TRUNCATE(dagtot,0)) GE 0.26  AND ( dagtot - TRUNCATE(dagtot,0)) < 0.76 THEN dagtot = TRUNCATE(dagtot,0) + 0.50.
               ELSE  IF (dagtot - TRUNCATE(dagtot,0)) GE 0.76  THEN dagtot = TRUNCATE(dagtot,0) + 1.                  
               IF dagtot < TIDREGITAB.LONTILLANTAL THEN DO:                     
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                  SUBSTRING(tidut.UT,44) = STRING(regvnr)
                  SUBSTRING(tidut.UT,53) = regdagnamn.  
                  SUBSTRING(tidut.UT,57) = "Du har registrerat " + STRING(TIDREGITAB.LONTILLANTAL) + " timmar Lart 215, men har bara " + STRING(dagtot) + " timmar i arbetstid".                        
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                  
               END.                                  
            END.
         END.
      END.   
   END.               
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR  Guru.Konstanter:globforetag = "elpa" THEN DO:                              
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.AONR BEGINS "94" USE-INDEX PSTART NO-LOCK:                                                                        
         IF TIDREGITAB.AONR = "940" OR TIDREGITAB.AONR = "941" OR TIDREGITAB.AONR = "949" THEN DO:                  
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
            SUBSTRING(tidut.UT,44) = STRING(regvnr)
            SUBSTRING(tidut.UT,53) = regdagnamn.  
            SUBSTRING(tidut.UT,57) = Guru.Konstanter:gaok + " " + TIDREGITAB.AONR + " används inte för tidskrivande bolag".                        
            IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                
         END.
      END.
      
   END.
   RUN oblkomm_UI.              
   RUN paminn_UI.
   RUN spfrisk_UI.
   RUN utbkoll_UI.
   RUN skyliftkoll_UI.
   
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /* kontroll bilar*/
      heldag = 0.
      EMPTY TEMP-TABLE ltillagg NO-ERROR. 
      EMPTY TEMP-TABLE bilkoll  NO-ERROR.                
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE
      AND TIDREGITAB.PRISTYP NE "FRÅNVARO." USE-INDEX PSTART NO-LOCK:
         nytid = TIDREGITAB.TOTALT.
         RUN TIMSEK.P.
         kolltid = sekunder / 3600.
         heldag = heldag + kolltid.
      END.
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
      TIDREGITAB.DATUM = regdatum AND TIDREGITAB.LONTILLAGG NE "" NO-LOCK:
         nytid = TIDREGITAB.LONTILLANTAL.
         RUN TIMSEK.P.
         CREATE ltillagg.
         ASSIGN ltillagg.LONTILLAGG = TIDREGITAB.LONTILLAGG
         ltillagg.LONTILLANTAL = sekunder / 3600. 
      END.
      FOR EACH ltillagg BREAK BY ltillagg.LONTILLAGG :
      ACCUMULATE ltillagg.LONTILLANTAL (TOTAL BY ltillagg.LONTILLAGG).
         IF LAST-OF(ltillagg.LONTILLAGG) THEN DO: 
            CREATE bilkoll.
            ASSIGN
            bilkoll.LONTILLAGG = ltillagg.LONTILLAGG
            bilkoll.LONTILLANTAL = (ACCUM TOTAL BY ltillagg.LONTILLAGG ltillagg.LONTILLANTAL).     
         END.
      END.
      FOR EACH bilkoll WHERE bilkoll.LONTILLANTAL > heldag :
         FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = bilkoll.LONTILLAGG
         AND SUBSTRING(LONTILL.TYPKOD,1,3) = "BIL" NO-LOCK NO-ERROR.
         IF AVAILABLE LONTILL THEN DO: 
            IF Guru.Konstanter:globforetag = "gkal" THEN DO:
                IF LONTILL.LONTILLAGG = "SKYL" THEN DO:
                                    
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                  SUBSTRING(tidut.UT,44) = STRING(regvnr)
                  SUBSTRING(tidut.UT,53) = regdagnamn.  
                  SUBSTRING(tidut.UT,57) = "Bil: " + SUBSTRING(bilkoll.LONTILLAGG,1,4) +  " är registrerad med " +  STRING(bilkoll.LONTILLANTAL,">9.99") +  " timmar. Totala arbetstiden är " + STRING(heldag,">9.99") + " timmar".
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               END.
            END.
            ELSE DO:   
               IF LONTILL.LONTILLAGG BEGINS "4" THEN.
               ELSE DO:                     
                  CREATE tidut.               
                  ASSIGN
                  SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                  SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                  SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                  SUBSTRING(tidut.UT,44) = STRING(regvnr)
                  SUBSTRING(tidut.UT,53) = regdagnamn.  
                  SUBSTRING(tidut.UT,57) = "Bil: " + SUBSTRING(bilkoll.LONTILLAGG,1,5) +  " är registrerad med " +  STRING(bilkoll.LONTILLANTAL,">9.99") +  " timmar. Totala arbetstiden är " + STRING(heldag,">9.99") + " timmar".
                  IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").
               END.
            END.   
         END.
      END.            
   END.                      
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                           
      seku = 0.
      IF PERSONALTAB.BEREDSKAPSAVTAL = "I" THEN DO:
         IF FLEXAVT.FLEXTID = TRUE THEN DO:               
            FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD AND
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.OKOD1 NE "" USE-INDEX PSTART NO-LOCK:                                                      
               IF TIDREGITAB.OANT1 > 0 THEN DO:                     
                  FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = tidpers.PERSONALKOD AND
                  tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE AND tidbuff.OVERTIDUTTAG = "F" USE-INDEX PSTART NO-LOCK:
                     IF tidbuff.AONR = "155" AND tidbuff.DELNR = 0 THEN musz = musz.
                     ELSE IF tidbuff.AONR = "160" AND tidbuff.DELNR = 0 THEN.
                     ELSE IF tidbuff.AONR = "170" AND tidbuff.DELNR = 0 THEN.
                     ELSE DO:                        
                        nytid = tidbuff.TOTALT.
                        RUN TIMSEK.P.
                        seku = seku + sekunder.
                     END.
                  END.
                  sekunder = seku.
                  RUN SEKTIM.P.
                  IF nytid < regtotalt THEN DO:                                          
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
                     SUBSTRING(tidut.UT,44,8) = STRING(regdatum,"99/99/99").                  
                     SUBSTRING(tidut.UT,53) = regdagnamn.  
                     SUBSTRING(tidut.UT,57) = "Full arbetstid måste vara registrerad innan övertid accepteras".                                                
                  END.                                  
               END.
            END.
         END.   
      END.
   END.               
END PROCEDURE.

PROCEDURE mancheck_UI :
   RUN sjjuni_UI.
   RUN paminnpark_UI.
   RUN beredkoll_UI.
   RUN frvtimkoll_UI.
   RUN atkfrisk_UI.
   
   RUN frvtimVECKA_UI.
   RUN l302check_UI.
   RUN l778check_UI.
   RUN kollovgod_UI.
   RUN kollkompledsaldo_UI. 
   RUN sjuk90_UI.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
      /*Max 50 övertidstimma per månad , max 200 övertidtimmar per år*/
      /*Nya regler allmän övertid 200 tim/kalkenderår , extra övertid + 150 timmar vid särskilda skäl
      Lena 20171114
      */            
      /*bytt till 150 tim 20171114 för att de ska ha en chans att ta ut komp för att få jobba 50 tim ytterligare.*/
      IF Guru.Konstanter:globforetag = "MISV" THEN varngr = 150.
      ELSE if Guru.Konstanter:globforetag = "SUND" THEN varngr = 150.
      ELSE if Guru.Konstanter:globforetag = "SNAT" THEN varngr = 150.
      ELSE if Guru.Konstanter:globforetag = "gkal" THEN varngr = 150.
      ELSE varngr = 150. 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      dispens = FALSE.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "DISPENSÖ"                   
      inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.            
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN dispens = extradatatemp.SOKLOG[1].         
      END.   
      ELSE dispens = FALSE.                          
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      dispensm = FALSE.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "DISPENSÖM"                   
      inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.            
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN dispensm = extradatatemp.SOKLOG[1].         
      END.   
      ELSE dispensm = FALSE.            
      ASSIGN
      ovar = 0
      ovman = 0.
      OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
      AND TIDREGITAB.DATUM GE DATE(01,01,YEAR(bdatum)) AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.OKOD1 NE ""  NO-LOCK.
      GET FIRST toq NO-LOCK.
      DO WHILE AVAILABLE (TIDREGITAB):          
         /*ändrad till verklig tid inte inte avrundad tid 20171122 Lena */     
         /*ovar = ovar + klock100(TIDREGITAB.OANT1) + klock100(TIDREGITAB.OANT2) + klock100(TIDREGITAB.OANT3).
         IF TIDREGITAB.DATUM GE bdatum THEN ovman = ovman + klock100(TIDREGITAB.OANT1) + klock100(TIDREGITAB.OANT2) + klock100(TIDREGITAB.OANT3).*/
         ovar = ovar + klock100(TIDREGITAB.TOTALT).
         IF TIDREGITAB.DATUM GE bdatum THEN ovman = ovman + klock100(TIDREGITAB.TOTALT).
         GET NEXT toq NO-LOCK.
      END.
                     
      /*Nya regler allmän övertid 200 tim/kalkenderår , extra övertid + 150 timmar vid särskilda skäl
      Möjlighet att återför 50 timmar om komp tagtit ut i ledig tid 50 tim  Lena 20171114    */               
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      ater50 = FALSE.
      atertim = 0.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "ATERFOR50"                   
      inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.            
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN 
         ater50 = extradatatemp.SOKLOG[1]
         atertim = extradatatemp.SOKINT[1].                  
      END.   
      ELSE DO:
          ater50 = FALSE.
          atertim = 0.
      END.
      maxov = 200.
      IF Guru.Konstanter:globforetag = "misv" OR  Guru.Konstanter:globforetag = "sund" OR  Guru.Konstanter:globforetag = "snat" OR  Guru.Konstanter:globforetag = "gkal" THEN maxov = 200.
      
      IF ovar > maxov THEN DO:   /*200*/
         IF  (Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "misv" OR Guru.Konstanter:globforetag = "gkal")  AND dispens = TRUE THEN.
         ELSE DO:
            IF ater50 = TRUE THEN DO:                  
               IF ovar > maxov + atertim THEN DO:   /* återföring av max 50 tim  ex 200 + 30 = 230*/
                  IF Guru.Konstanter:globforetag = "snat" THEN DO:
                     /*OM DE SKA HA ANNAN ORDALYDELSE 20201201 LENA*/
                     CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
                     SUBSTRING(tidut.UT,57) = "Övertidstimmar i år är " + STRING(ovar,">>9.99") + " . Maxgränsen inklusive återförda timmar är " +  STRING(maxov + atertim) + " timmar. Kontakta ansvarig ledare. Facket SKALL informeras.".
                 END.
                 ELSE DO:
                    CREATE tidut.               
                     ASSIGN
                     SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
                     SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
                     SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
                     SUBSTRING(tidut.UT,57) = "Övertidstimmar i år är " + STRING(ovar,">>9.99") + " . Maxgränsen inklusive återförda timmar är " +  STRING(maxov + atertim) + " timmar. Kontakta ansvarig chef".
                 END.       
               END.  
            END.
            ELSE IF Guru.Konstanter:globforetag = "gkal" THEN DO:                
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
               SUBSTRING(tidut.UT,57) = "Övertidstimmar i år är " + STRING(ovar,">>9.99") + " . Maxgränsen är " + STRING(maxov) + ". Kontakta ansvarig chef.".
            END.
            ELSE IF Guru.Konstanter:globforetag = "snat" THEN DO:
               /*ny formulering 20201201 lena*/                
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
               SUBSTRING(tidut.UT,57) = "Övertidstimmar i år är " + STRING(ovar,">>9.99") + " . Maxgränsen är " + STRING(maxov) + ". Kontakta ansvarig ledare. Facket SKALL informeras".
            END.
            ELSE DO:                
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
               SUBSTRING(tidut.UT,57) = "Övertidstimmar i år är " + STRING(ovar,">>9.99") + " . Maxgränsen är " + STRING(maxov) + ". Om komp tagits ut i ledig tid kan 50 timmar 
               återföras. Kontakta ansvarig chef. Facket skall informeras".
            END.   
         END.                                             
         IF ovar > 350 THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
            SUBSTRING(tidut.UT,57) = "Övertidstimmar i år är " + STRING(ovar,">>9.99") + ". Maxgränsen är 350 timmar " + ". Kontakta ansvarig chef".
         END.            
      END.
      ELSE IF ovar > varngr THEN DO:               
         CREATE tidut.               
         ASSIGN
         SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
         SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
         SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
         SUBSTRING(tidut.UT,57) = "Varning! Övertidstimmar i år är " + STRING(ovar,">>9.99") + " . Du närmar dej maxgränsen utan dispens " + STRING(maxov) + " timmar / år.".
                  
      END.      
      IF dispensm = FALSE THEN DO:   
         IF ovman > 50 THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
            SUBSTRING(tidut.UT,57) = "Övertidstimmar denna månad är " + STRING(ovman,">>9.99") + " . Kontakta ansvarig chef".
         END.                                          
      END.      
   END.         
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:               
      IF Guru.Konstanter:globforetag = "LULE" THEN aodlak = "141".
      IF Guru.Konstanter:globforetag = "GKAL" THEN aodlak = "143".
      /*Max 10 besök läkare tandläkare sjukgymnast per år aonr 141*/            
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      dispens = FALSE.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "DISPENSLÄ"                   
      inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.            
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN dispens = extradatatemp.SOKLOG[1].         
      END.   
      ELSE dispens = FALSE.         
      IF dispens = FALSE THEN DO:            
         ASSIGN
         ovar = 0.            
         OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
         AND TIDREGITAB.DATUM GE DATE(01,01,YEAR(bdatum)) AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.AONR = aodlak
         AND TIDREGITAB.TIDLOG = TRUE NO-LOCK.
         GET FIRST toq NO-LOCK.
         DO WHILE AVAILABLE (TIDREGITAB):               
            ovar = ovar + 1.
            GET NEXT toq NO-LOCK.
         END.
         IF Guru.Konstanter:globforetag = "gkal" THEN DO:
            /* Kalmar både 143 och 144 men inge 2-timmarsgräns för 144 */
            OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
            AND TIDREGITAB.DATUM GE DATE(01,01,YEAR(bdatum)) AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.AONR = "144"
            AND TIDREGITAB.TIDLOG = TRUE NO-LOCK.
            GET FIRST toq NO-LOCK.
            DO WHILE AVAILABLE (TIDREGITAB):               
               ovar = ovar + 1.
               GET NEXT toq NO-LOCK.
            END.
         END.   
         IF ovar > 15 THEN DO:
            CREATE tidut.               
            ASSIGN
            SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
            SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
            SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
            SUBSTRING(tidut.UT,57) = "Antal besök läkare, tandläk, sjukgymn mm i år är " + STRING(ovar,">>9") + ". Max är 15. Kontakta lönepersonal".
         END.                   
         IF Guru.Konstanter:globforetag = "GKAL"  THEN DO:               
            OPEN QUERY toq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD
            AND TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.AONR = aodlak
            AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.TOTALT > 2 NO-LOCK.
            GET FIRST toq NO-LOCK.
            DO WHILE AVAILABLE (TIDREGITAB):                                    
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)                               
               SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Besök hos läkare, tandläk skall ej överstiga 2 timmar per tillfälle. Kontakta lönepersonal".
               GET NEXT toq NO-LOCK.
            END.
         END.
         IF Guru.Konstanter:globforetag = "GKAL" OR  Guru.Konstanter:globforetag = "elpa" THEN DO:                              
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = tidpers.PERSONALKOD 
            AND TIDREGITAB.DATUM GE bdatum AND TIDREGITAB.DATUM LE avdatum AND TIDREGITAB.AONR = "144" USE-INDEX PSTART NO-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO:                                                                                          
               CREATE tidut.               
               ASSIGN
               SUBSTRING(tidut.UT,1) = tidpers.PERSONALKOD
               SUBSTRING(tidut.UT,7) = tidpers.FORNAMN
               SUBSTRING(tidut.UT,23) = SUBSTRING(tidpers.EFTERNAMN,1,20)    
               SUBSTRING(tidut.UT,44) = STRING(regvnr)
               SUBSTRING(tidut.UT,53) = regdagnamn.  
               SUBSTRING(tidut.UT,57) = "Dubbelcheck:Har du visat upp din remiss hos lönekontoret för tiden skriven på 144 ?".                        
               IF gvisatidpermanad = TRUE THEN SUBSTRING(tidut.UT,44,8) = STRING(TIDREGITAB.DATUM,"99/99/99").                
            END.
         END.
      END.         
   END.
END PROCEDURE.
