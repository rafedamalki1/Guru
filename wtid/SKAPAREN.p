/*SKAPAREN.P*/
{APP.I}
&Scoped-define NEW NEW
{TIDALLT.I}
{RESRENUT.I}
DEFINE NEW SHARED VARIABLE resrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum5 AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nattrakt AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE reddatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE NEW SHARED VARIABLE bilmtrl AS LOGICAL NO-UNDO.  /*ANVÄNDS NU TILL TRAKTAMENTSZON ENDAG*/
DEFINE NEW SHARED VARIABLE fostart AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE foslut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE restart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE reslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE enfle LIKE TIDREGITAB.ENFLERDAGS NO-UNDO. 
DEFINE VARIABLE hjrec AS RECID NO-UNDO.
DEFINE VARIABLE hjrec2 AS RECID NO-UNDO.
DEFINE VARIABLE hjdat AS DATE NO-UNDO.  
DEFINE VARIABLE hjdat1 AS DATE NO-UNDO.
DEFINE VARIABLE hjdat2 AS DATE NO-UNDO.
DEFINE VARIABLE hjdat3 AS DATE NO-UNDO.
DEFINE VARIABLE hjtid AS INTEGER NO-UNDO.
DEFINE VARIABLE hjtid1 AS INTEGER NO-UNDO.
DEFINE VARIABLE hjtid2 AS INTEGER NO-UNDO.
DEFINE VARIABLE hjtid3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjvnr AS INTEGER NO-UNDO.
DEFINE VARIABLE tidres AS INTEGER NO-UNDO.
DEFINE VARIABLE energiavt AS LOGICAL NO-UNDO.
DEFINE VARIABLE rkoll AS INTEGER NO-UNDO.
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE akoll AS INTEGER NO-UNDO.
DEFINE VARIABLE bkoll AS INTEGER NO-UNDO.
DEFINE VARIABLE adkoll AS INTEGER NO-UNDO.
DEFINE VARIABLE bdkoll AS INTEGER NO-UNDO.
DEFINE VARIABLE tzon AS LOGICAL NO-UNDO.  /*ANVÄNDS NU TILL TRAKTAMENTSZON ENDAG*/
DEFINE VARIABLE hjrow AS ROWID NO-UNDO.
DEFINE VARIABLE kostavd AS LOGICAL NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.

DEFINE VARIABLE FILL-IN-ENFLE AS LOGICAL FORMAT "ENDAG/FLERDYGN":U INITIAL NO 
     LABEL "ENDAG/FLERDYGN" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN-FRIMAT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
       NO-UNDO.

DEFINE VARIABLE FILL-IN-MAT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     LABEL "FRI MAT" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
       NO-UNDO.

DEFINE VARIABLE FILL-IN-OKOST AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     LABEL "ÖVRIG KOSTNAD" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
       NO-UNDO.


DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "ENHET/SIGN" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN-REDTRAKT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN-REDVECKO AS INTEGER FORMAT "999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN-3MAN AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "RESMÅL" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
       NO-UNDO.

DEFINE VARIABLE FILL-IN-RESTID AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "TID MORGON/KVÄLL" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
       NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKPA AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .68
       NO-UNDO.

DEFINE VARIABLE FILL-IN-VECKO AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "STARTVNR" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
       NO-UNDO.

DEFINE VARIABLE FILL-IN-VECKO-2 AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "SLUTVNR" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
       NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "AONR" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
       NO-UNDO.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "BENÄMN." 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
       NO-UNDO.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "E-NAMN" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .91
      .

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "F-NAMN" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .91
      .

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "ENH/SIG" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .91
      .



DEFINE BUFFER resbuff FOR respers.
                    
DEFINE QUERY tidq FOR TIDREGITAB.
FIND FIRST resapptemp NO-LOCK NO-ERROR.
IF NOT AVAILABLE resapptemp THEN DO:   
   musfel = TRUE.
   musz = TRUE.
   RETURN.
END.

ASSIGN 

  
FILL-IN-ENFLE = resapptemp.ENFLE   
FILL-IN-FRIMAT = resapptemp.FRIMAT  
FILL-IN-3MAN = resapptemp.TREMAN
FILL-IN-MAT = resapptemp.MAT  
FILL-IN-RESMAL = resapptemp.RESMAL  
FILL-IN-REDTRAKT = resapptemp.REDTRAKT                 
FILL-IN-PKOD = resapptemp.PKOD  
enflerdygns = resapptemp.ENFLE
persrec = resapptemp.RECPERS  
bdatum = resapptemp.INDATUM  
avdatum = resapptemp.UTDATUM
reddatum = resapptemp.DATUMRED
bilmtrl = resapptemp.MTRL
fostart = resapptemp.FSTART 
foslut = resapptemp.FSLUT
restart = resapptemp.RSTART 
reslut = resapptemp.RSLUT
tzon = resapptemp.MTRL.
{FORESTYR.I}
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = resapptemp.PKOD AND 
PERSONALTAB.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
persrec = RECID(PERSONALTAB).
FIND FIRST ANSTFORMTAB WHERE ANSTFORM.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.
RUN restolk_UI.
PROCEDURE restolk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   kostavd = FALSE.
   IF  Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE"  
   THEN kostavd = TRUE.
   energiavt = FALSE.
      
   IF Guru.Konstanter:globforetag = "XSUND" THEN ASSIGN energiavt = TRUE.
   IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN energiavt = TRUE.  /* ej endagstraktamete*/   
   IF FILL-IN-ENFLE = TRUE AND energiavt = FALSE THEN DO:
      regdatum = bdatum. 
      RUN REGVEC.P.     
      RUN SLUTARB.P.
      rkoll = 0.      
      traktz:
      REPEAT:
         FOR EACH respers WHERE respers.DATUM = regdatum :            
            IF respers.START NE respers.SLUT AND respers.SLUT LE regstart THEN rkoll = 1.
            ELSE IF rkoll = 1 AND respers.START NE respers.SLUT AND respers.START GE regslut THEN DO:               
               musz = TRUE.
               IF tzon = TRUE THEN musz = TRUE.
               IF musz = TRUE THEN DO:
                  musz = FALSE.
                  OPEN QUERY trq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = FILL-IN-PKOD AND
                  TIDREGITAB.DATUM = respers.datum AND TIDREGITAB.START GE regstart AND TIDREGITAB.SLUT LE
                  regslut USE-INDEX PKOD NO-LOCK.
                  DO TRANSACTION:
                     GET FIRST trq EXCLUSIVE-LOCK.
                     DO WHILE AVAILABLE(TIDREGITAB):                    
                        IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
                        ELSE ASSIGN TIDREGITAB.TRAKTAMENTE = 01.                 
                        GET NEXT trq EXCLUSIVE-LOCK.
                     END.   
                  END. 
               END.               
            END.   
         END.         
         IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
            FIND FIRST respers WHERE respers.DATUM = regdatum  NO-ERROR.                                    
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = FILL-IN-PKOD AND                  
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START < regslut AND TIDREGITAB.SLUT > regstart AND TIDREGITAB.TIDLOG = TRUE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE TIDREGITAB  THEN DO:               
               FIND FIRST AONRTAB WHERE AONRTAB.AONR = respers.AONR AND AONRTAB.DELNR = respers.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.               
               IF restart LE regstart  THEN DO:
                  IF reslut GE regslut THEN RUN tidregskap_UI (INPUT regstart,INPUT regslut, INPUT 1,INPUT respers.VECKONUMMER,INPUT respers.DAG,INPUT respers.OVERTIDUTTAG,OUTPUT hjrow).                  
                  ELSE DO:
                     RUN tidregskap_UI (INPUT regstart,INPUT reslut, INPUT 1,INPUT respers.VECKONUMMER,INPUT respers.DAG,INPUT respers.OVERTIDUTTAG,OUTPUT hjrow).                  
                     RUN tidregskap_UI (INPUT reslut,INPUT regslut, INPUT 0,INPUT respers.VECKONUMMER,INPUT respers.DAG,INPUT respers.OVERTIDUTTAG,OUTPUT hjrow).
                  END.                     
               END.
               ELSE IF restart > regstart AND restart < regslut THEN DO:
                  IF reslut GE regslut THEN DO: 
                     RUN tidregskap_UI (INPUT regstart,INPUT restart, INPUT 0,INPUT respers.VECKONUMMER,INPUT respers.DAG,INPUT respers.OVERTIDUTTAG,OUTPUT hjrow).
                     RUN tidregskap_UI (INPUT restart,INPUT regslut, INPUT 1,INPUT respers.VECKONUMMER,INPUT respers.DAG,INPUT respers.OVERTIDUTTAG,OUTPUT hjrow).                  
                  END.
                  ELSE DO:
                     RUN tidregskap_UI (INPUT regstart,INPUT restart, INPUT 0,INPUT respers.VECKONUMMER,INPUT respers.DAG,INPUT respers.OVERTIDUTTAG,OUTPUT hjrow).
                     RUN tidregskap_UI (INPUT restart,INPUT reslut, INPUT 1,INPUT respers.VECKONUMMER,INPUT respers.DAG,INPUT respers.OVERTIDUTTAG,OUTPUT hjrow).                  
                     RUN tidregskap_UI (INPUT reslut,INPUT regslut, INPUT 0,INPUT respers.VECKONUMMER,INPUT respers.DAG,INPUT respers.OVERTIDUTTAG,OUTPUT hjrow).
                  END.                     
               END.                                   
               IF restart GE regstart AND reslut LE regslut AND restart < reslut THEN DO:                                        
                  FIND FIRST TIDREGITAB WHERE ROWID(TIDREGITAB) = hjrow NO-LOCK NO-ERROR.
                  tidtabrec = RECID(TIDREGITAB).
                  RUN TRAKTBER.P.
                  DO TRANSACTION:                  
                     FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = FILL-IN-PKOD AND                  
                     TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START GE regstart AND TIDREGITAB.SLUT LE regslut AND TIDREGITAB.TIDLOG = TRUE 
                     AND TIDREGITAB.TRAKTKOD NE ""   EXCLUSIVE-LOCK NO-ERROR.
                     IF AVAILABLE TIDREGITAB THEN DO:                        
                        ASSIGN TIDREGITAB.ENFLERDAGS = "Endag".
                     END.
                  END.
               END.
            END.
            ELSE DO:               
               OPEN QUERY trq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = FILL-IN-PKOD AND
               TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START GE restart AND TIDREGITAB.SLUT LE reslut USE-INDEX PKOD NO-LOCK.
               DO TRANSACTION:
                  GET FIRST trq EXCLUSIVE-LOCK.
                  DO WHILE AVAILABLE(TIDREGITAB):                    
                     IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
                     ELSE DO: 
                        ASSIGN TIDREGITAB.TRAKTAMENTE = 01.      
                        hjrow = ROWID(TIDREGITAB).
                     END.
                     GET NEXT trq EXCLUSIVE-LOCK.
                  END.   
               END.                
               
               IF restart > regstart AND restart < regslut AND restart < reslut THEN DO TRANSACTION:                  
                  FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = FILL-IN-PKOD AND                  
                  tidbuff.DATUM = regdatum AND tidbuff.START GE regstart AND tidbuff.SLUT LE regslut AND 
                  tidbuff.TIDLOG = TRUE AND tidbuff.START < restart AND tidbuff.SLUT > restart EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:
                     FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidbuff.AONR AND AONRTAB.DELNR = tidbuff.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.                     
                     RUN tidregskap_UI (INPUT restart,INPUT tidbuff.SLUT, INPUT 1,INPUT tidbuff.VECKONUMMER,INPUT tidbuff.DAG,INPUT tidbuff.OVERTIDUTTAG,OUTPUT hjrow).
                     ASSIGN tidbuff.SLUT = restart.
                     RUN tott_UI.                        
                  END.                  
               END.
               IF reslut < regslut AND reslut > regstart AND restart < reslut THEN DO TRANSACTION:                  
                  FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = FILL-IN-PKOD AND                  
                  tidbuff.DATUM = regdatum AND tidbuff.START GE regstart AND tidbuff.SLUT LE regslut AND 
                  tidbuff.TIDLOG = TRUE AND tidbuff.START < reslut AND tidbuff.SLUT > reslut EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:
                     FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidbuff.AONR AND AONRTAB.DELNR = tidbuff.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.                     
                     RUN tidregskap_UI (INPUT reslut,INPUT tidbuff.SLUT, INPUT 0,INPUT tidbuff.VECKONUMMER,INPUT tidbuff.DAG,INPUT tidbuff.OVERTIDUTTAG,OUTPUT hjrow).
                     ASSIGN                         
                     hjrow = ROWID(tidbuff)
                     tidbuff.TRAKTAMENTE = 1
                     tidbuff.SLUT = reslut.
                     RUN tott_UI.                        
                  END.                  
               END.
               IF reslut > regslut AND restart < regslut AND restart < reslut THEN DO TRANSACTION:                                                   
                  FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = FILL-IN-PKOD AND                  
                  tidbuff.DATUM = regdatum AND tidbuff.START GE regstart AND tidbuff.START < regslut AND  
                  tidbuff.TIDLOG = TRUE AND tidbuff.START < reslut AND tidbuff.SLUT > reslut EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:
                     FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidbuff.AONR AND AONRTAB.DELNR = tidbuff.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.                     
                     RUN tidregskap_UI (INPUT reslut,INPUT tidbuff.SLUT, INPUT 0,INPUT tidbuff.VECKONUMMER,INPUT tidbuff.DAG,INPUT tidbuff.OVERTIDUTTAG,OUTPUT hjrow).
                     ASSIGN                         
                     hjrow = ROWID(tidbuff)
                     tidbuff.TRAKTAMENTE = 1
                     tidbuff.SLUT = reslut.
                     RUN tott_UI.                        
                  END.                  
               END.               
               FIND FIRST TIDREGITAB WHERE ROWID(TIDREGITAB) = hjrow NO-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:
                  tidtabrec = RECID(TIDREGITAB).
                  RUN TRAKTBER.P.
               END.   
            END.
         END.
         IF tzon = TRUE THEN DO:
            OPEN QUERY trq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = FILL-IN-PKOD AND
            TIDREGITAB.DATUM = regdatum AND TIDREGITAB.START GE regstart AND TIDREGITAB.SLUT LE
            regslut USE-INDEX PKOD NO-LOCK.
            DO TRANSACTION:
               GET FIRST trq EXCLUSIVE-LOCK.
               DO WHILE AVAILABLE(TIDREGITAB):                    
                  IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN nytid = nytid.
                  ELSE IF TIDREGITAB.TRAKTAMENTE = 01 THEN nytid = nytid.
                  ELSE ASSIGN TIDREGITAB.TRAKTAMENTE = 01.                 
                  GET NEXT trq EXCLUSIVE-LOCK.
               END.   
            END. 
         END.
         regdatum = regdatum + 1.           
         rkoll = 0.
         IF regdatum > avdatum THEN LEAVE traktz.
         RUN SLUTARB.P.
      END.   
   END.                      
   hjdat = 01/01/95.           
   block:      
   FOR EACH respers: 
      ASSIGN           
      resrec = RECID(respers)             
      regvnr = respers.VECKONUMMER
      regdatum = respers.DATUM.     
      RUN SLUTARB.P.                                 
      IF respers.START = regstart AND respers.SLUT = regstart THEN DO:       
         IF hjdat = regdatum THEN NEXT block.                
         IF FILL-IN-ENFLE = FALSE THEN DO:
            RUN TRMANS2.P.
            RUN FLTRA2N.P.            
            IF FILL-IN-FRIMAT = FALSE THEN DO:
               RUN FLLON2N.P.
            END.
            IF FILL-IN-MAT = TRUE THEN DO:                    
               RUN MALAV2N.P.
               IF kostavd = TRUE THEN RUN KOSTAV2N.P.
            END.   
         END.
         ELSE DO:                    
            IF FILL-IN-MAT = TRUE THEN DO:               
               RUN MALAV2N.P.
               IF kostavd = TRUE THEN RUN KOSTAV2N.P.
            END.
         END.      
         hjdat = regdatum.                                           
      END.
      ELSE IF respers.START = regslut AND respers.SLUT = regslut THEN DO:                      
         IF hjdat = regdatum THEN NEXT block.              
         IF FILL-IN-ENFLE = FALSE THEN DO:
            RUN TRMANS2.P.
            RUN FLTRA2N.P.            
            IF FILL-IN-FRIMAT = FALSE THEN DO:
               RUN FLLON2N.P.
            END.
            IF FILL-IN-MAT = TRUE THEN DO:                              
               RUN MALAV2N.P.
               IF kostavd = TRUE THEN RUN KOSTAV2N.P.
            END.   
         END.
         ELSE DO:
            IF FILL-IN-MAT = TRUE THEN DO:                              
               RUN MALAV2N.P.
               IF kostavd = TRUE THEN RUN KOSTAV2N.P.
            END.
         END.           
         hjdat = regdatum.
      END.
      ELSE IF respers.START = respers.SLUT THEN DO:                      
         IF hjdat = regdatum THEN NEXT block.              
         IF FILL-IN-ENFLE = FALSE THEN DO:
            RUN TRMANS2.P.
            RUN FLTRA2N.P.            
            IF FILL-IN-FRIMAT = FALSE THEN DO:
               RUN FLLON2N.P.
            END.
            IF FILL-IN-MAT = TRUE THEN DO:                              
               RUN MALAV2N.P.
               IF kostavd = TRUE THEN RUN KOSTAV2N.P.
            END.   
         END.
         ELSE DO:
            IF FILL-IN-MAT = TRUE THEN DO:                              
               RUN MALAV2N.P.
               IF kostavd = TRUE THEN RUN KOSTAV2N.P.
            END.
         END.           
         hjdat = regdatum.
      END.
      ELSE DO TRANSACTION:                     
         CREATE TIDREGITAB.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = respers.AONR AND AONRTAB.DELNR = respers.DELNR
         USE-INDEX AONR NO-LOCK NO-ERROR.
         energiavt = FALSE.           
         IF Guru.Konstanter:globforetag = "Xsund" THEN ASSIGN energiavt = TRUE.
         IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN energiavt = TRUE.
         /*Kalmar har endagstraktamente men energiavt*/
         IF energiavt = TRUE THEN ASSIGN TIDREGITAB.TRAKTAMENTE = 00.         
         ELSE IF AONRTAB.TRAKTAMENTE = 00 THEN TIDREGITAB.TRAKTAMENTE = 01.
         ELSE TIDREGITAB.TRAKTAMENTE = AONRTAB.TRAKTAMENTE.        
         IF FILL-IN-ENFLE = TRUE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Endag".
         IF FILL-IN-ENFLE = FALSE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdag".
         ASSIGN 
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
         TIDREGITAB.VECKONUMMER = respers.VECKONUMMER 
         TIDREGITAB.DATUM = respers.DATUM
         TIDREGITAB.DAG = respers.DAG
         TIDREGITAB.START = respers.START
         TIDREGITAB.SLUT = respers.SLUT 
         TIDREGITAB.AONR = respers.AONR 
         TIDREGITAB.DELNR = respers.DELNR
         TIDREGITAB.PRISTYP = respers.PRISTYP
         TIDREGITAB.PRIS = respers.PRIS
         TIDREGITAB.TRAKTAUTO = TRUE
         TIDREGITAB.BILFORARE = respers.BILFORARE 
         TIDREGITAB.OVERTIDUTTAG = respers.OVERTIDUTTAG
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "RESREG" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv         
         TIDREGITAB.TIDLOG = TRUE  
         TIDREGITAB.RESMAL = FILL-IN-RESMAL.
         RUN pris_UI.         
         ASSIGN
         tidtabrec = RECID(TIDREGITAB)
         nattrakt = respers.NATTRAKT
         hjdat1 = avdatum
         hjdat2 = bdatum
         hjdat3 = regdatum.
         RUN RESA.P.     
         ASSIGN
         avdatum = hjdat1
         bdatum = hjdat2
         regdatum = hjdat3.        
         hjrec = RECID(TIDREGITAB).
         IF hjdat = regdatum THEN NEXT block.
         IF respers.DATUM = avdatum AND respers.SLUT LE regstart 
         THEN NEXT block.   
         IF respers.DATUM = avdatum AND respers.START NE 24
         AND respers.START NE respers.SLUT THEN DO:
             /* OM RESAN GÅR ÖVER BRYTGRÄNS SKALL BARA ETT TRAKTAMENTE TOLKAS*/             
            FIND FIRST resbuff WHERE resbuff.DATUM = respers.DATUM
            AND resbuff.START = respers.SLUT AND resbuff.START GE regslut NO-ERROR.
            IF AVAILABLE resbuff THEN NEXT block.
            /*tolka inte traktamentet för restiden om förrättningen fortsätter*/
            FIND FIRST resbuff WHERE resbuff.DATUM = respers.DATUM
            AND resbuff.START = 24 AND resbuff.START = resbuff.SLUT NO-ERROR.
            IF AVAILABLE resbuff THEN NEXT block.
         END.           
         IF FILL-IN-ENFLE = FALSE THEN DO:
            RUN TRMANSUM.P.   
            RUN FLTRAN.P.            
            IF FILL-IN-FRIMAT = FALSE THEN DO:
               RUN FLLONN.P.
            END.
            IF FILL-IN-MAT = TRUE THEN DO:               
               RUN MALAVN.P.
               IF kostavd = TRUE THEN RUN KOSTAVN.P.
            END.   
         END.
         ELSE DO:
            IF FILL-IN-MAT = TRUE THEN DO:                              
               RUN MALAVN.P.
               IF kostavd = TRUE THEN RUN KOSTAVN.P. 
            END.                  
            ASSIGN
            hjdat1 = avdatum
            hjdat2 = bdatum
            regdatum = TIDREGITAB.DATUM.            
            RUN TRAKTBER.P.
            ASSIGN  
            avdatum = hjdat1
            bdatum = hjdat2.
         END.  
         hjdat = regdatum.         
         IF TIDREGITAB.START = TIDREGITAB.SLUT THEN DO:
            IF TIDREGITAB.RESMAL BEGINS "Resans s" THEN musz = musz.
            ELSE DO:            
               FIND TIDREGITAB WHERE RECID(TIDREGITAB) = hjrec EXCLUSIVE-LOCK
               NO-ERROR.
               DELETE TIDREGITAB.
            END.
         END.         
      END.            
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO : 
         IF FILL-IN-ENFLE = TRUE THEN DO:
            /*75 kr avdrag om lart 701 37.5 kr avdrag om 700*/
            FIND FIRST  TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TRAKTKOD NE "" NO-LOCK NO-ERROR.            
            FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND tidbuff.DATUM = regdatum AND tidbuff.LONTILLAGG BEGINS "72" EXCLUSIVE-LOCK:                           
               IF AVAILABLE TIDREGITAB THEN DO:         
                  /*lart 847 = halvt måltidsavdrag*/
                  IF TIDREGITAB.TRAKTKOD = "700" THEN tidbuff.LONTILLAGG = "847".                                           
                  IF TIDREGITAB.TRAKTKOD = "700" THEN  ASSIGN tidbuff.LONTILLANTAL = 1.                  
                  ELSE IF TIDREGITAB.TRAKTKOD = "701" THEN ASSIGN tidbuff.LONTILLANTAL = 1.                                                      
               END.
               ELSE ASSIGN tidbuff.LONTILLANTAL = 0.                                             
            END.            
            FOR EACH tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND tidbuff.DATUM = regdatum AND tidbuff.LONTILLAGG BEGINS "78" EXCLUSIVE-LOCK:                           
               IF AVAILABLE TIDREGITAB THEN DO:         
                  IF TIDREGITAB.TRAKTKOD = "700" THEN ASSIGN tidbuff.LONTILLANTAL = 1.
                  ELSE IF TIDREGITAB.TRAKTKOD = "701" THEN ASSIGN tidbuff.LONTILLANTAL = 1.
               END.
               ELSE ASSIGN tidbuff.LONTILLANTAL = 0.                                             
            END.            
         END.
      END.
   END.      
   IF FILL-IN-ENFLE = TRUE THEN enfle = "Endag".
   IF FILL-IN-ENFLE = FALSE THEN enfle = "Flerdag".
   IF enfle = "Endag" THEN DO:   
      IF bdatum = avdatum THEN DO:      
         OPEN QUERY tidq 
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = bdatum AND
         TIDREGITAB.ENFLERDAGS = enfle USE-INDEX PSTART NO-LOCK.
         DO TRANSACTION:
            GET FIRST tidq EXCLUSIVE-LOCK.
            DO WHILE AVAILABLE(TIDREGITAB):
               ASSIGN TIDREGITAB.RESMAL = FILL-IN-RESMAL.         
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                  IF TIDREGITAB.TRAKTKOD NE "" THEN DO:                                 
                     TIDREGITAB.RESMAL = "Endags resa mellan: " + STRING(restart,">9.99") + " " + STRING(reslut,">9.99").                                                          
                  END.
               END.
               GET NEXT tidq EXCLUSIVE-LOCK.
            END.
         END.
      END.
   END.
   ASSIGN
   bkoll = 0
   akoll = 0
   bdkoll = 0
   adkoll = 0.   
   IF enfle = "Flerdag" THEN DO:   
      OPEN QUERY tidq 
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
      TIDREGITAB.ENFLERDAGS = enfle USE-INDEX PSTART NO-LOCK.
      DO TRANSACTION:
         GET FIRST tidq EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB):
            ASSIGN TIDREGITAB.RESMAL = FILL-IN-RESMAL.         
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
               IF TIDREGITAB.TRAKTKOD NE "" THEN DO:            
                  IF TIDREGITAB.DATUM = bdatum THEN DO:
                      TIDREGITAB.RESMAL = "Resans start: " + STRING(restart,">9.99").             
                      bdkoll = 1.
                  END.
                  IF TIDREGITAB.DATUM = avdatum THEN DO: 
                     TIDREGITAB.RESMAL = "Resans slut: " + STRING(reslut,">9.99").             
                     adkoll = 1.
                  END.
               END.
            END.
            ELSE DO:
               IF TIDREGITAB.DATUM = bdatum AND bkoll = 0 THEN DO:
                  TIDREGITAB.RESMAL = "Resans start: " + STRING(restart,">9.99").
                  bkoll = 1.
               END.
               IF TIDREGITAB.DATUM = avdatum AND akoll = 0 THEN DO:
                  TIDREGITAB.RESMAL = "Resans slut: " + STRING(reslut,">9.99").                         
                  akoll = 1.
               END.
             END.
             GET NEXT tidq EXCLUSIVE-LOCK.
         END.   
      END.
      IF bdkoll = 0 THEN DO TRANSACTION:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = bdatum AND TIDREGITAB.ENFLERDAGS = enfle USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR .
         IF AVAILABLE TIDREGITAB THEN DO:
            TIDREGITAB.RESMAL = "Resans start: " + STRING(restart,">9.99").
            bdkoll = 1.
         END.
         ELSE DO:
            regdatum = bdatum.
            RUN REGVEC.P.
            RUN REGDAG.P.
            CREATE TIDREGITAB.
            ASSIGN 
            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
            TIDREGITAB.VECKONUMMER = regvnr
            TIDREGITAB.DATUM = regdatum
            TIDREGITAB.DAG = regdagnamn
            TIDREGITAB.ENFLERDAGS = enfle
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "RESREG" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.RESMAL = "Resans start: " + STRING(restart,">9.99").
         END.
      END.
      IF adkoll = 0 THEN DO TRANSACTION:
         FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = avdatum AND TIDREGITAB.ENFLERDAGS = enfle USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            TIDREGITAB.RESMAL = "Resans slut: " + STRING(reslut,">9.99").                         
            adkoll = 1.
         END.
         ELSE DO:
            regdatum = avdatum.
            RUN REGVEC.P.
            RUN REGDAG.P.
            CREATE TIDREGITAB.
            ASSIGN 
            TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
            TIDREGITAB.VECKONUMMER = regvnr
            TIDREGITAB.DATUM = regdatum
            TIDREGITAB.DAG = regdagnamn
            TIDREGITAB.ENFLERDAGS = enfle
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "RESREG" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.TIDLOG = FALSE
            TIDREGITAB.RESMAL = "Resans slut: " + STRING(reslut,">9.99").                         
         END.
      END.
   END.
   energiavt = FALSE.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" THEN ASSIGN energiavt = TRUE.
   IF energiavt = TRUE THEN DO:
      
      regdatum = resapptemp.INDATUM.
      RUN REGVEC.P.
      hjvnr = regvnr.
      FIND FIRST RESTIDTAB WHERE RESTIDTAB.KOD = ANSTFORMTAB.KOD 
      AND RESTIDTAB.ENFL = "ENEJB" NO-LOCK NO-ERROR.
      kvalre:
      REPEAT:      
         ASSIGN tidres = 0.
         OPEN QUERY resq FOR EACH TIDREGITAB WHERE 
         TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
         TIDREGITAB.VECKONUMMER = hjvnr AND 
         TIDREGITAB.LONTILLAGG = RESTIDTAB.LONTILLAGG NO-LOCK BY TIDREGITAB.DATUM BY TIDREGITAB.START.
         DO TRANSACTION:
            GET FIRST resq NO-LOCK.
            DO WHILE AVAILABLE(TIDREGITAB):
               nytid = TIDREGITAB.LONTILLANTAL.
               RUN TIMSEK.P.
               tidres = tidres + sekunder.
               IF tidres > 36000 THEN DO:               
                  IF tidres - sekunder < 36000 THEN DO:
                     hjtid = sekunder.
                     hjtid1 = tidres - 36000.
                     hjtid2 = hjtid - hjtid1.
                     hjtid3 = TIDREGITAB.SLUT.
                     nytid = TIDREGITAB.START.
                     RUN TIMSEK.P.
                     sekunder = sekunder + hjtid2.
                     RUN SEKTIM.P.
                     ASSIGN 
                     TIDREGITAB.SLUT = nytid.
                     sekunder = hjtid2.
                     RUN SEKTIM.P.
                     ASSIGN
                     TIDREGITAB.LONTILLANTAL = nytid.
                     sekunder = hjtid1.
                     RUN SEKTIM.P.
                     CREATE tidbuff.
                     ASSIGN
                     tidbuff.TRAKTAMENTE = TIDREGITAB.TRAKTAMENTE                
                     tidbuff.ENFLERDAGS = TIDREGITAB.ENFLERDAGS
                     tidbuff.PERSONALKOD = TIDREGITAB.PERSONALKOD 
                     tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER 
                     tidbuff.DATUM = TIDREGITAB.DATUM
                     tidbuff.DAG = TIDREGITAB.DAG
                     tidbuff.START = TIDREGITAB.SLUT
                     tidbuff.SLUT = hjtid3 
                     tidbuff.AONR = TIDREGITAB.AONR 
                     tidbuff.DELNR = TIDREGITAB.DELNR
                     tidbuff.PRISTYP = TIDREGITAB.PRISTYP
                     tidbuff.PRIS = TIDREGITAB.PRIS
                     tidbuff.TRAKTAUTO = TIDREGITAB.TRAKTAUTO
                     tidbuff.BILFORARE = TIDREGITAB.BILFORARE 
                     tidbuff.OVERTIDUTTAG = TIDREGITAB.OVERTIDUTTAG
                     tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
                     tidbuff.PROGRAM = TIDREGITAB.PROGRAM
                     tidbuff.TIDLOG = TRUE  
                     tidbuff.RESMAL = TIDREGITAB.RESMAL                     
                     tidbuff.LONTILLANTAL = nytid.    
                                         
                     IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
                        ASSIGN tidbuff.LONTILLAGG = "4611".
                     END.
                     IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
                        ASSIGN tidbuff.LONTILLAGG = "262".
                     END.
                     IF Guru.Konstanter:globforetag = "LULE" THEN DO:
                        ASSIGN tidbuff.LONTILLAGG = "771".
                     END.
                     nytid = TIDREGITAB.START.
                     RUN TIMSEK.P.
                     ASSIGN
                     regstartsek = sekunder
                     nytid = TIDREGITAB.SLUT.
                     RUN TIMSEK.P.
                     ASSIGN
                     regslutsek = sekunder
                     regdatum = TIDREGITAB.DATUM   
                     regvnr = TIDREGITAB.VECKONUMMER.
                     RUN TOTTID.P.
                     ASSIGN TIDREGITAB.TOTALT = nytid. 

                     nytid = tidbuff.START.
                     RUN TIMSEK.P.
                     ASSIGN
                     regstartsek = sekunder
                     nytid = tidbuff.SLUT.
                     RUN TIMSEK.P.
                     ASSIGN
                     regslutsek = sekunder
                     regdatum = tidbuff.DATUM   
                     regvnr = tidbuff.VECKONUMMER.
                     RUN TOTTID.P.
                     ASSIGN tidbuff.TOTALT = nytid. 
                  END.
                  ELSE IF tidres - sekunder GE 36000 THEN DO:
                                         
                     IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
                        ASSIGN TIDREGITAB.LONTILLAGG = "4611".
                     END.
                     IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
                        ASSIGN TIDREGITAB.LONTILLAGG = "262".
                     END.
                     IF Guru.Konstanter:globforetag = "LULE" THEN DO:
                        ASSIGN TIDREGITAB.LONTILLAGG = "771".
                     END.
                  END.   
               END.   
               GET NEXT resq EXCLUSIVE-LOCK.
            END.
         END.
         regdatum = resapptemp.UTDATUM.
         RUN REGVEC.P.
         IF regvnr > hjvnr THEN ASSIGN hjvnr = hjvnr + 1.
         ELSE LEAVE kvalre.
      END.         
   END.            
   FOR EACH okost:
      IF okost.MOMS > 0 THEN ASSIGN okost.LONTILLANTAL = okost.LONTILLANTAL - okost.MOMS.                
      CREATE TIDREGITAB.
      ASSIGN
      TIDREGITAB.DATUM = okost.DATUM
      TIDREGITAB.AONR = okost.AONR 
      TIDREGITAB.DELNR = okost.DELNR
      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "OVRKOSTN" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv      
      TIDREGITAB.PERSONALKOD = okost.PERSONALKOD     
      TIDREGITAB.DAG = okost.DAG
      TIDREGITAB.VECKONUMMER = okost.VECKONUMMER
      TIDREGITAB.SLUT = 7.00
      TIDREGITAB.START = 7.00 
      TIDREGITAB.LONTILLAGG = okost.LONTILLAGG
      TIDREGITAB.LONTILLANTAL = okost.LONTILLANTAL 
      TIDREGITAB.LONAUTO = FALSE
      TIDREGITAB.TIDLOG = FALSE.
      IF SUBSTRING(okost.LONKODTEXT,41) = "" THEN .
      ELSE TIDREGITAB.RESMAL = SUBSTRING(okost.LONKODTEXT,41).
      IF FILL-IN-ENFLE = TRUE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Endag".
      IF FILL-IN-ENFLE = FALSE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdag".
      IF okost.MOMS > 0 THEN DO:
         FIND FIRST LONTILL WHERE LONTILL.LONKODTEXT = 'MOMS' AND LONTILL.KOD = ANSTFORMTAB.KOD
         NO-LOCK NO-ERROR.
         IF AVAILABLE LONTILL THEN DO:
            CREATE TIDREGITAB.
            ASSIGN
            TIDREGITAB.DATUM = okost.DATUM
            TIDREGITAB.AONR = okost.AONR 
            TIDREGITAB.DELNR = okost.DELNR            
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "OVRKOSTN" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.PERSONALKOD = okost.PERSONALKOD     
            TIDREGITAB.DAG = okost.DAG
            TIDREGITAB.VECKONUMMER = okost.VECKONUMMER
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.START = 7.00 
            TIDREGITAB.LONTILLAGG = LONTILL.LONTILLAGG
            TIDREGITAB.LONTILLANTAL = okost.MOMS 
            TIDREGITAB.LONAUTO = FALSE
            TIDREGITAB.TIDLOG = FALSE.
            IF FILL-IN-ENFLE = TRUE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Endag".
            IF FILL-IN-ENFLE = FALSE THEN ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdag".
            IF SUBSTRING(okost.LONKODTEXT,41) = "" THEN .
            ELSE TIDREGITAB.RESMAL = SUBSTRING(okost.LONKODTEXT,41).
         END.
      END. 
   END.
END PROCEDURE.
PROCEDURE pris_UI :
   {PRISBEFTYP.I}   
END PROCEDURE.

PROCEDURE tidregskap_UI :
   DEFINE INPUT PARAMETER tstart AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER tslut AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER tz AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER venr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER rdag AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER rutt AS CHARACTER NO-UNDO.   
   DEFINE OUTPUT PARAMETER hjrow AS ROWID NO-UNDO.
   CREATE TIDREGITAB.                     
   ASSIGN 
   TIDREGITAB.START = tstart
   TIDREGITAB.SLUT = tslut
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   TIDREGITAB.TRAKTAMENTE = tz
   TIDREGITAB.ENFLERDAGS = "Endag"
   TIDREGITAB.VECKONUMMER = venr
   TIDREGITAB.DATUM = regdatum
   TIDREGITAB.DAG = rdag
   TIDREGITAB.AONR = AONRTAB.AONR 
   TIDREGITAB.DELNR = AONRTAB.DELNR
   TIDREGITAB.PRISTYP = AONRTAB.PRISTYP   
   TIDREGITAB.TRAKTAUTO = TRUE   
   TIDREGITAB.OVERTIDUTTAG = rutt
   SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "RESREG" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
   TIDREGITAB.TIDLOG = TRUE  
   TIDREGITAB.RESMAL = FILL-IN-RESMAL.   
   nytid = TIDREGITAB.START.
   hjrow = ROWID(TIDREGITAB).
   RUN TIMSEK.P.
   ASSIGN
   regstartsek = sekunder
   nytid = TIDREGITAB.SLUT.
   RUN TIMSEK.P.
   ASSIGN
   regslutsek = sekunder
   regdatum = TIDREGITAB.DATUM   
   regvnr = TIDREGITAB.VECKONUMMER.
   RUN TOTTID.P.
   ASSIGN TIDREGITAB.TOTALT = nytid. 
   RUN pris_UI.
END PROCEDURE.

PROCEDURE tott_UI :
   
   nytid = tidbuff.START.
   RUN TIMSEK.P.
   ASSIGN
   regstartsek = sekunder
   nytid = tidbuff.SLUT.
   RUN TIMSEK.P.
   ASSIGN
   regslutsek = sekunder
   regdatum = tidbuff.DATUM   
   regvnr = tidbuff.VECKONUMMER.
   RUN TOTTID.P.
   ASSIGN tidbuff.TOTALT = nytid.                         
END PROCEDURE.

