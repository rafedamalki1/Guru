/*SKAPURES.P*/
{APP.I}
&Scoped-define NEW NEW
{URESRUT.I}
DEFINE NEW SHARED VARIABLE resrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum5 AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nattrakt AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE startresa LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE utsve LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE inutr LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE ututr LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE insve LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE slutresa LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE flygu AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE flygh AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE uland LIKE LAND.LAND NO-UNDO.
DEFINE NEW SHARED VARIABLE procent AS DECIMAL NO-UNDO.
DEFINE VARIABLE enfle LIKE TIDREGITAB.ENFLERDAGS NO-UNDO. 
DEFINE VARIABLE hjrec AS RECID NO-UNDO.
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
DEFINE VARIABLE hdatu1 AS DATE NO-UNDO.
DEFINE VARIABLE hdatu2 AS DATE NO-UNDO.
DEFINE VARIABLE hdatu3 AS DATE NO-UNDO.
DEFINE VARIABLE hdath1 AS DATE NO-UNDO.
DEFINE VARIABLE hdath2 AS DATE NO-UNDO.
DEFINE VARIABLE hdath3 AS DATE NO-UNDO.
DEFINE VARIABLE sverige AS DECIMAL NO-UNDO.
DEFINE VARIABLE utland AS DECIMAL NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "9999999" NO-UNDO.  
DEFINE VARIABLE allmank AS LOGICAL NO-UNDO.
DEFINE VARIABLE akoll AS INTEGER NO-UNDO.
DEFINE VARIABLE bkoll AS INTEGER NO-UNDO.
DEFINE VARIABLE kostavd AS LOGICAL NO-UNDO.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE NEW  SHARED VARIABLE reddatum LIKE TIDREGITAB.DATUM NO-UNDO.
DEFINE NEW SHARED VARIABLE FILL-IN-REDTRAKT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     LABEL "5. Reducerat traktamente                         ?" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
       NO-UNDO.
DEFINE NEW  SHARED VARIABLE FILL-IN-3MAN AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.
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

DEFINE VARIABLE FILL-IN-RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "RESMÅL" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
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

                    
DEFINE QUERY tidq FOR TIDREGITAB.
FIND FIRST uresapptemp NO-LOCK NO-ERROR.
IF NOT AVAILABLE uresapptemp THEN DO:   
   musfel = TRUE.
   musz = TRUE.
   RETURN.
END.
ASSIGN 

 
FILL-IN-MAT = uresapptemp.MAT  
FILL-IN-RESMAL = uresapptemp.RESMAL  
FILL-IN-PKOD = uresapptemp.PKOD  
persrec = uresapptemp.RECPERS  
bdatum = uresapptemp.INDATUM  
avdatum = uresapptemp.UTDATUM
startresa = uresapptemp.START 
utsve =   uresapptemp.UTSVE   
inutr = uresapptemp.INUTR   
ututr = uresapptemp.UTUTR
insve = uresapptemp.INSVE   
slutresa = uresapptemp.SLUT
uland = uresapptemp.LAND
hdatu1 = uresapptemp.DATU1
hdatu2 = uresapptemp.DATU2
hdatu3 = uresapptemp.DATU3
hdath1 = uresapptemp.DATH1
hdath2 = uresapptemp.DATH2
hdath3 = uresapptemp.DATH3   
FILL-IN-REDTRAKT = FALSE
FILL-IN-FRIMAT = FALSE
FILL-IN-3MAN = FALSE.     
{FORESTYR.I}
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = uresapptemp.PKOD AND 
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
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" THEN ASSIGN energiavt = TRUE.
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
         IF respers.DATUM = avdatum AND respers.SLUT LE regstart 
         THEN NEXT block. 
         DO:
            RUN TRMANS2.P.    
            musz = FALSE.        
            IF ( respers.DATUM GE hdatu1 AND respers.DATUM LE hdatu3 ) OR ( respers.DATUM GE hdath1 AND respers.DATUM LE hdath3 ) THEN DO:           
               IF respers.DATUM = hdatu1 THEN DO:
                  IF hdatu1 NE hdatu2 THEN musz = TRUE.  /*svenskt TRAKT*/
                  ELSE IF hdatu1 NE hdatu3 THEN musz = TRUE. /*svenskt TRAKT*/
                  ELSE DO:
                     nytid = startresa.
                     RUN TIMSEK.P.
                     ASSIGN
                     seku = sekunder
                     nytid = utsve.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     RUN SEKTIM.P.
                     sverige = nytid.
                     nytid = inutr.
                     RUN TIMSEK.P.
                     sekunder = 86400 - sekunder. 
                     RUN SEKTIM.P.
                     utland = nytid.
                     IF sverige > utland THEN musz = TRUE.  /*SVENSKT TRAKT*/
                  END.
               END.      
               IF respers.DATUM = hdath3 THEN DO:
                  IF hdath3 NE hdath1  THEN musz = TRUE.  /*SVENSKT TRAKT*/
                  ELSE IF hdath3 NE hdath2 THEN musz = TRUE. /*SVENSKT TRAKT*/
                  ELSE DO:
                     nytid = insve.
                     RUN TIMSEK.P.
                     ASSIGN
                     seku = sekunder
                     nytid = slutresa.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     RUN SEKTIM.P.
                     sverige = nytid.
                     utland = ututr.
                     IF sverige > utland THEN musz = TRUE.  /*SVENSKT TRAKT*/
                  END.
               END. 
               allmank = FALSE.
               IF hdatu3 GE ( hdatu1 + 2) AND respers.DATUM > hdatu2 AND respers.DATUM < hdatu3 THEN DO: 
                  /*  dag på allmänna kommunikationer 180 kr*/
                  allmank = TRUE.
                  musz = TRUE.
               END.   
               IF hdath3 GE ( hdath1 + 2) AND respers.DATUM > hdath1 AND respers.DATUM < hdath2 THEN DO:
                  /*  dag på allmänna kommunikationer 180 kr*/
                  allmank = TRUE.
                  musz = TRUE.
               END.
            END.   
            IF musz = TRUE THEN DO:
               musz = FALSE.               
               RUN FLTRAKT2.P.
               IF allmank = TRUE THEN musz = musz.
               ELSE DO:                  
                  RUN FLLON2.P.                  
               END.
               allmank = FALSE.   
               FILL-IN-FRIMAT = FALSE.
               IF FILL-IN-MAT = TRUE THEN DO:
                  RUN MALAV2.P.
                  IF kostavd = TRUE THEN RUN KOSTAV2.P.                  
               END.                             
            END.
            ELSE DO: 
                RUN FLUTL2.P.
                IF kostavd = TRUE THEN RUN KOSTAV2.P.
            END.
         END.         
         hjdat = regdatum.                                           
      END.
      ELSE IF respers.START = regslut AND respers.SLUT = regslut THEN DO:             
         IF hjdat = regdatum THEN NEXT block.
         IF respers.DATUM = avdatum AND respers.SLUT LE regstart 
         THEN NEXT block. 
         DO:
            RUN TRMANS2.P.            
            musz = FALSE.        
            IF ( respers.DATUM GE hdatu1 AND respers.DATUM LE hdatu3 ) OR ( respers.DATUM GE hdath1 AND respers.DATUM LE hdath3 ) THEN DO:           
               IF respers.DATUM = hdatu1 THEN DO:
                  IF hdatu1 NE hdatu2 THEN musz = TRUE.  /*svenskt TRAKT*/
                  ELSE IF hdatu1 NE hdatu3 THEN musz = TRUE. /*svenskt TRAKT*/
                  ELSE DO:
                     nytid = startresa.
                     RUN TIMSEK.P.
                     ASSIGN
                     seku = sekunder
                     nytid = utsve.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     RUN SEKTIM.P.
                     sverige = nytid.
                     nytid = inutr.
                     RUN TIMSEK.P.
                     sekunder = 86400 - sekunder. 
                     RUN SEKTIM.P.
                     utland = nytid.
                     IF sverige > utland THEN musz = TRUE.  /*SVENSKT TRAKT*/
                  END.
               END.      
               IF respers.DATUM = hdath3 THEN DO:
                  IF hdath3 NE hdath1 THEN musz = TRUE.  /*svenskt TRAKT*/
                  ELSE IF hdath3 NE hdath2 THEN musz = TRUE. /*svenskt TRAKT*/
                  ELSE DO:
                     nytid = insve.
                     RUN TIMSEK.P.
                     ASSIGN
                     seku = sekunder
                     nytid = slutresa.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     RUN SEKTIM.P.
                     sverige = nytid.
                     utland = ututr.
                     IF sverige > utland THEN musz = TRUE.  /*SVENSKT TRAKT*/
                  END.
               END.
               allmank = FALSE.
               IF hdatu3 GE ( hdatu1 + 2) AND respers.DATUM > hdatu2 AND respers.DATUM < hdatu3 THEN DO:
                  /*  dag på allmänna kommunikationer 180 kr*/
                  allmank = TRUE.
                  musz = TRUE.
               END.   
               IF hdath3 GE ( hdath1 + 2) AND respers.DATUM > hdath1 AND respers.DATUM < hdath2 THEN DO: 
                  /*  dag på allmänna kommunikationer 180 kr*/
                  allmank = TRUE.
                  musz = TRUE.
               END. 
            END.   
            IF musz = TRUE THEN DO:
               musz = FALSE.               
               RUN FLTRAKT2.P.
               IF allmank = TRUE THEN musz = musz.
               ELSE DO:                                               
                  RUN FLLON2.P.                  
               END.   
               allmank = FALSE.   
               FILL-IN-FRIMAT = FALSE.
               IF FILL-IN-MAT = TRUE THEN DO:
                  RUN MALAV2.P.
                  IF kostavd = TRUE THEN RUN KOSTAV2.P.              
               END.             
            END.
            ELSE DO: 
                RUN FLUTL2.P.                          
                IF kostavd = TRUE THEN RUN KOSTAV2.P.
            END.
         END.
         hjdat = regdatum.
      END.
      ELSE DO TRANSACTION:               
         CREATE TIDREGITAB.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = respers.AONR AND AONRTAB.DELNR = respers.DELNR
         USE-INDEX AONR NO-LOCK NO-ERROR.
         energiavt = FALSE.
            
         IF energiavt = TRUE THEN ASSIGN TIDREGITAB.TRAKTAMENTE = 00.         
         ELSE IF AONRTAB.TRAKTAMENTE = 00 THEN TIDREGITAB.TRAKTAMENTE = 01.
         ELSE TIDREGITAB.TRAKTAMENTE = AONRTAB.TRAKTAMENTE.        
         ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdagu".         
         ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD 
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
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "URESREG" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv         
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
         DO:
            RUN TRMANSUM.P.            
            musz = FALSE.        
            IF ( respers.DATUM GE hdatu1 AND respers.DATUM LE hdatu3 ) OR ( respers.DATUM GE hdath1 AND respers.DATUM LE hdath3 ) THEN DO:           
               IF respers.DATUM = hdatu1 THEN DO:
                  IF hdatu1 NE hdatu2 THEN musz = TRUE.  /*svenskt TRAKT*/
                  ELSE IF hdatu1 NE hdatu3 THEN musz = TRUE. /*svenskt TRAKT*/
                  ELSE DO:
                     nytid = startresa.
                     RUN TIMSEK.P.
                     ASSIGN
                     seku = sekunder
                     nytid = utsve.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     RUN SEKTIM.P.
                     sverige = nytid.
                     nytid = inutr.
                     RUN TIMSEK.P.
                     sekunder = 86400 - sekunder. 
                     RUN SEKTIM.P.
                     utland = nytid.
                     IF sverige > utland THEN musz = TRUE.  /*SVENSKT TRAKT*/
                  END.
               END.      
               IF respers.DATUM = hdath3 THEN DO:
                  IF hdath3 NE hdath1 THEN musz = TRUE.  /*svenskt TRAKT*/
                  ELSE IF hdath3 NE hdath2 THEN musz = TRUE. /*svenskt TRAKT*/
                  ELSE DO:
                     nytid = insve.
                     RUN TIMSEK.P.
                     ASSIGN
                     seku = sekunder
                     nytid = slutresa.
                     RUN TIMSEK.P.
                     sekunder = sekunder - seku.
                     RUN SEKTIM.P.
                     sverige = nytid.
                     utland = ututr.
                     IF sverige > utland THEN musz = TRUE.  /*SVENSKT TRAKT*/
                  END.
               END. 
               allmank = FALSE.
               IF hdatu3 GE ( hdatu1 + 2) AND respers.DATUM > hdatu2 AND respers.DATUM < hdatu3 THEN DO: 
                  /*  dag på allmänna kommunikationer 180 kr*/
                  allmank = TRUE.
                  musz = TRUE.
               END.   
               IF hdath3 GE ( hdath1 + 2) AND respers.DATUM > hdath1 AND respers.DATUM < hdath2 THEN DO:
                  /*  dag på allmänna kommunikationer 180 kr*/
                  allmank = TRUE.
                  musz = TRUE.
               END.
            END.   
            IF musz = TRUE THEN DO:
               musz = FALSE.               
               RUN FLTRAKT2.P.
               IF allmank = TRUE THEN musz = musz.
               ELSE DO:                                 
                  RUN FLLON.P.                  
               END.   
               allmank = FALSE.   
               FILL-IN-FRIMAT = FALSE.
               IF FILL-IN-MAT = TRUE THEN DO:
                  RUN MALAV.P.
                  IF kostavd = TRUE THEN RUN KOSTAV.P.
               END. 
            
            END.
            ELSE DO: 
                RUN FLUTL.P.            
                IF kostavd = TRUE THEN RUN KOSTAV.P.
            END.
         END.        
         hjdat = regdatum.
         IF TIDREGITAB.START = TIDREGITAB.SLUT THEN DO:
            FIND TIDREGITAB WHERE RECID(TIDREGITAB) = hjrec EXCLUSIVE-LOCK
            NO-ERROR.
            DELETE TIDREGITAB.
         END.         
      END.                 
   END.      
   ASSIGN enfle = "Flerdagu"   
   bkoll = 0
   akoll = 0.
   OPEN QUERY tidq 
   FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
   TIDREGITAB.ENFLERDAGS = enfle USE-INDEX PSTART NO-LOCK.
   DO TRANSACTION:
      GET FIRST tidq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF FILL-IN-RESMAL = "" THEN ASSIGN TIDREGITAB.RESMAL = uland.
         ELSE ASSIGN TIDREGITAB.RESMAL = uland + " "+ FILL-IN-RESMAL.         
         IF TIDREGITAB.DATUM = bdatum AND bkoll = 0 THEN DO:
            TIDREGITAB.RESMAL = "Utlandsr.start:" + STRING(startresa,">9.99") + " " + STRING(utsve,">9.99") + " " + STRING(inutr,">9.99") + " " + uland.
            bkoll = 1.
         END.
         IF TIDREGITAB.DATUM = avdatum AND akoll = 0 THEN DO:
            TIDREGITAB.RESMAL = "Utlandsr.slut: " + STRING(slutresa,">9.99") + " "  + STRING(ututr,">9.99") + " " + STRING(insve,">9.99") + " " + uland.                         
            akoll = 1.
         END.         
         GET NEXT tidq EXCLUSIVE-LOCK.
      END.   
   END. 
   IF energiavt = TRUE THEN DO:      
      regdatum = uresapptemp.INDATUM.
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
         regdatum = uresapptemp.UTDATUM.
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
      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "UOVRKOSTN" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv      
      TIDREGITAB.PERSONALKOD = okost.PERSONALKOD     
      TIDREGITAB.DAG = okost.DAG
      TIDREGITAB.VECKONUMMER = okost.VECKONUMMER
      TIDREGITAB.SLUT = 7.00
      TIDREGITAB.START = 7.00 
      TIDREGITAB.LONTILLAGG = okost.LONTILLAGG
      TIDREGITAB.LONTILLANTAL = okost.LONTILLANTAL 
      TIDREGITAB.LONAUTO = FALSE
      TIDREGITAB.TIDLOG = FALSE.
      ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdagu".      
      IF okost.MOMS > 0 THEN DO:
         FIND FIRST LONTILL WHERE LONTILL.LONKODTEXT = 'MOMS'
         NO-LOCK NO-ERROR.
         IF AVAILABLE LONTILL THEN DO:
            CREATE TIDREGITAB.
            ASSIGN
            TIDREGITAB.DATUM = okost.DATUM
            TIDREGITAB.AONR = okost.AONR 
            TIDREGITAB.DELNR = okost.DELNR            
            SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "UOVRKOSTN" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            TIDREGITAB.PERSONALKOD = okost.PERSONALKOD     
            TIDREGITAB.DAG = okost.DAG
            TIDREGITAB.VECKONUMMER = okost.VECKONUMMER
            TIDREGITAB.SLUT = 7.00
            TIDREGITAB.START = 7.00 
            TIDREGITAB.LONTILLAGG = LONTILL.LONTILLAGG
            TIDREGITAB.LONTILLANTAL = okost.MOMS 
            TIDREGITAB.LONAUTO = FALSE
            TIDREGITAB.TIDLOG = FALSE.
            ASSIGN TIDREGITAB.ENFLERDAGS = "Flerdagu".           
         END.
      END. 
   END.
END PROCEDURE.
PROCEDURE pris_UI :
   {PRISBEFTYP.I}   
END PROCEDURE.
