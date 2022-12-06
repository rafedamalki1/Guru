/*RESBAPP.P*/
&Scoped-define NEW NEW 
{TIDALLT.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER inbdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER inavdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER enfle AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE extraaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE NEW SHARED VARIABLE extradnr LIKE TIDREGITAB.DELNR NO-UNDO.
DEFINE VARIABLE bdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE avdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE trazonvar AS INTEGER NO-UNDO.
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE arbnr AS CHARACTER NO-UNDO.

{RESDEF.I}
/*DEFINE NEW SHARED TEMP-TABLE maltidfil
   FIELD MPERSONALKOD AS CHARACTER 
   FIELD MDAG AS CHARACTER
   FIELD MVECKONUMMER AS INTEGER 
   FIELD MDATUM AS DATE 
   FIELD MFRU AS LOGICAL
   FIELD MLUN AS LOGICAL
   FIELD MMID AS LOGICAL.*/
ASSIGN
bdatum = inbdatum
avdatum = inavdatum.
ASSIGN 
bdatumspar = bdatum
avdatumspar = avdatum. 
FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
persrec = RECID(PERSONALTAB).
OPEN QUERY tidq 
FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
TIDREGITAB.ENFLERDAGS BEGINS enfle AND TIDREGITAB.GODKAND = '' AND TIDREGITAB.TIDLOG = TRUE USE-INDEX PSTART NO-LOCK.
DO TRANSACTION:
   GET FIRST tidq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      IF TIDREGITAB.PRISTYP = "RESTID..." THEN.
      ELSE DO:
         ASSIGN TIDREGITAB.ENFLERDAGS = ""
         TIDREGITAB.RESMAL = "".
      END.      
      GET NEXT tidq EXCLUSIVE-LOCK.
   END.   
END.            
CLOSE QUERY tidq.
OPEN QUERY tidq 
FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
TIDREGITAB.DATUM >= bdatum AND TIDREGITAB.DATUM <= avdatum AND
TIDREGITAB.ENFLERDAGS BEGINS enfle AND TIDREGITAB.GODKAND = '' USE-INDEX PSTART NO-LOCK.
DO TRANSACTION:
   GET FIRST tidq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):
      DELETE TIDREGITAB.
      GET NEXT tidq EXCLUSIVE-LOCK.
   END.   
END.            
CLOSE QUERY tidq.
DO TRANSACTION:
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
   TIDREGITAB.DATUM = bdatum AND TIDREGITAB.RESMAL BEGINS "Resans start:" AND
   TIDREGITAB.GODKAND = '' USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE TIDREGITAB THEN DO :
      TIDREGITAB.RESMAL = "".
   END.
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND
   TIDREGITAB.DATUM = avdatum AND TIDREGITAB.RESMAL BEGINS "Resans slut:" AND
   TIDREGITAB.GODKAND = '' USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE TIDREGITAB THEN DO :
      TIDREGITAB.RESMAL = "".
   END.
END.
regdatum = bdatumspar.
REPEAT:
   IF regdatum > avdatumspar THEN LEAVE.
   ASSIGN
   trazonvar = 0
   aonrvar = ""
   delnrvar = 0.
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE 
   TIDREGITAB.PERSONALKOD = pkod AND
   TIDREGITAB.DATUM = regdatum USE-INDEX PSTART NO-LOCK.
   DO TRANSACTION:
      GET FIRST tidq EXCLUSIVE-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF TIDREGITAB.TRAKTKOD NE '' THEN DO:
            IF TIDREGITAB.TRAKTAUTO = TRUE THEN DO:
               IF TIDREGITAB.TIDLOG = TRUE THEN DO:                    
                  IF enfle = "Endag" THEN DO:
                     IF aonrvar = TIDREGITAB.AONR AND delnrvar = TIDREGITAB.DELNR THEN enfle = enfle.
                     ELSE DO:
                        ASSIGN                     
                        aonrvar = TIDREGITAB.AONR 
                        delnrvar = TIDREGITAB.DELNR.
                        FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND 
                        AONRTAB.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.                     
                        IF AVAILABLE AONRTAB THEN trazonvar = AONRTAB.TRAKTAMENTE.
                     END.
                     TIDREGITAB.TRAKTAMENTE = trazonvar.
                  END.   
                  tidtabrec = RECID(TIDREGITAB).
                  RUN TRAKTBER.P.
               END.
               tidtabrec = RECID(TIDREGITAB).
               arbnr = TIDREGITAB.AONR.
               IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
                  /*ob-sjuk*/
                  IF arbnr = "110" THEN RUN OBBERSJ.P.
                  ELSE RUN OBBER.P.
               END.
               ELSE RUN OBBER.P.
            END.
         END.
         ELSE DO:
            ASSIGN 
            TIDREGITAB.TRAKTAUTO = TRUE.
            IF TIDREGITAB.TIDLOG = TRUE THEN DO:
               IF enfle = "Endag" THEN DO:
                  IF aonrvar = TIDREGITAB.AONR AND delnrvar = TIDREGITAB.DELNR THEN musz = musz.
                  ELSE DO:
                     ASSIGN                     
                     aonrvar = TIDREGITAB.AONR 
                     delnrvar = TIDREGITAB.DELNR.
                     FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND 
                     AONRTAB.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.                     
                     IF AVAILABLE AONRTAB THEN trazonvar = AONRTAB.TRAKTAMENTE.
                  END.
                  TIDREGITAB.TRAKTAMENTE = trazonvar.                  
               END.   
               tidtabrec = RECID(TIDREGITAB).
               arbnr = TIDREGITAB.AONR.
               RUN TRAKTBER.P.
            END.
            tidtabrec = RECID(TIDREGITAB).
            IF Guru.Konstanter:varforetypval[30] = 1 THEN DO:
               /*ob-sjuk*/
               IF arbnr = "110" THEN RUN OBBERSJ.P.
               ELSE RUN OBBER.P.
            END.
            ELSE RUN OBBER.P.
         END.                
         GET NEXT tidq EXCLUSIVE-LOCK.
      END.
   END.
   regdatum = regdatum + 1. 
   CLOSE QUERY tidq.
END.
