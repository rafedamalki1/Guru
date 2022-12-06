/*KOSTDOLD.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED

DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                */
{ALLDEF.I}
&Scoped-define NEW  
{RESDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define SHARED SHARED
{PHMT.I}
&Scoped-define NEW NEW
 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE regdatumspar2 AS DATE NO-UNDO.
DEFINE VARIABLE allmat AS LOGICAL NO-UNDO.
DEFINE VARIABLE ejand AS LOGICAL NO-UNDO.
DEFINE VARIABLE kollfru AS LOGICAL NO-UNDO.

EMPTY TEMP-TABLE mtidfil NO-ERROR. 
FOR EACH maltidfil  NO-LOCK:
   CREATE mtidfil.
   BUFFER-COPY maltidfil TO mtidfil.
END.

RUN grund_UI.

FOR EACH mtidfil:
   CREATE kostfil.
   BUFFER-COPY mtidfil TO kostfil.
END.

PROCEDURE grund_UI.
   ASSIGN 
   ejand = FALSE
   allmat = FALSE.
   /*tidigare Internat gör att det även är kostförmån för frukost. Lena Jönsson och Jarmo Klint har bestämt att detta inte längre gäller 20190221 */
   IF Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "cMISV" OR Guru.Konstanter:globforetag = "cELPA"  THEN allmat = TRUE.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL"
   OR Guru.Konstanter:globforetag = "elpa" THEN ejand = TRUE.
   
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   ASSIGN   
   regdatum = bdatum.
   regdatumspar2 = bdatum.
   RUN REGVEC.P.

   FIND FIRST mtidfil NO-LOCK NO-ERROR.
   regdatum = mtidfil.MDATUM.
   REPEAT:   
      IF regdatum > avdatum THEN LEAVE.
      FIND FIRST mtidfil WHERE mtidfil.MDATUM = regdatum  NO-LOCK NO-ERROR.
      kollfru = FALSE.
      IF mtidfil.MDAG = "mån" THEN DO:                                              
         IF allmat = TRUE THEN DO:   
            IF regdatum = bdatum OR regdatum = avdatum THEN kollfru = kollfru.
            ELSE IF mtidfil.MFRU = TRUE AND mtidfil.MLUN = TRUE AND mtidfil.MMID = TRUE THEN kollfru = TRUE.                              
         END.
         IF kollfru = TRUE THEN kollfru = FALSE.
         ELSE ASSIGN mtidfil.MFRU = FALSE. 
      END.
      IF mtidfil.MDAG = "tis" THEN DO:
         IF allmat = TRUE THEN DO:   
            IF regdatum = bdatum OR regdatum = avdatum THEN kollfru = kollfru.
            ELSE IF mtidfil.MFRU = TRUE AND mtidfil.MLUN = TRUE AND mtidfil.MMID = TRUE THEN kollfru = TRUE.                              
         END.
         IF kollfru = TRUE THEN kollfru = FALSE.
         ELSE ASSIGN mtidfil.MFRU = FALSE.
      END.
      IF mtidfil.MDAG = "ons" THEN DO:
         IF allmat = TRUE THEN DO:   
            IF regdatum = bdatum OR regdatum = avdatum THEN kollfru = kollfru.
            ELSE IF mtidfil.MFRU = TRUE AND mtidfil.MLUN = TRUE AND mtidfil.MMID = TRUE THEN kollfru = TRUE.                              
         END.
         IF kollfru = TRUE THEN kollfru = FALSE.
         ELSE ASSIGN mtidfil.MFRU = FALSE.
      END.
      IF mtidfil.MDAG = "tor" THEN DO:
         IF allmat = TRUE THEN DO:   
            IF regdatum = bdatum OR regdatum = avdatum THEN kollfru = kollfru.
            ELSE IF mtidfil.MFRU = TRUE AND mtidfil.MLUN = TRUE AND mtidfil.MMID = TRUE THEN kollfru = TRUE.                              
         END.
         IF kollfru = TRUE THEN kollfru = FALSE.
         ELSE ASSIGN mtidfil.MFRU = FALSE.
      END.
      IF mtidfil.MDAG = "fre" THEN DO:
         IF allmat = TRUE THEN DO:   
            IF regdatum = bdatum OR regdatum = avdatum THEN kollfru = kollfru.
            ELSE IF mtidfil.MFRU = TRUE AND mtidfil.MLUN = TRUE AND mtidfil.MMID = TRUE THEN kollfru = TRUE.                              
         END.
         IF kollfru = TRUE THEN kollfru = FALSE.
         ELSE ASSIGN mtidfil.MFRU = FALSE.
      END.
      IF mtidfil.MDAG = "lör" THEN DO:
         IF allmat = TRUE THEN DO:   
            IF regdatum = bdatum OR regdatum = avdatum THEN kollfru = kollfru.
            ELSE IF mtidfil.MFRU = TRUE AND mtidfil.MLUN = TRUE AND mtidfil.MMID = TRUE THEN kollfru = TRUE.                              
         END.
         IF kollfru = TRUE THEN kollfru = FALSE.
         ELSE ASSIGN mtidfil.MFRU = FALSE.
      END.
      IF mtidfil.MDAG = "sön" THEN DO:
         IF allmat = TRUE THEN DO:   
            IF regdatum = bdatum OR regdatum = avdatum THEN kollfru = kollfru.
            ELSE IF mtidfil.MFRU = TRUE AND mtidfil.MLUN = TRUE AND mtidfil.MMID = TRUE THEN kollfru = TRUE.                              
         END.
         IF kollfru = TRUE THEN kollfru = FALSE.
         ELSE ASSIGN mtidfil.MFRU = FALSE.
         LEAVE.
      END.
      regdatum = regdatum + 1.
   END.   
   

     
END.




