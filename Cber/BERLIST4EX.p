/*BERLIST4EX.P*/
DEFINE SHARED TEMP-TABLE extemp
  {EXTEMPDEF.I}
DEFINE TEMP-TABLE innehall NO-UNDO  
{BYGGINNEHALL.I}
&Scoped-define NEW 
DEFINE SHARED VARIABLE valkonst  AS LOGICAL NO-UNDO.


  

{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
DEFINE TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)"
   FIELD RADVAR AS INTEGER.
DEFINE {&NEW} {&SHARED} TEMP-TABLE excoltemp
   FIELD COLNAME AS CHARACTER
   FIELD COLNUM AS INTEGER
   FIELD WDTH AS INTEGER.

DEFINE {&NEW} {&SHARED} TEMP-TABLE bryttemp NO-UNDO
   FIELD ROWVAR AS INTEGER
   INDEX ROWVAR ROWVAR.
&Scoped-define SHARED SHARED
{KONVALTEMP.I}     
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{BILDBERTEMP.I}
DEFINE INPUT PARAMETER sidlangdex AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER kompsida AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR innehall.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
DEFINE VARIABLE cRange AS CHARACTER NO-UNDO.
DEFINE VARIABLE chWorkSheet AS HANDLE NO-UNDO.
DEFINE VARIABLE link AS CHARACTER NO-UNDO.
DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE excelapph AS HANDLE NO-UNDO.
DEFINE VARIABLE opapph AS HANDLE NO-UNDO.
DEFINE VARIABLE opvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE startc AS CHARACTER NO-UNDO.
DEFINE VARIABLE slutc AS CHARACTER NO-UNDO.
DEFINE VARIABLE slutbredd AS INTEGER NO-UNDO.
DEFINE VARIABLE allac              AS CHARACTER  EXTENT 50 NO-UNDO. /*alla kolumner*/
DEFINE VARIABLE utnr               AS INTEGER    EXTENT 50 NO-UNDO.
DEFINE VARIABLE allachar           AS LOGICAL    EXTENT 50 NO-UNDO. /*skall kolumn vara text även om den innehåller tal*/
DEFINE VARIABLE klar AS LOGICAL NO-UNDO.
DEFINE VARIABLE utberapph AS HANDLE NO-UNDO.
DEFINE VARIABLE startpos AS INTEGER NO-UNDO.
DEFINE VARIABLE data AS CHARACTER NO-UNDO.
DEFINE VARIABLE exrow AS INTEGER NO-UNDO.
DEFINE VARIABLE filvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE bildvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE rowspar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE samerow AS INTEGER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.

EMPTY TEMP-TABLE excoltemp NO-ERROR. 
EMPTY TEMP-TABLE bryttemp NO-ERROR.
ASSIGN
startc = "A"
slutc = "I".
RUN BEREXCELIN.P PERSISTENT SET excelapph
   (INPUT startc,INPUT slutc).  
RUN Ininnehall_UI IN excelapph (INPUT TABLE innehall).   
RUN kompsida_UI IN excelapph (INPUT kompsida).
IF Guru.Konstanter:appcon THEN DO:
   RUN UTBERAPP.P PERSISTENT SET utberapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
END.
ELSE DO:
   RUN UTBERAPP.P PERSISTENT SET utberapph.
END.
EMPTY TEMP-TABLE bildbertemp NO-ERROR.
FIND FIRST extemp  WHERE extemp.DATA NE "" NO-LOCK NO-ERROR.
IF AVAILABLE extemp THEN DO:
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF SUBSTRING(extemp.DATA,1,15) = "Materiel/Upplag" OR SUBSTRING(extemp.DATA,1,13) = "Byggprotokoll" THEN DO:
         RUN bildberhmt_UI IN utberapph (INPUT Guru.Konstanter:globanv,INPUT valaonr,INPUT valomrade,OUTPUT table bildbertemp).
         FIND FIRST bildbertemp NO-LOCK NO-ERROR.
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      IF SUBSTRING(extemp.DATA,1,12) = "Satsläggning" OR SUBSTRING(extemp.DATA,1,13) = "Byggprotokoll" THEN DO:
         RUN bildberhmt_UI IN utberapph (INPUT Guru.Konstanter:globanv,INPUT valaonr,INPUT valomrade,OUTPUT table bildbertemp).
         FIND FIRST bildbertemp NO-LOCK NO-ERROR.
      END.
   END.
   ELSE DO:
      IF SUBSTRING(extemp.DATA,1,15) = "Materiel/upplag" OR SUBSTRING(extemp.DATA,1,13) = "Byggprotokoll" THEN DO:
         RUN bildberhmt_UI IN utberapph (INPUT Guru.Konstanter:globanv,INPUT valaonr,INPUT valomrade,OUTPUT table bildbertemp).
         FIND FIRST bildbertemp NO-LOCK NO-ERROR.
      END.
   END.
END.
FOR EACH kon_val WHERE NO-LOCK:
   FIND FIRST bildbertemp WHERE bildbertemp.NUM = kon_val.NUM  NO-LOCK NO-ERROR.   
   IF AVAILABLE bildbertemp THEN DO:
      MESSAGE "Vill Ni se kopplade dokument och bilder i beredningen?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar.
      IF svar = TRUE THEN DO:
         /*Anders Olsson Elpool i Umeå AB  22 feb 2017 11:14:03 
         blir jätteproblem med sidbrytningar!  
        
         MESSAGE "Vill Ni ha bilderna sist i listan?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar.
         IF svar = TRUE THEN RUN koppsvarkonst_UI IN excelapph (FALSE).
         ELSE RUN koppsvarkonst_UI IN excelapph (TRUE).
        */
      END.   
      ELSE IF svar = FALSE THEN EMPTY TEMP-TABLE bildbertemp NO-ERROR.
      LEAVE.
   END.    
END.
FOR EACH bildbertemp WHERE NO-LOCK:
   FIND FIRST kon_val WHERE kon_val.NUM = bildbertemp.NUM NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kon_val THEN DELETE bildbertemp.
END.
RUN start_UI. 
/* RUN sattbryt_UI IN excelapph (INPUT TABLE bryttemp). */
/* RUN koppdok_UI. */
/*
IF svar = TRUE THEN RUN sidbrytbredd_UI IN excelapph (INPUT 1). /* 1 = stående, 2 = liggande */
*/
/*
RUN sidbrytbredd_UI IN excelapph (INPUT 1).
*/
             
RUN innehall_UI IN excelapph.
 

RUN slutexcel_UI IN excelapph.
IF VALID-HANDLE(utberapph) THEN DELETE PROCEDURE utberapph NO-ERROR.
IF VALID-HANDLE(excelapph) THEN DELETE PROCEDURE excelapph NO-ERROR.
IF VALID-HANDLE(opapph) THEN DELETE PROCEDURE opapph NO-ERROR.
ASSIGN
opapph = ?
utberapph = ?
excelapph = ?. 
PROCEDURE start_UI:
   DEFINE VARIABLE extrabredd AS INTEGER NO-UNDO.
  
   {AONRUTSID.I}
   ASSIGN
   slutbredd = 16
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 28
   utnr[4] = 43
   utnr[5] = 53
   utnr[6] = 60
   utnr[7] = 69
   utnr[8] = 77 
   utnr[9] = 93 
   utnr[10] = 110
   utnr[11] = 126
   utnr[12] = 140
   allachar[1] = TRUE
   allac[1] = "A"          
   allac[2] = "B"          
   allac[3] = "C"          
   allac[4] = "D"          
   allac[5] = "E"          
   allac[6] = "F"          
   allac[7] = "G"          
   allac[8] = "H"          
   allac[9] = "I"          
   allac[10] = "J"         
   allac[11] = "K"         
   allac[12] = "L"         
   allac[13] = "M"         
   allac[14] = "N"         
   allac[15] = "O"         
   allac[16] = "P"         
   allac[17] = "Q"         
   allac[18] = "R"         
   allac[19] = "S"         
   allac[20] = "T"         
   allac[21] = "U"         
   allac[22] = "V"         
   allac[23] = "W"         
   allac[24] = "X"         
   allac[25] = "Y"         
   allac[26] = "Z"         
   allac[27] = "AA"        
   allac[28] = "AB"
   raknare = 1
   bildvar = FALSE.
   DO WHILE utnr[raknare + 1] > 0:
      CREATE excoltemp.
      ASSIGN 
      excoltemp.COLNAME = allac[raknare]
      excoltemp.COLNUM= utnr[raknare]
      excoltemp.WDTH = (utnr[raknare + 1]) - utnr[raknare]
      raknare = raknare + 1.
      IF excoltemp.COLNAME = "E" THEN excoltemp.WDTH = excoltemp.WDTH + 2.
      IF excoltemp.COLNAME = "H" AND Guru.GlobalaVariabler:EgenskaperTillByggkontroll[1] = TRUE THEN excoltemp.WDTH = 5.
   END.  
   raknare = 0.   
   RUN startexcel_UI IN excelapph (INPUT "Byggprotokoll").   
   RUN sattcolumner_UI IN excelapph (INPUT TABLE excoltemp).
   RUN excelhead_UI  IN excelapph (INPUT 3,aoutvar).
   /*KOMPSIDA*/
   IF kompsida = TRUE THEN DO:
      FOR EACH extemp WHERE extemp.DATA = "" AND extemp.BILD = FALSE NO-LOCK:
         extemp.BORT = TRUE.
      END.
   END.
   FIND FIRST extemp NO-LOCK NO-ERROR.
   IF AVAILABLE extemp THEN DO:
      RUN extemp_UI IN excelapph.
   END.  
   /*KOMPSIDA*/
   FOR EACH extemp WHERE extemp.BORT = TRUE :
      extemp.BORT = FALSE.
   END.
END PROCEDURE.

  

