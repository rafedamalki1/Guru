/*BERESEK.I KÖRS INTE*/  
DEFINE VARIABLE xhop AS CHARACTER NO-UNDO.   
DEFINE SHARED VARIABLE berelogvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE beresekvar AS LOGICAL EXTENT 50 NO-UNDO.
xhop = "BERE".      
IF berelogvar = FALSE THEN DO:
   berelogvar = TRUE.
   FIND FIRST XSEK WHERE XSEK.MENYVART = xhop AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.     
   
   ASSIGN
   Guru.Konstanter:beresekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:beresekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:beresekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:beresekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:beresekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:beresekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:beresekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:beresekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:beresekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:beresekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:beresekvar[11] = XSEK.SEK[11]
   /*beresekvar[12] = XSEK.SEK[12] används ej*/
   Guru.Konstanter:beresekvar[13] = XSEK.SEK[13].
END.   
IF Guru.Konstanter:beresekvar[1] = TRUE THEN ENABLE BTN_NY WITH FRAME FRAME-A.
       
IF Guru.Konstanter:beresekvar[2] = TRUE THEN ENABLE BTN_UPP WITH FRAME FRAME-A.

IF Guru.Konstanter:beresekvar[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME FRAME-A.  

IF Guru.Konstanter:beresekvar[4] = TRUE THEN ENABLE BTN_KALK WITH FRAME FRAME-A.

IF Guru.Konstanter:beresekvar[5] = TRUE THEN ENABLE BTN_LIST WITH FRAME FRAME-A.              

IF Guru.Konstanter:beresekvar[6] = TRUE THEN ENABLE BTN_INK WITH FRAME FRAME-A.  

/* IF Guru.Konstanter:beresekvar[7] = TRUE THEN ENABLE BTN_BORTKOPP WITH FRAME FRAME-A. */

/* IF Guru.Konstanter:beresekvar[8] = TRUE THEN ENABLE BTN_KOPKAL WITH FRAME FRAME-A. */

IF Guru.Konstanter:beresekvar[9] = TRUE THEN ENABLE BTN_INAKTIV WITH FRAME FRAME-A.

IF Guru.Konstanter:beresekvar[10] = TRUE THEN ENABLE BTN_ADM WITH FRAME FRAME-A.

IF Guru.Konstanter:beresekvar[11] = TRUE THEN ENABLE BTN_LAS WITH FRAME FRAME-A.
           



     
