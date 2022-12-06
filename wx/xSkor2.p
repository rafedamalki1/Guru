/*xSKor.p*/
/* KÖR TIDFEL MELLAN VECKOKÖRNINGARNA*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.
 
DEFINE  VARIABLE appcon AS LOGICAL NO-UNDO.
DEFINE VARIABLE manval AS LOGICAL NO-UNDO.
DEFINE VARIABLE samvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vknummer AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}
vkdatum = 02/29/2004.
gvisatidpermanad = TRUE.
globanv = "elpa".
vknummer = "w20040304".
regdatum = 02/29/2004.
IF globforetag = "elpa" THEN samvar = "\\pc012\d\delad\pro9s\elLESAMM.TXT".
   IF globforetag = "GRAN" THEN samvar = "\\granguru\guru_ser\server\pro9s\GRLESAMM.TXT".
   IF globforetag = "GRIT" THEN samvar = "\\granguru\guru_ser\server\pro9s\ITLESAMM.TXT". 
   IF globforetag = "GADM" THEN samvar = "\\granguru\guru_ser\server\pro9s\ADLESAMM.TXT".
   IF globforetag = "GKAL" THEN samvar = "\\granguru\guru_ser\server\pro9s\KALESAMM.TXT".
   IF globforetag = "SUND" THEN samvar = "\\BEREDNING1\DELAD\SERVER\PRO9S\SULESAMM.TXT".        
   RUN LESAMMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
   (INPUT 1,INPUT samvar,OUTPUT TABLE tidut).

     
      RUN SUFEEKO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT samvar,INPUT vkdatum).           


   RUN XTIDFSATT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
   (INPUT samvar,INPUT globforetag,INPUT gvisatidpermanad,INPUT vkdatum,
   INPUT vknummer).

   
   
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         DELETE OBJECT Guru.Konstanter:apphand.

