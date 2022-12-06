/*KOMPUTBETH.P*/
DEFINE TEMP-TABLE kompbettemp
   FIELD PERSONALKOD AS CHARACTER    
   FIELD DATUM AS DATE  
   FIELD TIMMAR AS DECIMAL  
   FIELD ANVANDARE AS CHARACTER  
   FIELD ACCFORE AS DECIMAL  
   FIELD ACCEFTER AS DECIMAL
   FIELD ACCKOMP AS DECIMAL
   FIELD FORE AS DECIMAL  /*VISNINGSFÄLT ACCFORE + ACCKOMP*/
   FIELD EFTER AS DECIMAL  /*VISNINGSFÄLT ACCEFTER + ACCKOMP*/
   FIELD FREC AS RECID
   INDEX PKOD IS PRIMARY PERSONALKOD DATUM
   INDEX DATUM DATUM PERSONALKOD
   INDEX TIMMAR TIMMAR 
   INDEX ACCFORE   ACCFORE   
   INDEX ACCEFTER  ACCEFTER  
   INDEX ANVANDARE ANVANDARE .
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER frecvar AS RECID NO-UNDO.
DEFINE INPUT PARAMETER hjtim AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR kompbettemp.

DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE hjvart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).

END FUNCTION.


FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
IF vadgora = 1 THEN DO:
   EMPTY TEMP-TABLE kompbettemp  NO-ERROR.    
   OPEN QUERY fq FOR EACH KBET NO-LOCK.
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(KBET):
      CREATE kompbettemp.
      BUFFER-COPY KBET TO kompbettemp.     
      kompbettemp.FORE = klock60(klock100(kompbettemp.ACCFORE) + klock100(kompbettemp.ACCKOMP)).                  
      kompbettemp.EFTER = klock60(klock100(kompbettemp.ACCEFTER) + klock100(kompbettemp.ACCKOMP)).      
      kompbettemp.FREC = RECID(KBET).
      GET NEXT fq NO-LOCK.
   END.
END.
IF vadgora = 2 THEN DO TRANSACTION:
   FIND FIRST kompbettemp WHERE kompbettemp.FREC = frecvar NO-ERROR.
   IF kompbettemp.FREC = ? THEN DO:
      CREATE KBET.
      kompbettemp.FREC = RECID(KBET).
   END.
   ELSE DO:
      FIND KBET WHERE RECID(KBET) = kompbettemp.FREC EXCLUSIVE-LOCK NO-ERROR.
   END.
   BUFFER-COPY kompbettemp TO KBET.
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FIND FIRST KOMPSALDO WHERE KOMPSALDO.PERSONALKOD =  kompbettemp.PERSONALKOD EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE KOMPSALDO THEN DO:
         CREATE KOMPSALDO.              
         ASSIGN KOMPSALDO.PERSONALKOD = kompbettemp.PERSONALKOD.
      END.            
      IF frecvar = ? THEN DO:
         /*DET GÖRS BARA NYA REGISTRERINGAR*/
         ASSIGN 
         KBET.ACCFORE = KOMPSALDO.PERIODKOMP
         kompbettemp.ACCFORE = KOMPSALDO.PERIODKOMP
         KBET.ACCKOMP = KOMPSALDO.ACCKOMP
         kompbettemp.ACCKOMP = KOMPSALDO.ACCKOMP.
         kompbettemp.FORE = klock60(klock100(kompbettemp.ACCFORE) + klock100(kompbettemp.ACCKOMP)).         
         KOMPSALDO.PERIODKOMP = klock60(klock100(KOMPSALDO.PERIODKOMP) + klock100(KBET.TIMMAR)).
         KBET.ACCEFTER = KOMPSALDO.PERIODKOMP. 
         kompbettemp.ACCEFTER = KBET.ACCEFTER.
         kompbettemp.EFTER = klock60(klock100(kompbettemp.ACCEFTER) + klock100(kompbettemp.ACCKOMP)).         
      END.         
   END. 
END.

