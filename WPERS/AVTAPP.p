/*AVTAPP.P*/
DEFINE TEMP-TABLE avtaltemp
   FIELD AVTALNAMN AS CHARACTER
   FIELD AVTALTYP AS CHARACTER
   FIELD TYP AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX TYP IS PRIMARY TYP ORDNING.
DEFINE VARIABLE varordning AS INTEGER NO-UNDO.
DEFINE VARIABLE sommar AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR avtaltemp.
FIND FIRST FORETAG  NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
varordning = 0.
OPEN QUERY bfq FOR EACH BEFATTNINGSTAB USE-INDEX BEF 
NO-LOCK BY BEFATTNINGSTAB.NAMN.
GET FIRST bfq NO-LOCK.
DO WHILE AVAILABLE(BEFATTNINGSTAB):                     
   IF Guru.Konstanter:globforetag = "sund"  AND BEFATTNINGSTAB.NAMN = "Ej tid"  THEN.
   ELSE IF Guru.Konstanter:globforetag = "SNAT"  AND BEFATTNINGSTAB.NAMN = "Ej tid"  THEN.
   /*ELSE IF Guru.Konstanter:globforetag = "MISV" AND BEFATTNINGSTAB.NAMN = "Ej tid"  THEN.*/
   ELSE DO:   
      IF BEFATTNINGSTAB.PLUSD = FALSE THEN DO:
         varordning = varordning + 1.
         CREATE avtaltemp.
         ASSIGN
         avtaltemp.ORDNING = varordning
         avtaltemp.AVTALNAMN = BEFATTNINGSTAB.NAMN 
         avtaltemp.AVTALTYP = BEFATTNINGSTAB.BEFATTNING 
         avtaltemp.TYP = "BEF".
      END. 
   END.
   GET NEXT bfq NO-LOCK.
END.     
varordning = 0.
OPEN QUERY berq FOR EACH BERTAB USE-INDEX BERTAB NO-LOCK.
GET FIRST berq NO-LOCK.   
DO WHILE AVAILABLE(BERTAB):
   
   sommar = FALSE.
   FIND FIRST BHOJ WHERE BHOJ.BERBYT = BERTAB.BEREDSKAPSAVTAL NO-LOCK NO-ERROR.
   IF AVAILABLE BHOJ THEN sommar = TRUE.
   /*IF Guru.Konstanter:globforetag = "sund" AND BERTAB.BEREDSKAPSAVTAL = "BR" THEN sommar = TRUE. /*Bredband skall inte vara valbart som avtal,bara ett val i regbilden för Beredskap driftled 7-16*/
   IF Guru.Konstanter:globforetag = "SNAT" AND BERTAB.BEREDSKAPSAVTAL = "BR" THEN sommar = TRUE. /*Bredband skall inte vara valbart som avtal,bara ett val i regbilden för Beredskap driftled 7-16*/*/
   IF sommar = TRUE THEN sommar = sommar.
   ELSE DO:   
      varordning = varordning + 1.
      CREATE avtaltemp.
      ASSIGN
      avtaltemp.ORDNING = varordning
      avtaltemp.AVTALNAMN = BERTAB.FORKL 
      avtaltemp.AVTALTYP = BERTAB.BEREDSKAPSAVTAL
      avtaltemp.TYP = "BER".
   END.
   GET NEXT berq NO-LOCK.      
END.      
varordning = 0.
OPEN QUERY traq FOR EACH TRAAVTAB USE-INDEX TRAAVTAB NO-LOCK.
GET FIRST traq NO-LOCK.
DO WHILE AVAILABLE(TRAAVTAB): 
   varordning = varordning + 1.
   CREATE avtaltemp.
   ASSIGN
   avtaltemp.ORDNING = varordning
   avtaltemp.AVTALNAMN = TRAAVTAB.FORKLARING 
   avtaltemp.AVTALTYP = TRAAVTAB.TRAAVTAL
   avtaltemp.TYP = "TRA".
   GET NEXT traq NO-LOCK.      
END.      
varordning = 0.
OPEN QUERY anstq FOR EACH ANSTFORMTAB USE-INDEX ANSTF NO-LOCK.
GET FIRST anstq NO-LOCK.
DO WHILE AVAILABLE(ANSTFORMTAB):         
   
   sommar = FALSE.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:            
      /* Ta bort sommaranställningar dessa sköts i otolkpr.p*/
      IF ANSTFORMTAB.KOD = "TO" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "TT" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "KO" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "T5" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "TV" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "TY" THEN sommar = TRUE.
      IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         /*mittsverige vill lägga upp personal som bara flexar.*/
         IF ANSTFORMTAB.KOD = "T" THEN sommar = TRUE. /*ej tidskrivande personal ej längre valbart - alla skriver tid*/
      END.   
   END.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:            
      /* Ta bort sommaranställningar dessa sköts i otolkpr.p*/
      IF ANSTFORMTAB.KOD = "TO" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "TP" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "KO" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "KQ" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "TS" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "TU" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "TX" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "T2" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "T9" THEN sommar = TRUE.
      IF ANSTFORMTAB.KOD = "ES" THEN sommar = TRUE.
      
   END.
   IF sommar = TRUE THEN sommar = sommar.
   ELSE DO:   
      varordning = varordning + 1.
      CREATE avtaltemp.
      ASSIGN
      avtaltemp.ORDNING = varordning
      avtaltemp.AVTALNAMN = ANSTFORMTAB.ANSTALLNING 
      avtaltemp.AVTALTYP = ANSTFORMTAB.ANSTALLNING
      avtaltemp.TYP = "ANS".
   END.
   GET NEXT anstq NO-LOCK.      
END.      
varordning = 0.
OPEN QUERY vktq FOR EACH VECKOARBETID USE-INDEX VECKOSCHEMA NO-LOCK.
GET FIRST vktq NO-LOCK.   
DO WHILE AVAILABLE(VECKOARBETID):
   varordning = varordning + 1.
   CREATE avtaltemp.
   ASSIGN
   avtaltemp.ORDNING = varordning
   avtaltemp.AVTALNAMN = STRING(VECKOARBETID.VECKOSCHEMA,">99") 
   avtaltemp.AVTALTYP = STRING(VECKOARBETID.VECKOSCHEMA,">99") 
   avtaltemp.TYP = "VEC".
   GET NEXT vktq NO-LOCK.
END.

varordning = 0.
varordning = varordning + 1.
CREATE avtaltemp.
ASSIGN
avtaltemp.ORDNING = varordning
avtaltemp.AVTALNAMN = "Alla med aktivt rullschema" 
avtaltemp.AVTALTYP = "Alla med aktivt rullschema" 
avtaltemp.TYP = "RUL".
OPEN QUERY rulq FOR EACH RULLSCHEMA USE-INDEX RULLID NO-LOCK.
GET FIRST rulq NO-LOCK.   
DO WHILE AVAILABLE(RULLSCHEMA):
   varordning = varordning + 1.
   CREATE avtaltemp.
   ASSIGN
   avtaltemp.ORDNING = varordning
   avtaltemp.AVTALNAMN = RULLSCHEMA.BENAMNING 
   avtaltemp.AVTALTYP = STRING(RULLSCHEMA.RULLID,">99") 
   avtaltemp.TYP = "RUL".
   GET NEXT rulq NO-LOCK.
END.
