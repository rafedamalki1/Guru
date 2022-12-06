/* VECOKOLL.P KOLL SA ATT VECKONUMMER AR RIMLIGT */
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.          
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE vecknumm AS INTEGER FORMAT "999" NO-UNDO. 
DEFINE VARIABLE vnr4 AS INTEGER NO-UNDO.  
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.          
DEFINE VARIABLE regvnrspar AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE dar AS INTEGER FORMAT "9999" NO-UNDO.

IF SUBSTRING(STRING(regvnr,"999"),2 ,2) = "00" THEN DO:    
   MESSAGE STRING(regvnr,"999") "Är inte ett rimligt veckonummer." VIEW-AS ALERT-BOX.
   musz = TRUE.
   RETURN.
END. 
IF SUBSTRING(STRING(regvnr,"999"),2 ,2) > "53" THEN DO:    
   MESSAGE STRING(regvnr,"999") "Är inte ett rimligt veckonummer." VIEW-AS ALERT-BOX.
   musz = TRUE.
   RETURN.
END.   

/*FÖR ATT FÅ RÄTT START DECAD*/
IF INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) = 9 THEN DO:
   IF INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) = 0 THEN DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) + 10.      
   END.
   ELSE DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
   END.
END. 
ELSE IF INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) = 0 THEN DO:
   IF INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) = 9 THEN DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) - 10.      
   END.
   ELSE DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
   END.
END.      
ELSE DO:
   dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
         INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
END.   
ASSIGN
regvnrspar = regvnr
regdatumspar = regdatum
vecknumm = regvnr.
vnr4 = INTEGER(SUBSTRING(STRING(vecknumm,"999"),1 ,1)).
ASSIGN
vnr4 = vnr4 + dar
regdatum = DATE(12,31,vnr4).
RUN arslut_UI.
RUN REGVEC.P.
IF SUBSTRING(STRING(vecknumm,"999"),2 ,2) = "53" THEN DO:
   IF SUBSTRING(STRING(vecknumm,"999"),2 ,2) NE 
   SUBSTRING(STRING(regvnr,"999"),2 ,2) THEN DO:
      MESSAGE STRING(vecknumm,"999") "Är inte ett rimligt veckonummer." VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.
END.
IF regvnr < vecknumm THEN DO: 
   ASSIGN
   regvnr = regvnrspar
   regdatum = regdatumspar.
   MESSAGE STRING(vecknumm,"999") "Är inte ett rimligt veckonummer." VIEW-AS ALERT-BOX.
   musz = TRUE.
   RETURN.
END.
regvnr = 001.
regvnr = regvnr + INTEGER(SUBSTRING(STRING(vnr4,"9999"),4,1)) * 100.
IF regvnr > vecknumm THEN DO:
   ASSIGN
   regvnr = regvnrspar
   regdatum = regdatumspar. 
   MESSAGE STRING(vecknumm,"999") "Är inte ett rimligt veckonummer." VIEW-AS ALERT-BOX.
   musz = TRUE.
   RETURN.
END.                  
ASSIGN     
regvnr = regvnrspar
regdatum = regdatumspar.
PROCEDURE arslut_UI:
   IF WEEKDAY(regdatum) = 1 THEN DO:          /*SÖN*/
      regdatum = regdatum. 
   END.
   ELSE IF WEEKDAY(regdatum) = 2 THEN DO:   /*MÅN*/
      regdatum = regdatum - 1. 
   END.   
   ELSE IF WEEKDAY(regdatum) = 3 THEN DO:   /*TIS*/
      regdatum = regdatum - 3. 
   END.
   ELSE IF WEEKDAY(regdatum) = 4 THEN DO:   /*ONS*/
      regdatum = regdatum - 4. 
   END.
   ELSE IF WEEKDAY(regdatum) = 5 THEN DO:   /*TOR*/
      regdatum = regdatum.
   END.
   ELSE IF WEEKDAY(regdatum) = 6 THEN DO:   /*FRE*/  
      regdatum = regdatum. 
   END.
   ELSE IF WEEKDAY(regdatum) = 7 THEN DO:   /*LÖR*/
      regdatum = regdatum.
   END.
END PROCEDURE.
