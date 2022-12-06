     /* VECODAT.P R REGISTRERINGSVECKA */
/*VECKA + DAG GER DATUM VECKDAT.P*/
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE regvnrspar AS INTEGER NO-UNDO.
DEFINE VARIABLE dag1 AS date NO-UNDO.
DEFINE VARIABLE dag2 AS integer NO-UNDO.
DEFINE VARIABLE rdagh2 AS DECIMAL FORMAT "-9" NO-UNDO.
DEFINE VARIABLE regvnrh AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE regdagnr AS INTEGER FORMAT "9" NO-UNDO.
DEFINE VARIABLE antaldag AS INTEGER NO-UNDO.  
DEFINE VARIABLE ye1 AS INTEGER NO-UNDO.     
DEFINE VARIABLE ye2 AS INTEGER NO-UNDO.    
DEFINE VARIABLE dar AS INTEGER FORMAT "9999" NO-UNDO.
DEFINE VARIABLE indate AS DATE NO-UNDO.
/*FÖR ATT FÅ RÄTT START DECAD*/
/*IF regvnr = 0 OR regdagnamn = "" THEN DO:
   MESSAGE "NU ÄR DET NÅGOT SOM ÄR FEL! KONTAKTA ANDERS OLSSON ELPOOL TEL 090/184540
   DU KAN VINNA EN T-TRÖJA !" VIEW-AS ALERT-BOX. 
END.*/
/*om årtalet slutar på 9*/
IF INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) = 9 THEN DO:
   /*om veckans år börjar på 0 ta nästa dec*/ 
   IF INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) = 0 THEN DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) + 10.      
   END.
   ELSE DO:
      /*samma dec*/
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
   END.
END. 
/*om årtalet slutar på 0*/
ELSE IF INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) = 0 THEN DO:
   /*om veckans år börjar på 9 ta föregående dec*/ 
   IF INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) = 9 THEN DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) - 10.      
   END.
   ELSE DO:
         /*samma dec*/
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
   END.
END.        
/*om årtalet slutar på 8*/
ELSE  IF INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) = 8 THEN DO:
      /*samma dec*/
   dar = INTEGER(STRING(YEAR(TODAY),"9999")) - INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)). 
END. 
ELSE DO:
    /*om veckans år börjar på 9 ta föregående dec*/ 
   IF INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) = 9 THEN DO:
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)) - 10.      
   END.        
   ELSE DO:   
            /*samma dec*/
      dar = INTEGER(STRING(YEAR(TODAY),"9999")) - 
            INTEGER(SUBSTRING(STRING(YEAR(TODAY),"9999"),4,1)).
   END. 
END.
IF SUBSTRING(STRING(regvnr,"999"),2,2) = "52" THEN DO:
   indate = regdatum.
   regvnrspar = regvnr.
   dag2 = INTEGER(SUBSTRING(STRING(regvnr,"999"),1 ,1)).
   dag2 = dar + dag2.
   regdatum = DATE(12,31,dag2).
   RUN REGVEC.P.
   regdatum = indate.
   IF regvnrspar = regvnr THEN DO:
      regvnr = regvnrspar.
      RUN v52_UI.
   END.
   ELSE DO:
      regvnr = regvnrspar.
      RUN dat_UI.
   END.
END.

ELSE IF SUBSTRING(STRING(regvnr,"999"),2,2) = "53"  OR 
   SUBSTRING(STRING(regvnr,"999"),2,2) = "01" THEN DO:       
   RUN v52_UI.
END.
ELSE DO:                                         
   RUN dat_UI.
END.
     
PROCEDURE v52_UI:
   IF SUBSTRING(STRING(regvnr,"999"),2,2) = "01" THEN DO:       
      ye1 = INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)). 
   END.
   ELSE DO:  
      ye1 = INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) + 1.     
   END.
   ASSIGN
   dag2 = dar + ye1
   dag1 = DATE(01,01,dag2).   
   IF WEEKDAY(dag1) = 1 THEN DO:          /*SÖN*/
      IF (SUBSTRING(STRING(regvnr,"999"),2,2)) = "52" THEN DO:
         IF regdagnamn = 'sön' THEN regdagnr = 0.
         ELSE IF regdagnamn = 'mån' THEN regdagnr = -6.
         ELSE IF regdagnamn = 'tis' THEN regdagnr = -5.
         ELSE IF regdagnamn = 'ons' THEN regdagnr = -4.
         ELSE IF regdagnamn = 'tor' THEN regdagnr = -3.
         ELSE IF regdagnamn = 'fre' THEN regdagnr = -2.
         ELSE IF regdagnamn = 'lör' THEN regdagnr = -1. 
         regdatum = dag1 + regdagnr.
      END.
      ELSE RUN dat_UI. 
   END.
   ELSE IF WEEKDAY(dag1) = 2 THEN DO:   /*MÅN*/
      RUN dat_UI. 
   END.   
   ELSE IF WEEKDAY(dag1) = 3 THEN DO:   /*TIS*/
      IF (SUBSTRING(STRING(regvnr,"999"),2,2)) = "01" THEN DO:
         IF regdagnamn = 'sön' THEN regdagnr = 5.
         ELSE IF regdagnamn = 'mån' THEN regdagnr = -1.
         ELSE IF regdagnamn = 'tis' THEN regdagnr = 0.
         ELSE IF regdagnamn = 'ons' THEN regdagnr = 1.
         ELSE IF regdagnamn = 'tor' THEN regdagnr = 2.
         ELSE IF regdagnamn = 'fre' THEN regdagnr = 3.
         ELSE IF regdagnamn = 'lör' THEN regdagnr = 4. 
         regdatum = dag1 + regdagnr.
      END.
      ELSE RUN dat_UI. 
   END.
   ELSE IF WEEKDAY(dag1) = 4 THEN DO:   /*ONS*/
      IF (SUBSTRING(STRING(regvnr,"999"),2,2)) = "01" THEN DO:
         IF regdagnamn = 'sön' THEN regdagnr = 4.
         ELSE IF regdagnamn = 'mån' THEN regdagnr = -2.
         ELSE IF regdagnamn = 'tis' THEN regdagnr = -1.
         ELSE IF regdagnamn = 'ons' THEN regdagnr = 0.
         ELSE IF regdagnamn = 'tor' THEN regdagnr = 1.
         ELSE IF regdagnamn = 'fre' THEN regdagnr = 2.
         ELSE IF regdagnamn = 'lör' THEN regdagnr = 3. 
         regdatum = dag1 + regdagnr.
      END.
      ELSE RUN dat_UI. 
   END.
   ELSE IF WEEKDAY(dag1) = 5 THEN DO:   /*TOR*/
      IF (SUBSTRING(STRING(regvnr,"999"),2,2)) = "01" THEN DO:
         IF regdagnamn = 'sön' THEN regdagnr = 3.
         ELSE IF regdagnamn = 'mån' THEN regdagnr = -3.
         ELSE IF regdagnamn = 'tis' THEN regdagnr = -2.
         ELSE IF regdagnamn = 'ons' THEN regdagnr = -1.
         ELSE IF regdagnamn = 'tor' THEN regdagnr = 0.
         ELSE IF regdagnamn = 'fre' THEN regdagnr = 1.
         ELSE IF regdagnamn = 'lör' THEN regdagnr = 2. 
         regdatum = dag1 + regdagnr.
      END.
      ELSE RUN dat_UI. 
   END.
   ELSE IF WEEKDAY(dag1) = 6 THEN DO:   /*FRE*/  
      IF (SUBSTRING(STRING(regvnr,"999"),2,2)) = "53" THEN DO:
         IF regdagnamn = 'sön' THEN regdagnr = 2.
         ELSE IF regdagnamn = 'mån' THEN regdagnr = -4.
         ELSE IF regdagnamn = 'tis' THEN regdagnr = -3.
         ELSE IF regdagnamn = 'ons' THEN regdagnr = -2.
         ELSE IF regdagnamn = 'tor' THEN regdagnr = -1.
         ELSE IF regdagnamn = 'fre' THEN regdagnr = 0.
         ELSE IF regdagnamn = 'lör' THEN regdagnr = 1. 
         regdatum = dag1 + regdagnr.
      END.
      ELSE RUN dat_UI. 
   END.
   ELSE IF WEEKDAY(dag1) = 7 THEN DO:   /*LÖR*/
        IF (SUBSTRING(STRING(regvnr,"999"),2,2)) = "52" THEN DO:
         IF regdagnamn = 'sön' THEN regdagnr = 1.
         ELSE IF regdagnamn = 'mån' THEN regdagnr = -5.
         ELSE IF regdagnamn = 'tis' THEN regdagnr = -4.
         ELSE IF regdagnamn = 'ons' THEN regdagnr = -3.
         ELSE IF regdagnamn = 'tor' THEN regdagnr = -2.
         ELSE IF regdagnamn = 'fre' THEN regdagnr = -1.
         ELSE IF regdagnamn = 'lör' THEN regdagnr = 0. 
         regdatum = dag1 + regdagnr.
      END.
      ELSE RUN dat_UI. 
   END.             
   ASSIGN
   dag2 = dar + ye1
   dag1 = DATE(01,01,dag2).   
END PROCEDURE.

PROCEDURE dat_UI.   
   ASSIGN
   dag2 = INTEGER(SUBSTRING(STRING(regvnr,"999"),1 ,1)).
   dag2 = dar + dag2.
   dag1 = DATE(01,01,dag2).
   IF WEEKDAY(dag1) = 1 THEN rdagh2 = 1.
   ELSE IF WEEKDAY(dag1) = 2 THEN rdagh2 = 0.
   ELSE IF WEEKDAY(dag1) = 3 THEN rdagh2 = -1.
   ELSE IF WEEKDAY(dag1) = 4 THEN rdagh2 = -2.
   ELSE IF WEEKDAY(dag1) = 5 THEN rdagh2 = -3.
   ELSE IF WEEKDAY(dag1) = 6 THEN rdagh2 = 3.
   ELSE IF WEEKDAY(dag1) = 7 THEN rdagh2 = 2.
   IF regdagnamn = 'SÖN' THEN regdagnr = 6.
   ELSE IF regdagnamn = 'mån' THEN regdagnr = 0.
   ELSE IF regdagnamn = 'tis' THEN regdagnr = 1.
   ELSE IF regdagnamn = 'ons' THEN regdagnr = 2.
   ELSE IF regdagnamn = 'tor' THEN regdagnr = 3.
   ELSE IF regdagnamn = 'fre' THEN regdagnr = 4.
   ELSE IF regdagnamn = 'lör' THEN regdagnr = 5. 
   IF regvnr < 100 THEN regvnrh = regvnr - 0.
   ELSE IF regvnr >= 100 AND regvnr < 200  THEN regvnrh = regvnr - 100.
   ELSE IF regvnr >= 200 AND regvnr < 300  THEN regvnrh = regvnr - 200.
   ELSE IF regvnr >= 300 AND regvnr < 400  THEN regvnrh = regvnr - 300.
   ELSE IF regvnr >= 400 AND regvnr < 500  THEN regvnrh = regvnr - 400.
   ELSE IF regvnr >= 500 AND regvnr < 600  THEN regvnrh = regvnr - 500.
   ELSE IF regvnr >= 600 AND regvnr < 700  THEN regvnrh = regvnr - 600.
   ELSE IF regvnr >= 700 AND regvnr < 800  THEN regvnrh = regvnr - 700. 
   ELSE IF regvnr >= 800 AND regvnr < 900  THEN regvnrh = regvnr - 800.
   ELSE IF regvnr >= 900 AND regvnr < 1000 THEN regvnrh = regvnr - 900.
   antaldag = (regvnrh * 7 - 6) + rdagh2 + regdagnr.
   regdatum = dag1 + antaldag - 1.
END.                     
