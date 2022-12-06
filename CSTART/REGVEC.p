  /* REGVEC.P R REGISTRERINGSVECKA DATUM GER VECKONUMMER*/
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE indate AS DATE NO-UNDO.
DEFINE VARIABLE yr AS INTEGER NO-UNDO.
DEFINE VARIABLE wn AS INTEGER NO-UNDO.
DEFINE VARIABLE d1 AS INTEGER NO-UNDO.
DEFINE VARIABLE dat1 AS DATE NO-UNDO.
DEFINE VARIABLE yyyyww AS INTEGER NO-UNDO FORMAT "999999".

indate = regdatum. 
ASSIGN
  yr   = YEAR(indate)
  d1   = WEEKDAY(DATE( 1 , 1 , yr))
  dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 
         ELSE DATE(1, 10, yr) - d1 )
  wn   = TRUNCATE((indate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
ASSIGN
  yr     = yr - 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
 			    DATE(1, 10, yr) - d1 )
  wn     = TRUNCATE((indate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
ASSIGN
  yr     = yr + 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
	      THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.

regvnr = INTEGER(SUBSTRING(STRING(yyyyww,"999999"),4,3)).
Guru.GlobalaVariabler:regvnr = regvnr.
/*
DEFINE VARIABLE rdag2 AS INTEGER FORMAT "9" NO-UNDO. 
DEFINE VARIABLE rdag AS INTEGER FORMAT "9" NO-UNDO.
DEFINE VARIABLE arh2 AS DECIMAL FORMAT "99.99999999" NO-UNDO.
DEFINE VARIABLE dag1 AS DATE NO-UNDO.
DEFINE VARIABLE dag2 AS INTEGER NO-UNDO. 
DEFINE VARIABLE ye1 AS INTEGER NO-UNDO.     
DEFINE VARIABLE ye2 AS INTEGER NO-UNDO. 
DEFINE VARIABLE regdag2 AS INTEGER FORMAT "9" NO-UNDO.
DEFINE VARIABLE ardatum AS DATE NO-UNDO.

IF MONTH(regdatum) = 12 THEN ye2 = ye1 + 1.
ELSE DO:
   ye2 = ye1.
   ye1 = ye1 - 1.
END.   
IF regdatum >= DATE(12,26,ye1) AND regdatum <= DATE(01,03,ye2) THEN DO:   
   dag1 = DATE(01,01,ye2).        
   IF WEEKDAY(dag1) = 1 THEN DO:        /*sön*/
      IF regdatum >= DATE(12,26,ye1) AND regdatum <= DATE(01,01,ye2) THEN 
      regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 52.
      ELSE regvnr = INTEGER(SUBSTRING(STRING(ye2),4,1)) * 100 + 01.
   END.
   ELSE IF WEEKDAY(dag1) = 2 THEN DO:          /*mån*/    
      IF regdatum >= DATE(12,26,ye1) AND regdatum <= DATE(12,31,ye1) THEN 
      regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 52.
      ELSE regvnr = INTEGER(SUBSTRING(STRING(ye2),4,1)) * 100 + 01.
   END.
   ELSE IF WEEKDAY(dag1) = 3 THEN DO:         /*tis*/    
      IF regdatum >= DATE(12,26,ye1) AND regdatum <= DATE(12,30,ye1) THEN 
      regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 52.
      ELSE regvnr = INTEGER(SUBSTRING(STRING(ye2),4,1)) * 100 + 01.
   END.
   ELSE IF WEEKDAY(dag1) = 4 THEN DO:         /*ons*/ 
      IF regdatum >= DATE(12,26,ye1) AND regdatum <= DATE(12,29,ye1) THEN 
      regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 52.
      ELSE regvnr = INTEGER(SUBSTRING(STRING(ye2),4,1)) * 100 + 01.
   END.
   ELSE IF WEEKDAY(dag1) = 5 THEN DO:        /*tor*/ 
      IF regdatum >= DATE(12,26,ye1) AND regdatum <= DATE(12,28,ye1) THEN 
      regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 52.
      ELSE regvnr = INTEGER(SUBSTRING(STRING(ye2),4,1)) * 100 + 01.
   END.
   ELSE IF WEEKDAY(dag1) = 6 THEN DO:          /*fre*/ 
      IF regdatum >= DATE(12,26,ye1) AND regdatum <= DATE(12,27,ye1) THEN 
      regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 52.
      ELSE regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 53.
   END.
   ELSE IF WEEKDAY(dag1) = 7 THEN DO:         /*LÖR*/         
      IF regdatum = DATE(12,26,ye1) THEN 
      regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 51.
      ELSE IF regdatum >= DATE(12,27,ye1) AND regdatum <= DATE(01,02,ye2) THEN 
      regvnr = INTEGER(SUBSTRING(STRING(ye1),4,1)) * 100 + 52.
      ELSE regvnr = INTEGER(SUBSTRING(STRING(ye2),4,1)) * 100 + 01.
   END.
END.
ELSE DO:   
   arh2 = decimal(SUBSTRING(STRING(YEAR(regdatum)),4  ,1)).
   dag1 = DATE(01,01,YEAR(regdatum)).   
   IF WEEKDAY(dag1) = 1 THEN rdag2 = 1.          /*sön*/
   ELSE IF WEEKDAY(dag1) = 2 THEN rdag2 = 0.          /*mån*/
   ELSE IF WEEKDAY(dag1) = 3 THEN rdag2 = -1.         /*tis*/
   ELSE IF WEEKDAY(dag1) = 4 THEN rdag2 = -2.         /*ons*/
   ELSE IF WEEKDAY(dag1) = 5 THEN rdag2 = -3.         /*tor*/
   ELSE IF WEEKDAY(dag1) = 6 THEN rdag2 = 3.          /*fre*/
   ELSE IF WEEKDAY(dag1) = 7 THEN rdag2 = 2.          /*lör*/    
   IF WEEKDAY(regdatum) = 1 THEN rdag = 0.          /*sön*/
   ELSE IF WEEKDAY(regdatum) = 2 THEN rdag = 6.          /*mån*/
   ELSE IF WEEKDAY(regdatum) = 3 THEN rdag = 5.         /*tis*/
   ELSE IF WEEKDAY(regdatum) = 4 THEN rdag = 4.         /*ons*/
   ELSE IF WEEKDAY(regdatum) = 5 THEN rdag = 3.         /*tor*/
   ELSE IF WEEKDAY(regdatum) = 6 THEN rdag = 2.          /*fre*/
   ELSE IF WEEKDAY(regdatum) = 7 THEN rdag = 1.          /*lör*/
   regdag2 = (regdatum + rdag) - DATE(01,01,YEAR(regdatum)).
   regdag2 = regdag2 + 1.
   regvnr = (regdag2 - rdag2) / 7.
   regvnr = arh2 * 100 + regvnr.    
END.
*/


