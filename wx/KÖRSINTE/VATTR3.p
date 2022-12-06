  /*VATTR3.P*/
DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO. 
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE tidnu AS CHARACTER NO-UNDO.
DEFINE VARIABLE lopnr AS INTEGER NO-UNDO.
DEFINE VARIABLE kontnr AS INTEGER NO-UNDO.
DEFINE VARIABLE forstarad AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE aoomrade LIKE AONRTAB.OMRADE NO-UNDO.          
DEFINE VARIABLE timtid AS DECIMAL FORMAT "99.99" NO-UNDO. 
DEFINE VARIABLE typdatum LIKE EKRAPPRESULT.ETRANSDATUM NO-UNDO. 
DEFINE VARIABLE radnr AS INTEGER NO-UNDO.
DEFINE VARIABLE ltvar AS CHARACTER NO-UNDO.
   
DEFINE NEW SHARED TEMP-TABLE ekoforst
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT 
   FIELD EORG LIKE EKRAPPRESULT.EORG
   FIELD EORGIDNUM LIKE OMRADETAB.ORGIDNUM
   FIELD ENUM AS INTEGER FORMAT "999"  
   FIELD EGEO LIKE EKRAPPRESULT.EGEO      
   FIELD ETRANSDATUM LIKE EKRAPPRESULT.ETRANSDATUM
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD EANTAL  LIKE EKRAPPRESULT.EANTAL 	       
   FIELD EANSTNR LIKE PERSONALTAB.ANSTNR 	       
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT
   INDEX ENUM ENUM.
{AMERICANEUROPEAN.I}   
/*HUR SKALL ÖVERTIDSTIMMAR RÄKNAS*/   
OPEN QUERY persq FOR EACH PERSONALTAB WHERE PERSONALTAB.BRAVO = TRUE 
USE-INDEX PERSONALKOD NO-LOCK.
GET FIRST persq NO-LOCK.
DO WHILE AVAILABLE(PERSONALTAB):
   musz = FALSE.
   aoomrade = "".
   OPEN QUERY tidq FOR EACH TIDREGITAB WHERE
   TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   TIDREGITAB.GODKAND BEGINS "G" AND TIDREGITAB.VECKOKORD = "" AND
   TIDREGITAB.PRISTYP NE "FRÅNVARO." AND TIDREGITAB.DATUM <= vkdatum AND 
   TIDREGITAB.TIDLOG = TRUE NO-LOCK. 
   GET FIRST tidq NO-LOCK.
   DO WHILE AVAILABLE(TIDREGITAB):  
      IF TIDREGITAB.AONR = "" THEN DO: 
         aoomrade = aoomrade.	        	      	       
      END.                             
      ELSE IF TIDREGITAB.PRISTYP = 'RESTID...' THEN DO: 
         aoomrade = aoomrade.	        	      	       
      END.
      ELSE IF LENGTH(TIDREGITAB.AONR) <= 3 THEN DO: 
         aoomrade = aoomrade.	        	      	       
      END.                           
      ELSE IF TIDREGITAB.PRISTYP = "EJ.KOSTN." THEN DO: 
         aoomrade = aoomrade.	        	      	       
      END.
      ELSE DO:         
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND
         AONRTAB.DELNR = TIDREGITAB.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
         aoomrade = AONRTAB.OMRADE.   
         IF AONRTAB.OMRADE = "" THEN aoomrade = PERSONALTAB.OMRADE.                  
         typdatum = TODAY.
         IF DAY(TODAY) >= 1 AND DAY(TODAY) <= 7 THEN typdatum = TODAY - 8.            
    	  FIND FIRST ekoforst WHERE
	  ekoforst.ENY = FALSE AND
         ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD AND 
         ekoforst.EORG = PERSONALTAB.OMRADE AND 
         ekoforst.EGEO = aoomrade AND
         ekoforst.EPROJEKT = TIDREGITAB.AONR AND         
         ekoforst.ETRANSDATUM = typdatum      
         USE-INDEX PERSORG NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekoforst THEN DO:
    	     CREATE ekoforst.
	     ASSIGN 
	     ekoforst.ENY = FALSE
	     ekoforst.EPERSONALKOD = TIDREGITAB.PERSONALKOD
	     ekoforst.EPROJEKT = TIDREGITAB.AONR 
	     ekoforst.EORG = PERSONALTAB.OMRADE
	     ekoforst.EANSTNR = PERSONALTAB.ANSTNR  
   	     ekoforst.EGEO = aoomrade 	    	    
	     ekoforst.ETRANSDATUM = typdatum.
         END.                                                      
	  /*TIDREGISTRERING*/	          	       
	  /*TIMMAR OCH PENNGAR*/                       
         ASSIGN ekoforst.EOVERJA = FALSE. 
         IF TIDREGITAB.OKOD1 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
         ELSE IF TIDREGITAB.OKOD2 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
         ELSE IF TIDREGITAB.OKOD3 NE " " THEN ASSIGN ekoforst.EOVERJA = TRUE.
         IF ekoforst.EOVERJA = FALSE AND TIDREGITAB.TOTALT > 0 THEN DO:                  
            nytid = TIDREGITAB.TOTALT.
            RUN TIMSEK.P.
            timtid = (sekunder / 3600).
            ASSIGN 
            ekoforst.EANTAL = ekoforst.EANTAL + timtid.
         END.
         ELSE DO:
          /*ÖVERTIDTILLÄGG*/            
            IF TIDREGITAB.OKOD1 NE " " THEN DO: 
               ASSIGN
               nytid = TIDREGITAB.OANT1.
               RUN TIMSEK.P.               
               ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                              
            END.          
            IF TIDREGITAB.OKOD2 NE " " THEN DO: 
               ASSIGN               
               nytid = TIDREGITAB.OANT2.
               RUN TIMSEK.P.
               ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                 
            END.                 
            IF TIDREGITAB.OKOD3 NE " " THEN DO: 
               ASSIGN              
               nytid = TIDREGITAB.OANT3.
               RUN TIMSEK.P.
               RUN TIMSEK.P.                  
               ASSIGN ekoforst.EANTAL = ekoforst.EANTAL + (sekunder / 3600).                                     
            END.           
         END.
      END.
      GET NEXT tidq NO-LOCK.
   END.  /*ELSE DO*/
   GET NEXT persq NO-LOCK.
END.   /*FOR EACH*/     
OPEN QUERY eomrq FOR EACH ekoforst,
EACH OMRADETAB WHERE OMRADETAB.OMRADE = ekoforst.EORG NO-LOCK.   
GET FIRST eomrq.
DO WHILE AVAILABLE(ekoforst):
   ekoforst.ENUM = INTEGER(SUBSTRING(STRING(OMRADETAB.ORGIDNUM),1,3)).
   ekoforst.EORGIDNUM = OMRADETAB.ORGIDNUM.   
   GET NEXT eomrq.
END. 
radnr = 0.
FOR EACH ekoforst WHERE ekoforst.ENUM = 912:
   radnr = radnr + 1.
END.  
IF radnr > 0 THEN DO:
   ltvar = "LT10".
   DO TRANSACTION:
      FIND FIRST KORPERIOD EXCLUSIVE-LOCK NO-ERROR. 
      lopnr = KORPERIOD.PERIOD.
      ASSIGN
      KORPERIOD.KORDATUM = TODAY
      KORPERIOD.PERIOD = KORPERIOD.PERIOD + 1.
   END.   
   kontnr = 960000.
   RUN namn_UI.
   RUN ut_UI (INPUT 912).
END.
radnr = 0.
FOR EACH ekoforst WHERE ekoforst.ENUM = 911:
   radnr = radnr + 1.
END.  
IF radnr > 0 THEN DO:
   ltvar = "LT11".
   DO TRANSACTION:
      FIND FIRST KORPERIOD EXCLUSIVE-LOCK NO-ERROR. 
      lopnr = KORPERIOD.PERIOD.
      ASSIGN
      KORPERIOD.KORDATUM = TODAY
      KORPERIOD.PERIOD = KORPERIOD.PERIOD + 1.
   END.
   kontnr = 960000.
   RUN namn_UI.
   RUN ut_UI (INPUT 911).
END.
radnr = 0.
FOR EACH ekoforst WHERE ekoforst.ENUM = 913:
   radnr = radnr + 1.
END.  
IF radnr > 0 THEN DO:
   ltvar = "LT12".
   DO TRANSACTION:
      FIND FIRST KORPERIOD EXCLUSIVE-LOCK NO-ERROR. 
      lopnr = KORPERIOD.PERIOD.
      ASSIGN
      KORPERIOD.KORDATUM = TODAY
      KORPERIOD.PERIOD = KORPERIOD.PERIOD + 1.
   END.
   kontnr = 960000.   
   RUN namn_UI.   
   RUN ut_UI (INPUT 913).
END.
radnr = 0.
FOR EACH ekoforst WHERE ekoforst.ENUM = 910:
   radnr = radnr + 1.
END.  
IF radnr > 0 THEN DO:
   ltvar = "LT13".
   DO TRANSACTION:
      FIND FIRST KORPERIOD EXCLUSIVE-LOCK NO-ERROR. 
      lopnr = KORPERIOD.PERIOD.
      ASSIGN
      KORPERIOD.KORDATUM = TODAY
      KORPERIOD.PERIOD = KORPERIOD.PERIOD + 1.
   END.
   kontnr = 960000.
   RUN namn_UI.
   RUN ut_UI (INPUT 910).
END. 
{EUROPEANAMERICAN.I}  
PROCEDURE namn_UI:
   tidnu = SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2).
   prognamn = "/guru/export/2520" + "-" + STRING(TODAY,"99999999") + "-" + tidnu + "." + lc(ltvar).
   forstarad = "2520" + STRING(TODAY,"99999999") + tidnu + ltvar + "/usr/sap/KIS/tratt/2520-" + STRING(TODAY,"99999999") + "-" + tidnu + "." + lc(ltvar) + "." + STRING(radnr,"99999999").    
END PROCEDURE.

PROCEDURE ut_UI:
   DEFINE INPUT PARAMETER bolagvar AS INTEGER NO-UNDO. 
   OUTPUT TO VALUE(prognamn).
   PUT forstarad AT 1 SKIP.
   FOR EACH ekoforst WHERE ekoforst.ENUM = bolagvar:
      str= "25XX35" + STRING(lopnr,"99999999") + STRING(ekoforst.ETRANSDATUM,"99999999") +
      "000" + STRING(ekoforst.EANSTNR,"99999999") + "ZNORM" + "           " +
      STRING(INTEGER(ekoforst.EPROJEKT),"999999999999") + "                  " + 
      STRING(ekoforst.EANTAL * 1000,"999999999999999") + "000000000000000" + " " +
      STRING(kontnr,"9999999999")+ "002520" + STRING(ekoforst.EORGIDNUM,"9999").
      PUT str SKIP.
   END.   
END PROCEDURE.
