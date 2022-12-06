/*STVAFR2.P körs ej*/  
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
/*DEFINE SHARED VARIABLE globanv LIKE ANVANDARE.ANVANDARE NO-UNDO.*/

DEFINE VARIABLE rapphj1 AS CHARACTER FORMAT "X(8)" NO-UNDO.      /*HJALP ANTAL*/
DEFINE VARIABLE overrapp1 AS CHARACTER FORMAT "X(100)" NO-UNDO.      /*LON*/
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE anstall AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE anstperson LIKE PERSONALTAB.ANSTNR NO-UNDO.
DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LON*/
DEFINE VARIABLE proc1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE pnr LIKE PERSONALTAB.ANSTNR NO-UNDO.      /*LON*/
DEFINE SHARED TEMP-TABLE franvaro
   FIELD ANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99999.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"
   FIELD STARTKL LIKE TIDREGITAB.START
   FIELD SLUTKL LIKE TIDREGITAB.SLUT
   INDEX FRANVARO IS PRIMARY  ANSTNR FRAN LART ASCENDING.  
DEFINE SHARED TEMP-TABLE pa90fil
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD START LIKE TIDREGITAB.START
   FIELD SLUT LIKE TIDREGITAB.SLUT
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL LIKE TIDREGITAB.BERANTAL
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD SLUTDATUM LIKE TIDREGITAB.DATUM INITIAL ?
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD MANAD AS INTEGER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PAVTAL AS CHARACTER
   FIELD TID AS LOGICAL
   FIELD BERKOLL AS LOGICAL INITIAL FALSE
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING.
DEFINE TEMP-TABLE r3till
   FIELD PANSTNR LIKE PERSONALTAB.ANSTNR
   FIELD START LIKE TIDREGITAB.START
   FIELD SLUT LIKE TIDREGITAB.SLUT
   FIELD PLONTILLAGG LIKE TIDREGITAB.LONTILLAGG
   FIELD PLONTILLANTAL LIKE TIDREGITAB.LONTILLANTAL
   FIELD POVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   FIELD POVERANTAL LIKE TIDREGITAB.OVERANTAL
   FIELD PTRAKTKOD LIKE TIDREGITAB.TRAKTKOD
   FIELD PTRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL
   FIELD PBEREDSKAP LIKE TIDREGITAB.BEREDSKAP
   FIELD PBERANTAL LIKE TIDREGITAB.BERANTAL
   FIELD PDATUM LIKE TIDREGITAB.DATUM
   FIELD SLUTDATUM LIKE TIDREGITAB.DATUM INITIAL ?
   FIELD PVECKONUMMER LIKE TIDREGITAB.VECKONUMMER
   FIELD MANAD AS INTEGER
   FIELD PSORT AS CHARACTER FORMAT "XX"
   FIELD PPAR8 AS DECIMAL FORMAT "-99.99"
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD PAVTAL AS CHARACTER
   FIELD TID AS LOGICAL
   FIELD BERKOLL AS LOGICAL INITIAL FALSE
   INDEX PANSTNR IS PRIMARY PANSTNR ASCENDING.
regdatum = TODAY.
RUN REGVEC.P.
{AMERICANEUROPEAN.I}
if globforetag = "elpa" then DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\test22.d NO-ECHO.
   for each franvaro:
      display franvaro.
   end.
   output close.   
END.  

IF globforetag = "VATT" THEN DO:
   OUTPUT TO /guru/export/lon/stansfrtid.d NO-ECHO.
END.   
ELSE DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\stansfrtid.d NO-ECHO.
END.   
FOR EACH franvaro:
    CREATE pa90fil.
    ASSIGN pa90fil.PANSTNR = franvaro.ANSTNR
	       pa90fil.PLONTILLAGG = franvaro.LART	       
	       pa90fil.PDATUM = franvaro.FRAN	
           pa90fil.SLUTDATUM = franvaro.TILL
	       pa90fil.AONR = franvaro.AONR
           pa90fil.DELNR = franvaro.DELNR           
           pa90fil.TID = TRUE
           pa90fil.START = franvaro.STARTKL
           pa90fil.SLUT = franvaro.SLUTKL.
END.

IF globforetag = "VATT" THEN DO: 
   OUTPUT TO /guru/export/lon/stanstid.d NO-ECHO.
END.
IF globforetag = "elpa" THEN DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\stanstid.d.
END.  
anstperson = "".
FOR EACH pa90fil WHERE pa90fil.TID = TRUE BY pa90fil.PANSTNR BY pa90fil.PDATUM BY pa90fil.START
BY pa90fil.PLONTILLAGG BY pa90fil.POVERTIDTILL BY pa90fil.PBEREDSKAP :
   IF anstperson NE pa90fil.PANSTNR THEN PUT SKIP (1).
   IF pa90fil.START NE pa90fil.SLUT THEN DO: /* TA EJ MED 7-7 00-00 OSV*/
      ASSIGN overrapp1 = "".
      ASSIGN
      SUBSTRING(overrapp1,2,8) = STRING(pa90fil.PANSTNR).
      ASSIGN
      SUBSTRING(overrapp1,11,10) = STRING(pa90fil.PDATUM,"9999/99/99").
      IF pa90fil.SLUTDATUM NE ? THEN DO:
         SUBSTRING(overrapp1,22,10) = STRING(pa90fil.SLUTDATUM,"9999/99/99").
      END.
      ASSIGN
      SUBSTRING(overrapp1,33,5) = STRING(pa90fil.START,"99.99")
      SUBSTRING(overrapp1,39,5) = STRING(pa90fil.SLUT,"99.99").
      IF pa90fil.PLONTILLAGG NE "" THEN SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.PLONTILLAGG,1,4).
      ELSE IF pa90fil.POVERTIDTILL NE "" THEN SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.POVERTIDTILL,1,4).
      ELSE IF pa90fil.PBEREDSKAP NE "" THEN SUBSTRING(overrapp1,45,4) = SUBSTRING(pa90fil.PBEREDSKAP,1,4). 
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP. 
   END.   
   anstperson = pa90fil.PANSTNR.    
END.
OUTPUT CLOSE.
FOR EACH pa90fil WHERE pa90fil.PLONTILLAGG = "1150":
    nytid = pa90fil.PLONTILLANTAL.
    RUN TIMSEK.P.
    nytid = sekunder / 3600.
    pa90fil.PLONTILLANTAL = nytid.
END.
FOR EACH pa90fil WHERE pa90fil.PLONTILLAGG = "1160":
    nytid = pa90fil.PLONTILLANTAL.
    RUN TIMSEK.P.
    nytid = sekunder / 3600.
    pa90fil.PLONTILLANTAL = nytid.
END.

anstperson = "".
FOR EACH pa90fil  WHERE pa90fil.TID = FALSE AND pa90fil.PLONTILLAGG NE ""
   BREAK BY pa90fil.PANSTNR BY pa90fil.PLONTILLAGG:
   ACCUMULATE pa90fil.PLONTILLANTAL (TOTAL BY pa90fil.PANSTNR BY pa90fil.PLONTILLAGG).
   IF LAST-OF(pa90fil.PLONTILLAGG) THEN DO:   
      CREATE r3till.
      ASSIGN r3till.PANSTNR = pa90fil.PANSTNR 	  
      r3till.PLONTILLAGG = pa90fil.PLONTILLAGG 	    
	  r3till.PDATUM =  vkdatum
	  r3till.PVECKONUMMER = pa90fil.PVECKONUMMER 
	  r3till.AONR = pa90fil.AONR 
      r3till.DELNR =   pa90fil.DELNR 
      r3till.MANAD =   pa90fil.MANAD 
      r3till.PAVTAL  =   pa90fil.PAVTAL 
      r3till.TID = FALSE
      r3till.PLONTILLANTAL = (ACCUM TOTAL BY pa90fil.PLONTILLAGG pa90fil.PLONTILLANTAL). 
      
   END.
END.
anstperson = "".
FOR EACH pa90fil  WHERE pa90fil.TID = FALSE AND
   pa90fil.PTRAKTKOD NE ""  BREAK BY pa90fil.PANSTNR BY pa90fil.PTRAKTKOD:   
   ACCUMULATE pa90fil.PTRAKTANTAL (TOTAL BY pa90fil.PANSTNR BY pa90fil.PTRAKTKOD).  
   IF LAST-OF(pa90fil.PTRAKTKOD) THEN DO:   
      CREATE r3till.
      ASSIGN r3till.PANSTNR = pa90fil.PANSTNR 
	  r3till.PTRAKTKOD = pa90fil.PTRAKTKOD 	           
	  r3till.PDATUM =  vkdatum
	  r3till.PVECKONUMMER = pa90fil.PVECKONUMMER 
	  r3till.AONR = pa90fil.AONR 
      r3till.DELNR =   pa90fil.DELNR 
      r3till.MANAD =   pa90fil.MANAD 
      r3till.PAVTAL  =   pa90fil.PAVTAL 
      r3till.TID = FALSE      
      r3till.PTRAKTANTAL = (ACCUM TOTAL BY pa90fil.PTRAKTKOD pa90fil.PTRAKTANTAL).
   END.
END.       
IF globforetag = "VATT" THEN DO: 
   OUTPUT TO /guru/export/lon/stanstill.d NO-ECHO.
END.
IF globforetag = "elpa" THEN DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\stanstill.d.
END.   
anstperson = "".

FOR EACH r3till WHERE r3till.TID = FALSE BY r3till.PANSTNR BY r3till.PDATUM BY r3till.PLONTILLAGG
BY r3till.PTRAKTKOD:
   IF anstperson NE r3till.PANSTNR THEN PUT SKIP (1).
   IF r3till.PLONTILLANTAL = 0 AND r3till.PTRAKTANTAL = 0 THEN overrapp1 = overrapp1.
   ELSE DO:
      ASSIGN overrapp1 = "".   
      ASSIGN
      SUBSTRING(overrapp1,2,8) = STRING(r3till.PANSTNR).
      IF r3till.PLONTILLAGG NE "" THEN DO:
          /*IF r3till.PLONTILLAGG = "1150" OR r3till.PLONTILLAGG = "1160" THEN DO:
             SUBSTRING(overrapp1,45,4) = SUBSTRING(r3till.PLONTILLAGG,1,4).
             nytid = r3till.PLONTILLANTAL.
             RUN TIMSEK.P.
             nytid = sekunder / 3600.
             SUBSTRING(overrapp1,50,4) = STRING(nytid,">>>>>9.99").
          END.*/
         
          SUBSTRING(overrapp1,45,4) = SUBSTRING(r3till.PLONTILLAGG,1,4).
          SUBSTRING(overrapp1,50,4) = STRING(r3till.PLONTILLANTAL,">>>>>9.99").
         
      END.    
      ELSE IF r3till.PTRAKTKOD NE "" THEN DO:
         SUBSTRING(overrapp1,45,4) = SUBSTRING(r3till.PTRAKTKOD,1,4).
         SUBSTRING(overrapp1,50,4) = STRING(r3till.PTRAKTANTAL,">>>>>9.99").
      END.   
      ASSIGN
      SUBSTRING(overrapp1,11,10) = STRING(r3till.PDATUM,"9999/99/99"). 
   END.
   
   anstperson = r3till.PANSTNR.   
   PUT overrapp1.   /*  r3tillen.P    */
   PUT SKIP. 
END.
OUTPUT CLOSE.
/*anstperson = "".
FIND FIRST franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE franvaro THEN DO:

   IF anstperson NE franvaro.ANSTNR THEN PUT SKIP (1).
   ASSIGN overrapp1 = "".
   ASSIGN
   SUBSTRING(overrapp1,2,8) = STRING(franvaro.ANSTNR).
   ASSIGN
   SUBSTRING(overrapp1,11,10) = STRING(franvaro.FRAN,"9999/99/99").
   SUBSTRING(overrapp1,22,10) = STRING(franvaro.TILL,"9999/99/99").
   IF franvaro.STARTKL NE 0 AND franvaro.SLUTKL NE 0 THEN DO:
      ASSIGN
      SUBSTRING(overrapp1,33,5) = STRING(franvaro.STARTKL,"99.99")
      SUBSTRING(overrapp1,39,5) = STRING(franvaro.SLUTKL,"99.99").
   END.   
   SUBSTRING(overrapp1,45,4) = SUBSTRING(franvaro.LART,1,4).
   PUT overrapp1.   /*  pa90filen.P    */
   PUT SKIP. 
   anstperson = franvaro.ANSTNR.                                         
   DELETE franvaro.   
END.
REPEAT:
  FIND NEXT franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE franvaro THEN LEAVE.
  IF AVAILABLE franvaro THEN DO:    
     IF anstperson NE franvaro.ANSTNR THEN PUT SKIP (1).
      ASSIGN overrapp1 = "".
      ASSIGN
      SUBSTRING(overrapp1,2,8) = STRING(franvaro.ANSTNR).
      ASSIGN
      SUBSTRING(overrapp1,11,10) = STRING(franvaro.FRAN,"9999/99/99").
      SUBSTRING(overrapp1,22,10) = STRING(franvaro.TILL,"9999/99/99").
      IF franvaro.STARTKL NE 0 AND franvaro.SLUTKL NE 0 THEN DO:
         ASSIGN
         SUBSTRING(overrapp1,33,5) = STRING(franvaro.STARTKL,"99.99")
         SUBSTRING(overrapp1,39,5) = STRING(franvaro.SLUTKL,"99.99").
      END.   
      SUBSTRING(overrapp1,45,4) = SUBSTRING(franvaro.LART,1,4).
      PUT overrapp1.   /*  pa90filen.P    */
      PUT SKIP. 
      anstperson = franvaro.ANSTNR.                                         
      DELETE franvaro.
            
  END.
END.*/
OUTPUT CLOSE.
IF globforetag = "VATT" THEN DO: 
   prognamn = "/guru/export/lon/tid".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy /guru/export/lon/stanstid.d VALUE(prognamn).    
   prognamn = "/guru/export/lon/till".
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy /guru/export/lon/stanstill.d VALUE(prognamn).
/*   prognamn = "/guru/export/lon/fr".
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy /guru/export/lon/stansfrtid.d VALUE(prognamn).  
   OS-APPEND /guru/export/lon/stansfrtid.d /guru/export/lon/stanstid.d.        
   prognamn = "/guru/export/lon/tidfr".                             
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy /guru/export/lon/stanstid.d VALUE(prognamn).    */
END.   
IF globforetag = "ELPA" THEN DO: 
   prognamn = "D:\delad\pro8\GURU\apptemp\tid".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy D:\delad\pro8\GURU\apptemp\stanstid.d VALUE(prognamn).    
   prognamn = "D:\delad\pro8\GURU\apptemp\till".
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy D:\delad\pro8\GURU\apptemp\stanstill.d VALUE(prognamn).
   /*prognamn = "D:\delad\pro8\GURU\apptemp\fr".
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy D:\delad\pro8\GURU\apptemp\stansfrtid.d VALUE(prognamn).   
   OS-APPEND D:\delad\pro8\GURU\apptemp\stansfrtid.d D:\delad\pro8\GURU\apptemp\stanstid.d.        
   prognamn = "D:\delad\pro8\GURU\apptemp\tidfr".                             
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy D:\delad\pro8\GURU\apptemp\stanstid.d VALUE(prognamn).*/
END.     
/*IF globforetag = "VATT" THEN DO:
   OUTPUT TO /guru/export/lon/log.d NO-ECHO APPEND.
END.   
ELSE DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\log.d NO-ECHO APPEND.
END.  
PUT globanv TODAY STRING(TIME,"HH:MM:SS") vkdatum.
OUTPUT CLOSE.
DO TRANSACTION:
   CREATE LOGGOD.
   ASSIGN 
   LOGGOD.FGB = "V"
   LOGGOD.DATUM = TODAY
   LOGGOD.ANVANDARE = Guru.Konstanter:globanv.
END.*/
 
{EUROPEANAMERICAN.I}