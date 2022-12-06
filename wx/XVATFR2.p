/*XVATFR2.P SKAPAR PA90FIL FRANVARO.*/  
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

DEFINE VARIABLE antal LIKE TIDREGITAB.LONTILLANTAL NO-UNDO.   /*LON*/
DEFINE VARIABLE proc1 AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE pnr LIKE PERSONALTAB.PERSONNUMMER NO-UNDO.      /*LON*/
DEFINE VARIABLE tperiod AS INTEGER FORMAT "999" NO-UNDO. 
DEFINE SHARED TEMP-TABLE franvaro
   FIELD PERSONNUMMER LIKE PERSONALTAB.PERSONNUMMER
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR
   FIELD FRAN LIKE TIDREGITAB.DATUM
   FIELD TILL LIKE TIDREGITAB.DATUM
   FIELD PROCENT AS INTEGER FORMAT "999"
   FIELD TIMMAR AS DECIMAL FORMAT "99999.99"
   FIELD LART AS CHARACTER FORMAT "X(4)"
   INDEX FRANVARO IS PRIMARY  PERSONNUMMER FRAN LART ASCENDING.  

regdatum = TODAY.
RUN REGVEC.P.
ASSIGN
tperiod = regvnr + 19.
if globforetag = "elpa" then DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\test22.d NO-ECHO.
   for each franvaro:
      display franvaro.
   end.
   output close.   
END.  

IF globforetag = "VATT" THEN DO:
   OUTPUT TO /guru/export/lon/fran1.d NO-ECHO.
END.   
ELSE DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\fran1.d NO-ECHO.
END.   
 

FIND FIRST franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE franvaro THEN DO:
   
      /*nytid = franvaro.TIMMAR.
      RUN TIMSEK.P.
      franvaro.TIMMAR = sekunder / 36.*/
      ASSIGN overrapp1 = "".
      ASSIGN anstall = SUBSTRING(STRING(franvaro.PERSONNUMMER),1,6) +
                    SUBSTRING(STRING(franvaro.PERSONNUMMER),7,4).
      ASSIGN
      SUBSTRING(overrapp1,2,10) = anstall
      SUBSTRING(overrapp1,15,4) = SUBSTRING(franvaro.LART,1,4)
     
      
      SUBSTRING(overrapp1,22,6) = SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
                                  SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
                                  SUBSTRING(STRING(franvaro.FRAN),7 ,2)
      SUBSTRING(overrapp1,30,1) = "+"                             
      SUBSTRING(overrapp1,31,8) = STRING(franvaro.TIMMAR,"99999.99")                             
      SUBSTRING(overrapp1,40,1) = "+"  
      SUBSTRING(overrapp1,41,10) = "0000000.00"   
      SUBSTRING(overrapp1,52,6) = SUBSTRING(STRING(franvaro.TILL),1 ,2) +
                                  SUBSTRING(STRING(franvaro.TILL),4 ,2) +
                                  SUBSTRING(STRING(franvaro.TILL),7 ,2)
      SUBSTRING(overrapp1,59,3) = STRING(franvaro.PROCENT,"999").                            
                                  
      PUT overrapp1.
      PUT SKIP.
      DELETE franvaro.
   
END.
REPEAT:
  FIND NEXT franvaro USE-INDEX franvaro EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE franvaro THEN LEAVE.
  IF AVAILABLE franvaro THEN DO:    
        /*nytid = franvaro.TIMMAR.
        RUN TIMSEK.P.
        franvaro.TIMMAR = sekunder / 36.*/
        ASSIGN overrapp1 = "".
        ASSIGN anstall = SUBSTRING(STRING(franvaro.PERSONNUMMER),1,6) +
                    SUBSTRING(STRING(franvaro.PERSONNUMMER),7,4).
        ASSIGN
        SUBSTRING(overrapp1,2,10) = anstall
         
        SUBSTRING(overrapp1,15,4) = SUBSTRING(franvaro.LART,1,4)
        SUBSTRING(overrapp1,22,6) = SUBSTRING(STRING(franvaro.FRAN),1 ,2) +
                                    SUBSTRING(STRING(franvaro.FRAN),4 ,2) +
                                    SUBSTRING(STRING(franvaro.FRAN),7 ,2)
                                    
       SUBSTRING(overrapp1,30,1) = "+"                             
       SUBSTRING(overrapp1,31,8) = STRING(franvaro.TIMMAR,"99999.99")                             
       SUBSTRING(overrapp1,40,1) = "+"  
       SUBSTRING(overrapp1,41,10) = "0000000.00"                           
       SUBSTRING(overrapp1,52,6) = SUBSTRING(STRING(franvaro.TILL),1 ,2) +
                                    SUBSTRING(STRING(franvaro.TILL),4 ,2) +
                                    SUBSTRING(STRING(franvaro.TILL),7 ,2)                                                                           
       SUBSTRING(overrapp1,59,3) = STRING(franvaro.PROCENT,"999").                             
       PUT overrapp1.
       PUT SKIP.
       DELETE franvaro.    
  END.
END.
OUTPUT CLOSE.
IF globforetag = "VATT" THEN DO: 
   prognamn = "/guru/export/lon/lo".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy /guru/export/lon/lon.d VALUE(prognamn).    
   prognamn = "/guru/export/lon/fr".
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy /guru/export/lon/fran1.d VALUE(prognamn).
   prognamn = "/guru/export/lon/fe".
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy /guru/export/lon/frfelma.d VALUE(prognamn).  
   OS-APPEND /guru/export/lon/lon.d /guru/export/lon/lonfr.d.        
   OS-APPEND /guru/export/lon/fran1.d /guru/export/lon/lonfr.d. 
   prognamn = "/guru/export/lon/al".                             
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy /guru/export/lon/lonfr.d VALUE(prognamn).
END.   
IF globforetag = "ELPA" THEN DO: 
   prognamn = "D:\delad\pro8\GURU\apptemp\lo".
   prognamn = prognamn  + STRING(TODAY,"999999") + ".d".        
   OS-copy D:\delad\pro8\GURU\apptemp\lon.d VALUE(prognamn).    
   prognamn = "D:\delad\pro8\GURU\apptemp\fr".
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy D:\delad\pro8\GURU\apptemp\fran1.d VALUE(prognamn).
   prognamn = "D:\delad\pro8\GURU\apptemp\fe".
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy D:\delad\pro8\GURU\apptemp\frfelma.d VALUE(prognamn).   
   OS-APPEND D:\delad\pro8\GURU\apptemp\lon.d D:\delad\pro8\GURU\apptemp\lonfr.d.        
   OS-APPEND D:\delad\pro8\GURU\apptemp\fran1.d D:\delad\pro8\GURU\apptemp\lonfr.d.  
   prognamn = "D:\delad\pro8\GURU\apptemp\al".                             
   prognamn = prognamn +  STRING(TODAY,"999999") + ".d".    
   OS-copy D:\delad\pro8\GURU\apptemp\lonfr.d VALUE(prognamn).
END.     
IF globforetag = "VATT" THEN DO:
   OUTPUT TO /guru/export/lon/log.d NO-ECHO APPEND.
END.   
ELSE DO:
   OUTPUT TO D:\delad\pro8\GURU\apptemp\log.d NO-ECHO APPEND.
END.  
PUT globanv TODAY STRING(TIME,"HH:MM:SS").
OUTPUT CLOSE.
 
