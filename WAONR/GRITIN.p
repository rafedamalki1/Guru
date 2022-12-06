/*GRITIN.P*/   
DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
IF vart = 1 THEN RETURN.
   
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE tider AS CHARACTER NO-UNDO. 
DEFINE VARIABLE vartvar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE indate AS DATE NO-UNDO. 
DEFINE VARIABLE rad LIKE KOSTREG.RADNR NO-UNDO.
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(132)".  
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE kostemp 
   FIELD ANVANDARE  LIKE KOSTREG.ANVANDARE 
   FIELD AONR       LIKE KOSTREG.AONR 
   FIELD BENAMNING  LIKE KOSTREG.BENAMNING 
   FIELD BETDATUM   LIKE KOSTREG.BETDATUM 
   FIELD BOKKONTO   LIKE KOSTREG.BOKKONTO 
   FIELD DELNR      LIKE KOSTREG.DELNR 
   FIELD FAKBES     LIKE KOSTREG.FAKBES 
   FIELD FAKTNR     LIKE KOSTREG.FAKTNR 
   FIELD FAKTURERAD LIKE KOSTREG.FAKTURERAD 
   FIELD INKOMST    LIKE KOSTREG.INKOMST 
   FIELD KOSTAUTO   LIKE KOSTREG.KOSTAUTO 
   FIELD LEVKOD     LIKE KOSTREG.LEVKOD 
   FIELD MASKKOST   LIKE KOSTREG.MASKKOST 
   FIELD MOMS       LIKE KOSTREG.MOMS 
   FIELD MTRL       LIKE KOSTREG.MTRL 
   FIELD OVRKR      LIKE KOSTREG.OVRKR 
   FIELD PERSKOST   LIKE KOSTREG.PERSKOST 
   FIELD RADNR      LIKE KOSTREG.RADNR 
   FIELD REGDATUM   LIKE KOSTREG.REGDATUM 
   FIELD TRAKTKOST  LIKE KOSTREG.TRAKTKOST
   INDEX KOST IS PRIMARY AONR DELNR RADNR.
{AMERICANEUROPEAN.I}   
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.   
IF Guru.Konstanter:globforetag = "ELPA" THEN DO:            
   prognamnvar = "D:\DELAD\PRO9\guru\import\GG1PST".
   progkopia = "D:\DELAD\PRO9\guru\import\GG1PST" + STRING(TODAY,"999999").    
   progque = "D:\DELAD\PRO9\guru\import\GG1.Q".
END.
ELSE DO:            
   kommandoprog = "\\granguru\guru_ser\server\PRO9S\import\GRITIN.TXT".
   kommando = "DIR/a:-d /b \\granguru\guru_ser\server\PRO9S\import\GG1PST* > \\granguru\guru_ser\server\PRO9S\import\GRITIN.TXT".   
END.
OS-DELETE VALUE(kommandoprog) NO-ERROR.
OS-COMMAND SILENT VALUE(kommando).
INPUT FROM VALUE(kommandoprog) NO-ECHO.
REPEAT:
   DO TRANSACTION: 
      CREATE infil.
      ASSIGN.
      IMPORT infil   NO-ERROR.
   END.
END.
INPUT CLOSE.
FOR EACH infil:   
   IF INDEX(infil.PROGNAMN,"GG1PST.txt") = 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
   IF INDEX(infil.PROGNAMN,".FDF") NE 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
END.
FIND FIRST infil NO-ERROR.
IF NOT AVAILABLE infil THEN DO:
   OUTPUT TO \\granguru\guru_ser\server\PRO9S\autotid.txt  APPEND.
   PUT "DET FANNS INGEN FIL FRÅN EKO " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   RETURN.
END.
FOR EACH infil:  
   tider = REPLACE(STRING(TIME,"HH:MM"),":","").
   progkopia = "\\granguru\guru_ser\server\PRO9S\import\backineko\" + infil.PROGNAMN + STRING(TODAY,"99999999") + (tider).          
   prognamnvar = "\\granguru\guru_ser\server\PRO9S\import\" + infil.PROGNAMN.
   progque = "\\granguru\guru_ser\server\PRO9S\import\GG1.Q".
   tider = REPLACE(STRING(TIME,"HH:MM"),":","").
   RUN in_UI.      
   OS-RENAME VALUE(prognamnvar) VALUE(progkopia).
END.
{EUROPEANAMERICAN.I}
PROCEDURE in_UI:
   FOR EACH tidin:
      DELETE tidin.
   END.   
   OS-DELETE SILENT VALUE(progque).       
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN RETURN.
   ELSE OS-COMMAND SILENT VALUE(kommando) VALUE(prognamnvar) > VALUE(progque).      
   INPUT FROM VALUE(progque) NO-ECHO CONVERT TARGET "iso8859-1" SOURCE "ibm850".
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.   
      REPEAT:
         IF INDEX(words,'"',1) = 0 THEN LEAVE.
         words = REPLACE(words,'"',' ').
      END.
      CREATE TIDIN.   
      ASSIGN TIDIN.TIN = words.   
   END.
   INPUT CLOSE.                                                                         
   FOR EACH tidin:   
      /*VILKA SKALL EJ MED ?*/
      IF SUBSTRING(TIDIN.TIN,1,1) = "S" THEN NEXT.      
      IF SUBSTRING(TIDIN.TIN,3,1) = "9" THEN NEXT.
      IF SUBSTRING(TIDIN.TIN,15,6) = "" THEN NEXT. /*AONR*/
      rad = rad + 1.                                                                      
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(TIDIN.TIN,15,6) /*AND 
      AONRTAB.DELNR = INTEGER(SUBSTRING(TIDIN.TIN,21,3))    */
      USE-INDEX AONR NO-LOCK NO-ERROR.   
      IF AVAILABLE AONRTAB THEN DO:
         FIND LAST KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND 
         KOSTREG.DELNR = AONRTAB.DELNR
         USE-INDEX KOST NO-LOCK NO-ERROR.  
         rad = 1.                                                                      
         IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1. 
         indate = DATE(INTEGER(SUBSTRING(TIDIN.TIN,72,2)),01,INTEGER(SUBSTRING(TIDIN.TIN,68,4))).
         IF MONTH(indate) = 12 THEN DO:
            indate = DATE(12,31,YEAR(indate)).
         END.
         ELSE DO:   
            indate = DATE((MONTH(indate) + 1),01,YEAR(indate)) - 1.
         END.
         IF indate <= TODAY - 35 THEN indate = TODAY.
         CREATE KOSTREG.
         ASSIGN  
         KOSTREG.RADNR = rad
         KOSTREG.AONR = AONRTAB.AONR
         KOSTREG.DELNR = AONRTAB.DELNR
         KOSTREG.REGDATUM = TODAY       
         KOSTREG.REGDATUM = indate 
         KOSTREG.BETDATUM = TODAY
         KOSTREG.BENAMNING = SUBSTRING(TIDIN.TIN,86,19) 
         KOSTREG.BOKKONTO = SUBSTRING(TIDIN.TIN,3,5)
         KOSTREG.FAKTNR = SUBSTRING(TIDIN.TIN,37,8)
         KOSTREG.FAKTURERAD = ?
         KOSTREG.LEVKOD = ""
         SUBSTRING(KOSTREG.ANVANDARE,1,12) = "EKONOM"
         KOSTREG.KOSTAUTO = TRUE. 
         VALIDATE KOSTREG.
         vartvar = "".
         RUN kontokoll_UI (INPUT KOSTREG.BOKKONTO,OUTPUT vartvar).
         IF vartvar = "OVRIGT" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
         END.
         IF vartvar = "INTAKT" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) * -1 / 1000.
         END.  
         IF vartvar = "MASK1" OR vartvar = "MASK2" OR vartvar = "MASK3" THEN DO:
            ASSIGN KOSTREG.MASKKOST = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
         END.
         IF vartvar = "MTRL1" OR vartvar = "MTRL2" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
         END. 
         IF vartvar = "PERS1" OR vartvar = "PERS2" THEN DO:
            ASSIGN KOSTREG.PERSKOST = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
         END.                          
      END.  
   END.     
END PROCEDURE.
PROCEDURE kontokoll_UI:
   DEFINE INPUT PARAMETER varkont LIKE KOSTREG.BOKKONTO NO-UNDO.
   DEFINE OUTPUT PARAMETER varvart AS CHARACTER NO-UNDO.
   IF SUBSTRING(varkont,1,1) = "3" THEN vartvar = "INTAKT".
   ELSE IF varkont = "65400" THEN varvart = "MASK1".
   ELSE IF varkont = "65920" THEN varvart = "MASK2".
   ELSE IF varkont = "65980" THEN varvart = "MASK2".
   ELSE IF varkont = "62110" THEN varvart = "MASK3".
   ELSE IF varkont = "62120" THEN varvart = "MASK3".
   ELSE IF varkont = "62300" THEN varvart = "MASK3".
   

   ELSE IF varkont = "12200" THEN varvart = "MTRL2".
   ELSE IF varkont = "12210" THEN varvart = "MTRL2".
   ELSE IF varkont = "54670" THEN varvart = "MTRL2".
   ELSE IF varkont = "54690" THEN varvart = "MTRL2".

   ELSE IF varkont = "58100" THEN varvart = "PERS2". 
   ELSE IF varkont = "58200" THEN varvart = "PERS2". 
   ELSE IF varkont = "58310" THEN varvart = "PERS2". 
   ELSE IF varkont = "58320" THEN varvart = "PERS2". 
   ELSE IF varkont = "58910" THEN varvart = "PERS2". 
   ELSE IF varkont = "58920" THEN varvart = "PERS2". 
   ELSE IF varkont = "60710" THEN varvart = "PERS2". 
   ELSE IF varkont = "60720" THEN varvart = "PERS2". 

   ELSE IF varkont = "41030" THEN varvart = "OVRIGT".
   ELSE IF varkont = "50110" THEN varvart = "OVRIGT".
   ELSE IF varkont = "50800" THEN varvart = "OVRIGT".
   ELSE IF varkont = "52100" THEN varvart = "OVRIGT".
   ELSE IF varkont = "52210" THEN varvart = "OVRIGT".
   ELSE IF varkont = "52290" THEN varvart = "OVRIGT".
   ELSE IF varkont = "54200" THEN varvart = "OVRIGT".
   ELSE IF varkont = "56110" THEN varvart = "OVRIGT".
   ELSE IF varkont = "56120" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "56130" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "56150" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "57100" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "65410" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "65420" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "69700" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "69810" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "73800" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "73840" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "76100" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "76110" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "76210" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "76310" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "76320" THEN varvart = "OVRIGT". 
   ELSE IF varkont = "84200" THEN varvart = "OVRIGT".       
END PROCEDURE.                 
                               
                               
