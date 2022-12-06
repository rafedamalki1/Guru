/*SUNDIN.P*/   
/*DUBBELKOLL
  BEGÄR BOKDATUM I TRANS + VERKSAMHET
  2. KOLLA OM AONR VERIFNUMMER + FINNS MED TIDIGARE DATUM.
  3. OM JA STOPPA INLÄSNINGEN OCH VARNA.
  4. OM NEJ OK.
  
*/
DEFINE STREAM eko.  
DEFINE STREAM ekospar.
DEFINE STREAM ekofel.  
DEFINE STREAM ekofelspar.
  
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE tider AS CHARACTER NO-UNDO. 
DEFINE VARIABLE indate AS DATE NO-UNDO. 
DEFINE VARIABLE rad LIKE KOSTREG.RADNR NO-UNDO.
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE koant AS INTEGER NO-UNDO.
DEFINE VARIABLE rrakn AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE okaonr NO-UNDO
  FIELD AONR AS CHARACTER
  FIELD  DELNR AS INTEGER
  INDEX AONR AONR DELNR. 
DEFINE TEMP-TABLE slutut
   FIELD DEBKRED AS LOGICAL 
   FIELD OMRADE AS CHARACTER
   FIELD PROJEKT AS CHARACTER 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD VERDATUM AS CHARACTER
   FIELD KOSTNADSSLAG AS CHARACTER
   FIELD ANTAL AS DECIMAL 
   FIELD BELOPP AS DECIMAL       
   FIELD K1 AS CHARACTER 
   FIELD K2 AS CHARACTER
   FIELD K2POS8 AS CHARACTER
   INDEX ORG IS PRIMARY DEBKRED OMRADE PROJEKT KOSTNADSSLAG K1 K2.
DEFINE TEMP-TABLE tidin
   FIELD RAD AS INTEGER
   FIELD TIN AS CHARACTER FORMAT "X(132)"
   FIELD DATUM AS DATE
   INDEX DATUM IS PRIMARY DATUM
   INDEX RAD RAD.
DEFINE TEMP-TABLE supertidin LIKE tidin.
DEFINE TEMP-TABLE tidinfel LIKE tidin.
DEFINE TEMP-TABLE tidinkonto
   FIELD TIN AS CHARACTER FORMAT "X(132)"
   FIELD DATUM AS DATE
   INDEX DATUM IS PRIMARY DATUM.
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
DEFINE TEMP-TABLE inkoll NO-UNDO LIKE KOSTREG.
DEFINE VARIABLE grundmapp AS CHARACTER NO-UNDO.

{AMERICANEUROPEAN.I}
EMPTY TEMP-TABLE okaonr NO-ERROR. 
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.   
IF Guru.Konstanter:globforetag = "ELPA" THEN DO: 
   grundmapp = "C:\DELAD\pro9\".           
   prognamnvar = "C:\DELAD\pro9\Import\GURUIN.TXT".
   progkopia = "C:\DELAD\pro9\Import\GN9PST" + STRING(TODAY,"999999").    
   progque = "C:\DELAD\pro9\Import\GN9PST.Q".
   kommandoprog ="C:\DELAD\pro9\Import\guruin.txt".
   kommando = "DIR/a:-d /b C:\DELAD\pro9\Import\GURU.* > C:\DELAD\pro9\Import\GURUIN.TXT".   
END.
ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
   grundmapp = "D:\DELAD\PRO10S\".          
   kommandoprog = "D:\DELAD\PRO10S\import\GURUIN.TXT".
   kommando = "DIR/a:-d /b D:\DELAD\PRO10S\import\GURU.* > D:\DELAD\PRO10S\import\GURUIN.TXT".
      
END.
ELSE DO:  
   grundmapp = "D:\DELAD\SERVER\PRO10S\".          
   kommandoprog = "D:\DELAD\SERVER\PRO10S\import\GURUIN.TXT".
   kommando = "DIR/a:-d /b D:\DELAD\SERVER\PRO10S\import\GURU.* > D:\DELAD\SERVER\PRO10S\import\GURUIN.TXT".      
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
   IF INDEX(infil.PROGNAMN,"GURU.") = 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
END.
FIND FIRST infil NO-ERROR.
IF NOT AVAILABLE infil THEN DO:
   OUTPUT TO value(grundmapp + "autotid.txt")  APPEND.
   PUT "DET FANNS INGEN FIL FRÅN EKO " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END.
   
FOR EACH infil:  
   EMPTY TEMP-TABLE supertidin NO-ERROR. 
   EMPTY TEMP-TABLE tidinkonto NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR. 
   EMPTY TEMP-TABLE slutut NO-ERROR. 
   EMPTY TEMP-TABLE kostemp NO-ERROR. 
   tider = REPLACE(STRING(TIME,"HH:MM"),":","").
   progkopia = grundmapp + "backimport\" + infil.PROGNAMN + tider.          
   prognamnvar = grundmapp + "import\" + infil.PROGNAMN.
   progque = grundmapp + "import\GN9PST.Q".
   /*fil läs in i Guru kostregskapas*/
   OUTPUT TO value(grundmapp + "autotid.txt")  APPEND.
   PUT "FILER FRÅN EKO " Guru.Konstanter:globforetag " " prognamnvar " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   RUN in_UI.      
   OS-RENAME VALUE(prognamnvar) VALUE(progkopia).
   ASSIGN
   prognamnvar = grundmapp + "export\KOSTREG\GURUUT"
   progkopia = grundmapp + "backexport\GURUUT" + STRING(TODAY,"99999999").
   /*mot kontering tillbaka från aonrkontokod*/
   RUN ut_UI.

   ASSIGN
   
   progkopia = grundmapp + "backimport\GURUinfelElnät" + STRING(TODAY,"99999999").
   RUN ut2_UI.
   
END.

EMPTY TEMP-TABLE infil NO-ERROR. 
/*servanet 2009-01-12*/
kommandoprog = grundmapp + "import\GURUINSN.TXT".
kommando = "DIR/a:-d /b " + grundmapp + "import\GURUSN.* > " + grundmapp + "import\GURUINSN.TXT".   

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
   IF INDEX(infil.PROGNAMN,"GURUSN.") = 0 THEN DO:       
      DELETE infil.
      NEXT.
   END.
END.
FIND FIRST infil NO-ERROR.
IF NOT AVAILABLE infil THEN DO:
   
   OUTPUT TO VALUE(grundmapp + "autotid.txt")  APPEND.
   PUT "DET FANNS INGEN FIL FRÅN EKO servanet " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END.

FOR EACH infil:  
   EMPTY TEMP-TABLE supertidin NO-ERROR. 
   EMPTY TEMP-TABLE tidinkonto NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR. 
   EMPTY TEMP-TABLE slutut NO-ERROR. 
   EMPTY TEMP-TABLE kostemp NO-ERROR. 
   tider = REPLACE(STRING(TIME,"HH:MM"),":","").
   
   progkopia = grundmapp + "backimport\" + infil.PROGNAMN + tider.          
   prognamnvar = grundmapp + "import\" + infil.PROGNAMN.
   progque = grundmapp + "\import\GN9PST.Q".
   RUN in_UI.      
   OS-RENAME VALUE(prognamnvar) VALUE(progkopia).
   ASSIGN
   prognamnvar = grundmapp + "export\KOSTREG\GURUUTSN"
   progkopia = grundmapp + "backexport\GURUUTSN" + STRING(TODAY,"99999999").
   RUN ut_UI.

   
   ASSIGN
   
   progkopia = grundmapp + "backimport\GURUinfelSN" + STRING(TODAY,"99999999").
   RUN ut2_UI.
END.
OUTPUT TO VALUE(grundmapp + "backimport\ALLAINLASTAPROJ.txt") APPEND.
FOR EACH okaonr WHERE NO-LOCK:
   PUT UNFORMATTED okaonr.AONR " " STRING(okaonr.DELNR,"999") " " TODAY SKIP.  
END.
 OUTPUT CLOSE.
{EUROPEANAMERICAN.I}
PROCEDURE in_UI:
   EMPTY TEMP-TABLE tidinfel NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR.    
   OS-DELETE SILENT VALUE(progque).       
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN RETURN.
   ELSE OS-COMMAND SILENT VALUE(kommando) VALUE(prognamnvar) > VALUE(progque).      
   INPUT FROM VALUE(progque) NO-ECHO.
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850".*/
   rrakn = 0.
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.   
      REPEAT:
         IF INDEX(words,'"',1) = 0 THEN LEAVE.
         words = REPLACE(words,'"',' ').
      END.
      rrakn = rrakn + 1.
      CREATE tidin.   
      ASSIGN tidin.TIN = words
      tidin.RAD = rrakn.
   END.
   INPUT CLOSE.
   FOR EACH tidin USE-INDEX DATUM:  
      IF SUBSTRING(tidin.TIN,37,6) =  "682000" THEN DO:
         DELETE tidin.
         NEXT.
      END.   
      IF SUBSTRING(tidin.TIN,37,6) =  "369999" THEN DO:
         DELETE tidin.
         NEXT.
      END.   
      /*DELNR OK*/
      /*SUND DELNR*/
      rad = rad + 1.                                                                      
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,1,5) AND 
      AONRTAB.DELNR = INTEGER(SUBSTRING(tidin.TIN,6,3))    
      USE-INDEX AONR NO-LOCK NO-ERROR.   
      indate = DATE(SUBSTRING(TIDIN.TIN,143,8)).       
      tidin.DATUM = indate.
      IF AVAILABLE AONRTAB THEN DO:
         FIND LAST KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND 
         KOSTREG.DELNR = AONRTAB.DELNR
         USE-INDEX KOST NO-LOCK NO-ERROR.  
         rad = 1.                                                                      
         IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1.     
         indate = DATE(SUBSTRING(TIDIN.TIN,143,8)).       
         tidin.DATUM = indate.
         EMPTY TEMP-TABLE inkoll NO-ERROR. 
         CREATE inkoll.
         ASSIGN  
         inkoll.RADNR = rad
         inkoll.AONR = AONRTAB.AONR
         inkoll.DELNR = AONRTAB.DELNR
         /*DATUM EKO*/
         inkoll.REGDATUM = indate       
         /*DATUM IN I GURU*/
         inkoll.BETDATUM = TODAY
         inkoll.BENAMNING = TRIM(SUBSTRING(tidin.TIN,80,35)) + " " + SUBSTRING(tidin.TIN,44,35)
         inkoll.BOKKONTO = SUBSTRING(tidin.TIN,37,6)   
         inkoll.FAKTNR = SUBSTRING(tidin.TIN,29,7)
         inkoll.FAKTURERAD = ?
         inkoll.LEVKOD = ""
         SUBSTRING(inkoll.ANVANDARE,1,12) = "EKONOM"
         inkoll.KOSTAUTO = TRUE. 
        
         /*
         3 intäkt
         40-46 mtrl
         47-49 frmtj
         5 övrigt
         6 övrigt    (saknades 18/2 )
         7 personal  - ändrat till övrigt 18/2 2002 av Lena
         8 tas ej med  - ändrat till övrigt  19/2 2002 av Lena
         90-92 tas ej med ändrat till övrigt  3/4 2002 av Anders
         93 intäkt
         94-99 övrigt  (saknades 18/2)
         inkoll.FAKTNR = SUBSTRING(tidin.TIN,30,6) ändrad till inkoll.FAKTNR = SUBSTRING(tidin.TIN,29,7) 10/05/2005 Lena
         */
         IF SUBSTRING(inkoll.BOKKONTO,1,1) = "3"OR
            SUBSTRING(inkoll.BOKKONTO,1,2) = "93" THEN DO:
            ASSIGN inkoll.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,12,15)) * -1.
         END.  
         IF SUBSTRING(inkoll.BOKKONTO,1,2)= "40" OR 
            SUBSTRING(inkoll.BOKKONTO,1,2)= "41" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "42" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "43" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "44" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "45" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "46" THEN DO:
              ASSIGN inkoll.MTRL = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END. 
         IF SUBSTRING(inkoll.BOKKONTO,1,2)= "47" OR 
            SUBSTRING(inkoll.BOKKONTO,1,2)= "48" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "49" THEN DO:
            ASSIGN inkoll.MASKKOST = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END.
         IF SUBSTRING(inkoll.BOKKONTO,1,1)= "5" OR 
            SUBSTRING(inkoll.BOKKONTO,1,1)= "6" OR
            SUBSTRING(inkoll.BOKKONTO,1,1)= "7" OR
            SUBSTRING(inkoll.BOKKONTO,1,1)= "8" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "90" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "91" OR 
            SUBSTRING(inkoll.BOKKONTO,1,2)= "92" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "94" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "95" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "96" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "97" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "98" OR
            SUBSTRING(inkoll.BOKKONTO,1,2)= "99" THEN DO:
            ASSIGN inkoll.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END.
                  
         FIND FIRST KOSTREG WHERE KOSTREG.FAKTNR = inkoll.FAKTNR AND
         KOSTREG.AONR = inkoll.AONR AND KOSTREG.DELNR = inkoll.DELNR AND
         YEAR(KOSTREG.REGDATUM) = YEAR(inkoll.REGDATUM) AND
         KOSTREG.BOKKONTO = inkoll.BOKKONTO AND KOSTREG.INKOMST = inkoll.INKOMST AND                     
         KOSTREG.MTRL = inkoll.MTRL AND KOSTREG.MASKKOST = inkoll.MASKKOST AND
         KOSTREG.OVRKR = inkoll.OVRKR USE-INDEX KOST NO-LOCK NO-ERROR.  
         IF NOT AVAILABLE KOSTREG THEN DO:
            CREATE KOSTREG.
            BUFFER-COPY inkoll TO KOSTREG.
            FIND FIRST okaonr WHERE okaonr.AONR = KOSTREG.AONR AND okaonr.DELNR = KOSTREG.DELNR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE okaonr THEN DO:
               CREATE okaonr.
               BUFFER-COPY KOSTREG TO okaonr.
            END.
         END.
         ELSE DO:
            CREATE tidinfel.
            BUFFER-COPY tidin TO tidinfel.
            DELETE tidin.
         END.
         EMPTY TEMP-TABLE inkoll NO-ERROR. 
            /*
         CREATE KOSTREG.
         ASSIGN  
         KOSTREG.RADNR = rad
         KOSTREG.AONR = AONRTAB.AONR
         KOSTREG.DELNR = AONRTAB.DELNR
         KOSTREG.REGDATUM = indate       
         KOSTREG.BETDATUM = TODAY
         KOSTREG.BENAMNING = TRIM(SUBSTRING(tidin.TIN,80,35)) + " " + SUBSTRING(tidin.TIN,44,35)
         KOSTREG.BOKKONTO = SUBSTRING(tidin.TIN,37,6)
         KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,29,7)
         KOSTREG.FAKTURERAD = ?
         KOSTREG.LEVKOD = ""
         SUBSTRING(KOSTREG.ANVANDARE,1,12) = "EKONOM"
         KOSTREG.KOSTAUTO = TRUE. 
         VALIDATE KOSTREG.
         /*
         3 intäkt
         40-46 mtrl
         47-49 frmtj
         5 övrigt
         6 övrigt    (saknades 18/2 )
         7 personal  - ändrat till övrigt 18/2 2002 av Lena
         8 tas ej med  - ändrat till övrigt  19/2 2002 av Lena
         90-92 tas ej med ändrat till övrigt  3/4 2002 av Anders
         93 intäkt
         94-99 övrigt  (saknades 18/2)
         KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,30,6) ändrad till KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,29,7) 10/05/2005 Lena
         */
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3"OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2) = "93" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,12,15)) * -1.
         END.  
         IF SUBSTRING(KOSTREG.BOKKONTO,1,2)= "40" OR 
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "41" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "42" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "43" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "44" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "45" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "46" THEN DO:
              ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END. 
         IF SUBSTRING(KOSTREG.BOKKONTO,1,2)= "47" OR 
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "48" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "49" THEN DO:
            ASSIGN KOSTREG.MASKKOST = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END.
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "5" OR 
            SUBSTRING(KOSTREG.BOKKONTO,1,1)= "6" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,1)= "7" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,1)= "8" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "90" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "91" OR 
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "92" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "94" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "95" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "96" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "97" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "98" OR
            SUBSTRING(KOSTREG.BOKKONTO,1,2)= "99" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,12,15)). 
         END.
          */
         /*Ändrat av Lena till övrigkostnad efter samtal med Ingrid 18/2 2002
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "7" THEN DO:      
            ASSIGN KOSTREG.PERSKOST = DECIMAL(SUBSTRING(tidin.TIN,12,15)).
         END.                                 */
      END.
   END.
END PROCEDURE.
PROCEDURE ut_UI :
   
   DEBUGGER:SET-BREAK().
   FOR EACH tidin USE-INDEX DATUM:
      /*SUND DELNR*/ 
      
      IF SUBSTRING(tidin.TIN,1,5) = "     " THEN DELETE tidin.
      ELSE IF SUBSTRING(tidin.TIN,37,6) =  "682000" THEN DELETE tidin.
      ELSE IF SUBSTRING(tidin.TIN,37,6) =  "369999" THEN DELETE tidin.
      ELSE IF SUBSTRING(tidin.TIN,37,6) =  "369100" THEN DELETE tidin. 
      ELSE DO:   
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,1,5) AND 
         AONRTAB.DELNR = INTEGER(SUBSTRING(tidin.TIN,6,3))    
         USE-INDEX AONR NO-LOCK NO-ERROR.   
         IF AVAILABLE AONRTAB THEN DO:
            /*posten får omvänt tecken +-*/
            RUN sut_UI.     
         END.
         ELSE DO:
            /*posten får omvänt tecken +-*/
            RUN sut_UI.
             /*post ut med -tecken för aonr som ej finns i guru*/
            RUN motkont_UI.
            DELETE tidin.
            
         END.
      END.         
   END.
   FOR EACH tidin USE-INDEX DATUM:
      /*kontofördelning poster finns nu i tidinkonto*/
      RUN konto_UI.
   END.
   FOR EACH tidinkonto USE-INDEX DATUM:
      /*kontofördelning nytabb för poster som ska ut tab supertidin.*/   
      RUN skut_UI.     
   END.
   OUTPUT STREAM eko TO VALUE(prognamnvar) APPEND.
   PUT STREAM eko UNFORMATTED "GURU "  TODAY SKIP.
   OUTPUT STREAM ekospar TO VALUE(progkopia) APPEND.
   FOR EACH supertidin:
      PUT STREAM eko UNFORMATTED
      supertidin.TIN AT 1 SKIP.
      PUT STREAM ekospar UNFORMATTED
      supertidin.TIN AT 1 SKIP.
   END.
   OUTPUT STREAM eko CLOSE. 
   OUTPUT STREAM ekospar CLOSE.
END PROCEDURE.

PROCEDURE ut2_UI :   
   OUTPUT STREAM ekofelspar TO VALUE(progkopia) APPEND.
   PUT STREAM ekofelspar UNFORMATTED "eko-dubletter "  TODAY SKIP.
   FOR EACH tidinfel:      
      PUT STREAM ekofelspar UNFORMATTED
      tidinfel.TIN AT 1 SKIP.
   END.   
   OUTPUT STREAM ekofelspar CLOSE.
END PROCEDURE.

PROCEDURE motkont_UI:
   /*post ut med -tecken för aonr som ej finns i guru*/
   DEFINE VARIABLE nypris AS DECIMAL NO-UNDO.
   DEFINE VARIABLE nyantal AS DECIMAL NO-UNDO.
   ASSIGN
   nyantal = -1 * DECIMAL(SUBSTRING(tidin.TIN,127,15))
   nypris = -1 * DECIMAL(SUBSTRING(tidin.TIN,12,15)).
   ASSIGN
   SUBSTRING(tidin.TIN,1,5) = "99999"
   SUBSTRING(tidin.TIN,12,15) = STRING(nypris,"->>>>>>>>>>9.99")
   SUBSTRING(tidin.TIN,127,15) = STRING(nyantal,"->>>>>>>>>>9.99").
   CREATE supertidin.
   BUFFER-COPY tidin TO supertidin.          
END PROCEDURE.
PROCEDURE sut_UI:
   /*post ut med -tecken*/
   DEFINE VARIABLE nypris AS DECIMAL NO-UNDO.
   nypris = -1 * DECIMAL(SUBSTRING(tidin.TIN,12,15)).
   SUBSTRING(tidin.TIN,12,15) = STRING(nypris,"->>>>>>>>>>9.99").
   CREATE supertidin.
   BUFFER-COPY tidin TO supertidin.     
       
END PROCEDURE.
PROCEDURE skut_UI:
   /*kontofördelning ut*/
   CREATE supertidin.
   BUFFER-COPY tidinkonto TO supertidin.  
END PROCEDURE.

PROCEDURE konto_UI:
   /*kontofördelning*/
   DEFINE VARIABLE nypris AS DECIMAL NO-UNDO.
   DEFINE VARIABLE kontonr AS INTEGER NO-UNDO.
   DEFINE VARIABLE kollpris AS DECIMAL NO-UNDO.
   EMPTY TEMP-TABLE slutut NO-ERROR. 
   /*SUND DELNR*/
   koant = 0.
   nypris = -1 * DECIMAL(SUBSTRING(tidin.TIN,12,15)).
   OPEN QUERY qa FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = SUBSTRING(tidin.TIN,1,5) AND 
   AONRKONTKOD.DELNR = INTEGER(SUBSTRING(tidin.TIN,6,3)) NO-LOCK.
   GET FIRST qa NO-LOCK.
   IF NOT AVAILABLE AONRKONTKOD  THEN RETURN.
   DO WHILE AVAILABLE(AONRKONTKOD):
      CREATE slutut.
      ASSIGN  
      slutut.PROJEKT = AONRKONTKOD.AONR           
      slutut.DELNR = AONRKONTKOD.DELNR      
      slutut.K1 = AONRKONTKOD.K1               
      slutut.K2 = AONRKONTKOD.K2                       
      slutut.BELOPP = ROUND((nypris * AONRKONTKOD.SATS%) / 100,2).                     
      koant = koant + 1.
      GET NEXT qa NO-LOCK.
   END.
   FOR EACH slutut:
      kollpris = kollpris + slutut.BELOPP.
   END.
   IF nypris NE kollpris THEN DO:
      FIND LAST slutut NO-ERROR.
      slutut.BELOPP = slutut.BELOPP + (nypris - kollpris). 
   END.
   IF koant = 1 THEN DO:
      FIND FIRST supertidin WHERE supertidin.DATUM = tidin.DATUM AND supertidin.RAD = tidin.RAD NO-ERROR.       
      IF AVAILABLE supertidin THEN DO:
         FIND LAST slutut NO-ERROR.
         IF SUBSTRING(supertidin.TIN,116,4) = slutut.K1 AND 
         SUBSTRING(supertidin.TIN,152,8) = slutut.K2 THEN DO:                    
            DELETE supertidin.
            DELETE tidin.
            EMPTY TEMP-TABLE slutut NO-ERROR. 
            
         END.
      END.
   END.     
   FOR EACH slutut:                                         
      IF LENGTH(slutut.K2) = 1 THEN slutut.K2 = slutut.K2 + "0000000".
      ELSE IF LENGTH(slutut.K2) = 2 THEN slutut.K2 = slutut.K2 + "000000".
      ELSE IF LENGTH(slutut.K2) = 3 THEN slutut.K2 = slutut.K2 + "00000".
      ELSE IF LENGTH(slutut.K2) = 4 THEN slutut.K2 = slutut.K2 + "0000".
      ELSE IF LENGTH(slutut.K2) = 5 THEN slutut.K2 = slutut.K2 + "000".
      ELSE IF LENGTH(slutut.K2) = 6 THEN slutut.K2 = slutut.K2 + "00".
      ELSE IF LENGTH(slutut.K2) = 7 THEN slutut.K2 = slutut.K2 + "0".      
      CREATE tidinkonto.
      ASSIGN
      tidinkonto.DATUM = tidin.DATUM
      tidinkonto.TIN = tidin.TIN.
      ASSIGN
      SUBSTRING(tidinkonto.TIN,12,15) = STRING(slutut.BELOPP,"->>>>>>>>>>9.99")
      SUBSTRING(tidinkonto.TIN,116,4) = slutut.K1
      SUBSTRING(tidinkonto.TIN,152,8) = slutut.K2.
      DELETE slutut.
   END.
END PROCEDURE.



