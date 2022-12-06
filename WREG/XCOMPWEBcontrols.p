/*XCOMPWEBcontrols.p*/

/*************************************************************************
*  Programsyfte:  Kompilerar alla *.p och *.w filer som ligger i mappen \\pc122\delad\pro9\Guru\2Guru\Controls
*                 OCH DESS UNDER MAPPAR
*DESSA SPARAS I \\pc122\WebGuru\GuruOnWeb\2Guru\ControlsT 

TOSHIBAN kompilerar \\pc122\delad\pro9\Guru\2Guru\Controls 
 över alla  \\pc122\WebGuru\GuruOnWeb\2Guru\ControlsT program för rätt excel version



*  
**************************************************************************/
SESSION:SUPPRESS-WARNINGS = TRUE. 
DEFINE VARIABLE allasvar AS LOGICAL.
DEFINE VARIABLE mappsvar AS LOGICAL.
DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE searchdir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE savedir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tempsearchdir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE guruwtidir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE wcguruwtidir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tempsavedir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cdirnames AS CHARACTER NO-UNDO.
DEFINE VARIABLE delfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE curdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE statok AS INTEGER NO-UNDO.
DEFINE VARIABLE numcdir AS INTEGER NO-UNDO.
DEFINE VARIABLE dirnr AS INTEGER NO-UNDO.
DEFINE VARIABLE webforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE webdate AS DATE NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(70)" LABEL "File" NO-UNDO.
DEFINE VARIABLE filnamnR AS CHARACTER NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(70)" LABEL "Directory" NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE iCOMP AS INTEGER NO-UNDO.
DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE net AS CHARACTER NO-UNDO.
DEFINE VARIABLE GuruComp AS LOGICAL FORMAT "guru/ekg" LABEL "Guru eller EKG" NO-UNDO.
DEFINE VARIABLE webbdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcvar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE copydirFrom AS CHARACTER NO-UNDO.
DEFINE VARIABLE copydirTo AS CHARACTER NO-UNDO.
pcvar = "pc122".
webdate = 01/01/89.
GuruComp  = TRUE.
DEFINE TEMP-TABLE kompdir NO-UNDO
   FIELD SERCHMAPPKOMP AS CHARACTER
   FIELD SAVEMAPPKOMP AS CHARACTER
   FIELD MAPPKOMP AS CHARACTER
   FIELD NIVA AS INTEGER
   FIELD KLAR AS LOGICAL
   INDEX MAPPKOMP NIVA SERCHMAPPKOMP MAPPKOMP.
DEFINE BUFFER kompdirbuff FOR kompdir.
REPEAT:
   IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
   IF LDBNAME(1) = ? THEN LEAVE.
END.
SESSION:DEBUG-ALERT = YES.
ASSIGN
numcdir = 0
dirnr = 1
utfil = SESSION:TEMP-DIR + "cmplog.txt" /*Logfil för kompilering.*/

searchdir = "\\" + pcvar + "\delad\pro9\guru\".  /*Sökväg till de filer som ska kompileras.*/
OUTPUT TO VALUE(utfil).
PUT TODAY .
OUTPUT CLOSE.
   
webbdir = "\\" + pcvar + "\WebGuru\GuruOnWeb\".
net = "\".
RUN clsmappar_UI.                                                                                 
RUN \\pc122\delad\pro9\Guru\CSTART\OPENDOC.P (utfil,"","",NO). 
PROCEDURE clsmappar_UI :
   EMPTY TEMP-TABLE kompdir NO-ERROR. 
   DEFINE VARIABLE antalniv AS INTEGER NO-UNDO.
   antalniv = 1.    
   
   searchdir = searchdir + "2GURU\".
   INPUT FROM OS-DIR(searchdir) NO-ECHO.
   REPEAT:
      SET filnamn ^ attrlist.
      /* om filen är en mapp.*/
      IF attrlist = "D" THEN DO: 
         IF filnamn BEGINS "." THEN.
         ELSE IF filnamn MATCHES '*TesterTester*' THEN .
         ELSE DO: 
            CREATE kompdir.
            ASSIGN
            kompdir.NIVA = antalniv
            kompdir.SERCHMAPPKOMP = searchdir + filnamn + "\"
            kompdir.MAPPKOMP = filnamn.
            filnamn = "".
            /*Sparar alla mappnamn i en textsträng.*/
         END.   
      END.
   END.
  INPUT CLOSE.
   
   REPEAT:
      FIND FIRST kompdir WHERE kompdir.NIVA = antalniv NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE kompdir THEN LEAVE.
      FOR EACH kompdir  WHERE kompdir.NIVA = antalniv NO-LOCK:
         INPUT FROM OS-DIR(kompdir.SERCHMAPPKOMP) NO-ECHO.
         REPEAT:
            SET filnamn ^ attrlist.
            /*Kolla om filen är en mapp.*/
            IF attrlist = "D" THEN DO: 
               IF filnamn BEGINS "." THEN.
               ELSE IF filnamn MATCHES '*TesterTester*' THEN .
               ELSE DO: 
                  CREATE kompdirbuff.
                  ASSIGN
                  kompdirbuff.NIVA = antalniv + 1
                  kompdirbuff.SERCHMAPPKOMP = kompdir.SERCHMAPPKOMP + filnamn + "\"
                  
                  kompdirbuff.MAPPKOMP = filnamn.
                  filnamn = "".
                  /*Sparar alla mappnamn i en textsträng.*/
               END.   
            END.
         END.
         INPUT CLOSE.
      END.
      antalniv = antalniv + 1.      
   END.
   mappsvar = TRUE.
   FOR EACH kompdir WHERE kompdir.KLAR = FALSE:
      /*Plockar ut namnet för aktuell mapp*/
      IF kompdir.SERCHMAPPKOMP MATCHES '*CONTROLS*' THEN.
      ELSE NEXT. 
      MESSAGE "Vill du kompilera mappen: " kompdir.SERCHMAPPKOMP "?" SKIP 
      "totalt borde det bli 45 *.r filer" SKIP
      "Obs kolla mappen D:\WebGuru\GuruOnWeb\2Guru\ControlsT\Controls och skip
      D:\WebGuru\GuruOnWeb\2Guru\ControlsT\Controls\Subclasses"
      VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE mappsvar.
      IF mappsvar = TRUE THEN DO:
                  
         INPUT FROM OS-DIR(kompdir.SERCHMAPPKOMP) NO-ECHO.
         
        
         REPEAT:
            /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
            SET filnamn dirlist VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 attrlist.
            IF filnamn MATCHES "*.cls" THEN DO:
               /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
               FILE-INFO:FILE-NAME = SEARCH(kompdir.SERCHMAPPKOMP + filnamn ).
               IF FILE-INFO:FILE-MOD-DATE >= webdate OR FILE-INFO:FILE-MOD-DATE = ? THEN DO:
                  iCOMP = iCOMP + 1.                  
                  MESSAGE kompdir.MAPPKOMP " Compile file: " + filnamn + " into " + webbdir + "2Guru\".
                
                  OUTPUT TO VALUE(utfil) APPEND. 
                  IF felkoll = TRUE THEN PUT filnamn SKIP.
                    
                  COMPILE VALUE(dirlist) SAVE INTO VALUE(webbdir + "2Guru\ControlsT").
                  OUTPUT CLOSE.
                 /*                 
                  filnamn = REPLACE(filnamn,".cls",".r").
                  copydirFrom = webbdir + "2Guru\ControlsT\Controls\" + filnamn.
                  copydirTo = webbdir + "2Guru\ControlsT\" + filnamn.
                  
                  OS-RENAME VALUE(copydirFrom) VALUE(copydirTo).
                  
                  copydirFrom = webbdir + "2Guru\ControlsT\Controls\Subclasses\" + filnamn.
                  IF SEARCH(copydirFrom) NE ? THEN DO:
                     copydirTo = webbdir + "2Guru\ControlsT\Subclasses\" + filnamn. 
                     OS-RENAME VALUE(copydirFrom) VALUE(copydirTo).
                  END.
                  */   
               END.                            
            END.
            
            IF filnamn MATCHES "*.resx" THEN DO:
               kompdir.MAPPKOMP = SUBSTRING(kompdir.SERCHMAPPKOMP,INDEX(kompdir.SERCHMAPPKOMP,"2GURU\") + 6 ) + "".
                  /*Skriver ut vilken fil som håller på att kompileras och kompilera sedan.*/
            /*   kompdir.MAPPKOMP = REPLACE(kompdir.MAPPKOMP,"Controls","ControlsT").
            
              MESSAGE dirlist skip
               webbdir skip
                 kompdir.MAPPKOMP
               VIEW-AS ALERT-BOX.
            */ 
               
               OS-COPY VALUE(dirlist) VALUE(webbdir + "\2Guru\ControlsT\" + kompdir.MAPPKOMP).
            END.
            
                       
         END.
         INPUT CLOSE.
      END.
      kompdir.KLAR = TRUE.      
   END.
   DISPLAY iCOMP.
   
END PROCEDURE.


/*bilder till webclient*/ 
