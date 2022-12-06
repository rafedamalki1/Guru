/*GRANIN.P*/   
    
DEFINE VARIABLE tider AS CHARACTER NO-UNDO. 
DEFINE VARIABLE indate AS DATE NO-UNDO. 
DEFINE VARIABLE rad LIKE KOSTREG.RADNR NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(132)".  
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
   
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG. 
{AMERICANEUROPEAN.I}  
IF Guru.Konstanter:globforetag = "GRAN" THEN DO:            
   /*GN9PST
   SYD OCH NORD
   */
   tider = REPLACE(STRING(TIME,"HH:MM"),":","").
   prognamn = "\\granguru\guru_ser\server\PRO9S\import\GN9PST.txt".
   progkopia = "\\granguru\guru_ser\server\PRO9S\import\backineko\GRAN" + STRING(TODAY,"999999") + tider.    
   progque = "\\granguru\guru_ser\server\PRO9S\import\GN9PST.Q".
END.
IF Guru.Konstanter:globforetag = "ELPA" THEN DO:            
   prognamn = "D:\DELAD\PRO9\guru\import\GN9PST".
   progkopia = "D:\DELAD\PRO9\guru\import\GN9PST" + STRING(TODAY,"999999").    
   progque = "D:\DELAD\PRO9\guru\import\GN9PST.Q".
END.
OUTPUT TO \\granguru\guru_ser\server\PRO9S\autotid.txt  APPEND.
PUT prognamn ""  Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
filename = SEARCH(prognamn).
IF filename = ? THEN musz = TRUE.
ELSE DO:  
   OUTPUT TO \\granguru\guru_ser\server\PRO9S\autotid.txt  APPEND.
   PUT "DET FANNS FIL FRÅN EKO " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   OS-DELETE SILENT VALUE(progque).       
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN musz = TRUE.
   ELSE OS-COMMAND SILENT VALUE(kommando) VALUE(prognamn) > VALUE(progque).   
END. 
IF musz = TRUE THEN DO:
   OUTPUT TO \\granguru\guru_ser\server\PRO9S\autotid.txt  APPEND.
   PUT "DET FANNS INGEN FIL FRÅN EKO " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.

   musz = FALSE.
   RETURN.
END.
INPUT FROM VALUE(progque) NO-ECHO
CONVERT TARGET "iso8859-1" SOURCE "ibm850".
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
   /*DELNR OK*/
   IF SUBSTRING(TIDIN.TIN,1,1) = "S" THEN NEXT.
   /*eva h 2003/06/06*/
   /*IF SUBSTRING(TIDIN.TIN,3,1) = "7" THEN NEXT.*/
   IF SUBSTRING(TIDIN.TIN,3,1) = "9" THEN NEXT.
   IF SUBSTRING(TIDIN.TIN,15,6) = "" THEN NEXT. /*AONR*/
   
   /*OBS    EV  DELNR ???
   DATUM INTÄKT + EL - */     
   rad = rad + 1.                                                                      
   /* 
      CREATE kostemp.
      ASSIGN  
      kostemp.RADNR = rad
      kostemp.AONR = SUBSTRING(TIDIN.TIN,15,6)
      kostemp.DELNR = INTEGER(SUBSTRING(TIDIN.TIN,21,3))
      kostemp.REGDATUM  = TODAY       
      kostemp.BENAMNING = SUBSTRING(TIDIN.TIN,86,19) 
      kostemp.BOKKONTO = SUBSTRING(TIDIN.TIN,3,5)
      kostemp.FAKTNR = SUBSTRING(TIDIN.TIN,37,8)
      kostemp.FAKTURERAD = ?
      kostemp.LEVKOD = ""
      kostemp.ANVANDARE = "EKONOM"
      kostemp.KOSTAUTO = TRUE. 
      IF kostemp.BENAMNING = "FRIDELLS GLAS" THEN DO:
         rad = rad.
      END. 
      /*
      3 intäkt
      4 mtrl
      5 övrigt
      6 frmtj
      7 tas ej med
      8 övrigt
      9 tas ej med
      */
      IF SUBSTRING(kostemp.BOKKONTO,1,1) = "3" THEN DO:
         ASSIGN kostemp.INKOMST = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) * -1 / 1000.
      END.  
      IF SUBSTRING(kostemp.BOKKONTO,1,1)= "4" THEN DO:
         ASSIGN kostemp.MTRL = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
      END. 
      IF SUBSTRING(kostemp.BOKKONTO,1,1)= "5" THEN DO:
         ASSIGN kostemp.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
      END.
      IF SUBSTRING(kostemp.BOKKONTO,1,1)= "6" THEN DO:
         ASSIGN kostemp.MASKKOST = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
      END.
      IF SUBSTRING(kostemp.BOKKONTO,1,1)= "8" THEN DO:
         ASSIGN kostemp.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20))/ 1000 . 
      END.                       
  */
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
      /*
      3 intäkt
      4 mtrl
      5 övrigt
      6 frmtj
      7 tas ej med
      8 övrigt
      9 tas ej med
      */
      
      IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
         ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) * -1 / 1000.
      END.  
      IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "4" THEN DO:
         ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
      END. 
      IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "5" THEN DO:
         ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
      END.
      
      IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "6" THEN DO:      
         /*eva h 2003/06/06*/
         IF KOSTREG.BOKKONTO <= "65919" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000.
         END.
         ELSE DO:
            IF KOSTREG.BOKKONTO <= "69699" THEN DO: 
               ASSIGN KOSTREG.MASKKOST = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
            END.
            ELSE DO:
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000.        
            END.
         END.
         /*
         /*ENLIGT EVA H 00/07/06 */
         IF KOSTREG.BOKKONTO >= "65900" AND KOSTREG.BOKKONTO <= "65919" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000.
         END.   
         /*eva h 2003/06/06*/
         ELSE DO:
            IF KOSTREG.BOKKONTO = "69700" OR 
            KOSTREG.BOKKONTO = "69810" OR 
            KOSTREG.BOKKONTO = "69820" OR
            KOSTREG.BOKKONTO = "69950" OR
            KOSTREG.BOKKONTO = "69960" OR
            KOSTREG.BOKKONTO = "69970" OR
            KOSTREG.BOKKONTO = "69975" OR
            KOSTREG.BOKKONTO = "69990" 
            THEN DO:
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000.
            END.
            ELSE DO:
               ASSIGN KOSTREG.MASKKOST = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
            END.           
         END.*/
      END.
      
      IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "8" THEN DO:
         ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000. 
      END.                       
      /*eva h 2003/06/06*/
      IF SUBSTRING(KOSTREG.BOKKONTO,1,1)= "7" THEN DO:      
         IF KOSTREG.BOKKONTO = "76100" OR 
            KOSTREG.BOKKONTO = "76110" OR 
            KOSTREG.BOKKONTO = "76210" OR
            KOSTREG.BOKKONTO = "76310" OR
            KOSTREG.BOKKONTO = "76320" OR
            KOSTREG.BOKKONTO = "76910" OR
            KOSTREG.BOKKONTO = "76930" OR
            KOSTREG.BOKKONTO = "76940" OR
            KOSTREG.BOKKONTO = "76990" OR
            KOSTREG.BOKKONTO = "79900" 
         THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(TIDIN.TIN,143,20)) / 1000.
         END.
         ELSE DO:
            DELETE KOSTREG.         
         END.
      END.
                                         
   END.
END.

OS-RENAME VALUE(prognamn) VALUE(progkopia).
{EUROPEANAMERICAN.I}
/*
FOR EACH KOSTEMP:
DISPLAY KOSTEMP.AONR FORMAT "X(7)" kostemp.BOKKONTO LABEL "BOK"
 kostemp.INKOMST kostemp.MTRL kostemp.OVRKR 
      kostemp.MASKKOST 
      kostemp.OVRKR 
      kostemp.BENAMNING WITH FRAME CC FONT 11 DOWN.
      END.

                
*/
