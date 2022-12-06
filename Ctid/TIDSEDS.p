/*TIDSEDS.P*/
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
{GLOBVAR2DEL1.I}
{REGVAR.I}
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE langnamn AS INTEGER NO-UNDO.
DEFINE VARIABLE sidnr AS INTEGER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE utrec AS RECID NO-UNDO.
DEFINE VARIABLE forstasidan AS LOGICAL NO-UNDO.
{TIDUTTTSHARED.I} 
DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE FRAME FRAME-TID3
     tidut.UT AT ROW 1 COL 1 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 141 BY 1
    WITH DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.13 ROW 7
         SCROLLABLE SIZE 142 BY 16.59. 
langnamn = LENGTH(regmannamn). 
ASSIGN
forstasidan = TRUE
sidnr = 1
rad = 4.           
PUT sidnr AT 120.

FOR EACH tidut:                             
   IF forstasidan = TRUE THEN DO:
      PUT SKIP (3). 
      forstasidan = FALSE.  
   END.
   PUT tidut.UT AT 2. 
   IF SUBSTRING(tidut.UT,2,5) = pkod AND
      SUBSTRING(tidut.UT,9,langnamn) = regmannamn THEN DO:
      NEXT.
   END.
   IF rad = Guru.GlobalaVariabler:globsidl - 6 THEN DO:
      PUT SKIP (2).
      PUT 
      "RIKTIGHET INTYGAS:____________________________________________ "
      AT 2.
      PUT 
      "TIDSEDELN GODKÄNNES:_______________________________________ " 
      AT 72 SKIP.
      PUT SKIP (1).
      PUT pkod AT 2 regmannamn FORMAT "x(30)" AT 10 SKIP.
      PUT SKIP (1).
      /*SIDBRYTNING*/      
      sidnr = sidnr + 1.
      PUT sidnr AT 120. 
      PUT SKIP (3).                     
      ASSIGN      
      rad = 4.                        
   END. 
   ELSE rad = rad + 1.       
END.  
PUT SKIP(Guru.GlobalaVariabler:globsidl - rad).  
