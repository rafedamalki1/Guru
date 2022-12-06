/*EXPROTO.P*/
{TIDUTTTNEW.I}
DEFINE INPUT PARAMETER excellista AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER globforetagIN AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidut.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE bladvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE protvar AS INTEGER NO-UNDO.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.

{GLOBVAR2DEL1.I}

{EXECLIN.I}

RUN startexcel_UI.
{EXCELFEL.I}
RUN noscreenexcel_UI.
RUN sambilagor_UI.
{EXCELFEL.I}
RUN screenexcel_UI.
RUN slutexcel_UI.
{EXCELFEL.I}
PROCEDURE sambilagor_UI:
   protvar = 1.
   bladvar2 = 0.
     
   FIND FIRST tidut WHERE SUBSTRING(tidut.UT,45,14) = "Utskriftsdatum" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidut THEN DO:
      FIND FIRST tidut WHERE SUBSTRING(tidut.UT,53,14) = "Utskriftsdatum" NO-LOCK NO-ERROR.
   END.
   IF AVAILABLE tidut THEN DO:
      
      /*sammanställning och bilagor*/
      RUN samman_UI.
   
      REPEAT:
         IF NOT AVAILABLE tidut THEN LEAVE.
         IF ERROR-STATUS:NUM-MESSAGES > 0 OR felexcel = TRUE THEN DO:
            /*inställning bara 1 flik skapas i excelboken- gå ur. Standard 3 flikar*/
            felexcel = TRUE.
            RUN screenexcel_UI.
            LEAVE.
         END.
         iRad = 1.
         RUN bilagor_UI. /*även ytterligare sammanställningar om flera markägare valts*/
   
      END.   
   END.
   ELSE DO:
      /*enskild bilaga*/
      FIND FIRST tidut NO-LOCK NO-ERROR.
      REPEAT:
         IF NOT AVAILABLE tidut THEN LEAVE.
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN LEAVE.
         ELSE DO:
            FIND PREV tidut NO-LOCK.     
            IF SUBSTRING(tidut.UT,132) = "$" THEN FIND NEXT tidut NO-LOCK NO-ERROR.      
         END.
         iRad = 1.
         RUN bilagor1_UI. /*även ytterligare sammanställningar om flera markägare valts*/
   
      END.
   END.
   {EXCELFEL.I}
END.

PROCEDURE samman_UI:
   /*Vilka kolumner*/
   
   ASSIGN
   startc = "A"
   slutc = "D"
   slutbredd = 15
   utnr[1] = 1
   utnr[2] = 48
   utnr[3] = 62
   utnr[4] = 76.
   ASSIGN
   estartnr[1] = 1
   estartnr[2] = 41
   estartnr[3] = 54
   estartnr[4] = 67.   
   bladvar = 0.
   RUN nyttbladexcel_UI.
   chWorkSheet:Name = "Sammanst" NO-ERROR.   
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "VAST" THEN DO:     
     RUN imageexcel2_UI.
   END.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK.
   REPEAT:
      IF NOT AVAILABLE tidut  THEN LEAVE.
      IF SUBSTRING(tidut.UT,132) = "$" THEN LEAVE.
      IF SUBSTRING(tidut.UT,88,1) = "I" THEN SUBSTRING(tidut.UT,88,1) = " ".
      IF SUBSTRING(tidut.UT,30,8) = " IFYLLES"  THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.      
      ELSE IF SUBSTRING(tidut.UT,5,37) = "V Ä R D E R I N G S P R O T O K O L L"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,30,19) = "VÄRDERINGSPROTOKOLL"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 10,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,5) = "=====" THEN DO:
         musz = musz.
      END.      
      ELSE IF SUBSTRING(tidut.UT,5,5) = "_____"  THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 3,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,5) = "-----" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 6,INPUT 0).
      END.
   
      ELSE IF SUBSTRING(tidut.UT,29,21) = " ___________________ " THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 10,INPUT FALSE,INPUT 3,INPUT 0).
      END.
      ELSE DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 12,INPUT 0).
      END.     
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK.            
   END.   
END PROCEDURE.
PROCEDURE bilagor_UI:
   /*Vilka kolumner*/
   ASSIGN
   startc = "A"
   slutc = "C"
   slutbredd = 3
   utnr[1] = 1
   utnr[2] = 66
   utnr[3] = 74.
   FIND NEXT tidut NO-LOCK.
   IF AVAILABLE tidut  THEN DO:   
      RUN nyttbladexcel_UI.
      IF SUBSTRING(tidut.UT,45,14) = "Utskriftsdatum" OR SUBSTRING(tidut.UT,53,14) = "Utskriftsdatum" THEN DO:
        protvar = protvar + 1.
        IF protvar > 1  THEN chWorkSheet:Name = STRING(protvar) + "-Sammanst" NO-ERROR.   
        ELSE chWorkSheet:Name = "Sammanst" NO-ERROR.   
      END.
      ELSE DO:                
          IF protvar > 1  THEN chWorkSheet:Name = STRING(protvar) + "-bilaga " + SUBSTRING(tidut.UT,56,1) NO-ERROR.
          ELSE chWorkSheet:Name = "bilaga " + SUBSTRING(tidut.UT,56,1) NO-ERROR.
          IF  Guru.Konstanter:globforetag = "VAST" THEN DO:     
             RUN imageexcel2_UI.
          END.         
      END.
      {EXCELFEL.I}
   END.
   /*Rubriker*/   
   REPEAT:
      IF NOT AVAILABLE tidut THEN LEAVE.      
      IF SUBSTRING(tidut.UT,88,1) = "I" THEN SUBSTRING(tidut.UT,88,1) = " ".
      IF SUBSTRING(tidut.UT,30,8) = " IFYLLES"  THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,37) = "V Ä R D E R I N G S P R O T O K O L L"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,30,19) = "VÄRDERINGSPROTOKOLL"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,36) = "OCKULÄRUPPSKATTNING AV BESTÅNDSVOLYM"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,36) = "OCKULÄRUPPSKATTNING AV BESTÅNDSVOLYM"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,29) = "ROTNETTO /FÖRDYRAD AVVERKNING"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      
      ELSE IF SUBSTRING(tidut.UT,5,5) = "=====" THEN musz = musz.      
      ELSE IF SUBSTRING(tidut.UT,5,5) = "_____"  THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 3,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,5) = "-----" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 6,INPUT 0).
      END.      
      ELSE DO:         
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 12,INPUT 0).
      END.
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK.   
      IF NOT AVAILABLE tidut THEN LEAVE.      
      IF SUBSTRING(tidut.UT,132) = "$" THEN LEAVE.      
   END.
   
END PROCEDURE.
PROCEDURE bilagor1_UI:
   /*Vilka kolumner*/
   ASSIGN
   startc = "A"
   slutc = "C"
   slutbredd = 3
   utnr[1] = 1
   utnr[2] = 66
   utnr[3] = 74.   
   IF AVAILABLE tidut  THEN DO:   
      RUN nyttbladexcel_UI.      
      IF protvar > 1  THEN chWorkSheet:Name = STRING(protvar) + "-bilaga " + SUBSTRING(tidut.UT,56,1) NO-ERROR.
      ELSE chWorkSheet:Name = "bilaga " + SUBSTRING(tidut.UT,56,1) NO-ERROR.
            
   END.
   {EXCELFEL.I}
   /*Rubriker*/   
   REPEAT:      

      IF NOT AVAILABLE tidut THEN LEAVE.      
      IF SUBSTRING(tidut.UT,88,1) = "I" THEN SUBSTRING(tidut.UT,88,1) = " ".
      IF SUBSTRING(tidut.UT,30,8) = " IFYLLES"  THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,37) = "V Ä R D E R I N G S P R O T O K O L L"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,30,19) = "VÄRDERINGSPROTOKOLL"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,36) = "OCKULÄRUPPSKATTNING AV BESTÅNDSVOLYM"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,29) = "ROTNETTO /FÖRDYRAD AVVERKNING"  THEN DO:
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT " ",INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT TRUE,INPUT 12,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,5) = "=====" THEN musz = musz.      
      ELSE IF SUBSTRING(tidut.UT,5,5) = "_____"  THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 3,INPUT 0).
      END.
      ELSE IF SUBSTRING(tidut.UT,5,5) = "-----" THEN DO:
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 6,INPUT 0).
      END.      
      ELSE DO:         
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Courier New",INPUT 8,INPUT FALSE,INPUT 12,INPUT 0).
      END.
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK.   
      IF NOT AVAILABLE tidut THEN LEAVE.      
      IF SUBSTRING(tidut.UT,132) = "$" THEN LEAVE.      
   END.
   
END PROCEDURE.


PROCEDURE imageexcel2_UI:
   /*FOREBILDER*/
   {LOGGOR.I}
   
   ASSIGN iColumn = iColumn + 1.
   chWorkSheet:rows(iColumn):SELECT NO-ERROR.   
   
   IF link NE  ? THEN chWorkSheetRange = chWorkSheet:Pictures:INSERT(link) NO-ERROR. 
   ELSE RETURN.     
   chWorkSheetRange:TOP = 1 NO-ERROR.
   chWorkSheetRange:LEFT = 1 NO-ERROR.
   RELEASE OBJECT chWorkSheetRange NO-ERROR.
   chWorkSheetRange = ?.  
   chExcelApplication:VISIBLE = TRUE NO-ERROR.
   ASSIGN iColumn = iColumn + 7.  
   
   {EXCELFEL.I}         
END PROCEDURE.


