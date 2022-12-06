
/*------------------------------------------------------------------------
File:        rcodeinf.p
Description: Provides extended information about PROGRESS r-code files.
Author:      George Potemkin
Version:     1.0
Created:     03/25/92
Modified:
------------------------------------------------------------------------*/

/* --
   You can use only this part of the procedure if you need to read
   an extended information about one r-code file.
-- .*/
/* ***************************  Definitions  ************************** */

DEFINE VARIABLE chrRCodeFile AS CHARACTER FORMAT "x(60)" LABEL "File".

&SCOPED-DEFINE MagicNumber 1456395017
&SCOPED-DEFINE ListSeparator ","

&IF "{&WINDOW-SYSTEM}" <> "TTY"
&THEN &SCOPED-DEFINE EditorHeight 1.5
&ELSE &SCOPED-DEFINE EditorHeight 3
&ENDIF


&SCOPED-DEFINE Editor VIEW-AS EDITOR SIZE 37 BY {&EditorHeight} SCROLLBAR-VERTICAL 

DEFINE TEMP-TABLE RCodeInfo
 FIELD FileName      AS CHARACTER LABEL "Name"      FORMAT "x(50)"
 FIELD Reliability   AS LOGICAL   LABEL ""          FORMAT "/?"
 FIELD CompVersion   AS INTEGER   LABEL "Version"   FORMAT ">>>>9"
 FIELD RCodeSize     AS INTEGER   LABEL "Size"    FORMAT ">>>>>>9"
 FIELD RCodeLength   AS INTEGER   LABEL "Length"    FORMAT ">>>>>>9"
 FIELD RCodeCRC      AS INTEGER   LABEL "CRC"       FORMAT ">>>>9"
 FIELD InitSegment   AS INTEGER   LABEL "Initial"   FORMAT ">>>>9"
 FIELD ActionSegment AS INTEGER   LABEL "Action"    FORMAT ">>>>9"
 FIELD ECodeSegment  AS CHARACTER LABEL "E-Code"    FORMAT "x(23)"
 FIELD DebugSegment  AS INTEGER   LABEL "Debug"     FORMAT ">>>>9"
 FIELD IProcNumber   AS INTEGER   LABEL "Int-Proc#" FORMAT ">>9"
 FIELD IProcSegment  AS CHARACTER LABEL "Int-Proc"  FORMAT "x(50)" {&Editor}
 FIELD FrameNumber   AS INTEGER   LABEL "Frame#"    FORMAT ">>9"
 FIELD FrameSegment  AS CHARACTER LABEL "Frame"     FORMAT "x(50)" {&Editor}
 FIELD Languages     AS CHARACTER LABEL "Languages" FORMAT "x(50)"
 FIELD TextSegment   AS CHARACTER LABEL "Text"      FORMAT "x(50)" {&Editor}
 FIELD CodePage      AS CHARACTER LABEL "Code Page" FORMAT "x(9)"
 INDEX FileName IS UNIQUE PRIMARY
         FileName.


DEFINE VARIABLE chrSaveFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE logResult    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE STREAM strRCodeFile.
DEFINE STREAM strTemp.

/* Internal Procedures **********************************************/
/* Notes: GetShort and GetLong should be UDF
          but UDF is not available in V7
*/

/*------------------------------------------------------------------*/
PROCEDURE GetShort:
/* Desc: Reads 2-bytes integer value from the file.
 Caller: RCodeInfo
*/
  DEFINE  INPUT PARAMETER intPosition AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER intResult   AS INTEGER NO-UNDO.

  DEFINE VARIABLE rawRecord AS RAW NO-UNDO.
  DEFINE VARIABLE intByte AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
 
  ASSIGN LENGTH(rawRecord) = 2.
  SEEK   STREAM strRCodeFile TO intPosition.
  IMPORT STREAM strRCodeFile UNFORMATTED rawRecord.

  ASSIGN intResult = 0.
  DO i=2 TO 1 BY -1:
    ASSIGN intByte=GET-BYTE(rawRecord,i).
    IF intByte < 0 THEN ASSIGN intByte = 256 + intByte.
    ASSIGN intResult = intResult * 256 + intByte.
  END.
/*  
  ASSIGN intResult=GET-SHORT(rawRecord,1).
  IF intResult < 0 THEN  ASSIGN intResult=65536 + intResult.
*/

END PROCEDURE. /* GetShort */

/*------------------------------------------------------------------*/
PROCEDURE GetLong:
/* Desc: Reads 4-bytes integer value from the file.
 Caller: RCodeInfo
*/
  DEFINE  INPUT PARAMETER intPosition AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER intResult   AS INTEGER NO-UNDO.

  DEFINE VARIABLE rawRecord AS RAW NO-UNDO.
  DEFINE VARIABLE intByte AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
 
  ASSIGN LENGTH(rawRecord) = 4.
  SEEK   STREAM strRCodeFile TO intPosition.
  IMPORT STREAM strRCodeFile UNFORMATTED rawRecord.

  ASSIGN intResult = 0.
  DO i=4 TO 1 BY -1:
    ASSIGN intByte=GET-BYTE(rawRecord,i).
    IF intByte < 0 THEN ASSIGN intByte = 256 + intByte.
    ASSIGN intResult = intResult * 256 + intByte.
  END.
/*
  ASSIGN intResult=GET-LONG(rawRecord,1).
*/
END PROCEDURE. /* GetLong */

/*-----------------------------------------------------------------*/
PROCEDURE RCodeInfo:
/* Desc: Provides extended information about a specific PROGRESS r-code file.
  Input: R-code filename.
 Output: Temporal table - RCodeInfo.
 Callee: GetShort, GetLong
*/
  DEFINE  INPUT PARAMETER chrFileName AS CHARACTER NO-UNDO.
  DEFINE PARAMETER BUFFER RCodeInfo FOR RCodeInfo.

  DEFINE VARIABLE rawRecord AS RAW NO-UNDO.
  DEFINE VARIABLE intFileOffset AS INTEGER   NO-UNDO.
  DEFINE VARIABLE intValue      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE chrValue      AS CHARACTER NO-UNDO.

  INPUT  STREAM strRCodeFile FROM VALUE(chrFileName).

/* Is it real r-code? Check first four bytes. */
  RUN GetLong(0,OUTPUT intValue).
  IF intValue <> {&MagicNumber} THEN
  DO:
    MESSAGE chrRCodeFile "has wrong format." SKIP
           "This procedure can analyze the r-code of V7 and above."
    VIEW-AS ALERT-BOX ERROR.
    INPUT STREAM strRCodeFile CLOSE.
/*    RETURN NO-APPLY. */
  END.
  ELSE
  DO TRANSACTION:
  
    FIND FIRST RCodeInfo WHERE RCodeInfo.FileName=chrFileName NO-LOCK NO-ERROR.
    IF NOT AVAILABLE RCodeInfo THEN
    DO:
      CREATE RCodeInfo.
      ASSIGN RCodeInfo.FileName=chrFileName. 
    END.

    RUN GetShort(14, OUTPUT RCodeInfo.CompVersion).
    RUN GetLong (64, OUTPUT RCodeInfo.RCodeLength).
    RUN GetShort(72, OUTPUT RCodeInfo.InitSegment).
    RUN GetShort(98, OUTPUT RCodeInfo.ActionSegment).

    ASSIGN chrValue="".
    DO i=100 TO 106 BY 2:
      RUN GetShort(i,OUTPUT intValue).  
      ASSIGN chrValue = chrValue + MIN(chrValue,{&ListSeparator})
                      + (IF intValue=? THEN "?" ELSE STRING(intValue)).
    END.
    ASSIGN RCodeInfo.ECodeSegment=chrValue.

    RUN GetShort(108,OUTPUT RCodeInfo.DebugSegment).
    RUN GetShort(110,OUTPUT RCodeInfo.IProcNumber).
    RUN GetShort(112,OUTPUT RCodeInfo.FrameNumber).

    ASSIGN intFileOffset = 112
           chrValue="".

    DO i=1 TO IProcNumber:
      ASSIGN intFileOffset = intFileOffset + 8.
      RUN GetShort(intFileOffset,OUTPUT intValue). 
      ASSIGN chrValue = chrValue + MIN(chrValue,{&ListSeparator})
                      + (IF intValue=? THEN "?" ELSE STRING(intValue)).
    END.
    ASSIGN RCodeInfo.IProcSegment=chrValue.

/* Skip some kind of default frame.
   Thanks to Grant Maizels <grant.maizels@cogita.com.au> 
*/
    IF FrameNumber > 0 THEN
    ASSIGN intFileOffset = intFileOffset + 8
           FrameNumber = FrameNumber - 1.
    ASSIGN chrValue = "".

    DO i=1 TO FrameNumber:
      ASSIGN intFileOffset = intFileOffset + 8.
      RUN GetShort(intFileOffset,OUTPUT intValue). 
      ASSIGN chrValue = chrValue + MIN(chrValue,{&ListSeparator})
                      + (IF intValue=? THEN "?" ELSE STRING(intValue)).
    END.
    ASSIGN RCodeInfo.FrameSegment=chrValue
           intFileOffset = intFileOffset + 2
           chrValue = "".

    DO i=1 TO 1:
      ASSIGN intFileOffset = intFileOffset + 8.
      RUN GetShort(intFileOffset,OUTPUT intValue). 
      ASSIGN chrValue = chrValue + MIN(chrValue,{&ListSeparator})
                      + (IF intValue=? THEN "?" ELSE STRING(intValue)).
    END.

    ASSIGN RCodeInfo.TextSegment=chrValue
           LENGTH(rawRecord) = 16
           intFileOffset = intFileOffset + 2.

    SEEK STREAM strRCodeFile TO intFileOffset.
    IMPORT STREAM strRCodeFile UNFORMATTED rawRecord.
    ASSIGN RCodeInfo.CodePage=GET-STRING(rawRecord,1)
           intFileOffset=intFileOffset + 103.

    RUN GetShort(intFileOffset,OUTPUT RCodeInfo.RCodeCRC).

    SEEK STREAM strRCodeFile TO END.
    ASSIGN RCodeInfo.RCodeSize=SEEK(strRCodeFile)
    
/* Information is "reliable" if last two fields are equaled
   to the official values (RCODE-INFO)
*/
    RCODE-INFO:FILE-NAME = chrFileName NO-ERROR.
    ASSIGN RCodeInfo.Reliability=(RCodeInfo.RCodeCRC=RCODE-INFO:CRC-VALUE)
                             AND (RCodeInfo.CodePage=RCODE-INFO:CODEPAGE)
           RCodeInfo.Languages = RCODE-INFO:LANGUAGES
           LENGTH(rawRecord) = 0.
  END. /* TRANSACTION */

  INPUT STREAM strRCodeFile CLOSE.
END PROCEDURE. /* RCodeInfo */

/*------------------------------------------------------------------------
   The rest of the procedure is an interface for r-code list creating.
------------------------------------------------------------------------*/

DEFINE VARIABLE whWindow    AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chrTitle    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE chrFileName AS CHARACTER     NO-UNDO.
DEFINE VARIABLE chrFileAttr AS CHARACTER     NO-UNDO.
DEFINE VARIABLE chrLanguage LIKE RCodeInfo.Languages NO-UNDO.

&IF PROVERSION BEGINS "7"
&THEN &SCOPED-DEFINE ROWID RECID
&ELSE &SCOPED-DEFINE ROWID ROWID
&ENDIF

DEFINE VARIABLE rowRCodeInfo AS {&ROWID} NO-UNDO.

DEFINE QUERY qryRCodeInfo FOR RCodeInfo.

DEFINE BROWSE brwRCodeInfo
        QUERY qryRCodeInfo DISPLAY
                RCodeInfo.FileName 
                RCodeInfo.RCodeSize
                RCodeInfo.Reliability
WITH &IF "{&WINDOW-SYSTEM}" = "TTY"  &THEN 3 &ELSE 5  &ENDIF DOWN
WIDTH 65.
  

DEFINE BUTTON Btn_Add
       LABEL "&Add" 
       SIZE 12 BY 1.

DEFINE BUTTON Btn_Clear
       LABEL "&Clear":L 
       SIZE 12 BY 1.

DEFINE BUTTON Btn_Save
       LABEL "&Save":L 
       SIZE 12 BY 1.

DEFINE BUTTON Btn_Exit AUTO-END-KEY DEFAULT
       LABEL "Exit":L 
       SIZE 12 BY 1.

/* ************************  Frame Definitions  *********************** */

&SCOPED-DEFINE ButtonColon COL 66.5

DEFINE FRAME frmRCodeFiles SKIP(0.3)
    brwRCodeInfo AT ROW 1.2 COL 1 SKIP(0.3)
    Btn_Add      AT ROW 1.5 {&ButtonColon}
    Btn_Clear    AT ROW 2.7 {&ButtonColon}
    Btn_Save     AT ROW 3.9 {&ButtonColon}
    Btn_Exit     AT ROW 5.1 {&ButtonColon}
WITH NO-LABELS NO-UNDERLINE THREE-D
     DEFAULT-BUTTON Btn_Add
     CANCEL-BUTTON  Btn_Exit
     KEEP-TAB-ORDER OVERLAY 
     CENTERED ROW 1
     &IF "{&WINDOW-SYSTEM}" = "TTY"
     &THEN NO-BOX
     &ELSE TITLE "R-Code List"
     &ENDIF.

     

&SCOPED-DEFINE Left COLON 12
&SCOPED-DEFINE Right COLON 40

DEFINE FRAME frmRCodeInfo
 RCodeInfo.CompVersion {&Left}         RCodeInfo.InitSegment   {&Right}
 RCodeInfo.RCodeLength {&Left}         RCodeInfo.ActionSegment {&Right}
 RCodeInfo.CodePage    {&Left}         RCodeInfo.DebugSegment  {&Right}
 RCodeInfo.RCodeCRC    {&Left}         RCodeInfo.ECodeSegment  {&Right}
 RCodeInfo.IProcNumber {&Left}         RCodeInfo.IProcSegment  {&Right}
 RCodeInfo.FrameNumber {&Left}         RCodeInfo.FrameSegment  {&Right}
 chrLanguage  FORMAT "x(15)"
      VIEW-AS COMBO-BOX {&Left}        RCodeInfo.TextSegment   {&Right}
WITH SIDE-LABELS THREE-D
     KEEP-TAB-ORDER OVERLAY 
     CENTERED ROW 8
     WIDTH 80
     TITLE IF AVAILABLE RCodeInfo 
           THEN RCodeInfo.FileName
           ELSE "<No R-Code>".

&SCOPED-DEFINE ScrollableFields IProcSegment FrameSegment chrLanguage TextSegment
 
/*------------------------------------------------------------------*/
PROCEDURE OnOpenQuery:
/* Desc: Refresh frame frmRCodeInfo.
*/
  IF NUM-RESULTS("qryRCodeInfo")=0
  THEN DO:
    HIDE  FRAME frmRCodeInfo NO-PAUSE.
    CLEAR FRAME frmRCodeInfo NO-PAUSE.
    VIEW  FRAME frmRCodeInfo.
    DISABLE Btn_Clear WITH FRAME frmRCodeFiles.
    DISABLE {&ScrollableFields} WITH FRAME frmRCodeInfo.
  END.
  ELSE DO:
    APPLY "VALUE-CHANGED":U TO brwRCodeInfo IN FRAME frmRCodeFiles.
    ENABLE Btn_Clear WITH FRAME frmRCodeFiles.
    ENABLE {&ScrollableFields} WITH FRAME frmRCodeInfo.
  END.
END PROCEDURE. /* OnOpenQuery */

/* ************************  Control Triggers  ************************ */

ON VALUE-CHANGED OF brwRCodeInfo
DO:
  HIDE FRAME frmRCodeInfo NO-PAUSE.
  ASSIGN chrLanguage = ENTRY(1,RCodeInfo.Languages)
         chrLanguage:LIST-ITEMS IN FRAME frmRCodeInfo = RCodeInfo.Languages.
  DISPLAY chrLanguage WITH FRAME frmRCodeInfo.
  DISPLAY RCodeInfo 
   EXCEPT RCodeInfo.FileName
          RCodeInfo.RCodeSize
          RCodeInfo.Languages
          RCodeInfo.Reliability
  WITH FRAME frmRCodeInfo.
END.

/*----- Hit of ADD Button -----*/
ON CHOOSE OF Btn_Add IN FRAME frmRCodeFiles
DO:
  ASSIGN chrRCodeFile="ALL":U
         chrTitle="Please Select a R-Code File or Type " + "ALL.":U.

  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
    SYSTEM-DIALOG GET-FILE chrRCodeFile
        TITLE chrTitle
        FILTERS "R-code (*.r)" "*.r"
        USE-FILENAME
        UPDATE logResult.
  &ELSE
    RUN adecomm/_filecom.p
      ( INPUT "*.r" /* p_Filter */, 
        INPUT ""          /* p_Dir */ , 
        INPUT ""          /* p_Drive */ ,
        INPUT NO ,          /* p_Save_As */
        INPUT chrTitle ,
        INPUT NO        /* must_exist */,
        INPUT-OUTPUT chrRCodeFile,
        OUTPUT logResult). 
  &ENDIF

  IF logResult THEN
  DO:
    ASSIGN i = MAX(R-INDEX(chrRCodeFile,"/"),
                   R-INDEX(chrRCodeFile,"~\"))
           chrFileName = SUBSTRING(chrRCodeFile,i + 1).

/* Get information about all r-code in the specified directory. */
    IF chrFileName = "ALL" THEN
    DO:
      ASSIGN logResult = SESSION:SET-WAIT-STATE("GENERAL").
      INPUT STREAM strTemp FROM OS-DIR(SUBSTRING(chrRCodeFile,1,i - 1)).

      REPEAT:
        IMPORT STREAM strTemp chrFileName chrRCodeFile chrFileAttr.

        IF CAN-DO(chrFileAttr,"F") 
        AND SUBSTRING(chrFileName,LENGTH(chrFileName) - 1)=".r" THEN
        DO:
          &IF "{&OPSYS}" BEGINS "WIN":U
          &THEN ASSIGN chrRCodeFile=LC(chrRCodeFile).
          &ENDIF
          RUN RCodeInfo(chrRCodeFile, BUFFER RCodeInfo).
        END.
      END.

      INPUT STREAM strTemp CLOSE.
      ASSIGN logResult = SESSION:SET-WAIT-STATE("")
             rowRCodeInfo=?.
    END.
    ELSE

/* Get information about the specified r-code. */
    IF SEARCH(chrRCodeFile)<>? THEN
    DO:
      RUN RCodeInfo(chrRCodeFile, BUFFER RCodeInfo).
      ASSIGN rowRCodeInfo={&ROWID}(RCodeInfo).
    END.
  END. /* IF logResult  */
  
  OPEN QUERY qryRCodeInfo FOR EACH RCodeInfo NO-LOCK.
  IF NUM-RESULTS("qryRCodeInfo":U) > 0 THEN
  ASSIGN logResult = brwRCodeInfo:SCROLL-TO-CURRENT-ROW().
  IF rowRCodeInfo <> ? THEN
  REPOSITION qryRCodeInfo TO {&ROWID} rowRCodeInfo NO-ERROR.
  RUN OnOpenQuery.

END.   /* ON CHOOSE OF Btn_Add */

/*----- Hit of CLEAR Button -----*/
ON CHOOSE OF Btn_Clear IN FRAME frmRCodeFiles
DO:
  IF AVAILABLE RCodeInfo THEN 
  DO TRANSACTION:
    DELETE RCodeInfo.
    ASSIGN logResult = brwRCodeInfo:DELETE-CURRENT-ROW().
  END.
  /*
  OPEN QUERY qryRCodeInfo FOR EACH RCodeInfo NO-LOCK.
  RUN OnOpenQuery.*/
END.

/*----- Hit of SAVE Button -----*/
ON CHOOSE OF Btn_Save IN FRAME frmRCodeFiles
DO:
  ASSIGN chrTitle="Save to File."
         chrSaveFile=OS-GETENV("CLIENTMON").
  IF chrSaveFile = ? THEN chrSaveFile="client.mon".
  
  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
    SYSTEM-DIALOG GET-FILE chrSaveFile
        TITLE chrTitle
        FILTERS "Monitor File (*.mon)" "*.mon"
        SAVE-AS
        USE-FILENAME
        UPDATE logResult.
  &ELSE
    RUN adecomm/_filecom.p
      ( INPUT "*.mon" /* p_Filter */, 
        INPUT ""          /* p_Dir */ , 
        INPUT ""          /* p_Drive */ ,
        INPUT YES ,          /* p_Save_As */
        INPUT chrTitle ,
        INPUT NO     /* must_exist */,
        INPUT-OUTPUT chrRCodeFile,
        OUTPUT logResult). 
  &ENDIF

  IF logResult THEN
  DO:
    ASSIGN chrFileAttr=FILL("-":U,64).
    OUTPUT STREAM strTemp TO VALUE(chrSaveFile) APPEND.


    FOR EACH RCodeInfo NO-LOCK:

&SCOPED-DEFINE Label LABEL IN FRAME frmRCodeInfo TO 12 ": "

      PUT STREAM strTemp UNFORMATTED                  SKIP
         chrFileAttr   SKIP   RCodeInfo.FileName      SKIP
         CompVersion:{&Label} RCodeInfo.CompVersion   SKIP
           "Size" TO 12 ": "  RCodeInfo.RCodeSize     SKIP
         RCodeLength:{&Label} RCodeInfo.RCodeLength   SKIP
            CodePage:{&Label} RCodeInfo.CodePage      SKIP
            RCodeCRC:{&Label} RCodeInfo.RCodeCRC      SKIP
         InitSegment:{&Label} RCodeInfo.InitSegment   SKIP
       ActionSegment:{&Label} RCodeInfo.ActionSegment SKIP
        ECodeSegment:{&Label} RCodeInfo.ECodeSegment  SKIP
        DebugSegment:{&Label} RCodeInfo.DebugSegment  SKIP
         IProcNumber:{&Label} RCodeInfo.IProcNumber   SKIP.
         
      IF RCodeInfo.IProcNumber > 0 THEN
      PUT STREAM strTemp UNFORMATTED
        IProcSegment:{&Label} RCodeInfo.IProcSegment  SKIP.

      PUT STREAM strTemp UNFORMATTED
         FrameNumber:{&Label} RCodeInfo.FrameNumber   SKIP.
      IF RCodeInfo.FrameNumber > 0 THEN
      PUT STREAM strTemp UNFORMATTED
        FrameSegment:{&Label} RCodeInfo.FrameSegment  SKIP.

      PUT STREAM strTemp UNFORMATTED
         TextSegment:{&Label} RCodeInfo.TextSegment   SKIP
       "Languages" TO 12 ": " RCodeInfo.Languages     SKIP.

      IF RCodeInfo.Reliability <> TRUE THEN
      PUT STREAM strTemp UNFORMATTED
        "Warning: Information is unreliable." SKIP.

    END. /* FOR EACH RCodeInfo */
    OUTPUT STREAM strTemp CLOSE.
  END.   /* IF logResult */
END.     /* ON CHOOSE OF SAVE Button  */

/*----- Hit of EXIT Button -----*/
ON CHOOSE OF btn_Exit IN FRAME frmRCodeFiles
DO:
  QUIT.
END.

CREATE WIDGET-POOL.

IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW whWindow ASSIGN
         HIDDEN             = YES
         TITLE              = "Extended R-Code Information"
         HEIGHT             = 19
         WIDTH              = 80
         MAX-HEIGHT         = 16.5
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 20
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE whWindow = CURRENT-WINDOW.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(whWindow)
THEN whWindow:HIDDEN = no.

ON WINDOW-CLOSE OF whWindow
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

ON CLOSE OF THIS-PROCEDURE 
DO:
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(whWindow)
  THEN DELETE WIDGET whWindow.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

VIEW FRAME frmRCodeFiles IN WINDOW whWindow.
VIEW FRAME frmRCodeInfo  IN WINDOW whWindow.

DO ON ENDKEY UNDO,RETURN WITH FRAME frmRCodeFiles:
  ENABLE brwRCodeInfo
         Btn_Add
         Btn_Save
         Btn_Exit.
  WAIT-FOR CHOOSE OF btn_Exit.
END.



