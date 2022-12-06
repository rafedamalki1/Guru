&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: VALDBSERVERMULTI.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE wcguruwtidirstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.

DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hkeyvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAns AS CHARACTER NO-UNDO.
DEFINE VARIABLE flyttaordning AS INTEGER NO-UNDO.
DEFINE VARIABLE serverwtidir AS CHARACTER NO-UNDO.
{HKEYADMPER.I}  
{HKEYCURRENTUSER.I}
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandosortquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO. 
DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
   
DEFINE TEMP-TABLE serverprogram NO-UNDO
  FIELD ORDNING AS INTEGER
  FIELD KOMANDO AS CHARACTER
  FIELD FLYTTA AS LOGICAL
  INDEX ORDNING ORDNING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_KOM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES serverprogram

/* Definitions for BROWSE BRW_KOM                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KOM serverprogram.ORDNING ~
serverprogram.KOMANDO serverprogram.FLYTTA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOM 
&Scoped-define QUERY-STRING-BRW_KOM FOR EACH serverprogram NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOM OPEN QUERY BRW_KOM FOR EACH serverprogram NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOM serverprogram
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOM serverprogram


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS wcguruwtidir guruwtidir BRW_KOM Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS wcguruwtidir guruwtidir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE guruwtidir AS CHARACTER FORMAT "X(256)":U 
     LABEL "WT" 
     VIEW-AS FILL-IN 
     SIZE 120 BY 1 NO-UNDO.

DEFINE VARIABLE wcguruwtidir AS CHARACTER FORMAT "X(256)":U 
     LABEL "WC" 
     VIEW-AS FILL-IN 
     SIZE 120 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KOM FOR 
      serverprogram SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOM Dialog-Frame _STRUCTURED
  QUERY BRW_KOM NO-LOCK DISPLAY
      serverprogram.ORDNING FORMAT ">>9":U WIDTH 2
      serverprogram.KOMANDO FORMAT "x(256)":U
      serverprogram.FLYTTA FORMAT "yes/no":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 119 BY 16.5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     wcguruwtidir AT ROW 3 COL 3 COLON-ALIGNED WIDGET-ID 2
     guruwtidir AT ROW 5.25 COL 3 COLON-ALIGNED WIDGET-ID 4
     BRW_KOM AT ROW 8.25 COL 3.5 WIDGET-ID 200
     Btn_OK AT ROW 27 COL 34
     Btn_Cancel AT ROW 27 COL 61.5
     SPACE(49.49) SKIP(1.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Från wc till wtid"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: serverprogram T "?" NO-UNDO TEMP-DB serverprogram
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_KOM guruwtidir Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOM
/* Query rebuild information for BROWSE BRW_KOM
     _TblList          = "Temp-Tables.serverprogram"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.serverprogram.ORDNING
"ORDNING" ? ">>9" "integer" ? ? ? ? ? ? no ? no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.serverprogram.KOMANDO
"KOMANDO" ? "x(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.serverprogram.FLYTTA
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOM */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Från wc till wtid */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   
   dynqueh:GET-FIRST().
   MESSAGE serverprogram.KOMANDO
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL UPDATE valk1 AS LOGICAL.  
   CASE valk1:
      WHEN TRUE THEN DO:
         {muswait.i}
         DO WHILE dynqueh:QUERY-OFF-END = FALSE:
            guruwtidir = serverprogram.KOMANDO.
            DISPLAY wcguruwtidir guruwtidir 
            WITH FRAME Dialog-Frame.
            OS-COMMAND SILENT VALUE(serverprogram.KOMANDO).
            
            dynbuffh:BUFFER-DELETE() NO-ERROR. 
            RUN openq_UI.
            dynqueh:GET-FIRST().
         END. 
       /*  {musarrow.i}*/  
      END.
      WHEN FALSE THEN RETURN NO-APPLY.
      OTHERWISE RETURN NO-APPLY.
   END CASE.
   
   {musarrow.i}
   MESSAGE "klart!"
   VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME guruwtidir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL guruwtidir Dialog-Frame
ON LEAVE OF guruwtidir IN FRAME Dialog-Frame /* WT */
DO:
   guruwtidir = INPUT guruwtidir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wcguruwtidir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wcguruwtidir Dialog-Frame
ON LEAVE OF wcguruwtidir IN FRAME Dialog-Frame /* WC */
DO:
  wcguruwtidir = INPUT wcguruwtidir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOM
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ  ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.



/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   DEFINE VARIABLE ejok AS LOGICAL NO-UNDO.
   RUN ServerProAonr.p.
   RUN ServerProber.p.
   RUN ServerProDep.p.
   RUN ServerProFak.p.
   RUN ServerProFlex.p.
   RUN ServerProKalk.p.
   RUN ServerProMark.p.
   RUN ServerProMtrl.p.
   RUN ServerProPers.p.
   RUN ServerProPlan.p.
   RUN ServerProReg.p.
   RUN ServerProStor.p.
   RUN ServerProTid.p.
   RUN Modules\EKGdata\ServerProEkg.p.
   ASSIGN
   companyname = "SOFTWARE\Elpool i Umeå AB\".
   /*
    MESSAGE "GuruOnServer11 eller GuruOnWeb11" 
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO UPDATE applog AS LOGICAL.  
   CASE applog:
      WHEN TRUE THEN appnamn = "GuruOnServer11".
      WHEN FALSE THEN appnamn = "GuruOnWeb11".
   END CASE.
   */
   appnamn = "GuruOnWeb11".
   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      companyname = "SOFTWARE\Wow6432Node\Elpool i Umeå AB\".
      LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   END.   
   LOAD companyname BASE-KEY hkeyvar NO-ERROR. 
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN ejok = TRUE.
   USE companyname NO-ERROR. 
   GET-KEY-VALUE SECTION appnamn KEY "ApplicationDirectory" VALUE vAns.
   UNLOAD companyname NO-ERROR.
   wcguruwtidir = vAns + "\WTID".


   ASSIGN 
   companyname = "SOFTWARE\PSC\PROGRESS\".
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN appnamn = PROVERSION.
   ELSE appnamn = "11.2".
   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
  
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      appnamn = "11.2".
      LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "11.4".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "11.5".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "11.6".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "11.7".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      /*för att klara blandning mellan 32 bit och 64 bit.*/
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         companyname = "SOFTWARE\Wow6432Node\PSC\PROGRESS\".
         appnamn = "11.2".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            appnamn = "11.4".
            LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         END.
         IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            appnamn = "11.5".
            LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         END.
         IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            appnamn = "11.6".
            LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         END.
          IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
             appnamn = "11.7".
             LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
          END.
      END.   
   END.
      
   IF ERROR-STATUS:NUM-MESSAGES > 0 OR ejok = TRUE THEN DO:
      MESSAGE "Funkar inte! wcguruwtidir" wcguruwtidir SKIP 
      "companyname" companyname SKIP 
      "appnamn" appnamn
      VIEW-AS ALERT-BOX.
      ejok = TRUE.
    
   END. 
   ELSE DO:
      UNLOAD companyname + appnamn + "\" NO-ERROR .
      /*letAr efter PROGRESS INSTALLATIONEN*/  
      hkeyvar = "HKEY_LOCAL_MACHINE".
      LOAD companyname BASE-KEY hkeyvar NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES > 0 OR ejok = TRUE THEN ejok = TRUE.
      USE companyname NO-ERROR. 
      GET-KEY-VALUE SECTION appnamn + "\Startup" KEY "DLC" VALUE guruwtidir.
      UNLOAD companyname NO-ERROR.
   END.
   serverwtidir = REPLACE(guruwtidir,"DLC","GURU\WTID").
   
   guruwtidir = 'robocopy "' + wcguruwtidir + '" "' + serverwtidir + '" *.* /mir'.
   
   RUN Skapaserverprogram_UI (INPUT guruwtidir).
   
   RUN NyttKommand_UI.
   ejok = TRUE.
   RUN enable_UI.
   dynbrwh = BRW_KOM:HANDLE.
   RUN brwini_UI.
   dynqueh:GET-FIRST().
   guruwtidir = serverprogram.KOMANDO.
   DISPLAY wcguruwtidir guruwtidir 
   WITH FRAME Dialog-Frame.
   /*
   IF ejok = TRUE THEN.
   ELSE APPLY "CHOOSE" TO Btn_OK. 
   */
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwini_UI Dialog-Frame 
PROCEDURE brwini_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   dynqueh = dynbrwh:QUERY.
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   kommandosortquery = "FOR EACH " + dynbuffh:TABLE.
   dynok = dynqueh:QUERY-PREPARE(kommandosortquery).
   dynok = dynqueh:QUERY-OPEN() NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY wcguruwtidir guruwtidir 
      WITH FRAME Dialog-Frame.
  ENABLE wcguruwtidir guruwtidir BRW_KOM Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyttKommand_UI Dialog-Frame 
PROCEDURE NyttKommand_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(58)" LABEL "File" NO-UNDO.
   DEFINE VARIABLE filserver AS CHARACTER NO-UNDO.
   DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
   DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(60)" LABEL "Directory" NO-UNDO.
   DEFINE VARIABLE GuruVarDF AS CHARACTER NO-UNDO.
   GuruVarDF = 'robocopy "' + wcguruwtidir + "\GURU.df" + '" "' + serverwtidir + ' \GURU.df"'.
   
   wcguruwtidir = REPLACE(wcguruwtidir,"WTID",""). 
   
   INPUT FROM OS-DIR(wcguruwtidir) NO-ECHO.
   REPEAT:
      /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
      SET filnamn dirlist VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 attrlist.
      IF attrlist = "D" THEN DO:
         IF dirlist BEGINS wcguruwtidir + "C" OR dirlist BEGINS wcguruwtidir + "2GURU" THEN DO:
            guruwtidir = 'robocopy "' + dirlist + '" "' + serverwtidir + '" *.* /E'.
            RUN Skapaserverprogram_UI (guruwtidir).            
                                        
         END.
      END.
   END.   
   INPUT CLOSE.
   
   RUN Skapaserverprogram_UI (GuruVarDF).  
   
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openq_UI Dialog-Frame 
PROCEDURE openq_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   dynok = dynqueh:QUERY-OPEN() NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postbort_UI Dialog-Frame 
PROCEDURE postbort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Skapaserverprogram_UI Dialog-Frame 
PROCEDURE Skapaserverprogram_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER fkomando AS CHARACTER NO-UNDO.
   
   CREATE serverprogram.
   flyttaordning = flyttaordning + 1.
   ASSIGN 
   serverprogram.ORDNING = flyttaordning
   serverprogram.KOMANDO = fkomando
   serverprogram.FLYTTA = TRUE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

