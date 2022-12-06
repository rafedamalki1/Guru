&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE comrec AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE comcon AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE comcommand AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE odbcdsn AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcserver AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcuserid AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcpassword AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbcnull AS CHARACTER NO-UNDO.

DEFINE VARIABLE val AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE sqltemp
   FIELD tint1 AS INTEGER
   FIELD tch1 AS CHARACTER
   FIELD tch2 AS CHARACTER
   FIELD tch3 AS CHARACTER
   FIELD tch4 AS CHARACTER
   INDEX tch1 AS PRIMARY tch1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_VAL

/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL FILL-IN-NAME FILL-IN-ANDNAME ~
FILL-IN-LASTNAME FILL-IN-LOGIN FILL-IN-PASS BTN_VAL BRW_VAL BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL FILL-IN-NAME FILL-IN-ANDNAME ~
FILL-IN-LASTNAME FILL-IN-LOGIN FILL-IN-PASS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avsluta" 
     SIZE 11 BY 1.13.

DEFINE BUTTON BTN_VAL 
     LABEL "Sänd" 
     SIZE 9 BY 1.13.

DEFINE VARIABLE FILL-IN-ANDNAME AS CHARACTER FORMAT "X(256)":U 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LASTNAME AS CHARACTER FORMAT "X(256)":U 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOGIN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Login" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAME AS CHARACTER FORMAT "X(256)":U 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PASS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visa", 1,
"Ta bort", 2,
"Uppdatera", 3,
"Lägg till", 4,
"Visa fält", 5
     SIZE 75 BY 1.5 NO-UNDO.


/* Browse definitions                                                   */
DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45.5 BY 6.5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_VAL AT ROW 1.5 COL 3.5 NO-LABEL
     FILL-IN-NAME AT ROW 4.75 COL 11.5 COLON-ALIGNED
     FILL-IN-ANDNAME AT ROW 4.75 COL 56 COLON-ALIGNED
     FILL-IN-LASTNAME AT ROW 5.96 COL 11.5 COLON-ALIGNED
     FILL-IN-LOGIN AT ROW 7.13 COL 11.5 COLON-ALIGNED
     FILL-IN-PASS AT ROW 8.38 COL 11.5 COLON-ALIGNED
     BTN_VAL AT ROW 9.63 COL 16
     BRW_VAL AT ROW 10 COL 34
     BTN_AVS AT ROW 15.5 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.96.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_VAL BTN_VAL DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON VALUE-CHANGED OF BRW_VAL IN FRAME DEFAULT-FRAME
DO:
  FOR EACH sqltemp:
/*      tint1          */
/*      tch1           */
/*      tch2           */
/*      tch3           */
/*      tch4           */
/*                     */
/*      ASSIGN BRW_VAL */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&Scoped-define SELF-NAME BTN_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VAL C-Win
ON CHOOSE OF BTN_VAL IN FRAME DEFAULT-FRAME /* Sänd */
DO:
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   RUN val_UI (INPUT RAD_VAL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ANDNAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ANDNAME C-Win
ON LEAVE OF FILL-IN-ANDNAME IN FRAME DEFAULT-FRAME /* Förnamn */
DO:
  FILL-IN-ANDNAME = INPUT FILL-IN-ANDNAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LASTNAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LASTNAME C-Win
ON LEAVE OF FILL-IN-LASTNAME IN FRAME DEFAULT-FRAME /* Efternamn */
DO:
  FILL-IN-LASTNAME = INPUT FILL-IN-LASTNAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LOGIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LOGIN C-Win
ON LEAVE OF FILL-IN-LOGIN IN FRAME DEFAULT-FRAME /* Login */
DO:
  FILL-IN-LOGIN = INPUT FILL-IN-LOGIN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NAME C-Win
ON LEAVE OF FILL-IN-NAME IN FRAME DEFAULT-FRAME /* Förnamn */
DO:
  FILL-IN-NAME = INPUT FILL-IN-NAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PASS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PASS C-Win
ON LEAVE OF FILL-IN-PASS IN FRAME DEFAULT-FRAME /* Password */
DO:
  FILL-IN-PASS = INPUT FILL-IN-PASS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
   RAD_VAL = INPUT RAD_VAL.
   RUN goma_UI (INPUT RAD_VAL).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
    comcon:Close NO-ERROR. 
   /* Don't forget to release the memory!! */
   RELEASE OBJECT comcon NO-ERROR.
   RELEASE OBJECT comcommand NO-ERROR.
   RELEASE OBJECT comrec NO-ERROR.  
   ASSIGN 
   comcon = ? 
   comcommand = ? 
   comrec = ?.
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   /* Create the connection object for the link to SQL */
   CREATE "ADODB.Connection" comcon.
   /* Create a recordset object ready to return the data */
   CREATE "ADODB.RecordSet" comrec.
   /* Create a command object for sending the SQL statement */
   CREATE "ADODB.Command" comcommand.
   /* Change the below values as necessary */
   ASSIGN 
   odbcdsn = "jalle" /* The ODBC DSN */
   odbcserver = "pc009" /* The name of the server hosting the SQL DB and DSN */
   odbcuserid = "jalle" /* The user id for access to the SQL Database */
   odbcpassword = "jalle". /* Password required by above user-id */
   
   comcon:Open ("data source=" + odbcdsn + ";server=" +
         odbcserver, odbcuserid, odbcpassword, 0 ) NO-ERROR.
   
   RUN enable_UI.
   IF ( ERROR-STATUS:NUM-MESSAGES > 0 ) THEN DO:
      MESSAGE "Error: Could not establish connection.".
   END.
   ELSE DO:
       MESSAGE "Connected!".
       RAD_VAL = 1.
       RUN goma_UI (INPUT RAD_VAL).
       RUN val_UI (INPUT RAD_VAL).                 
   END.
   

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL FILL-IN-NAME FILL-IN-ANDNAME FILL-IN-LASTNAME FILL-IN-LOGIN 
          FILL-IN-PASS 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RAD_VAL FILL-IN-NAME FILL-IN-ANDNAME FILL-IN-LASTNAME FILL-IN-LOGIN 
         FILL-IN-PASS BTN_VAL BTN_AVS 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
  IF val = 1 THEN DO:
     ASSIGN 
     FILL-IN-NAME:HIDDEN IN FRAME DEFAULT-FRAME = TRUE
     FILL-IN-LASTNAME:HIDDEN = TRUE
     FILL-IN-LOGIN:HIDDEN = TRUE
     FILL-IN-PASS:HIDDEN = TRUE
     FILL-IN-ANDNAME:HIDDEN = TRUE
     BTN_VAL:HIDDEN = TRUE.     
  END.
  IF val = 2 THEN DO:
     ASSIGN 
     FILL-IN-NAME:HIDDEN = FALSE
     FILL-IN-LASTNAME:HIDDEN = FALSE
     FILL-IN-LOGIN:HIDDEN = FALSE
     FILL-IN-PASS:HIDDEN = FALSE
     FILL-IN-ANDNAME:HIDDEN = TRUE
     BTN_VAL:HIDDEN = FALSE.
     DISPLAY FILL-IN-NAME FILL-IN-LASTNAME FILL-IN-LOGIN FILL-IN-PASS BTN_VAL WITH FRAME DEFAULT-FRAME.
  END.
  IF val = 3 THEN DO:
     ASSIGN 
     FILL-IN-NAME:HIDDEN = FALSE
     FILL-IN-LASTNAME:HIDDEN = FALSE
     FILL-IN-LOGIN:HIDDEN = FALSE
     FILL-IN-PASS:HIDDEN = FALSE
     FILL-IN-ANDNAME:HIDDEN = FALSE
     BTN_VAL:HIDDEN = FALSE.
     DISPLAY FILL-IN-NAME FILL-IN-LASTNAME FILL-IN-LOGIN FILL-IN-PASS FILL-IN-ANDNAME BTN_VAL WITH FRAME DEFAULT-FRAME.
  END.
  IF val = 4 THEN DO:
     ASSIGN 
     FILL-IN-NAME:HIDDEN = FALSE
     FILL-IN-LASTNAME:HIDDEN = FALSE
     FILL-IN-LOGIN:HIDDEN = FALSE
     FILL-IN-PASS:HIDDEN = FALSE
     FILL-IN-ANDNAME:HIDDEN = TRUE
     BTN_VAL:HIDDEN = FALSE.
     DISPLAY FILL-IN-NAME FILL-IN-LASTNAME FILL-IN-LOGIN FILL-IN-PASS BTN_VAL WITH FRAME DEFAULT-FRAME.
  END.
  IF val = 5 THEN DO:
     ASSIGN 
     FILL-IN-NAME:HIDDEN = TRUE
     FILL-IN-LASTNAME:HIDDEN = TRUE
     FILL-IN-LOGIN:HIDDEN = TRUE
     FILL-IN-PASS:HIDDEN = TRUE
     FILL-IN-ANDNAME:HIDDEN = TRUE
     BTN_VAL:HIDDEN = TRUE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val_UI C-Win 
PROCEDURE val_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
   IF val = 1 THEN DO:
      /*Hämta poster*/
      odbcquery = "SELECT * FROM member;".
      Assign
      comcommand:ActiveConnection = comcon
      comcommand:CommandText = odbcquery
      comcommand:CommandType = 1 /* adCmdText */
      comcon:CursorLocation = 3 /* adUseClient */
      comrec:CursorType = 3 /* adOpenStatic*/
      comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).
      DO WHILE NOT comrec:eof:
         CREATE sqltemp.
         tint1 = comrec:FIELDS("member_id"):VALUE.
         tch1 = comrec:FIELDS("firstname"):VALUE.
         tch2 = comrec:FIELDS("lastname"):VALUE.
         tch3 = comrec:FIELDS("login"):VALUE.
         tch4 = comrec:FIELDS("password"):VALUE.
         comrec:MoveNext.
      END.
      /*Visa i browse*/
   END.
   ELSE IF val = 2 THEN DO:
      odbcquery = "DELETE from MEMBER WHERE firstname = FILL-IN-NAME AND LOGIN = FILL-IN-LOGIN;".
      Assign
      comcommand:ActiveConnection = comcon
      comcommand:CommandText = odbcquery
      comcommand:CommandType = 1 /* adCmdText */
      comcon:CursorLocation = 3 /* adUseClient */
      comrec:CursorType = 3 /* adOpenStatic*/
      comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).
   END.
   ELSE IF val = 3 THEN DO:
      odbcquery = "UPDATE MEMBER SET firstname = FILL-IN-NAME, lastname = FILL-IN-LASTNAME , login = FILL-IN-LOGIN ,
                password = FILL-IN-PASS WHERE firstname = FILL-IN-ANDNAME;".
      Assign
      comcommand:ActiveConnection = comcon
      comcommand:CommandText = odbcquery
      comcommand:CommandType = 1 /* adCmdText */
      comcon:CursorLocation = 3 /* adUseClient */
      comrec:CursorType = 3 /* adOpenStatic*/
      comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).
   END.
   ELSE IF val = 4 THEN DO:
      odbcquery = "INSERT INTO MEMBER (firstname,lastname,login,password) VALUES (FILL-IN-NAME,FILL-IN-LASTNAME,FILL-IN-LOGIN,FILL-IN-PASS);".
      Assign
      comcommand:ActiveConnection = comcon
      comcommand:CommandText = odbcquery
      comcommand:CommandType = 1 /* adCmdText */
      comcon:CursorLocation = 3 /* adUseClient */
      comrec:CursorType = 3 /* adOpenStatic*/
      comrec = comcommand:Execute( OUTPUT odbcnull, "", 32 ).
   END.
   ELSE IF val = 5 THEN DO:
      MESSAGE "Visa fält i tabell VIEW-AS ALERT-BOX".
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

