&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports           PROGRESS
*/
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
/*          This .W file was created with the Progress UIB.             */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_STANS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for BROWSE BRW_STANS                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_STANS Customer.Cust-Num Customer.Name ~
Customer.City Customer.Address Customer.Phone 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STANS Customer.Cust-Num ~
Customer.Address 
&Scoped-define FIELD-PAIRS-IN-QUERY-BRW_STANS~
 ~{&FP1}Cust-Num ~{&FP2}Cust-Num ~{&FP3}~
 ~{&FP1}Address ~{&FP2}Address ~{&FP3}
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_STANS Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_STANS Customer
&Scoped-define OPEN-QUERY-BRW_STANS OPEN QUERY BRW_STANS FOR EACH Customer SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STANS Customer
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STANS Customer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_STANS}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-36 BRW_STANS BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY  NO-CONVERT-3D-COLORS
     LABEL "Avbryt":L 
     SIZE 12 BY 1.52.

DEFINE RECTANGLE RECT-36
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.8 BY 2.29
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_STANS FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_STANS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STANS C-Win _STRUCTURED
  QUERY BRW_STANS SHARE-LOCK NO-WAIT DISPLAY
      Customer.Cust-Num
      Customer.Name
      Customer.City
      Customer.Address
      Customer.Phone
  ENABLE
      Customer.Cust-Num
      Customer.Address HELP "focus"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 91 BY 6.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_STANS AT ROW 1.48 COL 3
     BTN_AVB AT ROW 15.57 COL 43.8
     RECT-36 AT ROW 15.24 COL 27.8
     "när jag byter från rad 1 till rad 2 med tab tangenten vill jag hamna i fältet" VIEW-AS TEXT
          SIZE 91 BY 1 TOOLTIP "Hur skall det finnas direktkopplingar till andra funktioner?" AT ROW 18.38 COL 2
     "ADRESS" VIEW-AS TEXT
          SIZE 91 BY 1 AT ROW 19.62 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95 BY 20.5.


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
         TITLE              = "Stansning av tiduppgifter"
         HEIGHT             = 21.14
         WIDTH              = 96.4
         MAX-HEIGHT         = 24.57
         MAX-WIDTH          = 97.2
         VIRTUAL-HEIGHT     = 24.57
         VIRTUAL-WIDTH      = 97.2
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_STANS TEXT-2 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STANS
/* Query rebuild information for BROWSE BRW_STANS
     _TblList          = "sports.Customer"
     _Options          = "SHARE-LOCK"
     _FldNameList[1]   > sports.Customer.Cust-Num
"Cust-Num" ? ? "integer" ? ? ? ? ? ? yes ?
     _FldNameList[2]   = sports.Customer.Name
     _FldNameList[3]   = sports.Customer.City
     _FldNameList[4]   > sports.Customer.Address
"Address" ? ? "character" ? ? ? ? ? ? yes "focus"
     _FldNameList[5]   = sports.Customer.Phone
     _Query            is OPENED
*/  /* BROWSE BRW_STANS */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Stansning av tiduppgifter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Stansning av tiduppgifter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_STANS
&Scoped-define SELF-NAME BRW_STANS
&Scoped-define SELF-NAME Customer.Cust-Num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Cust-Num BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF Customer.Cust-Num IN BROWSE BRW_STANS /* Cust-Num */
DO:        
   DISPLAY Customer.Cust-Num WITH BROWSE BRW_STANS.
   RUN felnr_UI.
       
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Cust-Num BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF Customer.Cust-Num IN BROWSE BRW_STANS /* Cust-Num */
DO:
   Customer.Cust-Num = INPUT BROWSE BRW_STANS Customer.Cust-Num.
   DISPLAY Customer.Cust-Num WITH BROWSE BRW_STANS.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Customer.Name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Name BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF Customer.Name IN BROWSE BRW_STANS /* Name */
DO:
   
   DISPLAY Customer.Name WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Name BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF Customer.Name IN BROWSE BRW_STANS /* Name */
DO:
   Customer.Name = INPUT BROWSE BRW_STANS Customer.Name.
   DISPLAY Customer.Name WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Customer.City
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.City BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF Customer.City IN BROWSE BRW_STANS /* City */
DO:
   
   DISPLAY Customer.City WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.City BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF Customer.City IN BROWSE BRW_STANS /* City */
DO:
   Customer.City = INPUT BROWSE BRW_STANS Customer.City.
   DISPLAY Customer.City WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Customer.Address
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Address BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF Customer.Address IN BROWSE BRW_STANS /* Address */
DO:
   DISPLAY Customer.Address WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Address BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF Customer.Address IN BROWSE BRW_STANS /* Address */
DO:
   Customer.Address = INPUT BROWSE BRW_STANS Customer.Address.
   DISPLAY Customer.Address WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Customer.Phone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Phone BRW_STANS _BROWSE-COLUMN C-Win
ON ENTRY OF Customer.Phone IN BROWSE BRW_STANS /* Phone */
DO:
   
   DISPLAY Customer.Phone WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Customer.Phone BRW_STANS _BROWSE-COLUMN C-Win
ON LEAVE OF Customer.Phone IN BROWSE BRW_STANS /* Phone */
DO:
   Customer.Cust-Num = INPUT BROWSE BRW_STANS Customer.Cust-Num.
   DISPLAY Customer.Cust-Num WITH BROWSE BRW_STANS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:

DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN enable_UI.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  ENABLE RECT-36 BRW_STANS BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE felnr_UI C-Win 
PROCEDURE felnr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   APPLY "ENTRY" TO Customer.ADDRESS IN BROWSE BRW_STANS.
   RETURN.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


