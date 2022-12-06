&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 04/04/97 -  1:08 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE defprisproch AS HANDLE NO-UNDO. /* Procedure handle, DEFPRISAPP.P */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL FORMAT "99.99" INITIAL 7.00 NO-UNDO. 

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED
{BEFTEMP.I}
{OVERTEXTEMP.I}
DEFINE SHARED TEMP-TABLE extraopltemp NO-UNDO LIKE overprislisttemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_BEF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES befattningstemp overtextfakttemp

/* Definitions for BROWSE BRW_BEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BEF befattningstemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEF befattningstemp.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEF befattningstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEF befattningstemp
&Scoped-define QUERY-STRING-BRW_BEF FOR EACH befattningstemp NO-LOCK ~
    BY befattningstemp.NAMN INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_BEF OPEN QUERY BRW_BEF FOR EACH befattningstemp NO-LOCK ~
    BY befattningstemp.NAMN INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_BEF befattningstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEF befattningstemp


/* Definitions for BROWSE BRW_OVER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_OVER overtextfakttemp.OTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OVER overtextfakttemp.OTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OVER overtextfakttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OVER overtextfakttemp
&Scoped-define QUERY-STRING-BRW_OVER FOR EACH overtextfakttemp NO-LOCK ~
    BY overtextfakttemp.OTEXT
&Scoped-define OPEN-QUERY-BRW_OVER OPEN QUERY BRW_OVER FOR EACH overtextfakttemp NO-LOCK ~
    BY overtextfakttemp.OTEXT.
&Scoped-define TABLES-IN-QUERY-BRW_OVER overtextfakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OVER overtextfakttemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BEF BRW_OVER CMB_DAGTYP FILL-IN_PRISA ~
FILL-IN_START FILL-IN_SLUT BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_DAGTYP FILL-IN_PRISA FILL-IN_START ~
FILL-IN_SLUT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_DAGTYP AS CHARACTER FORMAT "X(8)":U 
     LABEL "Dagtyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Vardagar","Lördagar","Söndagar" 
     DROP-DOWN-LIST
     SIZE 11.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_PRISA AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1.

DEFINE VARIABLE FILL-IN_SLUT AS DECIMAL FORMAT "99.99" INITIAL 16 
     LABEL "Slut tid" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_START AS DECIMAL FORMAT "99.99" INITIAL 7 
     LABEL "Start tid" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEF FOR 
      befattningstemp SCROLLING.

DEFINE QUERY BRW_OVER FOR 
      overtextfakttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEF DIALOG-1 _STRUCTURED
  QUERY BRW_BEF NO-LOCK DISPLAY
      befattningstemp.NAMN COLUMN-LABEL "Övertid för" FORMAT "x(20)":U
  ENABLE
      befattningstemp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 23.63 BY 6.5.

DEFINE BROWSE BRW_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OVER DIALOG-1 _STRUCTURED
  QUERY BRW_OVER NO-LOCK DISPLAY
      overtextfakttemp.OTEXT COLUMN-LABEL "Övertidstext" FORMAT "X(25)":U
  ENABLE
      overtextfakttemp.OTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 28 BY 6.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_BEF AT ROW 1.42 COL 1.5
     BRW_OVER AT ROW 1.42 COL 27.38
     CMB_DAGTYP AT ROW 8.42 COL 10.38 COLON-ALIGNED
     FILL-IN_PRISA AT ROW 9.88 COL 10.38 COLON-ALIGNED
     FILL-IN_START AT ROW 11.38 COL 10.38 COLON-ALIGNED
     FILL-IN_SLUT AT ROW 12.88 COL 10.38 COLON-ALIGNED
     BTN_OK AT ROW 14.29 COL 26.38
     BTN_AVB AT ROW 14.29 COL 41.38
     SPACE(1.11) SKIP(0.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Övertidsregler".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: befattningstemp T "?" NO-UNDO temp-db befattningstemp
      TABLE: overtextfakttemp T "?" NO-UNDO temp-db overtextfakttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_BEF 1 DIALOG-1 */
/* BROWSE-TAB BRW_OVER BRW_BEF DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEF
/* Query rebuild information for BROWSE BRW_BEF
     _TblList          = "Temp-Tables.befattningstemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.befattningstemp.NAMN|yes"
     _FldNameList[1]   > Temp-Tables.befattningstemp.NAMN
"befattningstemp.NAMN" "Övertid för" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OVER
/* Query rebuild information for BROWSE BRW_OVER
     _TblList          = "Temp-Tables.overtextfakttemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.overtextfakttemp.OTEXT|yes"
     _FldNameList[1]   > Temp-Tables.overtextfakttemp.OTEXT
"overtextfakttemp.OTEXT" "Övertidstext" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OVER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Övertidsregler */
DO:
   {muswait.i}
   musz = TRUE.     
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Övertidsregler */
DO:
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&Scoped-define SELF-NAME BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEF DIALOG-1
ON VALUE-CHANGED OF BRW_BEF IN FRAME DIALOG-1
DO:
   IF AVAILABLE befattningstemp THEN DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "overtextfakttemp.BEFATTNING = '" + befattningstemp.BEFATTNING + "'").
      RUN openbdynspec_UI IN  brwproc[2].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:  
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   ASSIGN
   CMB_DAGTYP = INPUT CMB_DAGTYP 
   FILL-IN_PRISA = INPUT FILL-IN_PRISA
   FILL-IN_START = INPUT FILL-IN_START
   FILL-IN_SLUT = INPUT FILL-IN_SLUT.      
   BRW_BEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   BRW_OVER:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   ASSIGN 
   extraopltemp.BEFATTNING = befattningstemp.BEFATTNING                  
   extraopltemp.OTEXTID = overtextfakttemp.OTEXTID
   extraopltemp.DAGTYP = CAPS(CMB_DAGTYP)
   extraopltemp.EQDAG = 2 
   extraopltemp.PRISA = FILL-IN_PRISA
   extraopltemp.START = FILL-IN_START
   extraopltemp.SLUT = FILL-IN_SLUT.   
   IF CMB_DAGTYP = "LÖRDAGAR" THEN ASSIGN extraopltemp.EQDAG = 7. 
   ELSE IF CMB_DAGTYP = "SÖNDAGAR" THEN ASSIGN extraopltemp.EQDAG = 8.
   RUN btnoknydef_UI IN defprisproch (INPUT TABLE extraopltemp).
   {BORTBRWPROC.I}
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_SLUT IN FRAME DIALOG-1 /* Slut tid */
DO:
   klocka = INPUT FILL-IN_SLUT.
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}                 
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN_SLUT = klocka.
   DISPLAY FILL-IN_SLUT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_START DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_START IN FRAME DIALOG-1 /* Start tid */
DO:
   klocka = INPUT FILL-IN_START.
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN_START = klocka.
   DISPLAY FILL-IN_START WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   FIND LAST extraopltemp NO-LOCK.
   IF AVAILABLE extraopltemp THEN DO:
      IF extraopltemp.EQDAG = 0 THEN DO:
         ASSIGN
         FILL-IN_PRISA = 0.00     
         FILL-IN_START = 00.00
         FILL-IN_SLUT = 7.00
         CMB_DAGTYP:SCREEN-VALUE = "VARDAGAR". 
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Nya övertidsregler".                 
      END.
      ELSE DO TRANSACTION:
         ASSIGN
         FILL-IN_PRISA = extraopltemp.PRISA     
         FILL-IN_START = extraopltemp.START
         FILL-IN_SLUT = extraopltemp.SLUT      
         CMB_DAGTYP:SCREEN-VALUE = extraopltemp.DAGTYP. 
         ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra på övertidsregler".       
      END.    
   END.
   CMB_DAGTYP = INPUT CMB_DAGTYP.         
   RUN enable_UI.       
   {FRMSIZED.I}    
   RUN openbdynspec_UI IN brwproc[1].
   
   IF AVAILABLE extraopltemp AND extraopltemp.EQDAG NE 0 THEN DO:
      FIND FIRST befattningstemp WHERE befattningstemp.BEFATTNING = extraopltemp.BEFATTNING NO-LOCK NO-ERROR.
      IF AVAILABLE befattningstemp THEN DO:
         RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(befattningstemp)).
         FIND FIRST befattningstemp WHERE befattningstemp.BEFATTNING = extraopltemp.BEFATTNING NO-LOCK NO-ERROR.
         RUN setcolsortvar_UI IN brwproc[2] (INPUT "overtextfakttemp.BEFATTNING = '" + befattningstemp.BEFATTNING + "'").
         RUN openbdynspec_UI IN brwproc[2].
         FIND FIRST overtextfakttemp WHERE overtextfakttemp.OTEXTID = extraopltemp.OTEXTID NO-LOCK NO-ERROR.
         IF AVAILABLE overtextfakttemp THEN RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(overtextfakttemp)).
      END.
   END.
   ELSE DO:
      FIND FIRST befattningstemp NO-LOCK NO-ERROR.
      IF AVAILABLE befattningstemp THEN DO:
         RUN setcolsortvar_UI IN brwproc[2] (INPUT "overtextfakttemp.BEFATTNING = '" + befattningstemp.BEFATTNING + "'").
         RUN openbdynspec_UI IN brwproc[2].
      END.
   END.
   {musarrow.i}  
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/  
   ASSIGN
   befattningstemp.NAMN:READ-ONLY IN BROWSE BRW_BEF = TRUE
   overtextfakttemp.OTEXT:READ-ONLY IN BROWSE BRW_OVER = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BEF:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_OVER:HANDLE IN FRAME {&FRAME-NAME}).     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY CMB_DAGTYP FILL-IN_PRISA FILL-IN_START FILL-IN_SLUT 
      WITH FRAME DIALOG-1.
  ENABLE BRW_BEF BRW_OVER CMB_DAGTYP FILL-IN_PRISA FILL-IN_START FILL-IN_SLUT 
         BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

