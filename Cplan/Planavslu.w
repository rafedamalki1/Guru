&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 - 12:41 pm

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
DEFINE INPUT PARAMETER plannrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
{PLANNRTEMP.I}
{SOKDEF.I}
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE SHARED TEMP-TABLE plantemp    
   FIELD PLANNR AS CHARACTER 
   FIELD ARTAL AS INTEGER 
   FIELD OMRADE AS CHARACTER
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD PLANNRTABREC AS RECID
   INDEX PLANNR IS PRIMARY PLANNR ARTAL ASCENDING.

{OMRTEMPW.I}  
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjdelvar AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE okvar AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valplantemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR valplantemp.OMRADE ~
valplantemp.PLANNR valplantemp.ARTAL valplantemp.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR 
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH valplantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH valplantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AONR valplantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR valplantemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_AVSAONR FBTN_VISAV BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_AVSAONR 
     LABEL "Avsluta Plannr":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISAV 
     LABEL "Visa":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      valplantemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR WINDOW-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      valplantemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      valplantemp.PLANNR COLUMN-LABEL "Plannr" FORMAT "X(6)":U
      valplantemp.ARTAL COLUMN-LABEL "Årtal" FORMAT "9999":U
      valplantemp.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 62.5 BY 11.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_AONR AT ROW 1.29 COL 1.5
     FBTN_AVSAONR AT ROW 2.71 COL 66.13
     FBTN_VISAV AT ROW 3.79 COL 66.13
     BTN_AVB AT ROW 13.17 COL 66.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.75 BY 13.46.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: valplantemp T "?" NO-UNDO temp-db valplantemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Avsluta plan"
         HEIGHT             = 13.54
         WIDTH              = 81.38
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_AONR 1 FRAME-A */
/* SETTINGS FOR BROWSE BRW_AONR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.valplantemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.valplantemp.OMRADE
"valplantemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valplantemp.PLANNR
"valplantemp.PLANNR" "Plannr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valplantemp.ARTAL
"valplantemp.ARTAL" "Årtal" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valplantemp.ORT
"valplantemp.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:

   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVSAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVSAONR WINDOW-1
ON CHOOSE OF FBTN_AVSAONR IN FRAME FRAME-A /* Avsluta Plannr */
DO:           
   RUN avsao_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISAV WINDOW-1
ON CHOOSE OF FBTN_VISAV IN FRAME FRAME-A /* Visa */
DO:
   antal_valda = BRW_AONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
   IF antal_valda = 0 THEN RETURN.
   {muswait.i}
   antal_raknare = 1.
   DO TRANSACTION:
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_AONR:FETCH-SELECTED-ROW(antal_raknare).       
         aonrrec = RECID(valplantemp).
         {SOKSTART.I}                              
         ASSIGN                                    
         soktemp.SOKVAL = 45                       
         soktemp.SOKCHAR[1] = valplantemp.PLANNR   
         soktemp.SOKINT[1] = valplantemp.ARTAL.    
         {SOKANROP.I}                              
         CREATE plantemp.                          
         ASSIGN                                    
         plantemp.PLANNR = valplantemp.PLANNR      
         plantemp.ARTAL = valplantemp.ARTAL        
         plantemp.OMRADE = valplantemp.OMRADE      
         plantemp.AONR = valplantemp.AONR          
         plantemp.DELNR = valplantemp.DELNR        
         plantemp.PLANNRTABREC = soktemp.SOKINT[2].        
         antal_raknare = antal_raknare + 1.
      END.
   END.        
   {AVBGOM.I}      
   RUN VISAPLAN.W (INPUT TABLE plantemp).
   {AVBFRAM.I}
   musz = FALSE.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}
   {muswait.i}   
   RUN enable_UI.   
     
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   
   FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
   valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   IF AVAILABLE valplantemp THEN DO:
      OPEN QUERY  BRW_AONR FOR EACH valplantemp NO-LOCK .
      FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
      valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.   
      RUN repo_UI (INPUT RECID(valplantemp)).
      status-ok = BRW_AONR:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
   ELSE DO:
      BRW_AONR:HIDDEN = TRUE.
      DISABLE FBTN_VISAV FBTN_AVSAONR WITH FRAME {&FRAME-NAME}.
   END.   
   
   ASSIGN
   valplantemp.OMRADE:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gomrk
   valplantemp.PLANNR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gplk
   FBTN_AVSAONR:LABEL = "Avsluta " + LC(Guru.Konstanter:gplk) 
   WINDOW-1:TITLE = "Avsluta " + LC(Guru.Konstanter:gpll).      
   {FRMSIZE.I} 
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aokoll_UI WINDOW-1 
PROCEDURE aokoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:
      RUN PLANNRAVAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vart,INPUT plannrvar,INPUT artalvar,
      INPUT-OUTPUT okvar).
   END.
   ELSE DO:
      RUN PLANNRAVAPP.P
      (INPUT vart,INPUT plannrvar,INPUT artalvar,
      INPUT-OUTPUT okvar).
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsao_UI WINDOW-1 
PROCEDURE avsao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   antal_valda = BRW_AONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
   antal_raknare = 1.
   regdatum = TODAY.   
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_AONR:FETCH-SELECTED-ROW(antal_raknare).
      ASSIGN
      plannrvar = valplantemp.PLANNR
      artalvar = valplantemp.ARTAL.
      aonrrec = RECID(valplantemp).
      /*Om plannummret är uppdelat på innevarande och nästa år*/
      IF valplantemp.UPP = TRUE AND valplantemp.UPPNR = TRUE THEN DO:
         MESSAGE Guru.Konstanter:gplk + ":" + valplantemp.PLANNR + " med Årtal:" +
         STRING(valplantemp.ARTAL,"9999") + " kan inte avslutas då det är årsuppdelat. Välj Plannr:" +
         valplantemp.PLANNR + " med Årtal:" + STRING(valplantemp.ARTAL + 1,"9999") +
         " om du vill göra avslut."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      ELSE DO:
         /*Om plannummret inte är uppdelat eller om det är nästa år*/
         okvar = FALSE.
         musz = FALSE.
         RUN aokoll_UI (INPUT 1).        
         IF okvar = FALSE THEN DO:
            RUN avslut_UI (INPUT 2).
            FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND
            valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
            ASSIGN valplantemp.PLANNRAVDATUM = regdatum.
         END.
         ELSE DO:
            MESSAGE Guru.Konstanter:gplk + ":" + valplantemp.PLANNR + " med Årtal:" +
            STRING(valplantemp.ARTAL,"9999") + " kan inte avslutas då det finns aonr kopplade till plannr:et som ej är avslutade."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
      END.
      antal_raknare = antal_raknare + 1.
   END.
   IF okvar = FALSE THEN DO:  
      aonrrec = RECID(valplantemp). 
      OPEN QUERY BRW_AONR FOR EACH valplantemp WHERE valplantemp.PLANNRAVDATUM = 01/01/1991 NO-LOCK .
      FIND FIRST valplantemp WHERE RECID(valplantemp) = aonrrec NO-LOCK NO-ERROR.
      FIND NEXT valplantemp WHERE valplantemp.PLANNRAVDATUM = 01/01/1991 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valplantemp THEN DO:
         FIND valplantemp WHERE RECID(valplantemp) = aonrrec NO-LOCK NO-ERROR.
         FIND PREV valplantemp WHERE valplantemp.PLANNRAVDATUM = 01/01/1991 NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE valplantemp THEN DO:
         aonrrec = RECID(valplantemp).     
         RUN repo_UI (INPUT aonrrec).
         status-ok = BRW_AONR:SELECT-FOCUSED-ROW() NO-ERROR.
      END.
   END.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avslut_UI WINDOW-1 
PROCEDURE avslut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:
      RUN PLANNRAVAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vart,INPUT plannrvar,INPUT artalvar,
      INPUT-OUTPUT okvar).
   END.
   ELSE DO:
      RUN PLANNRAVAPP.P
      (INPUT vart,INPUT plannrvar,INPUT artalvar,
      INPUT-OUTPUT okvar).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  ENABLE FBTN_AVSAONR FBTN_VISAV BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI WINDOW-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

