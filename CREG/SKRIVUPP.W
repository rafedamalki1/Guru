&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
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
{ALLDEF.I}
&Scoped-define NEW  
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE skrivproch AS HANDLE NO-UNDO. /* SKRIVUPPAPP.P */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE rb-print AS CHARACTER NO-UNDO.
DEFINE VARIABLE liggande AS LOGICAL NO-UNDO.
DEFINE VARIABLE utskriv AS LOGICAL NO-UNDO.
{TIDUTTTSHARED.I}
DEFINE VARIABLE iPrintDlg  AS INTEGER     NO-UNDO.
 
DEFINE VARIABLE lpPrintDlg AS MEMPTR      NO-UNDO.
&Scoped-define SHARED
{SKRIVARDEFTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_PRE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES skrivardeftemp

/* Definitions for BROWSE BRW_PRE                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_PRE skrivardeftemp.SKRIVARID ~
skrivardeftemp.SIDA skrivardeftemp.BREDD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRE skrivardeftemp.SIDA ~
skrivardeftemp.BREDD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PRE skrivardeftemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PRE skrivardeftemp
&Scoped-define QUERY-STRING-BRW_PRE FOR EACH skrivardeftemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_PRE OPEN QUERY BRW_PRE FOR EACH skrivardeftemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_PRE skrivardeftemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRE skrivardeftemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_PRE FBTN_SKRTEST BTN_NY BTN_BORT BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKRTEST 
     LABEL "Skrivar test" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PRE FOR 
      skrivardeftemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRE C-Win _STRUCTURED
  QUERY BRW_PRE NO-LOCK DISPLAY
      skrivardeftemp.SKRIVARID COLUMN-LABEL "Skrivare" FORMAT "x(30)":U
      skrivardeftemp.SIDA COLUMN-LABEL "Sidlängd!stående" FORMAT ">>9":U
      skrivardeftemp.BREDD COLUMN-LABEL "Sidlängd!liggande" FORMAT ">>9":U
  ENABLE
      skrivardeftemp.SIDA
      skrivardeftemp.BREDD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 52 BY 25.75
         TITLE "Skrivare".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_PRE AT ROW 1.5 COL 1.5
     FBTN_SKRTEST AT ROW 4 COL 55.5
     BTN_NY AT ROW 27.96 COL 9.88
     BTN_BORT AT ROW 27.96 COL 26.88
     BTN_AVB AT ROW 27.96 COL 55.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 69.38 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: skrivardeftemp T "?" NO-UNDO temp-db skrivardeftemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Ändra skrivare"
         HEIGHT             = 28.42
         WIDTH              = 69.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 69.63
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 69.63
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_PRE 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRE
/* Query rebuild information for BROWSE BRW_PRE
     _TblList          = "Temp-Tables.skrivardeftemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.skrivardeftemp.SKRIVARID
"skrivardeftemp.SKRIVARID" "Skrivare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.skrivardeftemp.SIDA
"skrivardeftemp.SIDA" "Sidlängd!stående" ">>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.skrivardeftemp.BREDD
"skrivardeftemp.BREDD" "Sidlängd!liggande" ">>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PRE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ändra skrivare */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ändra skrivare */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PRE
&Scoped-define SELF-NAME BRW_PRE
&Scoped-define SELF-NAME skrivardeftemp.SIDA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL skrivardeftemp.SIDA BRW_PRE _BROWSE-COLUMN C-Win
ON ENTRY OF skrivardeftemp.SIDA IN BROWSE BRW_PRE /* Sidlängd!stående */
DO:
   IF AVAILABLE skrivardeftemp THEN DO:
      DISPLAY skrivardeftemp.SIDA WITH BROWSE BRW_PRE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL skrivardeftemp.SIDA BRW_PRE _BROWSE-COLUMN C-Win
ON LEAVE OF skrivardeftemp.SIDA IN BROWSE BRW_PRE /* Sidlängd!stående */
DO:
   IF AVAILABLE skrivardeftemp THEN DO:
      IF skrivardeftemp.SIDA = INPUT BROWSE BRW_PRE skrivardeftemp.SIDA THEN musz = musz.
      ELSE DO:
         skrivardeftemp.SIDA = INPUT BROWSE BRW_PRE skrivardeftemp.SIDA.   
         IF skrivardeftemp.SIDA <= 0 THEN DO:
            MESSAGE "Sidlängden måste vara större än 0" VIEW-AS ALERT-BOX.      
            RETURN NO-APPLY.
         END.               
         DISPLAY skrivardeftemp.SIDA WITH BROWSE BRW_PRE.
         RUN andraskriv_UI IN skrivproch (INPUT skrivardeftemp.SKRIVARID,INPUT skrivardeftemp.SIDA,INPUT skrivardeftemp.BREDD).
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME skrivardeftemp.BREDD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL skrivardeftemp.BREDD BRW_PRE _BROWSE-COLUMN C-Win
ON ENTRY OF skrivardeftemp.BREDD IN BROWSE BRW_PRE /* Sidlängd!liggande */
DO:
   IF AVAILABLE skrivardeftemp THEN DO:
      DISPLAY skrivardeftemp.BREDD WITH BROWSE BRW_PRE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL skrivardeftemp.BREDD BRW_PRE _BROWSE-COLUMN C-Win
ON LEAVE OF skrivardeftemp.BREDD IN BROWSE BRW_PRE /* Sidlängd!liggande */
DO:
   IF AVAILABLE skrivardeftemp THEN DO:
      IF skrivardeftemp.BREDD = INPUT BROWSE BRW_PRE skrivardeftemp.BREDD THEN musz = musz.
      ELSE DO:
         skrivardeftemp.BREDD = INPUT BROWSE BRW_PRE skrivardeftemp.BREDD.   
         IF skrivardeftemp.BREDD <= 0 THEN DO:
            MESSAGE "Sidlängden måste vara större än 0" VIEW-AS ALERT-BOX.      
            RETURN NO-APPLY.
         END.               
         DISPLAY skrivardeftemp.BREDD WITH BROWSE BRW_PRE.
         RUN andraskriv_UI IN skrivproch (INPUT skrivardeftemp.SKRIVARID,INPUT skrivardeftemp.SIDA,INPUT skrivardeftemp.BREDD).
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE skrivardeftemp THEN DO:                 
      MESSAGE "Vill du ta bort skrivaren: " SKIP skrivardeftemp.SKRIVARID VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Ta bort skrivare" UPDATE svarb AS LOGICAL.
      IF svarb = TRUE THEN DO:
         RUN tabortskriv_UI IN skrivproch (INPUT skrivardeftemp.SKRIVARID).
         DELETE skrivardeftemp.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].            
      END.
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   {muswait.i}
   {SKRIVLS.I}
   
   IF utskriv = TRUE THEN DO:
      rb-print = SESSION:PRINTER-NAME.
      FIND FIRST skrivardeftemp WHERE skrivardeftemp.SKRIVARID = rb-print NO-LOCK NO-ERROR.
      IF NOT AVAILABLE skrivardeftemp THEN DO:
         RUN skapaskriv_UI IN skrivproch (INPUT rb-print).
         CREATE skrivardeftemp.
         ASSIGN
         skrivardeftemp.SIDA = 72
         skrivardeftemp.BREDD = 53
         skrivardeftemp.SKRIVARID = rb-print.
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST skrivardeftemp WHERE skrivardeftemp.SKRIVARID = rb-print NO-LOCK NO-ERROR.         
      END.
      RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(skrivardeftemp)).                                 
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRTEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRTEST C-Win
ON CHOOSE OF FBTN_SKRTEST IN FRAME DEFAULT-FRAME /* Skrivar test */
DO:   
   MESSAGE "Vill du testa stående utskrift svara Ja" SKIP
           "Vill du testa liggande utskrift svara Nej" 
            VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Utskrift test?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      MESSAGE "Glöm ej att välja stående utskrift,  under Egenskaper!" VIEW-AS ALERT-BOX.
      RUN SKRIVVAL.W (INPUT FALSE).  
      IF musz = TRUE THEN DO:
         musz = FALSE.         
      END.                       
      ELSE DO:              
         RUN utskap_UI.      
         FIND LAST tidut NO-LOCK NO-ERROR.     
         RUN EKLOGS.P.
      END.
      EMPTY TEMP-TABLE tidut NO-ERROR.       
   END.
   ELSE IF NOT svar THEN DO:       
      MESSAGE "Glöm ej att välja liggande utskrift, under Egenskaper! " VIEW-AS ALERT-BOX.
      RUN SKRIVVAL.W (INPUT TRUE).   
      IF musz = TRUE THEN DO:
         musz = FALSE.         
      END.                       
      ELSE DO:              
         RUN utskap_UI.      
         FIND LAST tidut NO-LOCK NO-ERROR.     
         RUN EKLOGL.P.
      END.
      EMPTY TEMP-TABLE tidut NO-ERROR.       
   END.                    
   ELSE DO:
      musz = musz.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRTEST C-Win
ON MOUSE-MENU-CLICK OF FBTN_SKRTEST IN FRAME DEFAULT-FRAME /* Skrivar test */
DO:
/*    RUN SIDLANGD.W. */
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
/*    RUN sparaavtal_UI IN avtregproch (INPUT ?, INPUT TABLE avtaltemp). */
   IF VALID-HANDLE(skrivproch) THEN DELETE PROCEDURE skrivproch. 
   {BORTBRWPROC.I}
   RUN disable_UI.
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
   {ALLSTARTDYN.I}
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN openbdynspec_UI IN brwproc[1].
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_PRE:HANDLE IN FRAME {&FRAME-NAME}).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKRIVUPPAPP.P PERSISTENT SET skrivproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN SKRIVUPPAPP.P PERSISTENT SET skrivproch.         
   END.   
   RUN laddaskriv_UI IN skrivproch (OUTPUT TABLE skrivardeftemp).
/*    RUN laddaavt_UI IN avtregproch (OUTPUT TABLE avtaltemp). */
/*    RUN setcolindex_UI IN brwproc[1] (INPUT " ORDNING ").    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  ENABLE BRW_PRE FBTN_SKRTEST BTN_NY BTN_BORT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utskap_UI C-Win 
PROCEDURE utskap_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   REPEAT i = 1 TO 145:
      CREATE tidut.
      ASSIGN tidut.UT = "Detta är rad nr " + STRING(i).
      IF i = 1 THEN tidut.UT = tidut.UT + " Inställningar liggande: " + STRING(Guru.GlobalaVariabler:globsidl) + " stående " + STRING(Guru.GlobalaVariabler:globsids).
      IF i = 2 THEN tidut.UT = tidut.UT + " OBS! Du skall få 3 rader mindre på varje sida än vad du har angivit!".                                           
      IF i = 3 THEN tidut.UT = "Antal kolumner     2         3         4         5         6         7         8         9".                                           
      IF i = 4 THEN tidut.UT = "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890".
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

