&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: KÖRS EJ

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/30/97 -  8:41 pm

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
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE treminuter AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE uttyp AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE allaspann AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

DEFINE SHARED TEMP-TABLE spann_temp2    
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID.

DEFINE TEMP-TABLE spar_temp
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 SEL_UPP BTN_KOR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON BTN_KOR 
     LABEL "Kör rapport" 
     SIZE 14 BY 1.5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 22.27
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 22.27
     BGCOLOR 8 .

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 60 BY 21
     BGCOLOR 7 FGCOLOR 15 FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     SEL_UPP AT ROW 1.5 COL 2.57 NO-LABEL
     BTN_KOR AT ROW 3 COL 65.72
     BTN_AVB AT ROW 18.65 COL 65.72
     RECT-2 AT ROW 1 COL 64
     RECT-3 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.88 BY 22.33
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Välj rapport som skall visas, skrivas ut"
         HEIGHT             = 22.35
         WIDTH              = 82.86
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 91.86
         VIRTUAL-HEIGHT     = 22.35
         VIRTUAL-WIDTH      = 91.86
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-C
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-3
ON CHOOSE OF BTN_AVB IN FRAME FRAME-C /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR WINDOW-3
ON CHOOSE OF BTN_KOR IN FRAME FRAME-C /* Kör rapport */
DO:
   SEL_UPP = INPUT SEL_UPP.   
   RUN vart_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-C
DO:
   SEL_UPP = INPUT SEL_UPP.     
   RUN vart_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-3
ON VALUE-CHANGED OF SEL_UPP IN FRAME FRAME-C
DO:
   SEL_UPP = INPUT SEL_UPP.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   RUN disable_UI.
END.

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
   ASSIGN
   RECT-2:BGCOLOR = ?
   RECT-3:BGCOLOR = ?  
   FRAME {&FRAME-NAME}:BGCOLOR = ? 
   SEL_UPP:FONT = Guru.Konstanter:varforetypval[20] 
   SEL_UPP:BGCOLOR = 15
   SEL_UPP:FGCOLOR = ?.

   {muswait.i}
   status-ok = SEL_UPP:ADD-LAST("Översikt över fördel. av planerade avbrott och drifts.") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("Kundkonsekvenser") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("Driftstörningar fördelade på felorsak") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("Driftstörningar fördelade på felorsak med SAIDI") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("Driftstörningars varaktighetsfördelning") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("Antal drabbade kunder per tidsintervall") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("Driftstörning fördelade på utlösande relä/felyttring") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("SAIFI - Medelavbrottsfrekvensen för 'alla kunder'") IN FRAME {&FRAME-NAME}.  
   status-ok = SEL_UPP:ADD-LAST("SAIDI - Medelavbrottstid för 'alla kunder'").
   status-ok = SEL_UPP:ADD-LAST("CAIDI - Medelavbrottstid för 'berörda kunder'").
   status-ok = SEL_UPP:ADD-LAST("ASAI - tillgängligheten i nätet").   
   status-ok = SEL_UPP:ADD-LAST("SAIFI - SAIDI - CAIDI - ASAI") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("SAIFI - Medelavbrottsfrekvensen för 'alla kunder' > 3 min") IN FRAME {&FRAME-NAME}.  
   status-ok = SEL_UPP:ADD-LAST("SAIDI - Medelavbrottstid för 'alla kunder' > 3 min").
   status-ok = SEL_UPP:ADD-LAST("CAIDI - Medelavbrottstid för 'berörda kunder' > 3 min").
   status-ok = SEL_UPP:ADD-LAST("ASAI - tillgängligheten i nätet > 3 min").   
   status-ok = SEL_UPP:ADD-LAST("SAIFI - SAIDI - CAIDI - ASAI > 3 min") IN FRAME {&FRAME-NAME}.
   status-ok = SEL_UPP:ADD-LAST("Lista återstående arbete").   
   status-ok = SEL_UPP:ADD-LAST("Detaljerad lista återstående arbete").
   status-ok = SEL_UPP:ADD-LAST("Störningar i EXCEL").
   /*
   status-ok = SEL_UPP:ADD-LAST("Störningar i EXCEL (snabb)").
   */
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "SEKG" THEN DO:         
      status-ok = SEL_UPP:ADD-LAST("Störningar i EXCEL (snabb)").
      status-ok = SEL_UPP:ADD-LAST("Summering av ledningsdata").
   END.
   FOR EACH spann_temp2:
      CREATE spar_temp.
      ASSIGN
      spar_temp.SPANID = spann_temp2.SPANID
      spar_temp.NAMN = spann_temp2.NAMN.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}    
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  DISPLAY SEL_UPP 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE RECT-2 RECT-3 SEL_UPP BTN_KOR BTN_AVB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vart_UI WINDOW-3 
PROCEDURE vart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   {AVBGOM.I}
   treminuter = FALSE.
   IF SEL_UPP = "SAIFI - Medelavbrottsfrekvensen för 'alla kunder'" THEN DO:
      vallista = 1.
      RUN SAIFI.W.                          
   END.        
   ELSE IF SEL_UPP = "SAIDI - Medelavbrottstid för 'alla kunder'" THEN DO:
      vallista = 2.
      RUN SAIFI.W.
   END.   
   ELSE IF SEL_UPP = "CAIDI - Medelavbrottstid för 'berörda kunder'" THEN DO:       
      vallista = 3.
      RUN SAIFI.W.
   END.
   ELSE IF SEL_UPP = "ASAI - tillgängligheten i nätet" THEN DO:       
      vallista = 4.
      RUN SAIFI.W.
   END.   
   IF SEL_UPP = "SAIFI - Medelavbrottsfrekvensen för 'alla kunder' > 3 min" THEN DO:
      ASSIGN
      treminuter = TRUE
      vallista = 5.
      RUN SAIFI.W.                          
   END.        
   ELSE IF SEL_UPP = "SAIDI - Medelavbrottstid för 'alla kunder' > 3 min" THEN DO:
      ASSIGN
      treminuter = TRUE
      vallista = 6.
      RUN SAIFI.W.
   END.   
   ELSE IF SEL_UPP = "CAIDI - Medelavbrottstid för 'berörda kunder' > 3 min" THEN DO:       
      ASSIGN
      treminuter = TRUE
      vallista = 7.
      RUN SAIFI.W.
   END.
   ELSE IF SEL_UPP = "ASAI - tillgängligheten i nätet > 3 min" THEN DO:       
      ASSIGN
      treminuter = TRUE
      vallista = 8.
      RUN SAIFI.W.
   END.   
   ELSE IF SEL_UPP = "Översikt över fördel. av planerade avbrott och drifts." THEN DO:
      RUN OVERSIKT.W.
   END.
   ELSE IF SEL_UPP = "Kundkonsekvenser" THEN DO:
      RUN KUNDKONS.W.
   END.
   ELSE IF SEL_UPP = "Driftstörningar fördelade på felorsak" THEN DO:
      IF uttyp = 2 THEN DO:
         MESSAGE "Obs! Ni har valt att visa både planerade avbrott och driftstörningar. Vid denna lista berörs ej planerade avbrott även om Ni så valt."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      ELSE IF UTTYP = 3 THEN DO:
         MESSAGE "Obs! Ni har valt att visa planerade avbrott. Vid denna lista berörs ej planerade avbrott även om Ni så valt. Utfallet kommer att visa driftstörningar."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.      
      RUN STORFEL.W.
   END.
   ELSE IF SEL_UPP = "Driftstörningar fördelade på felorsak med SAIDI" THEN DO:
      IF uttyp = 2 THEN DO:
         MESSAGE "Obs! Ni har valt att visa både planerade avbrott och driftstörningar. Vid denna lista berörs ej planerade avbrott även om Ni så valt."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      ELSE IF UTTYP = 3 THEN DO:
         MESSAGE "Obs! Ni har valt att visa planerade avbrott. Vid denna lista berörs ej planerade avbrott även om Ni så valt. Utfallet kommer att visa driftstörningar."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.      
      RUN STORFEL.W.
   END.
   ELSE IF SEL_UPP = "Driftstörningars varaktighetsfördelning" THEN DO:
      RUN STORVARA.W.
   END.
   ELSE IF SEL_UPP = "Antal drabbade kunder per tidsintervall" THEN DO:
      RUN STORVARAK.W.
   END.
   
   ELSE IF SEL_UPP = "Driftstörning fördelade på utlösande relä/felyttring" THEN DO:
      IF uttyp = 2 THEN DO:
         MESSAGE "Obs! Ni har valt att visa både planerade avbrott och driftstörningar. Vid denna lista berörs ej planerade avbrott även om Ni så valt."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      ELSE IF UTTYP = 3 THEN DO:
         MESSAGE "Obs! Ni har valt att visa planerade avbrott. Vid denna lista berörs ej planerade avbrott även om Ni så valt. Utfallet kommer att visa driftstörningar."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      IF allaspann = FALSE THEN DO:
         MESSAGE "Obs! Listan kommer att visa alla spänningsnivåer."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      RUN RELAFEL.W.
      EMPTY TEMP-TABLE spann_temp2 NO-ERROR.       
      FOR EACH spar_temp:
         CREATE spann_temp2.
         ASSIGN
         spann_temp2.SPANID = spar_temp.SPANID
         spann_temp2.NAMN = spar_temp.NAMN.
      END.
   END.   
   ELSE IF SEL_UPP = "SAIFI - SAIDI - CAIDI - ASAI" THEN DO:
      RUN TOTSAIFI.W.
   END.
   ELSE IF SEL_UPP = "SAIFI - SAIDI - CAIDI - ASAI > 3 min" THEN DO:
      ASSIGN
      treminuter = TRUE.
      RUN TOTSAIFI.W.
   END.
   ELSE IF SEL_UPP = "Lista återstående arbete" THEN DO:
      vallista = 1.
      RUN ATERARB.W.
   END.
   ELSE IF SEL_UPP = "Detaljerad lista återstående arbete" THEN DO:
      vallista = 2.
      RUN ATERARB.W.
   END.
   ELSE IF SEL_UPP = "Störningar i EXCEL" THEN DO:
      RUN STORNINGEX.P (INPUT 1).
   END.
   
   ELSE IF SEL_UPP = "Störningar i EXCEL (snabb)" THEN DO:
      RUN STORNINGEX.P (INPUT 2).
   END.
   
   ELSE IF SEL_UPP = "Summering av ledningsdata" THEN DO:
      RUN SUMLED.W.
   END.
   {AVBFRAM.I}
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

