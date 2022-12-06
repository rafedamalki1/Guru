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
DEFINE VARIABLE startmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE widgethandle AS HANDLE NO-UNDO.
run wSplash.w PERSISTENT (INPUT "Välkommen till GURU",
                          INPUT "Av: Ander Olsson, Lena Olsson, Niklas Granholm,",
                          INPUT "Ove Wanhainen, Harriet Berggren,",
                          INPUT "Germund Gustavsson, Mikael Eriksson.",
                          INPUT "Copyright © Elpool i Umeå AB 1988 - 2006. Med ensamrätt.",
                          INPUT 3).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-16 RECT-17 IMAGE-8 ED_GURU SEL_UPP ~
ED_START BUTTON-16 ED_WWW BTN_MEDD BTN_BYT BTN_BYTW BTN_UPPDAT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS ED_GURU SEL_UPP ED_START ED_WWW ~
FILL-IN-GURU 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     IMAGE-UP FILE "BILDER\btn_avs.gif":U
     LABEL "Avsluta":L 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_BYT 
     IMAGE-UP FILE "BILDER\btn_byt.gif":U
     LABEL "Byt användare" 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_BYTW 
     IMAGE-UP FILE "BILDER\btn_bytw.gif":U
     LABEL "Byt fönsterstorlek" 
     SIZE 19 BY 1.5 TOOLTIP "Anpassa Guru till din skärmupplösning.".

DEFINE BUTTON BTN_MEDD 
     IMAGE-UP FILE "BILDER\btn_medd.gif":U
     LABEL "Skapa meddelande till Guruanvändare" 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_UPPDAT 
     IMAGE-UP FILE "BILDER\btn_uppdat.gif":U
     LABEL "Uppdatera program" 
     SIZE 19 BY 1.5 TOOLTIP "Uppdatera din Guru applikation med den senaste versionen.".

DEFINE BUTTON BUTTON-16 
     IMAGE-UP FILE "BILDER\btn_over.gif":U
     LABEL "Button 16" 
     SIZE 6 BY 1.5.

DEFINE VARIABLE ED_GURU AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 28.63 BY 3.25
     FONT 17 NO-UNDO.

DEFINE VARIABLE ED_START AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 53.5 BY 10.5
     BGCOLOR 15 FGCOLOR 1 FONT 17 NO-UNDO.

DEFINE VARIABLE ED_WWW AS CHARACTER 
     VIEW-AS EDITOR NO-BOX
     SIZE 51.5 BY 1.42 NO-UNDO.

DEFINE VARIABLE FILL-IN-GURU AS CHARACTER FORMAT "X(256)":U INITIAL "Välkommen till" 
      VIEW-AS TEXT 
     SIZE 69 BY 1
     FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-8
     FILENAME "BILDER\elpool_gra.gif":U
     SIZE 23.5 BY 4.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 96 BY .5
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 96 BY .5
     BGCOLOR 1 .

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     LIST-ITEMS "Projektnummer","Beredning","Materielhantering","Kalkylering","Tidredovisning","Flextid","Uppföljning","Personaladministration","Sekretess","Register","Faktureringsrutin","Plannummer","Markvärdering","Avbrott/Störning","SMS-administration" 
     SIZE 31.63 BY 16.75
     BGCOLOR 8 FGCOLOR 1 FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ED_GURU AT ROW 3.75 COL 27.38 NO-LABEL
     SEL_UPP AT ROW 4.5 COL 58.25 NO-LABEL
     ED_START AT ROW 8 COL 2.5 NO-LABEL
     BUTTON-16 AT ROW 12 COL 90.5
     ED_WWW AT ROW 19.83 COL 2.5 NO-LABEL
     BTN_MEDD AT ROW 22.33 COL 1.5
     BTN_BYT AT ROW 22.33 COL 20.5
     BTN_BYTW AT ROW 22.33 COL 39.5
     BTN_UPPDAT AT ROW 22.33 COL 58.5
     BTN_AVB AT ROW 22.33 COL 77.5
     FILL-IN-GURU AT ROW 1.21 COL 2.5 NO-LABEL
     "  Funktioner:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 3.33 COL 57.38
          FGCOLOR 1 FONT 17
     RECT-16 AT ROW 2.25 COL 1
     RECT-17 AT ROW 21.75 COL 1
     IMAGE-8 AT ROW 2.96 COL 1.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.13 BY 23.17
         BGCOLOR 8 .


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
         HEIGHT             = 23.13
         WIDTH              = 96.25
         MAX-HEIGHT         = 27.92
         MAX-WIDTH          = 118.5
         VIRTUAL-HEIGHT     = 27.92
         VIRTUAL-WIDTH      = 118.5
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
ASSIGN 
       BTN_UPPDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       ED_GURU:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ED_START:AUTO-INDENT IN FRAME DEFAULT-FRAME      = TRUE
       ED_START:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       ED_WWW:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-GURU IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
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


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON MOUSE-SELECT-CLICK OF FRAME DEFAULT-FRAME
DO:
   ASSIGN
   ED_START:FGCOLOR = 1
   ED_START:FONT = 17.
   ASSIGN ED_START = startmedd.
   DISPLAY ED_START WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:       
   RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON ENDKEY OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:  
   RUN avb_UI.
   RETURN.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ED_GURU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ED_GURU C-Win
ON MOUSE-SELECT-CLICK OF ED_GURU IN FRAME DEFAULT-FRAME
DO:
  DISPLAY ED_GURU WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ED_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ED_START C-Win
ON MOUSE-SELECT-CLICK OF ED_START IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   ED_START:FGCOLOR = 1
   ED_START:FONT = 17.
   ASSIGN ED_START = startmedd.
   DISPLAY ED_START WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ED_WWW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ED_WWW C-Win
ON MOUSE-SELECT-CLICK OF ED_WWW IN FRAME DEFAULT-FRAME
DO:
   ED_WWW:FGCOLOR = 12.
   RUN OPENDOC.P ("http://www.elpool.se","","",NO).
   APPLY "ENTRY" TO SEL_UPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP C-Win
ON MOUSE-MENU-CLICK OF SEL_UPP IN FRAME DEFAULT-FRAME
DO:
   /*
   IF SEL_UPP = "Kalkylering" THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA"   OR 
      Guru.Konstanter:globforetag = "GRIT"  OR
      Guru.Konstanter:globforetag = "GADM" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:
         {AVBGOM.I}
         RUN EARES.W.
         {AVBFRAM.I}
      END.
   END.   
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP C-Win
ON MOUSE-SELECT-CLICK OF SEL_UPP IN FRAME DEFAULT-FRAME
DO:
   SEL_UPP = INPUT SEL_UPP.
   DISPLAY SEL_UPP WITH FRAME {&FRAME-NAME}.
   IF SEL_UPP = "Projektnummer" THEN DO:
      ASSIGN
      ED_START:FGCOLOR = 1
      ED_START:FONT = 6.
      ASSIGN ED_START = "     Projektnumret används för att koppla samman alla moduler i systemet.
 Koppling till konto och fördelning på olika konton kan göras. 
Dokument, avtal, bilder etc. som berör projektet kopplas eller kopieras in till projektet för
att alltid ge en sammanhållen dokumentflora."
+ CHR(10) + CHR(10) + "     " +
"Redovisning av tid sker mot projektnummer och koppling till ekonomi med automatisk kontofördelning blir möjlig. 
Utväxling av data kan normalt skapas på projektnummer som tvåvägskommunikation mellan GURU och ekonomisystemet.
Projektnummer serier kan skapas med utgångspunkt i lokala krav och underindelning 
av projekt kan göras.".     
   END.
   ELSE IF SEL_UPP = "Beredning" THEN DO:
      ASSIGN
      ED_START:FGCOLOR = 1
      ED_START:FONT = 6.
      ASSIGN ED_START = "     Beredning".     
   END.
   ELSE DO:
      ASSIGN
      ED_START:FGCOLOR = 1
      ED_START:FONT = 17.
      ASSIGN ED_START = startmedd.      
   END.
   DISPLAY ED_START WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP C-Win
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME DEFAULT-FRAME
DO:
   SEL_UPP = INPUT SEL_UPP.     
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP C-Win
ON VALUE-CHANGED OF SEL_UPP IN FRAME DEFAULT-FRAME
DO:
   SEL_UPP = INPUT SEL_UPP.    
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
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(widgethandle) THEN DELETE PROCEDURE widgethandle NO-ERROR.
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   DEFINE VARIABLE varname AS CHARACTER NO-UNDO.
/*    { gradient.i } */
   ED_WWW:LOAD-MOUSE-POINTER("GLOVE":U).
   ASSIGN
   ED_WWW:FGCOLOR = 1
   ED_WWW:FONT = 6.
   ASSIGN ED_GURU = "GURU - " + CHR(10) + "GrUndregistrering Redovisning & Uppföljning".
   /*systemet som anpassas efter kundens behov och önskemål.*/
   ASSIGN startmedd = "Nytt meddelande från Elpool!" + CHR(10) +
                     "Nu finns det nya uppdateringar... Gå på ""Uppdatera program"" för att uppdatera din klient.".
   ASSIGN ED_START = startmedd.
   ASSIGN ED_WWW = "Besök oss på www.elpool.se. Där kan ni läsa om våra nyheter och senaste uppdateringar.".
   ASSIGN SEL_UPP = "Projektnummer".
   widgethandle = ED_START:HANDLE IN FRAME {&FRAME-NAME}.
   ED_GURU:MOVE-AFTER-TAB-ITEM(widgethandle).
   RUN enable_UI.
   
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
  DISPLAY ED_GURU SEL_UPP ED_START ED_WWW FILL-IN-GURU 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-16 RECT-17 IMAGE-8 ED_GURU SEL_UPP ED_START BUTTON-16 ED_WWW 
         BTN_MEDD BTN_BYT BTN_BYTW BTN_UPPDAT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

