&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE datumenh AS HANDLE NO-UNDO EXTENT 37.
DEFINE VARIABLE ye AS INTEGER FORMAT "9999" NO-UNDO.
DEFINE VARIABLE man AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE dagraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE maxdagnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE regdatum2 AS DATE NO-UNDO.
DEFINE VARIABLE indate AS DATE NO-UNDO.
DEFINE VARIABLE mregvnr AS INTEGER NO-UNDO.
DEFINE VARIABLE forstaregdatum AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_AR CMB_MANAD btn_over-7 btn_back-7 ~
FILL-IN-V-1 BTN-1 BTN-2 BTN-3 BTN-4 BTN-5 BTN-6 BTN-7 FILL-IN-V-2 BTN-8 ~
BTN-9 BTN-10 BTN-11 BTN-12 BTN-13 BTN-14 FILL-IN-V-3 BTN-15 BTN-16 BTN-17 ~
BTN-18 BTN-19 BTN-20 BTN-21 FILL-IN-V-4 BTN-22 BTN-23 BTN-24 BTN-25 BTN-26 ~
BTN-27 BTN-28 FILL-IN-V-5 BTN-29 BTN-30 BTN-31 BTN-32 BTN-33 BTN-34 BTN-35 ~
BTN-36 BTN-37 FILL-IN-V-6 Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS CMB_AR CMB_MANAD FILL-IN-V-1 FILL-IN-V-2 ~
FILL-IN-V-3 FILL-IN-V-4 FILL-IN-V-5 FILL-IN-V-6 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-1 
     LABEL "Button 1" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-10 
     LABEL "Btn 2" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-11 
     LABEL "Btn 3" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-12 
     LABEL "Btn 4" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-13 
     LABEL "Btn 5" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-14 
     LABEL "Btn 6" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-15 
     LABEL "Btn 7" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-16 
     LABEL "Btn 9" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-17 
     LABEL "Btn 2" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-18 
     LABEL "Btn 3" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-19 
     LABEL "Btn 4" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-2 
     LABEL "Btn 2" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-20 
     LABEL "Btn 5" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-21 
     LABEL "Btn 6" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-22 
     LABEL "Btn 7" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-23 
     LABEL "Btn 9" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-24 
     LABEL "Btn 2" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-25 
     LABEL "Btn 3" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-26 
     LABEL "Btn 4" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-27 
     LABEL "Btn 5" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-28 
     LABEL "Btn 6" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-29 
     LABEL "Btn 7" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-3 
     LABEL "Btn 3" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-30 
     LABEL "Btn 9" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-31 
     LABEL "Btn 2" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-32 
     LABEL "Btn 3" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-33 
     LABEL "Btn 4" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-34 
     LABEL "Btn 5" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-35 
     LABEL "Btn 6" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-36 
     LABEL "Btn 7" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-37 
     LABEL "Btn 9" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-4 
     LABEL "Btn 4" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-5 
     LABEL "Btn 5" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-6 
     LABEL "Btn 6" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-7 
     LABEL "Btn 7" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-8 
     LABEL "Btn 8" 
     SIZE 3 BY 1.13.

DEFINE BUTTON BTN-9 
     LABEL "Btn 9" 
     SIZE 3 BY 1.13.

DEFINE BUTTON btn_back-7 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON btn_over-7 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE VARIABLE CMB_AR AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.5 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_MANAD AS CHARACTER FORMAT "X(9)":U INITIAL "januari" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december" 
     DROP-DOWN-LIST
     SIZE 12.5 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-V-1 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-V-2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-V-3 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-V-4 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-V-5 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-V-6 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     CMB_AR AT ROW 1.25 COL 3.5 NO-LABEL WIDGET-ID 10
     CMB_MANAD AT ROW 1.25 COL 15.5 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     btn_over-7 AT ROW 2.25 COL 36 WIDGET-ID 8
     btn_back-7 AT ROW 2.33 COL 3.5 WIDGET-ID 6
     FILL-IN-V-1 AT ROW 5 COL 5.5 NO-LABEL WIDGET-ID 88
     BTN-1 AT ROW 5 COL 10 WIDGET-ID 102
     BTN-2 AT ROW 5 COL 14 WIDGET-ID 104
     BTN-3 AT ROW 5 COL 18 WIDGET-ID 106
     BTN-4 AT ROW 5 COL 22 WIDGET-ID 108
     BTN-5 AT ROW 5 COL 26 WIDGET-ID 110
     BTN-6 AT ROW 5 COL 30 WIDGET-ID 112
     BTN-7 AT ROW 5 COL 34.25 WIDGET-ID 114
     FILL-IN-V-2 AT ROW 6.63 COL 5.5 NO-LABEL WIDGET-ID 90
     BTN-8 AT ROW 6.63 COL 10 WIDGET-ID 188
     BTN-9 AT ROW 6.63 COL 14 WIDGET-ID 118
     BTN-10 AT ROW 6.63 COL 18 WIDGET-ID 120
     BTN-11 AT ROW 6.63 COL 22 WIDGET-ID 122
     BTN-12 AT ROW 6.63 COL 26 WIDGET-ID 124
     BTN-13 AT ROW 6.63 COL 30 WIDGET-ID 126
     BTN-14 AT ROW 6.63 COL 34.25 WIDGET-ID 128
     FILL-IN-V-3 AT ROW 8.25 COL 5.5 NO-LABEL WIDGET-ID 92
     BTN-15 AT ROW 8.25 COL 10 WIDGET-ID 130
     BTN-16 AT ROW 8.25 COL 14 WIDGET-ID 144
     BTN-17 AT ROW 8.25 COL 18 WIDGET-ID 132
     BTN-18 AT ROW 8.25 COL 22 WIDGET-ID 134
     BTN-19 AT ROW 8.25 COL 26 WIDGET-ID 136
     BTN-20 AT ROW 8.25 COL 30 WIDGET-ID 138
     BTN-21 AT ROW 8.25 COL 34.25 WIDGET-ID 140
     FILL-IN-V-4 AT ROW 9.88 COL 5.5 NO-LABEL WIDGET-ID 94
     BTN-22 AT ROW 9.88 COL 10 WIDGET-ID 142
     BTN-23 AT ROW 9.88 COL 14 WIDGET-ID 158
     BTN-24 AT ROW 9.88 COL 18 WIDGET-ID 146
     BTN-25 AT ROW 9.88 COL 22 WIDGET-ID 148
     BTN-26 AT ROW 9.88 COL 26 WIDGET-ID 150
     BTN-27 AT ROW 9.88 COL 30 WIDGET-ID 152
     BTN-28 AT ROW 9.88 COL 34.25 WIDGET-ID 154
     FILL-IN-V-5 AT ROW 11.5 COL 5.5 NO-LABEL WIDGET-ID 96
     BTN-29 AT ROW 11.5 COL 10 WIDGET-ID 156
     BTN-30 AT ROW 11.5 COL 14 WIDGET-ID 172
     BTN-31 AT ROW 11.5 COL 18 WIDGET-ID 160
     BTN-32 AT ROW 11.5 COL 22 WIDGET-ID 162
     BTN-33 AT ROW 11.5 COL 26 WIDGET-ID 164
     BTN-34 AT ROW 11.5 COL 30 WIDGET-ID 166
     BTN-35 AT ROW 11.5 COL 34.25 WIDGET-ID 168
     BTN-36 AT ROW 13 COL 10 WIDGET-ID 170
     BTN-37 AT ROW 13 COL 14 WIDGET-ID 186
     FILL-IN-V-6 AT ROW 13.13 COL 5.5 NO-LABEL WIDGET-ID 98
     Btn_OK AT ROW 15.25 COL 21.5
     Btn_Cancel AT ROW 15.25 COL 21.5
     "Vnr   M   T   O   T   F   L   S" VIEW-AS TEXT
          SIZE 35.5 BY 1 AT ROW 3.75 COL 5 WIDGET-ID 100
          BGCOLOR 15 FGCOLOR 0 FONT 24
     SPACE(1.99) SKIP(13.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Almanacka"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AR IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-V-1 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-V-2 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-V-3 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-V-4 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-V-5 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-V-6 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Almanacka */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-1 Dialog-Frame
ON CHOOSE OF BTN-1 IN FRAME Dialog-Frame /* Button 1 */
DO:
   RUN Klar_UI (datumenh[1]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-10 Dialog-Frame
ON CHOOSE OF BTN-10 IN FRAME Dialog-Frame /* Btn 2 */
DO:
   RUN Klar_UI (datumenh[10]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-11 Dialog-Frame
ON CHOOSE OF BTN-11 IN FRAME Dialog-Frame /* Btn 3 */
DO:
   RUN Klar_UI (datumenh[11]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-12 Dialog-Frame
ON CHOOSE OF BTN-12 IN FRAME Dialog-Frame /* Btn 4 */
DO:
   RUN Klar_UI (datumenh[12]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-13 Dialog-Frame
ON CHOOSE OF BTN-13 IN FRAME Dialog-Frame /* Btn 5 */
DO:
   RUN Klar_UI (datumenh[13]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-14 Dialog-Frame
ON CHOOSE OF BTN-14 IN FRAME Dialog-Frame /* Btn 6 */
DO:
   RUN Klar_UI (datumenh[14]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-15 Dialog-Frame
ON CHOOSE OF BTN-15 IN FRAME Dialog-Frame /* Btn 7 */
DO:
   RUN Klar_UI (datumenh[15]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-16 Dialog-Frame
ON CHOOSE OF BTN-16 IN FRAME Dialog-Frame /* Btn 9 */
DO:
   RUN Klar_UI (datumenh[16]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-17 Dialog-Frame
ON CHOOSE OF BTN-17 IN FRAME Dialog-Frame /* Btn 2 */
DO:
   RUN Klar_UI (datumenh[17]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-18 Dialog-Frame
ON CHOOSE OF BTN-18 IN FRAME Dialog-Frame /* Btn 3 */
DO:
   RUN Klar_UI (datumenh[18]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-19 Dialog-Frame
ON CHOOSE OF BTN-19 IN FRAME Dialog-Frame /* Btn 4 */
DO:
   RUN Klar_UI (datumenh[19]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-2 Dialog-Frame
ON CHOOSE OF BTN-2 IN FRAME Dialog-Frame /* Btn 2 */
DO:
   RUN Klar_UI (datumenh[2]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-20 Dialog-Frame
ON CHOOSE OF BTN-20 IN FRAME Dialog-Frame /* Btn 5 */
DO:
   RUN Klar_UI (datumenh[20]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-21 Dialog-Frame
ON CHOOSE OF BTN-21 IN FRAME Dialog-Frame /* Btn 6 */
DO:
   RUN Klar_UI (datumenh[21]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-22 Dialog-Frame
ON CHOOSE OF BTN-22 IN FRAME Dialog-Frame /* Btn 7 */
DO:
   RUN Klar_UI (datumenh[22]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-23 Dialog-Frame
ON CHOOSE OF BTN-23 IN FRAME Dialog-Frame /* Btn 9 */
DO:
   RUN Klar_UI (datumenh[23]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-24 Dialog-Frame
ON CHOOSE OF BTN-24 IN FRAME Dialog-Frame /* Btn 2 */
DO:
   RUN Klar_UI (datumenh[24]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-25 Dialog-Frame
ON CHOOSE OF BTN-25 IN FRAME Dialog-Frame /* Btn 3 */
DO:
   RUN Klar_UI (datumenh[25]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-26 Dialog-Frame
ON CHOOSE OF BTN-26 IN FRAME Dialog-Frame /* Btn 4 */
DO:
   RUN Klar_UI (datumenh[26]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-27 Dialog-Frame
ON CHOOSE OF BTN-27 IN FRAME Dialog-Frame /* Btn 5 */
DO:
   RUN Klar_UI (datumenh[27]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-28 Dialog-Frame
ON CHOOSE OF BTN-28 IN FRAME Dialog-Frame /* Btn 6 */
DO:
   RUN Klar_UI (datumenh[28]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-29 Dialog-Frame
ON CHOOSE OF BTN-29 IN FRAME Dialog-Frame /* Btn 7 */
DO:
   RUN Klar_UI (datumenh[29]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-3 Dialog-Frame
ON CHOOSE OF BTN-3 IN FRAME Dialog-Frame /* Btn 3 */
DO:
   RUN Klar_UI (datumenh[3]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-30 Dialog-Frame
ON CHOOSE OF BTN-30 IN FRAME Dialog-Frame /* Btn 9 */
DO:
   RUN Klar_UI (datumenh[30]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-31
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-31 Dialog-Frame
ON CHOOSE OF BTN-31 IN FRAME Dialog-Frame /* Btn 2 */
DO:
   RUN Klar_UI (datumenh[31]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-32 Dialog-Frame
ON CHOOSE OF BTN-32 IN FRAME Dialog-Frame /* Btn 3 */
DO:
   RUN Klar_UI (datumenh[32]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-33 Dialog-Frame
ON CHOOSE OF BTN-33 IN FRAME Dialog-Frame /* Btn 4 */
DO:
   RUN Klar_UI (datumenh[33]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-34
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-34 Dialog-Frame
ON CHOOSE OF BTN-34 IN FRAME Dialog-Frame /* Btn 5 */
DO:
   RUN Klar_UI (datumenh[34]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-35 Dialog-Frame
ON CHOOSE OF BTN-35 IN FRAME Dialog-Frame /* Btn 6 */
DO:
   RUN Klar_UI (datumenh[35]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-36
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-36 Dialog-Frame
ON CHOOSE OF BTN-36 IN FRAME Dialog-Frame /* Btn 7 */
DO:
   RUN Klar_UI (datumenh[36]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-37
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-37 Dialog-Frame
ON CHOOSE OF BTN-37 IN FRAME Dialog-Frame /* Btn 9 */
DO:
   RUN Klar_UI (datumenh[37]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-4 Dialog-Frame
ON CHOOSE OF BTN-4 IN FRAME Dialog-Frame /* Btn 4 */
DO:
   RUN Klar_UI (datumenh[4]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-5 Dialog-Frame
ON CHOOSE OF BTN-5 IN FRAME Dialog-Frame /* Btn 5 */
DO:
   RUN Klar_UI (datumenh[5]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-6 Dialog-Frame
ON CHOOSE OF BTN-6 IN FRAME Dialog-Frame /* Btn 6 */
DO:
   RUN Klar_UI (datumenh[6]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-7 Dialog-Frame
ON CHOOSE OF BTN-7 IN FRAME Dialog-Frame /* Btn 7 */
DO:
   RUN Klar_UI (datumenh[7]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-8 Dialog-Frame
ON CHOOSE OF BTN-8 IN FRAME Dialog-Frame /* Btn 8 */
DO:
   RUN Klar_UI (datumenh[8]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-9 Dialog-Frame
ON CHOOSE OF BTN-9 IN FRAME Dialog-Frame /* Btn 9 */
DO:
   RUN Klar_UI (datumenh[9]:LABEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back-7 Dialog-Frame
ON CHOOSE OF btn_back-7 IN FRAME Dialog-Frame
DO:
   IF man = 01 THEN DO:
      CMB_AR = CMB_AR - 1.
      man = 12.
   END.
   ELSE man = man - 1.
   IF man = 01 THEN CMB_MANAD = STRING("januari"). 
   ELSE IF man = 02 THEN CMB_MANAD = STRING("februari").
   ELSE IF man = 03 THEN CMB_MANAD = STRING("mars").
   ELSE IF man = 04 THEN CMB_MANAD = STRING("april").
   ELSE IF man = 05 THEN CMB_MANAD = STRING("maj").
   ELSE IF man = 06 THEN CMB_MANAD = STRING("juni").
   ELSE IF man = 07 THEN CMB_MANAD = STRING("juli").
   ELSE IF man = 08 THEN CMB_MANAD = STRING("augusti").
   ELSE IF man = 09 THEN CMB_MANAD = STRING("september").
   ELSE IF man = 10 THEN CMB_MANAD = STRING("oktober").
   ELSE IF man = 11 THEN CMB_MANAD = STRING("november").
   ELSE IF man = 12 THEN CMB_MANAD = STRING("december").   
   DISP CMB_AR CMB_MANAD WITH FRAME {&FRAME-NAME}. 
   APPLY "VALUE-CHANGED" TO CMB_MANAD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over-7 Dialog-Frame
ON CHOOSE OF btn_over-7 IN FRAME Dialog-Frame
DO:
   IF man = 12 THEN DO:
      CMB_AR = CMB_AR + 1.
      man = 01.
   END.
   ELSE man = man + 1.
   IF man = 01 THEN CMB_MANAD = STRING("januari"). 
   ELSE IF man = 02 THEN CMB_MANAD = STRING("februari").
   ELSE IF man = 03 THEN CMB_MANAD = STRING("mars").
   ELSE IF man = 04 THEN CMB_MANAD = STRING("april").
   ELSE IF man = 05 THEN CMB_MANAD = STRING("maj").
   ELSE IF man = 06 THEN CMB_MANAD = STRING("juni").
   ELSE IF man = 07 THEN CMB_MANAD = STRING("juli").
   ELSE IF man = 08 THEN CMB_MANAD = STRING("augusti").
   ELSE IF man = 09 THEN CMB_MANAD = STRING("september").
   ELSE IF man = 10 THEN CMB_MANAD = STRING("oktober").
   ELSE IF man = 11 THEN CMB_MANAD = STRING("november").
   ELSE IF man = 12 THEN CMB_MANAD = STRING("december").   
   DISP CMB_AR CMB_MANAD WITH FRAME {&FRAME-NAME}. 
   APPLY "VALUE-CHANGED" TO CMB_MANAD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR Dialog-Frame
ON LEAVE OF CMB_AR IN FRAME Dialog-Frame
DO:
   CMB_AR = INPUT CMB_AR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR Dialog-Frame
ON VALUE-CHANGED OF CMB_AR IN FRAME Dialog-Frame
DO:
   CMB_AR = INPUT CMB_AR.
   RUN laddadag_UI. /*LADDAR DAGAR. I FILL-IN-FÄLT*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MANAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD Dialog-Frame
ON LEAVE OF CMB_MANAD IN FRAME Dialog-Frame
DO:
   CMB_MANAD = INPUT CMB_MANAD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD Dialog-Frame
ON VALUE-CHANGED OF CMB_MANAD IN FRAME Dialog-Frame
DO:
   CMB_MANAD = INPUT CMB_MANAD.
   RUN laddadag_UI. /*LADDAR DAGAR I FILL-IN-FÄLT*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   IF Guru.GlobalaVariabler:regdatum = ? THEN Guru.GlobalaVariabler:regdatum = TODAY.   
   RUN daghandle_UI (INPUT BTN-1:HANDLE).
   RUN daghandle_UI (INPUT BTN-2:HANDLE).
   RUN daghandle_UI (INPUT BTN-3:HANDLE).
   RUN daghandle_UI (INPUT BTN-4:HANDLE).
   RUN daghandle_UI (INPUT BTN-5:HANDLE).
   RUN daghandle_UI (INPUT BTN-6:HANDLE).
   RUN daghandle_UI (INPUT BTN-7:HANDLE).
   RUN daghandle_UI (INPUT BTN-8:HANDLE).
   RUN daghandle_UI (INPUT BTN-9:HANDLE).
   RUN daghandle_UI (INPUT BTN-10:HANDLE).
   RUN daghandle_UI (INPUT BTN-11:HANDLE).
   RUN daghandle_UI (INPUT BTN-12:HANDLE).
   RUN daghandle_UI (INPUT BTN-13:HANDLE).
   RUN daghandle_UI (INPUT BTN-14:HANDLE).
   RUN daghandle_UI (INPUT BTN-15:HANDLE).
   RUN daghandle_UI (INPUT BTN-16:HANDLE).
   RUN daghandle_UI (INPUT BTN-17:HANDLE).
   RUN daghandle_UI (INPUT BTN-18:HANDLE).
   RUN daghandle_UI (INPUT BTN-19:HANDLE).
   RUN daghandle_UI (INPUT BTN-20:HANDLE).
   RUN daghandle_UI (INPUT BTN-21:HANDLE).
   RUN daghandle_UI (INPUT BTN-22:HANDLE).
   RUN daghandle_UI (INPUT BTN-23:HANDLE).
   RUN daghandle_UI (INPUT BTN-24:HANDLE).
   RUN daghandle_UI (INPUT BTN-25:HANDLE).
   RUN daghandle_UI (INPUT BTN-26:HANDLE).
   RUN daghandle_UI (INPUT BTN-27:HANDLE).
   RUN daghandle_UI (INPUT BTN-28:HANDLE).
   RUN daghandle_UI (INPUT BTN-29:HANDLE).
   RUN daghandle_UI (INPUT BTN-30:HANDLE).
   RUN daghandle_UI (INPUT BTN-31:HANDLE).
   RUN daghandle_UI (INPUT BTN-32:HANDLE).
   RUN daghandle_UI (INPUT BTN-33:HANDLE).
   RUN daghandle_UI (INPUT BTN-34:HANDLE).
   RUN daghandle_UI (INPUT BTN-35:HANDLE).
   RUN daghandle_UI (INPUT BTN-36:HANDLE).
   RUN daghandle_UI (INPUT BTN-37:HANDLE).
   ASSIGN   
   forstaregdatum = Guru.GlobalaVariabler:regdatum.
   ASSIGN  /*LADDAR ÅR I CMB_AR*/
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(Guru.GlobalaVariabler:regdatum) - 3,"9999"))  
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(Guru.GlobalaVariabler:regdatum) - 2,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(Guru.GlobalaVariabler:regdatum) - 1,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(Guru.GlobalaVariabler:regdatum),"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(Guru.GlobalaVariabler:regdatum) + 1,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(Guru.GlobalaVariabler:regdatum) + 2,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(Guru.GlobalaVariabler:regdatum) + 3,"9999")).
   ASSIGN
   status-ok = CMB_AR:DELETE("0")
   CMB_AR:SCREEN-VALUE = STRING(YEAR(Guru.GlobalaVariabler:regdatum),"9999")
   CMB_AR = INPUT CMB_AR.
   RUN manad_UI. /*LADDAR MÅNADER I CMB_MANAD*/
   RUN enable_UI.
   RUN laddadag_UI. /*LADDAR DAGAR I FILL-IN-FÄLT*/    
   
   
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dagar_UI Dialog-Frame 
PROCEDURE dagar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE datumnr AS INTEGER NO-UNDO.
   dagraknare = 0.
   REPEAT:
      dagraknare = dagraknare + 1.
      IF dagraknare > 7 THEN LEAVE.
      datumenh[dagraknare]:HIDDEN = TRUE.
   END.
   IF Guru.GlobalaVariabler:regdagnamn = "MÅN" THEN DO:
      dagraknare = 0.
      REPEAT:
         dagraknare = dagraknare + 1.
         IF dagraknare > 7 THEN LEAVE.
         datumenh[dagraknare]:HIDDEN = FALSE.
      END.
   END.      
   ELSE IF Guru.GlobalaVariabler:regdagnamn = "TIS" THEN DO:
      dagraknare = 1.
      REPEAT:
         dagraknare = dagraknare + 1.
         IF dagraknare > 7 THEN LEAVE.
         datumenh[dagraknare]:HIDDEN = FALSE.
      END.
   END.      
   ELSE IF Guru.GlobalaVariabler:regdagnamn = "ONS" THEN DO:
      dagraknare = 2.
      REPEAT:
         dagraknare = dagraknare + 1.
         IF dagraknare > 7 THEN LEAVE.
         datumenh[dagraknare]:HIDDEN = FALSE.
      END.
   END.      
   ELSE IF Guru.GlobalaVariabler:regdagnamn = "TOR" THEN DO:
      dagraknare = 3.
      REPEAT:
         dagraknare = dagraknare + 1.
         IF dagraknare > 7 THEN LEAVE.
         datumenh[dagraknare]:HIDDEN = FALSE.
      END.
   END.      
   ELSE IF Guru.GlobalaVariabler:regdagnamn = "FRE" THEN DO:
      dagraknare = 4.
      REPEAT:
         dagraknare = dagraknare + 1.
         IF dagraknare > 7 THEN LEAVE.
         datumenh[dagraknare]:HIDDEN = FALSE.
      END.
   END.      
   ELSE IF Guru.GlobalaVariabler:regdagnamn = "LÖR" THEN DO:
      dagraknare = 5.
      REPEAT:
         dagraknare = dagraknare + 1.
         IF dagraknare > 7 THEN LEAVE.
         datumenh[dagraknare]:HIDDEN = FALSE.
      END.
   END.  
   ELSE IF Guru.GlobalaVariabler:regdagnamn = "SÖN" THEN DO:
      dagraknare = 6.
      REPEAT:
         dagraknare = dagraknare + 1.
         IF dagraknare > 7 THEN LEAVE.
         datumenh[dagraknare]:HIDDEN = FALSE.
      END.
   END.  
   datumenh[29]:HIDDEN = TRUE.
   datumenh[30]:HIDDEN = TRUE.
   datumenh[31]:HIDDEN = TRUE. 
   datumenh[32]:HIDDEN = TRUE.
   datumenh[33]:HIDDEN = TRUE.
   datumenh[34]:HIDDEN = TRUE. 
   datumenh[35]:HIDDEN = TRUE.
   datumenh[36]:HIDDEN = TRUE.
   datumenh[37]:HIDDEN = TRUE. 
  
   datumnr = 1.   
   dagraknare = 0.
   REPEAT:
      dagraknare = dagraknare + 1.
      IF datumnr > maxdagnr THEN LEAVE.
      IF dagraknare < 8 AND datumenh[dagraknare]:HIDDEN = FALSE THEN datumenh[dagraknare]:LABEL = STRING(datumnr).  
      ELSE IF dagraknare > 7 THEN DO:
         datumenh[dagraknare]:HIDDEN = FALSE.
         datumenh[dagraknare]:LABEL = STRING(datumnr).
      END. 
      IF datumenh[dagraknare]:HIDDEN = FALSE THEN datumnr = datumnr + 1.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE daghandle_UI Dialog-Frame 
PROCEDURE daghandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER dh AS HANDLE NO-UNDO.
   dagraknare = dagraknare + 1.
   ASSIGN 
   datumenh[dagraknare] = dh.
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
  DISPLAY CMB_AR CMB_MANAD FILL-IN-V-1 FILL-IN-V-2 FILL-IN-V-3 FILL-IN-V-4 
          FILL-IN-V-5 FILL-IN-V-6 
      WITH FRAME Dialog-Frame.
  ENABLE CMB_AR CMB_MANAD btn_over-7 btn_back-7 FILL-IN-V-1 BTN-1 BTN-2 BTN-3 
         BTN-4 BTN-5 BTN-6 BTN-7 FILL-IN-V-2 BTN-8 BTN-9 BTN-10 BTN-11 BTN-12 
         BTN-13 BTN-14 FILL-IN-V-3 BTN-15 BTN-16 BTN-17 BTN-18 BTN-19 BTN-20 
         BTN-21 FILL-IN-V-4 BTN-22 BTN-23 BTN-24 BTN-25 BTN-26 BTN-27 BTN-28 
         FILL-IN-V-5 BTN-29 BTN-30 BTN-31 BTN-32 BTN-33 BTN-34 BTN-35 BTN-36 
         BTN-37 FILL-IN-V-6 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Klar_UI Dialog-Frame 
PROCEDURE Klar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER  dnr AS CHARACTER NO-UNDO.
   dnr = TRIM(dnr).
   
   Guru.GlobalaVariabler:regdatum = DATE(man,INTEGER(dnr),ye).
   APPLY "CHOOSE" TO Btn_OK IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddadag_UI Dialog-Frame 
PROCEDURE laddadag_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
     
   ye = CMB_AR.
   IF CMB_MANAD = STRING("januari") THEN DO:
      man = 01.
   END.
   ELSE IF CMB_MANAD = STRING("februari")THEN DO:
      man = 02.
   END.
   ELSE IF CMB_MANAD = STRING("mars")THEN DO:
      man = 03.
   END.
   ELSE IF CMB_MANAD = STRING("april")THEN DO:
      man = 04.
   END.
   ELSE IF CMB_MANAD = STRING("maj")THEN DO:
      man = 05.
   END.
   ELSE IF CMB_MANAD = STRING("juni")THEN DO:
      man = 06.
   END.
   ELSE IF CMB_MANAD = STRING("juli")THEN DO:
      man = 07.
   END.
   ELSE IF CMB_MANAD = STRING("augusti")THEN DO:
      man = 08.
   END.
   ELSE IF CMB_MANAD = STRING("september")THEN DO:
      man = 09.
   END.
   ELSE IF CMB_MANAD = STRING("oktober")THEN DO:
      man = 10.
   END.
   ELSE IF CMB_MANAD = STRING("november")THEN DO:
      man = 11.
   END.
   ELSE IF CMB_MANAD = STRING("december")THEN DO:
      man = 12.
   END.
   Guru.GlobalaVariabler:regdatum = DATE(man,01,ye).
   RUN REGDAGGLOB.p.
   IF CMB_MANAD = STRING("januari") OR CMB_MANAD = STRING("mars") OR 
      CMB_MANAD = STRING("maj") OR CMB_MANAD = STRING("juli") OR 
      CMB_MANAD = STRING("augusti") OR CMB_MANAD = STRING("oktober") OR 
      CMB_MANAD = STRING("december") THEN DO:
      maxdagnr = 31. 
   END.   
   ELSE IF CMB_MANAD = STRING("februari") THEN DO:
      regdatum2 = Guru.GlobalaVariabler:regdatum + 28.
      IF (SUBSTRING(STRING(regdatum2,"999999"),3,2)) = "02" THEN DO:
         maxdagnr = 29.
      END.   
      ELSE DO:
         maxdagnr = 28.
      END.   
   END.           
   ELSE DO:
      maxdagnr = 30.
   END.
   
   RUN dagar_UI.  
   
   indate = Guru.GlobalaVariabler:regdatum.
   RUN vnr_UI.
   FILL-IN-V-1 = mregvnr.
   indate = indate + 7.
   RUN vnr_UI.
   FILL-IN-V-2 = mregvnr.
   indate = indate + 7.
   RUN vnr_UI.
   FILL-IN-V-3 = mregvnr.
   indate = indate + 7.
   RUN vnr_UI.
   FILL-IN-V-4 = mregvnr.
   indate = indate + 7.
   RUN vnr_UI.
   FILL-IN-V-5 = mregvnr.
   indate = indate + 7.
   RUN vnr_UI.
   FILL-IN-V-6 = mregvnr.
   
   DISPLAY FILL-IN-V-1  
          FILL-IN-V-2 FILL-IN-V-3 
          FILL-IN-V-4 FILL-IN-V-5 FILL-IN-V-6    
      WITH FRAME {&FRAME-NAME}.   
   IF datumenh[36]:HIDDEN = TRUE THEN FILL-IN-V-6:HIDDEN = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE manad_UI Dialog-Frame 
PROCEDURE manad_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF MONTH(Guru.GlobalaVariabler:regdatum) = 01 THEN DO:
      CMB_MANAD:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING("januari").
   END.   
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 02 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("februari").
   END.  
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 03 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("mars").
   END.
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 04 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("april").
   END.  
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 05 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("maj").
   END.
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 06 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("juni").
   END.  
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 07 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("juli").
   END.
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 08 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("augusti").
   END.  
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 09 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("september").
   END.
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 10 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("oktober").
   END.  
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 11 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("november").
   END.
   ELSE IF MONTH(Guru.GlobalaVariabler:regdatum) = 12 THEN DO:
      CMB_MANAD:SCREEN-VALUE = STRING("december").
   END.       
   CMB_MANAD = INPUT CMB_MANAD. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vnr_UI Dialog-Frame 
PROCEDURE vnr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE yr AS INTEGER NO-UNDO.
   DEFINE VARIABLE wn AS INTEGER NO-UNDO.
   DEFINE VARIABLE d1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE dat1 AS DATE NO-UNDO.
   DEFINE VARIABLE yyyyww AS INTEGER NO-UNDO FORMAT "999999".
      
      
   ASSIGN
   yr   = YEAR(indate)
   d1   = WEEKDAY(DATE( 1 , 1 , yr))
   dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 
          ELSE DATE(1, 10, yr) - d1 )
   wn   = TRUNCATE((indate - dat1 + 7) / 7 , 0)
   yyyyww = yr * 100 + wn.
   
   IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
   ASSIGN
   yr     = yr - 1
   d1     = WEEKDAY(DATE( 1 , 1 , yr))
   dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
               DATE(1, 10, yr) - d1 )
   wn     = TRUNCATE((indate - dat1 + 7) / 7 , 0)
   yyyyww = yr * 100 + wn.
   ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
   ASSIGN
   yr     = yr + 1
   d1     = WEEKDAY(DATE( 1 , 1 , yr))
   yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
          THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.
   mregvnr = INTEGER(SUBSTRING(STRING(yyyyww,"999999"),5,2)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

