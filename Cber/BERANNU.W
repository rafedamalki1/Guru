&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-4



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-4 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/30/97 -  9:11 pm

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
DEFINE INPUT PARAMETER nr1 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER nr2 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER nr3 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valomr AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */   
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED 
{BERANN.I}
{AVDTEMP.I}
{AVDELNINGTEMP.I}
{OMRTEMPW.I}
&Scoped-define NEW NEW
{ANNAMN.I}
DEFINE NEW SHARED VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE vallinje AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valnat AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE siff AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE andrakod AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE id AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nyttidnr AS CHARACTER.
DEFINE SHARED VARIABLE berval2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE VARIABLE sparnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE byt1 AS CHARACTER NO-UNDO.  
DEFINE VARIABLE byt2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE byt3 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO. 
DEFINE VARIABLE berannapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-D
&Scoped-define BROWSE-NAME BRW_FORD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES berfortemp berlinjetemp bernattemp

/* Definitions for BROWSE BRW_FORD                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FORD berfortemp.OMRADE berfortemp.AVDNR ~
berfortemp.VIJUDID berfortemp.FORNR berfortemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FORD 
&Scoped-define QUERY-STRING-BRW_FORD FOR EACH berfortemp NO-LOCK ~
    BY berfortemp.FORNR
&Scoped-define OPEN-QUERY-BRW_FORD OPEN QUERY BRW_FORD FOR EACH berfortemp NO-LOCK ~
    BY berfortemp.FORNR.
&Scoped-define TABLES-IN-QUERY-BRW_FORD berfortemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FORD berfortemp


/* Definitions for BROWSE BRW_LINJE                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_LINJE berlinjetemp.LINNR ~
berlinjetemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LINJE 
&Scoped-define QUERY-STRING-BRW_LINJE FOR EACH berlinjetemp NO-LOCK ~
    BY berlinjetemp.LINNR
&Scoped-define OPEN-QUERY-BRW_LINJE OPEN QUERY BRW_LINJE FOR EACH berlinjetemp NO-LOCK ~
    BY berlinjetemp.LINNR.
&Scoped-define TABLES-IN-QUERY-BRW_LINJE berlinjetemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LINJE berlinjetemp


/* Definitions for BROWSE BRW_NAT                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_NAT bernattemp.NATNR bernattemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NAT 
&Scoped-define QUERY-STRING-BRW_NAT FOR EACH bernattemp NO-LOCK ~
    BY bernattemp.NATNR
&Scoped-define OPEN-QUERY-BRW_NAT OPEN QUERY BRW_NAT FOR EACH bernattemp NO-LOCK ~
    BY bernattemp.NATNR.
&Scoped-define TABLES-IN-QUERY-BRW_NAT bernattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NAT bernattemp


/* Definitions for FRAME FRAME-D                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_JURP CMB_AVD CMB_OMR BTN_HMT BRW_FORD ~
BRW_LINJE BRW_NAT BTN_NAMN BTN_NYF BTN_ANDRAF BTN_BORTF BTN_NYL BTN_ANDRAL ~
BTN_BORTL BTN_NYN BTN_ANDRAN BTN_BORTN BTN_AVB FILL-IN-TAB1 FILL-IN-TAB2 ~
FILL-IN-TAB3 
&Scoped-Define DISPLAYED-OBJECTS CMB_JURP CMB_AVD CMB_OMR FILL-IN-TAB1 ~
FILL-IN-TAB2 FILL-IN-TAB3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-4 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDRAF 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDRAL 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDRAN 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORTF 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTL 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORTN 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_HMT 
     LABEL "Hämta" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NAMN 
     LABEL "Ändra namn" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NYF 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NYL 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NYN 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB1 AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 36 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB2 AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 36 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB3 AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 36 BY 1
     BGCOLOR 7  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FORD FOR 
      berfortemp SCROLLING.

DEFINE QUERY BRW_LINJE FOR 
      berlinjetemp SCROLLING.

DEFINE QUERY BRW_NAT FOR 
      bernattemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FORD WINDOW-4 _STRUCTURED
  QUERY BRW_FORD NO-LOCK DISPLAY
      berfortemp.OMRADE FORMAT "X(256)":U WIDTH 5
      berfortemp.AVDNR FORMAT "X(256)":U WIDTH 5
      berfortemp.VIJUDID FORMAT "X(256)":U WIDTH 5
      berfortemp.FORNR FORMAT "X(10)":U
      berfortemp.NAMN FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36 BY 17.04.

DEFINE BROWSE BRW_LINJE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LINJE WINDOW-4 _STRUCTURED
  QUERY BRW_LINJE NO-LOCK DISPLAY
      berlinjetemp.LINNR FORMAT "X(10)":U
      berlinjetemp.NAMN FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36 BY 17.04.

DEFINE BROWSE BRW_NAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NAT WINDOW-4 _STRUCTURED
  QUERY BRW_NAT NO-LOCK DISPLAY
      bernattemp.NATNR FORMAT "X(10)":U
      bernattemp.NAMN FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36 BY 17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-D
     CMB_JURP AT ROW 1.88 COL 3.5
     CMB_AVD AT ROW 2.96 COL 2.5
     CMB_OMR AT ROW 4.04 COL 11.5 COLON-ALIGNED
     BTN_HMT AT ROW 5.13 COL 17
     BRW_FORD AT ROW 8.75 COL 1.5
     BRW_LINJE AT ROW 8.75 COL 38
     BRW_NAT AT ROW 8.75 COL 74.5
     BTN_NAMN AT ROW 9.58 COL 111.25
     BTN_NYF AT ROW 26.25 COL 1.5
     BTN_ANDRAF AT ROW 26.25 COL 13.5
     BTN_BORTF AT ROW 26.25 COL 25.5
     BTN_NYL AT ROW 26.25 COL 38
     BTN_ANDRAL AT ROW 26.25 COL 50
     BTN_BORTL AT ROW 26.25 COL 62
     BTN_NYN AT ROW 26.25 COL 74.5
     BTN_ANDRAN AT ROW 26.25 COL 86.5
     BTN_BORTN AT ROW 26.25 COL 98.5
     BTN_AVB AT ROW 27.75 COL 110
     FILL-IN-TAB1 AT ROW 7.46 COL 1.5 NO-LABEL
     FILL-IN-TAB2 AT ROW 7.46 COL 38 NO-LABEL
     FILL-IN-TAB3 AT ROW 7.46 COL 74.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.17
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: berfortemp T "?" NO-UNDO temp-db berfortemp
      TABLE: berlinjetemp T "?" NO-UNDO temp-db berlinjetemp
      TABLE: bernattemp T "?" NO-UNDO temp-db bernattemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-4 ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration av anläggningar"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW WINDOW-4
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-D
                                                                        */
/* BROWSE-TAB BRW_FORD BTN_HMT FRAME-D */
/* BROWSE-TAB BRW_LINJE BRW_FORD FRAME-D */
/* BROWSE-TAB BRW_NAT BRW_LINJE FRAME-D */
ASSIGN 
       BRW_FORD:HIDDEN  IN FRAME FRAME-D                = TRUE
       BRW_FORD:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-D = TRUE
       BRW_FORD:COLUMN-RESIZABLE IN FRAME FRAME-D       = TRUE.

ASSIGN 
       BRW_LINJE:HIDDEN  IN FRAME FRAME-D                = TRUE
       BRW_LINJE:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-D = TRUE
       BRW_LINJE:COLUMN-RESIZABLE IN FRAME FRAME-D       = TRUE.

ASSIGN 
       BRW_NAT:HIDDEN  IN FRAME FRAME-D                = TRUE
       BRW_NAT:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-D = TRUE
       BRW_NAT:COLUMN-RESIZABLE IN FRAME FRAME-D       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME FRAME-D
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME FRAME-D
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-TAB1 IN FRAME FRAME-D
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-TAB2 IN FRAME FRAME-D
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-TAB3 IN FRAME FRAME-D
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-4)
THEN WINDOW-4:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FORD
/* Query rebuild information for BROWSE BRW_FORD
     _TblList          = "Temp-Tables.berfortemp"
     _Options          = "NO-LOCK"
     _OrdList          = "RT9.berfortemp.FORNR|yes"
     _FldNameList[1]   > Temp-Tables.berfortemp.OMRADE
"berfortemp.OMRADE" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.berfortemp.AVDNR
"berfortemp.AVDNR" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.berfortemp.VIJUDID
"berfortemp.VIJUDID" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.berfortemp.FORNR
     _FldNameList[5]   = Temp-Tables.berfortemp.NAMN
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FORD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LINJE
/* Query rebuild information for BROWSE BRW_LINJE
     _TblList          = "Temp-Tables.berlinjetemp"
     _Options          = "NO-LOCK"
     _OrdList          = "RT9.berlinjetemp.LINNR|yes"
     _FldNameList[1]   = Temp-Tables.berlinjetemp.LINNR
     _FldNameList[2]   = Temp-Tables.berlinjetemp.NAMN
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LINJE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NAT
/* Query rebuild information for BROWSE BRW_NAT
     _TblList          = "Temp-Tables.bernattemp"
     _Options          = "NO-LOCK"
     _OrdList          = "RT9.bernattemp.NATNR|yes"
     _FldNameList[1]   = Temp-Tables.bernattemp.NATNR
     _FldNameList[2]   = Temp-Tables.bernattemp.NAMN
     _Query            is NOT OPENED
*/  /* BROWSE BRW_NAT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_FORD
&Scoped-define SELF-NAME BRW_FORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FORD WINDOW-4
ON VALUE-CHANGED OF BRW_FORD IN FRAME FRAME-D
DO: 
   status-ok = BRW_FORD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE berfortemp THEN DO:
      ASSIGN
      valford = berfortemp.FORNR. 
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " FORNR = '" + valford + "'").  
      RUN openbdynspec_UI IN brwproc[2].        
      APPLY "VALUE-CHANGED" TO BRW_LINJE IN FRAME {&FRAME-NAME}.    
   END.  
   ELSE DO:
      valford = "". 
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " FORNR = '" + valford + "'").  
      RUN openbdynspec_UI IN brwproc[2].        
      APPLY "VALUE-CHANGED" TO BRW_LINJE IN FRAME {&FRAME-NAME}.   
   END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LINJE
&Scoped-define SELF-NAME BRW_LINJE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LINJE WINDOW-4
ON VALUE-CHANGED OF BRW_LINJE IN FRAME FRAME-D
DO: 
   status-ok = BRW_LINJE:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE berlinjetemp THEN DO:
      ASSIGN      
      vallinje = berlinjetemp.LINNR. 
      RUN setcolsortvar_UI IN brwproc[3] (INPUT " FORNR = '" + valford + "' AND LINNR = '" + vallinje + "'").  
      RUN openbdynspec_UI IN brwproc[3].        
      APPLY "VALUE-CHANGED" TO BRW_NAT IN FRAME {&FRAME-NAME}.     
   END.
   ELSE DO:
      vallinje = "". 
      RUN setcolsortvar_UI IN brwproc[3] (INPUT " FORNR = '" + valford + "' AND LINNR = '" + vallinje + "'").  
      RUN openbdynspec_UI IN brwproc[3].        
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_NAT
&Scoped-define SELF-NAME BRW_NAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_NAT WINDOW-4
ON VALUE-CHANGED OF BRW_NAT IN FRAME FRAME-D
DO: 
   status-ok = BRW_NAT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE bernattemp THEN DO:
      ASSIGN      
      valnat = bernattemp.NATNR.      
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRAF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRAF WINDOW-4
ON CHOOSE OF BTN_ANDRAF IN FRAME FRAME-D /* Ändra */
DO:
   status-ok = BRW_FORD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   {muswait.i}
   andra = TRUE.     
   FIND FIRST annamntemp NO-LOCK NO-ERROR.   
   IF AVAILABLE berfortemp THEN DO:
      ASSIGN
      siff = 1
      sparnr = berfortemp.FORNR
      andrakod = berfortemp.FORNR.        
      RUN NYANNU.W.      
      IF musz = FALSE THEN DO:
         FIND FIRST berfortemp WHERE berfortemp.FORNR = sparnr NO-ERROR.
         DELETE berfortemp.
         RUN laddaberforen_UI IN berannapph (INPUT andrakod,OUTPUT TABLE berfortemp APPEND).        
         RUN openbdynspec_UI IN brwproc[1].        
         FIND FIRST berfortemp WHERE berfortemp.FORNR = andrakod NO-ERROR.
         IF AVAILABLE berfortemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(berfortemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.
         IF sparnr NE andrakod THEN DO:
            FOR EACH berlinjetemp WHERE berlinjetemp.FORNR = sparnr: 
               DELETE berlinjetemp.
            END.
            RUN laddaberlin_UI IN berannapph (INPUT andrakod,OUTPUT TABLE berlinjetemp APPEND).
            FOR EACH bernattemp WHERE bernattemp.FORNR = sparnr: 
               DELETE bernattemp.
            END.
            RUN laddabernat_UI IN berannapph 
            (INPUT andrakod,OUTPUT TABLE bernattemp APPEND).
         END.            
         APPLY "VALUE-CHANGED" TO BRW_FORD IN FRAME {&FRAME-NAME}.
         APPLY "VALUE-CHANGED" TO BRW_LINJE IN FRAME {&FRAME-NAME}.  
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen " + annamntemp.TAB1 + " att utföra ändring på."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   musz = FALSE.
   andra = FALSE.     
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRAL WINDOW-4
ON CHOOSE OF BTN_ANDRAL IN FRAME FRAME-D /* Ändra */
DO:
   {muswait.i}   
   andra = TRUE.     
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
   status-ok = BRW_LINJE:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   
   IF AVAILABLE berlinjetemp THEN DO: 
      ASSIGN
      sparnr = berlinjetemp.LINNR.  
      andrakod = berlinjetemp.LINNR.  
      siff = 2.       
      RUN NYANNU.W.      
      IF musz = FALSE THEN DO:
         FIND FIRST berlinjetemp WHERE berlinjetemp.LINNR = sparnr AND  berlinjetemp.FORNR = valford NO-ERROR.
         DELETE berlinjetemp.
         RUN laddaberlinen_UI IN berannapph (INPUT valford,INPUT andrakod,OUTPUT TABLE berlinjetemp APPEND).
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " FORNR = '" + valford + "'").  
         RUN openbdynspec_UI IN brwproc[2].   
         FIND FIRST berlinjetemp WHERE berlinjetemp.LINNR = andrakod AND berlinjetemp.FORNR = valford NO-ERROR.
         IF AVAILABLE berlinjetemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(berlinjetemp)).
            RUN lastselectdyn_UI IN brwproc[2].
         END.
         IF sparnr NE andrakod THEN DO:               
            FOR EACH bernattemp WHERE bernattemp.FORNR = valford AND bernattemp.LINNR = sparnr: 
               DELETE bernattemp.
            END.
            RUN laddabernatlinj_UI IN berannapph 
            (INPUT valford,INPUT andrakod,OUTPUT TABLE bernattemp APPEND).
         END.
         APPLY "VALUE-CHANGED" TO BRW_LINJE IN FRAME {&FRAME-NAME}. 
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen " + annamntemp.TAB2 + " att utföra ändring på."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.           
   musz = FALSE.
   andra = FALSE.     
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRAN WINDOW-4
ON CHOOSE OF BTN_ANDRAN IN FRAME FRAME-D /* Ändra */
DO:
   {muswait.i}   
   andra = TRUE.     
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
   status-ok = BRW_NAT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   
   IF AVAILABLE bernattemp THEN DO: 
      ASSIGN
      sparnr = bernattemp.NATNR
      andrakod = bernattemp.NATNR.  
      siff = 3.        
      RUN NYANNU.W.      
      IF musz = FALSE THEN DO:
         FIND FIRST bernattemp WHERE bernattemp.NATNR = sparnr AND bernattemp.LINNR = vallinje AND bernattemp.FORNR = valford NO-ERROR.
         DELETE bernattemp.
         RUN laddabernaten_UI IN berannapph 
         (INPUT valford,INPUT vallinje,INPUT andrakod,OUTPUT TABLE bernattemp APPEND).
         RUN setcolsortvar_UI IN brwproc[3] (INPUT " FORNR = '" + valford + "' AND LINNR = '" + vallinje + "'").  
         RUN openbdynspec_UI IN brwproc[3].  
         FIND FIRST bernattemp WHERE bernattemp.NATNR = andrakod AND bernattemp.LINNR = vallinje AND bernattemp.FORNR = valford NO-ERROR.
         IF AVAILABLE bernattemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(bernattemp)).
            RUN lastselectdyn_UI IN brwproc[3].
         END.
         APPLY "VALUE-CHANGED" TO BRW_NAT IN FRAME {&FRAME-NAME}. 
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen " + annamntemp.TAB3 + " att utföra ändring på."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.       
   musz = FALSE.
   andra = FALSE.     
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-4
ON CHOOSE OF BTN_AVB IN FRAME FRAME-D /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTF WINDOW-4
ON CHOOSE OF BTN_BORTF IN FRAME FRAME-D /* Ta bort */
DO:
   {muswait.i}    
   status-ok = BRW_FORD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   FIND FIRST annamntemp NO-LOCK NO-ERROR.   
   IF AVAILABLE berfortemp THEN DO: 
      MESSAGE "Vill du ta bort " + annamntemp.TAB1 + " Nr: " + berfortemp.FORNR VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
      IF svar THEN DO:
         andrakod = berfortemp.FORNR.
         DELETE berfortemp.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN bortberfor_UI IN berannapph (INPUT andrakod).         
         RUN openbdynspec_UI IN brwproc[1].             
         RUN lastselectdyn_UI IN brwproc[1].
         APPLY "VALUE-CHANGED" TO BRW_FORD IN FRAME {&FRAME-NAME}. 
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen " + annamntemp.TAB1 + " att ta bort." 
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.            
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTL WINDOW-4
ON CHOOSE OF BTN_BORTL IN FRAME FRAME-D /* Ta bort */
DO:
   {muswait.i}    
   status-ok = BRW_LINJE:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
      
   FIND FIRST berlinjetemp WHERE berlinjetemp.FORNR = valford USE-INDEX FORNR NO-LOCK NO-ERROR.
   IF AVAILABLE berlinjetemp THEN DO: 
      MESSAGE "Vill du ta bort " + annamntemp.TAB2 + " Nr: " + berlinjetemp.LINNR VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
      IF svar THEN DO:
         andrakod = berlinjetemp.LINNR.
         DELETE berlinjetemp.
         RUN selnextprevrow_UI IN brwproc[2].
         RUN bortberlin_UI IN berannapph (INPUT valford,INPUT andrakod). 
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " FORNR = '" + valford + "'").  
         RUN openbdynspec_UI IN brwproc[2].
         RUN lastselectdyn_UI IN brwproc[2].
         RUN setcolsortvar_UI IN brwproc[3] (INPUT " FORNR = '" + valford + "' AND LINNR = '" + vallinje + "'").  
         RUN openbdynspec_UI IN brwproc[3]. 
         APPLY "VALUE-CHANGED" TO BRW_LINJE IN FRAME {&FRAME-NAME}.          
      END.                  
   END.
   ELSE DO:
      MESSAGE "Det finns ingen " + annamntemp.TAB2 + " att ta bort." 
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTN WINDOW-4
ON CHOOSE OF BTN_BORTN IN FRAME FRAME-D /* Ta bort */
DO:
   {muswait.i}    
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
   status-ok = BRW_NAT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   IF AVAILABLE bernattemp THEN DO: 
      MESSAGE "Vill du ta bort " + annamntemp.TAB3 + " Nr: " + bernattemp.NATNR VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
      IF svar THEN DO:
         andrakod = bernattemp.NATNR.
         DELETE bernattemp.
         RUN selnextprevrow_UI IN brwproc[3].
         RUN bortnat_UI IN berannapph (INPUT valford,INPUT vallinje,INPUT andrakod).
         RUN setcolsortvar_UI IN brwproc[3] (INPUT " FORNR = '" + valford + "' AND LINNR = '" + vallinje + "'").  
         RUN openbdynspec_UI IN brwproc[3]. 
         RUN lastselectdyn_UI IN brwproc[3].
         APPLY "VALUE-CHANGED" TO BRW_NAT IN FRAME {&FRAME-NAME}. 
      END.          
   END.
   ELSE DO:
      MESSAGE "Det finns ingen " + annamntemp.TAB3 + " att ta bort." 
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.      
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HMT WINDOW-4
ON CHOOSE OF BTN_HMT IN FRAME FRAME-D /* Hämta */
DO:
   ASSIGN   
   CMB_AVD = INPUT FRAME {&FRAME-NAME} CMB_AVD 
   CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR. 
   IF jid = "" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   kommando = "".
   IF CMB_JURP NE "alla" THEN DO:
      FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR.
      kommando = " berfortemp.JUDID = '" + jurperstemp.JUDID + "'".      
   END.
   IF CMB_AVD NE "alla" THEN DO:
      IF kommando NE " " THEN kommando = kommando + " AND ".
      FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR. 
      kommando = kommando + " berfortemp.AVDNR = '" + STRING(avdtemp.AVDELNINGNR) + "'".
   END.
   IF CMB_OMR NE "alla" THEN DO:
      IF kommando NE " " THEN kommando = kommando + " AND ".
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.      
      kommando = kommando + "berfortemp.OMRADE = '" + omrtemp.OMRADE + "'".
   END.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT kommando).
   RUN openbdynspec_UI IN brwproc[1]. 
   APPLY "VALUE-CHANGED" TO BRW_FORD IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAMN WINDOW-4
ON CHOOSE OF BTN_NAMN IN FRAME FRAME-D /* Ändra namn */
DO:
   {muswait.i}  
   musz = FALSE.
   RUN TABANDU.W.
   IF musz = FALSE THEN DO: 
      EMPTY TEMP-TABLE annamntemp NO-ERROR. 
      RUN annamn_UI IN berannapph (OUTPUT TABLE annamntemp).
      FIND FIRST annamntemp NO-LOCK NO-ERROR.       
      ASSIGN
      FILL-IN-TAB1 = annamntemp.TAB1
      FILL-IN-TAB2 = annamntemp.TAB2
      FILL-IN-TAB3 = annamntemp.TAB3.            
      DISPLAY FILL-IN-TAB1 WITH FRAME {&FRAME-NAME}.  
      DISPLAY FILL-IN-TAB2 WITH FRAME {&FRAME-NAME}.  
      DISPLAY FILL-IN-TAB3 WITH FRAME {&FRAME-NAME}.        
      ASSIGN
      byt1 = annamntemp.TAB1
      byt2 = annamntemp.TAB2
      byt3 = annamntemp.TAB3.  
      musz = FALSE.
   END.       
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYF WINDOW-4
ON CHOOSE OF BTN_NYF IN FRAME FRAME-D /* Ny */
DO: 
   {muswait.i} 
   ASSIGN
   musz = FALSE
   andra = FALSE
   id = FALSE.     
   FIND FIRST annamntemp NO-LOCK NO-ERROR.   
   siff = 1.    
   RUN NYANNU.W.     
   IF musz = FALSE THEN DO:         
      RUN laddaberforen_UI IN berannapph (INPUT andrakod,OUTPUT TABLE berfortemp APPEND).
      RUN openbdynspec_UI IN brwproc[1]. 
      FIND FIRST berfortemp WHERE berfortemp.FORNR = andrakod NO-ERROR.
      IF AVAILABLE berfortemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(berfortemp)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
      APPLY "VALUE-CHANGED" TO BRW_FORD IN FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO BRW_LINJE IN FRAME {&FRAME-NAME}.  
   END.
   ELSE musz = FALSE.      

   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYL WINDOW-4
ON CHOOSE OF BTN_NYL IN FRAME FRAME-D /* Ny */
DO: 
   {muswait.i} 
   ASSIGN
   musz = FALSE
   andra = FALSE
   id = FALSE.     
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
      
   IF AVAILABLE berfortemp THEN DO:
      siff = 2.       
      RUN NYANNU.W.      
      IF musz = FALSE THEN DO:         
         RUN laddaberlinen_UI IN berannapph (INPUT valford,INPUT andrakod,OUTPUT TABLE berlinjetemp APPEND).
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " FORNR = '" + valford + "'").  
         RUN openbdynspec_UI IN brwproc[2].   
         FIND FIRST berlinjetemp WHERE berlinjetemp.LINNR = andrakod AND   berlinjetemp.FORNR = valford NO-ERROR.
         IF AVAILABLE berlinjetemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(berlinjetemp)).
            RUN lastselectdyn_UI IN brwproc[2].
         END.
         APPLY "VALUE-CHANGED" TO BRW_LINJE IN FRAME {&FRAME-NAME}. 
      END.
      ELSE musz = FALSE.             
   END.
   ELSE DO:
      MESSAGE "Det finns ingen " + annamntemp.TAB1 + " att koppla " + annamntemp.TAB2 + " till."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.           
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYN WINDOW-4
ON CHOOSE OF BTN_NYN IN FRAME FRAME-D /* Ny */
DO: 
   {muswait.i} 
   ASSIGN
   musz = FALSE
   andra = FALSE
   id = FALSE.     
   FIND FIRST annamntemp NO-LOCK NO-ERROR.   
   IF AVAILABLE berlinjetemp THEN DO:
      siff = 3.        
      RUN NYANNU.W.      
      IF musz = FALSE THEN DO:         
         RUN laddabernaten_UI IN berannapph 
         (INPUT valford,INPUT vallinje,INPUT andrakod,OUTPUT TABLE bernattemp APPEND).
         RUN setcolsortvar_UI IN brwproc[3] (INPUT " FORNR = '" + valford + "' AND LINNR = '" + vallinje + "'").  
         RUN openbdynspec_UI IN brwproc[3].  
         FIND FIRST bernattemp WHERE bernattemp.NATNR = andrakod AND bernattemp.LINNR = vallinje AND bernattemp.FORNR = valford NO-ERROR.
         IF AVAILABLE bernattemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(bernattemp)).
            RUN lastselectdyn_UI IN brwproc[3].
         END.
         APPLY "VALUE-CHANGED" TO BRW_NAT IN FRAME {&FRAME-NAME}. 
      END.
      ELSE musz = FALSE.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen " + annamntemp.TAB2 + " att koppla " + annamntemp.TAB3 + " till."
      VIEW-AS ALERT-BOX TITLE "Meddelande".  
   END.       
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD WINDOW-4
ON VALUE-CHANGED OF CMB_AVD IN FRAME FRAME-D /* Avdelning */
DO:     
   CMB_AVD = INPUT CMB_AVD.         
   {CMB_AVDN1.I}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_JURP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_JURP WINDOW-4
ON VALUE-CHANGED OF CMB_JURP IN FRAME FRAME-D /* Juridisp */
DO:     
   CMB_JURP = INPUT CMB_JURP.         
   {CMB_JURP.I}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR WINDOW-4
ON VALUE-CHANGED OF CMB_OMR IN FRAME FRAME-D /* Område */
DO:
   CMB_OMR = INPUT CMB_OMR.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FORD
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-4 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(berannapph) THEN DELETE PROCEDURE berannapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.      
   IF VALID-HANDLE(nyttaoapph2) THEN DO:
      RUN borthandle_UI IN nyttaoapph2.
      DELETE PROCEDURE nyttaoapph2 NO-ERROR.
      nyttaoapph2 = ?.
   END.  
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
   {ALLSTARTDYN.I}
   {muswait.i}    
   {JURPAVDSTART2.I}      
   ASSIGN 
   status-ok = CMB_OMR:ADD-FIRST("Alla").
   CMB_OMR:SCREEN-VALUE = "Alla".   
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = valomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.       
   END.
   RUN annamn_UI IN berannapph (OUTPUT TABLE annamntemp). 
   IF berval2 = FALSE THEN DO:
      RUN ladda_UI IN berannapph (OUTPUT TABLE berfortemp,
                                  OUTPUT TABLE berlinjetemp,
                                  OUTPUT TABLE bernattemp).
   END.
   FIND FIRST annamntemp NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE annamntemp THEN DO TRANSACTION:       
      CREATE annamntemp.
      ASSIGN
      annamntemp.TAB1 = "Fördelningsstation"
      annamntemp.TAB2 = "Linje"
      annamntemp.TAB3 = "Nätstation".
   END.    
   ASSIGN   
   berfortemp.OMRADE:LABEL IN BROWSE BRW_FORD = Guru.Konstanter:gomrk
   berfortemp.AVDNR:LABEL IN BROWSE BRW_FORD= Guru.Konstanter:gavdk
   berfortemp.VIJUDID:LABEL IN BROWSE BRW_FORD = Guru.Konstanter:gjuk
   CMB_JURP:LABEL = Guru.Konstanter:gjuk
   CMB_AVD:LABEL= Guru.Konstanter:gavdk
   CMB_OMR:LABEL = Guru.Konstanter:gomrk.  
   ASSIGN
   FILL-IN-TAB1 = annamntemp.TAB1
   FILL-IN-TAB2 = annamntemp.TAB2
   FILL-IN-TAB3 = annamntemp.TAB3
   byt1 = annamntemp.TAB1
   byt2 = annamntemp.TAB2
   byt3 = annamntemp.TAB3. 
   RUN enable_UI.      
   APPLY "CHOOSE" TO BTN_HMT IN FRAME {&FRAME-NAME}.           
   {FRMSIZE.I}    
   IF berval2 = TRUE THEN BTN_NAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF berval2 = TRUE THEN DO:     
      RUN openbdynspec_UI IN brwproc[1].       
      IF nr1 NE "" THEN DO:        
         FIND FIRST berfortemp WHERE berfortemp.FORNR = nr1 NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST berfortemp NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE berfortemp THEN DO:
         valford = berfortemp.FORNR.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(berfortemp)).
         IF berfortemp.OMRADE = "" THEN DO:
            CMB_OMR:SCREEN-VALUE = "Alla".
            CMB_OMR = "Alla".            
            APPLY "CHOOSE" TO BTN_HMT IN FRAME {&FRAME-NAME}.           
         END.
         RUN lastselectdyn_UI IN brwproc[1].
         APPLY "VALUE-CHANGED" TO BRW_FORD IN FRAME {&FRAME-NAME}.           
      END.      
      IF valford NE "" THEN DO:
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " FORNR = '" + valford + "'").  
      END.      
      RUN openbdynspec_UI IN brwproc[2]. 
      IF nr2 NE "" THEN DO:
         FIND FIRST berlinjetemp WHERE berlinjetemp.FORNR = valford AND berlinjetemp.LINNR = nr2 NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST berlinjetemp WHERE berlinjetemp.FORNR = valford NO-LOCK NO-ERROR.
      END.      
      IF AVAILABLE berlinjetemp THEN DO:
         vallinje = berlinjetemp.LINNR.
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(berlinjetemp)).
         RUN lastselectdyn_UI IN brwproc[2].
         APPLY "VALUE-CHANGED" TO BRW_LINJE IN FRAME {&FRAME-NAME}.           
      END.
      IF vallinje NE "" THEN DO:
         RUN setcolsortvar_UI IN brwproc[3] (INPUT " FORNR = '" + valford + "' AND LINNR = '" + vallinje + "'").  
      END.
      ELSE DO:
      END.
      RUN openbdynspec_UI IN brwproc[3].                 
      IF nr3 NE "" THEN DO:
         FIND FIRST bernattemp WHERE bernattemp.FORNR = valford AND bernattemp.LINNR = vallinje AND
         bernattemp.NATNR = nr3 NO-LOCK NO-ERROR.
      END.         
      ELSE DO:
         FIND FIRST bernattemp WHERE bernattemp.FORNR = valford AND bernattemp.LINNR = vallinje 
         NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE bernattemp THEN DO:
         valnat = bernattemp.NATNR.
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(bernattemp)).
         RUN lastselectdyn_UI IN brwproc[3].
         APPLY "VALUE-CHANGED" TO BRW_NAT IN FRAME {&FRAME-NAME}.           
      END.      
   END.
   ELSE DO:     
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST berfortemp NO-LOCK NO-ERROR.
      IF AVAILABLE berfortemp THEN DO:
         APPLY "VALUE-CHANGED" TO BRW_FORD IN FRAME {&FRAME-NAME}.
      END.
   END.
   {musarrow.i}
   {WIN_M_SLUT.I}
   FIND FIRST jurperstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurperstemp THEN CMB_JURP:HIDDEN = TRUE.   
   IF jid NE "" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-4 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_FORD:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_LINJE:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_NAT:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN brwsetupstop_UI IN brwproc[2] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[3] (INPUT 1).  
   RUN setdefaultcolbyname_UI IN brwproc[1] (INPUT "FORNR").
   RUN setcolindex_UI IN brwproc[2] (INPUT " LINNR ").
   RUN setcolindex_UI IN brwproc[3] (INPUT " NATNR ").
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERANNAPPJ.P PERSISTENT SET berannapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERANNAPPJ.P PERSISTENT SET berannapph.
      RUN MAONRAPP.P PERSISTENT SET aonrapph.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2.
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-4  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-4)
  THEN DELETE WIDGET WINDOW-4.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-4  _DEFAULT-ENABLE
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
  DISPLAY CMB_JURP CMB_AVD CMB_OMR FILL-IN-TAB1 FILL-IN-TAB2 FILL-IN-TAB3 
      WITH FRAME FRAME-D IN WINDOW WINDOW-4.
  ENABLE CMB_JURP CMB_AVD CMB_OMR BTN_HMT BRW_FORD BRW_LINJE BRW_NAT BTN_NAMN 
         BTN_NYF BTN_ANDRAF BTN_BORTF BTN_NYL BTN_ANDRAL BTN_BORTL BTN_NYN 
         BTN_ANDRAN BTN_BORTN BTN_AVB FILL-IN-TAB1 FILL-IN-TAB2 FILL-IN-TAB3 
      WITH FRAME FRAME-D IN WINDOW WINDOW-4.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-D}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

