&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 02/14/97 -  3:20 pm

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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{DEFSOK.I} 
{BMTRL.I}
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
&Scoped-define NEW NEW
{LOPTEMP.I}
{PTEMPLOPTEMP.I}      
DEFINE NEW SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.  
DEFINE SHARED VARIABLE lista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE varalla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valgrupp AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE erslev AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.   
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE satsvar2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE temprow AS ROWID NO-UNDO.
DEFINE VARIABLE kgrvar AS CHARACTER NO-UNDO.
/*MATERIEL DEFINITIONER*/
DEFINE SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_levtill AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.  
DEFINE VARIABLE vald_levfran AS CHARACTER NO-UNDO.  
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE mtxtillyapph AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE nettobredd AS DECIMAL NO-UNDO.
DEFINE VARIABLE nettobreddorg AS DECIMAL NO-UNDO.
DEFINE VARIABLE nettobreddsats AS DECIMAL NO-UNDO.
DEFINE VARIABLE nettobreddsatsorg AS DECIMAL NO-UNDO.
DEFINE VARIABLE enrspar AS CHARACTER NO-UNDO.
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE SHARED TEMP-TABLE grupp_temp NO-UNDO
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.
DEFINE INPUT PARAMETER rsch AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_BERKALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kalkbertemp loptemp

/* Definitions for BROWSE BRW_BERKALK                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_BERKALK kalkbertemp.ARBKOD ~
kalkbertemp.LOPNR kalkbertemp.BENAMNING kalkbertemp.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BERKALK 
&Scoped-define QUERY-STRING-BRW_BERKALK FOR EACH kalkbertemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_BERKALK OPEN QUERY BRW_BERKALK FOR EACH kalkbertemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_BERKALK kalkbertemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BERKALK kalkbertemp


/* Definitions for BROWSE BRW_KLG                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KLG loptemp.ARBKOD loptemp.LOPNR ~
loptemp.BENAMNING loptemp.ENHET loptemp.ANTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KLG loptemp.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KLG loptemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KLG loptemp
&Scoped-define QUERY-STRING-BRW_KLG FOR EACH loptemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_KLG OPEN QUERY BRW_KLG FOR EACH loptemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_KLG loptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KLG loptemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 IMAGE-10 IMAGE-11 RAD_VAL ~
BRW_BERKALK BRW_KLG BTN_VISA BTN_BYT BTN_TILL BTN_BORT BTN_AVB ~
FILL-IN-BARBKOD FILL-IN-KARBKOD 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL FILL-IN-BARBKOD FILL-IN-KARBKOD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BYT 
     LABEL "Ersätt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_TILL 
     LABEL "Lägg till" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BARBKOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbkod" 
     VIEW-AS FILL-IN 
     SIZE 8.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KARBKOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbkod" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-11
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ersätt", 1,
"Lägg till", 2,
"Ta bort", 3
     SIZE 13.5 BY 3.04 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.5 BY 2.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52.5 BY 2.5
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BERKALK FOR 
      kalkbertemp SCROLLING.

DEFINE QUERY BRW_KLG FOR 
      loptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BERKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BERKALK WINDOW-1 _STRUCTURED
  QUERY BRW_BERKALK NO-LOCK DISPLAY
      kalkbertemp.ARBKOD COLUMN-LABEL "Arbkod" FORMAT "X(6)":U
      kalkbertemp.LOPNR FORMAT ">>>":U
      kalkbertemp.BENAMNING FORMAT "X(40)":U WIDTH 20
      kalkbertemp.ENHET FORMAT "X(3)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 48.5 BY 22
         TITLE "Kalkylkoder beredningsupplägg".

DEFINE BROWSE BRW_KLG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KLG WINDOW-1 _STRUCTURED
  QUERY BRW_KLG NO-LOCK DISPLAY
      loptemp.ARBKOD COLUMN-LABEL "Arbkod" FORMAT "X(5)":U
      loptemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      loptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(40)":U
            WIDTH 20
      loptemp.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp.ANTAL COLUMN-LABEL "Antal" FORMAT "->>>>9.999":U
  ENABLE
      loptemp.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52 BY 22
         TITLE "Senaste kostnadskatalogen".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_VAL AT ROW 2.13 COL 111.38 NO-LABEL
     BRW_BERKALK AT ROW 2.75 COL 4
     BRW_KLG AT ROW 2.75 COL 56
     BTN_VISA AT ROW 7.54 COL 111.38
     BTN_BYT AT ROW 9 COL 111.38
     BTN_TILL AT ROW 10.08 COL 111.38
     BTN_BORT AT ROW 11.29 COL 111.38
     BTN_AVB AT ROW 26.63 COL 111.38
     FILL-IN-BARBKOD AT ROW 26.67 COL 10.5 COLON-ALIGNED
     FILL-IN-KARBKOD AT ROW 26.75 COL 64.5 COLON-ALIGNED
     "Till" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 1.25 COL 71.5
          FONT 17
     "Från" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 1.25 COL 21.25
          FONT 17
     RECT-3 AT ROW 25.25 COL 4
     RECT-4 AT ROW 25.25 COL 56
     IMAGE-10 AT ROW 25.46 COL 5.13
     IMAGE-11 AT ROW 25.46 COL 56.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 27.54
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: kalkbertemp T "?" NO-UNDO temp-db kalkbertemp
      TABLE: loptemp T "?" NO-UNDO temp-db loptemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Window 1"
         HEIGHT             = 27.92
         WIDTH              = 125
         MAX-HEIGHT         = 29.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 29.42
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_BERKALK RAD_VAL FRAME-A */
/* BROWSE-TAB BRW_KLG BRW_BERKALK FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BERKALK
/* Query rebuild information for BROWSE BRW_BERKALK
     _TblList          = "Temp-Tables.kalkbertemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.kalkbertemp.ARBKOD
"kalkbertemp.ARBKOD" "Arbkod" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.kalkbertemp.LOPNR
     _FldNameList[3]   > Temp-Tables.kalkbertemp.BENAMNING
"kalkbertemp.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.kalkbertemp.ENHET
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BERKALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KLG
/* Query rebuild information for BROWSE BRW_KLG
     _TblList          = "Temp-Tables.loptemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.loptemp.ARBKOD
"loptemp.ARBKOD" "Arbkod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.loptemp.LOPNR
"loptemp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.loptemp.BENAMNING
"loptemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.loptemp.ENHET
"loptemp.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.loptemp.ANTAL
"loptemp.ANTAL" "Antal" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KLG */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_KLG
&Scoped-define SELF-NAME BRW_KLG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KLG WINDOW-1
ON ROW-LEAVE OF BRW_KLG IN FRAME FRAME-A /* Senaste kostnadskatalogen */
DO:
   IF loptemp.ANTAL:VISIBLE IN BROWSE BRW_KLG = TRUE THEN DO:
      IF AVAILABLE loptemp THEN DO:
         DISPLAY loptemp.ANTAL WITH BROWSE BRW_KLG.
         loptemp.ANTAL = INPUT BROWSE BRW_KLG loptemp.ANTAL.
      END.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME loptemp.ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loptemp.ANTAL BRW_KLG _BROWSE-COLUMN WINDOW-1
ON ENTRY OF loptemp.ANTAL IN BROWSE BRW_KLG /* Antal */
DO:
   IF loptemp.ANTAL:VISIBLE IN BROWSE BRW_KLG = TRUE THEN DO:
      IF AVAILABLE loptemp THEN DO:
         DISPLAY loptemp.ANTAL WITH BROWSE BRW_KLG.
      END.
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loptemp.ANTAL BRW_KLG _BROWSE-COLUMN WINDOW-1
ON LEAVE OF loptemp.ANTAL IN BROWSE BRW_KLG /* Antal */
DO:
   IF loptemp.ANTAL:VISIBLE IN BROWSE BRW_KLG = TRUE THEN DO:
      IF AVAILABLE loptemp THEN DO:
        loptemp.ANTAL = INPUT BROWSE BRW_KLG loptemp.ANTAL.     
         DISPLAY loptemp.ANTAL WITH BROWSE BRW_KLG.
      END.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:   
   status-ok = BRW_BERKALK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE kalkbertemp THEN DO:
      MESSAGE "Välj något!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {muswait.i}   
   IF AVAILABLE kalkbertemp THEN DO:      
      EMPTY TEMP-TABLE ekalkbertemp NO-ERROR. 
      CREATE ekalkbertemp.
      BUFFER-COPY kalkbertemp TO ekalkbertemp.
   END.

   
   IF valgrupp NE ? THEN DO:
      FIND FIRST ekalkbertemp NO-LOCK NO-ERROR.
      FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = valgrupp NO-LOCK NO-ERROR.
      MESSAGE "Är det korrekt att Ni vill ta bort kalkylkod: " + ekalkbertemp.ARBKOD + " " + STRING(ekalkbertemp.lopnr,Guru.Konstanter:varforetypchar[6]) +
      " på alla ställen där det förekommer inom valda konstruktionsgrupper?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
   END.
   ELSE DO:
      FIND FIRST ekalkbertemp NO-LOCK NO-ERROR.
      MESSAGE "Är det korrekt att Ni vill ta bort kalkylkod: " + ekalkbertemp.ARBKOD + " " + STRING(ekalkbertemp.lopnr,Guru.Konstanter:varforetypchar[6]) +
      " på alla ställen där det förekommer?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
   END.
   IF svar THEN DO:            
      RUN bortkalkber_UI IN mtxtillyapph (INPUT valgrupp,INPUT TABLE ekalkbertemp,INPUT TABLE grupp_temp,INPUT varalla, INPUT rsch).      
      DELETE kalkbertemp.
      RUN selnextprevrow_UI IN brwproc[1]. 
      RUN refreshbrw_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1]. 
      RUN title_UI IN brwproc[1].
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYT WINDOW-1
ON CHOOSE OF BTN_BYT IN FRAME FRAME-A /* Ersätt */
DO:     
   status-ok = BRW_BERKALK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE kalkbertemp THEN DO:
      MESSAGE "Välj någon kalkylkod i Från-vallistan." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE kalkbertemp THEN DO:
      {muswait.i}
      EMPTY TEMP-TABLE ekalkbertemp NO-ERROR. 
      CREATE ekalkbertemp.
      BUFFER-COPY kalkbertemp TO ekalkbertemp.
   END.
   EMPTY TEMP-TABLE extraloptemp NO-ERROR. 
   status-ok = BRW_KLG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE loptemp THEN DO:     
      CREATE extraloptemp.
      BUFFER-COPY loptemp TO extraloptemp.      
   END.
   ELSE DO:
      MESSAGE "Du har inte valt något kalkylkod i Till-vallistan!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF ekalkbertemp.ARBKOD = extraloptemp.ARBKOD AND ekalkbertemp.LOPNR = extraloptemp.LOPNR THEN DO:
      MESSAGE "Du kan inte ersätta samma kalkylkod !" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST extraloptemp NO-LOCK NO-ERROR.
   IF valgrupp = ? THEN DO:            
      MESSAGE "Är det korrekt att Ni vill ersätta kalkylkod : " + ekalkbertemp.ARBKOD + " " + STRING(ekalkbertemp.LOPNR,Guru.Konstanter:varforetypchar[6]) +
      " med kalkylkod  : " + extraloptemp.ARBKOD + " " + STRING(extraloptemp.LOPNR,Guru.Konstanter:varforetypchar[6]) + " på alla konstruktioner?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE DO:
      MESSAGE "Är det korrekt att Ni vill ersätta kalkylkod : " + ekalkbertemp.ARBKOD + " " + STRING(ekalkbertemp.LOPNR,Guru.Konstanter:varforetypchar[6]) +
      " med kalkylkod  : " + extraloptemp.ARBKOD + " " + STRING(extraloptemp.LOPNR,Guru.Konstanter:varforetypchar[6]) + " på alla valda konstruktionsgrupper?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   IF svar THEN DO:
      RUN ersattkalkber_UI IN mtxtillyapph (INPUT valgrupp,INPUT TABLE ekalkbertemp,INPUT TABLE extraloptemp,INPUT TABLE grupp_temp,INPUT varalla, INPUT rsch).      
      RUN laddakalkber_UI IN mtxtillyapph (INPUT TABLE grupp_temp,OUTPUT TABLE kalkbertemp,INPUT varalla, INPUT rsch).            
      RUN openbdynspec_UI IN brwproc[1].             
      FIND FIRST kalkbertemp WHERE kalkbertemp.ARBKOD = extraloptemp.ARBKOD AND  kalkbertemp.LOPNR = extraloptemp.LOPNR NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kalkbertemp)).
      RUN lastselectdyn_UI IN brwproc[1].
      RUN title_UI IN brwproc[1].
      MESSAGE "Byte av kalkylkod klart!"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TILL WINDOW-1
ON CHOOSE OF BTN_TILL IN FRAME FRAME-A /* Lägg till */
DO:   
   status-ok = BRW_BERKALK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE kalkbertemp THEN DO:
      MESSAGE "Välj någon kalkylkod i Från-vallistan." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE kalkbertemp THEN DO:
      {muswait.i}
      EMPTY TEMP-TABLE ekalkbertemp NO-ERROR. 
      CREATE ekalkbertemp.
      BUFFER-COPY kalkbertemp TO ekalkbertemp.
   END.
   EMPTY TEMP-TABLE extraloptemp NO-ERROR. 
   status-ok = BRW_KLG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE loptemp THEN DO:     
      CREATE extraloptemp.
      BUFFER-COPY loptemp TO extraloptemp.      
   END.
   ELSE DO:
      MESSAGE "Du har inte valt något kalkylkod i Till-vallistan!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF ekalkbertemp.ARBKOD = extraloptemp.ARBKOD AND ekalkbertemp.LOPNR = extraloptemp.LOPNR THEN DO:
      MESSAGE "Du kan inte lägga till samma kalkylkod !" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST extraloptemp NO-LOCK NO-ERROR.
   IF valgrupp = ? THEN DO:            
      MESSAGE "Är det korrekt att Ni överallt där kalkylkod : " + ekalkbertemp.ARBKOD + " " + STRING(ekalkbertemp.LOPNR,Guru.Konstanter:varforetypchar[6]) +
      " finns, vill lägga till kalkylkod  : " + extraloptemp.ARBKOD + " " + STRING(extraloptemp.LOPNR,Guru.Konstanter:varforetypchar[6]) + " på alla konstruktioner?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE DO:
      MESSAGE "Är det korrekt att Ni överallt där kalkylkod : " + ekalkbertemp.ARBKOD + " " + STRING(ekalkbertemp.LOPNR,Guru.Konstanter:varforetypchar[6]) +
      " finns, vill lägga till kalkylkod  : " + extraloptemp.ARBKOD + " " + STRING(extraloptemp.LOPNR,Guru.Konstanter:varforetypchar[6]) + " på alla valda konstruktionsgrupper?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   IF svar THEN DO:

      RUN laggtillkalkber_UI IN mtxtillyapph (INPUT valgrupp,INPUT TABLE ekalkbertemp,INPUT TABLE extraloptemp,INPUT TABLE grupp_temp,INPUT varalla, INPUT rsch).
      /*RUN ersattkalkber_UI IN mtxtillyapph (INPUT valgrupp,INPUT TABLE ekalkbertemp,INPUT TABLE extraloptemp,INPUT TABLE grupp_temp,INPUT varalla, INPUT rsch).*/
      RUN laddakalkber_UI IN mtxtillyapph (INPUT TABLE grupp_temp,OUTPUT TABLE kalkbertemp,INPUT varalla, INPUT rsch).
            
      RUN openbdynspec_UI IN brwproc[1].             
      FIND FIRST kalkbertemp WHERE kalkbertemp.ARBKOD = extraloptemp.ARBKOD AND  kalkbertemp.LOPNR = extraloptemp.LOPNR NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kalkbertemp)).
      RUN lastselectdyn_UI IN brwproc[1].
      RUN title_UI IN brwproc[1].
      MESSAGE "Byte av kalkylkod klart!"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
   /*EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE byt_mtrl NO-ERROR. 
   status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE mtrlbertemp THEN DO:
      MESSAGE "Välj något materiel." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   mtrl_rowid = mtrlbertemp.MTRLROW.
   CREATE spec_mtrl.
   ASSIGN
   spec_mtrl.ENR = mtrlbertemp.ENR
   spec_mtrl.BENAMNING = mtrlbertemp.BENAMNING
   spec_mtrl.ENHET = mtrlbertemp.ENHET
   spec_mtrl.NPRIS = mtrlbertemp.PRIS
   spec_mtrl.LEVKOD = mtrlbertemp.LEVKOD
   spec_mtrl.BERKVANT = 1
   spec_mtrl.SATS = FALSE.
  
   FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
   IF satsvar = FALSE THEN DO: 
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE mtrltemp THEN DO:
         mtrl_rowid = mtrltemp.MTRLROW.
         CREATE byt_mtrl.
         ASSIGN
         byt_mtrl.ENR = mtrltemp.ENR
         byt_mtrl.BENAMNING = mtrltemp.BENAMNING
         byt_mtrl.ENHET = mtrltemp.ENHET
         byt_mtrl.NPRIS = mtrltemp.NPRIS
         byt_mtrl.LEVKOD = mtrltemp.LEVKOD
         byt_mtrl.BERKVANT = mtrltemp.BERKVANT
         byt_mtrl.SATS = FALSE.
      END.
      ELSE DO:
         MESSAGE "Du har inte valt något materiel!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO:
      status-ok = BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE satstemp THEN DO:
         mtrl_rowid = satstemp.SATSROW.
         CREATE byt_mtrl.
         ASSIGN
         byt_mtrl.ENR = satstemp.ENR
         byt_mtrl.BENAMNING = satstemp.BENAMNING
         byt_mtrl.ENHET = satstemp.ENHET
         byt_mtrl.NPRIS = satstemp.PRIS
         byt_mtrl.LEVKOD = satstemp.LEVKOD
         byt_mtrl.BERKVANT = satstemp.ANTAL.
      END.
      ELSE DO:
         MESSAGE "Du har inte valt något materiel!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.     
   IF byt_mtrl.ENR = spec_mtrl.ENR THEN DO:
      MESSAGE "Du kan inte lägga till samma " LC(Guru.Konstanter:genk) "!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST byt_mtrl NO-LOCK NO-ERROR.
   IF valgrupp = ? THEN DO:            
      MESSAGE "Är det korrekt att Ni vill lägga till " + LC(Guru.Konstanter:genk) + ":" + byt_mtrl.ENR + " där " + LC(Guru.Konstanter:genk) + ":" +
      spec_mtrl.ENR + " finns på alla konstruktioner?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE DO:
      MESSAGE "Är det korrekt att Ni vill lägga till " + LC(Guru.Konstanter:genk) + ":" + byt_mtrl.ENR + " där " + LC(Guru.Konstanter:genk) + ":" +
      spec_mtrl.ENR + " finns på alla valda konstruktionsgrupper?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   IF svar THEN DO:
      {muswait.i}

         /*
      RUN laggtill_UI IN mtxtillyapph (INPUT valgrupp, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl, INPUT TABLE grupp_temp).                  
      */
      RUN laggtill_UI IN mtxtillyapph (INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl, INPUT TABLE grupp_temp,INPUT varalla).                  
      {musarrow.i}
      enrspar = byt_mtrl.ENR.
      /*
      RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_lev,INPUT valgrupp,OUTPUT TABLE mtrlbertemp).
      */
      
      RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_levfran,INPUT TABLE grupp_temp,OUTPUT TABLE mtrlbertemp,INPUT varalla, INPUT erslev).
         
      IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
         /*nettopris beredning adm*/                
         EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
         RUN mbnettomark_UI IN nettoh (INPUT TABLE mtrlbertemp, INPUT-OUTPUT TABLE ikmtrltemp ).                           
      END.
      RUN openbdynspec_UI IN brwproc[1].             
      FIND FIRST mtrlbertemp WHERE mtrlbertemp.ENR = enrspar NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrlbertemp)).
      RUN lastselectdyn_UI IN brwproc[1].
      MESSAGE "Artikel har lagts till!"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.           */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-1
ON CHOOSE OF BTN_VISA IN FRAME FRAME-A /* Visa */
DO:   
   
   status-ok = BRW_BERKALK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE kalkbertemp THEN DO:
      {muswait.i}
      EMPTY TEMP-TABLE ekalkbertemp NO-ERROR. 
      CREATE ekalkbertemp.
      BUFFER-COPY kalkbertemp TO ekalkbertemp.      
      {AVBGOM.I}
      RUN KAKONSTU2.W (INPUT TABLE ekalkbertemp,INPUT rsch ).
      {AVBFRAM.I}
      {musarrow.i}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-A
DO:
   /*IF loptemp.ANTAL:VISIBLE IN BROWSE BRW_KLG = TRUE THEN DO:
      IF AVAILABLE loptemp THEN DO:
         APPLY "LEAVE" TO loptemp.ANTAL IN BROWSE BRW_KLG.
         loptemp.ANTAL:VISIBLE IN BROWSE BRW_KLG = FALSE.
      END.   
   END.*/
   loptemp.ANTAL:VISIBLE IN BROWSE BRW_KLG = FALSE.
   ASSIGN
   BRW_KLG:HIDDEN = FALSE
   BTN_BYT:HIDDEN = TRUE
   BTN_TILL:HIDDEN = TRUE
   BTN_BORT:HIDDEN = TRUE. 
      
   RAD_VAL = INPUT RAD_VAL.
   IF RAD_VAL = 1 THEN DO:
      BTN_BYT:HIDDEN = FALSE.      
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      loptemp.ANTAL:VISIBLE IN BROWSE BRW_KLG = TRUE.
      BTN_TILL:HIDDEN = FALSE.      
   END.
   ELSE DO:
      ASSIGN      
      BRW_KLG:HIDDEN = TRUE
      BTN_BORT:HIDDEN = FALSE.      
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BERKALK
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
   IF VALID-HANDLE(mtxtillyapph) THEN DELETE PROCEDURE mtxtillyapph.
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(nettoh) THEN DELETE PROCEDURE nettoh NO-ERROR.      
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
   {muswait.i}   
   {ALLSTARTDYN.I}
   
   IF valgrupp = ? THEN DO:
      {&WINDOW-NAME}:TITLE = "Byte av kalkylkod på ALLA konstruktioner".      
   END.
   ELSE DO:
      kgrvar = "".
      FOR EACH grupp_temp:
         kgrvar = kgrvar + " " + grupp_temp.BENAMNING.
      END.
      FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = valgrupp 
      NO-LOCK NO-ERROR.
      {&WINDOW-NAME}:TITLE = "Byte av kalkylkod på valda konstruktionsgrupper: " + kgrvar.
   END.  
   EMPTY TEMP-TABLE kalkbertemp NO-ERROR.
   ASSIGN
   BTN_BYT:LABEL = "Ersätt kalkylkod"
   BTN_TILL:LABEL = "Lägg till kalkylkod"
   BTN_BORT:LABEL = "Ta bort kalkylkod".
   IF rsch = 1 THEN DO:
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Kalkylkoder beredningsupplägg").                
   END.
   IF rsch = 2 THEN DO:
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Kalkylkoder schaktupplägg").                
   END.
   IF rsch = 3 THEN DO:
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Kalkylkoder berednings- och schakt-upplägg").                
   END.
   
   RUN laddakalkber_UI IN mtxtillyapph (INPUT TABLE grupp_temp,OUTPUT TABLE kalkbertemp,INPUT varalla, INPUT rsch).
   FIND FIRST kalkbertemp NO-LOCK NO-ERROR.
   IF AVAILABLE kalkbertemp THEN DO:
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].        
   END.
   RUN Loptempn_UI IN KalkClasserStart (INPUT 4, OUTPUT TABLE loptemp).
   FIND FIRST loptemp NO-LOCK NO-ERROR.
   IF AVAILABLE loptemp THEN DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
      RUN openbdynspec_UI IN brwproc[2].        
   END.
   loptemp.ANTAL:VISIBLE IN BROWSE BRW_KLG = FALSE.
   RUN enable_UI.   
   {FRMSIZE.I}      
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_VISA:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   Guru.GlobalaVariabler:colrighth = BTN_BYT:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   ASSIGN
   BTN_TILL:ROW = BTN_BYT:ROW
   BTN_BORT:ROW = BTN_BYT:ROW.      
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   {musarrow.i}  
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BERKALK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_KLG:HANDLE IN FRAME {&FRAME-NAME}).      
   
   RUN setcolindex_UI IN brwproc[1] (INPUT "ARBKOD BY LOPNR").
   RUN setcolindex_UI IN brwproc[2] (INPUT "ARBKOD BY LOPNR").
   
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-BARBKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ARBKOD").
   RUN addfillin_UI IN brwproc[2] 
      (INPUT FILL-IN-KARBKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ARBKOD").
   
   RUN settitlenum_UI IN brwproc[1] (INPUT TRUE).
   RUN settitlenum_UI IN brwproc[2] (INPUT TRUE).
   IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN KTXTILLYAPP.P PERSISTENT SET mtxtillyapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KTXTILLYAPP.P PERSISTENT SET mtxtillyapph.
   END.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avmark_UI WINDOW-1 
PROCEDURE avmark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT PARAMETER vber AS INTEGER NO-UNDO.
   IF vber = 1 THEN DO:
      status-ok = BRW_KLG:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   END.
   IF vber = 2 THEN DO:
      status-ok = BRW_BERKALK:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.      
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
  DISPLAY RAD_VAL FILL-IN-BARBKOD FILL-IN-KARBKOD 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-3 RECT-4 IMAGE-10 IMAGE-11 RAD_VAL BRW_BERKALK BRW_KLG BTN_VISA 
         BTN_BYT BTN_TILL BTN_BORT BTN_AVB FILL-IN-BARBKOD FILL-IN-KARBKOD 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

