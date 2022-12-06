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

  Created: 08/14/96 -  1:31 pm

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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{DEFSOK.I}
{ALLDEF.I}
{MTRLTEMP.I}
{HUVLEVTEMP.I}
{LEVTEMP.I}
{SOKDEF.I}
{MTTEMP.I}   
DEFINE NEW SHARED VARIABLE kundoffproch AS HANDLE NO-UNDO. /* KUNDOFFAPP.P */
DEFINE NEW SHARED VARIABLE avb AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE kalksprec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO.    
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO. 
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE entrymtrlantal AS LOGICAL NO-UNDO.
DEFINE VARIABLE entrykpris AS LOGICAL NO-UNDO.
DEFINE VARIABLE vadkalk AS INTEGER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE val AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO. /* MTRLHMT.P */
DEFINE VARIABLE antbest AS DECIMAL NO-UNDO. 
DEFINE VARIABLE kodlev AS CHARACTER NO-UNDO. 
DEFINE VARIABLE nettooff AS DECIMAL FORMAT "->9.99" NO-UNDO.   
DEFINE VARIABLE offlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE totbrutto AS DECIMAL NO-UNDO.     
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE VARIABLE FILL-IN-RABATT AS INTEGER NO-UNDO.

DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlevnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE antvar AS INTEGER NO-UNDO.

{SOKMTRL.I}    
{SOKTEMPNOUN.I}
/*{EGENBEN.I}*/
&Scoped-define SHARED SHARED
{SMTRL.I}
{HOPPSEK2W.I}
DEFINE BUFFER bmtrlbuf FOR bmtrl_mtrl.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp bmtrl_mtrl

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL bmtrl_mtrl.Enr bmtrl_mtrl.Benamning ~
bmtrl_mtrl.Enhet bmtrl_mtrl.BERKVANT bmtrl_mtrl.KPRIS bmtrl_mtrl.NPRIS ~
bmtrl_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL bmtrl_mtrl.BERKVANT ~
bmtrl_mtrl.KPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL bmtrl_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL bmtrl_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH bmtrl_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH bmtrl_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL bmtrl_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL bmtrl_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 IMAGE-6 BRW_HLEV BRW_MTRL BTN_IEXC ~
FBTN_SKAPA BTN_OVER FBTN_OFF FBTN_VAL FBTN_VISA btn_back FBTN_SKRIV BTN_LEV ~
CMB_LEV btn_uppant BTN_SPEC BTN_BORT BTN_TOTP BTN_FINN FILL-IN-ENR ~
FILL-IN-BEN RAD_SOK FBTN_OK BTN_AVB FILL-IN-SOKALT 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV FILL-IN-ENR FILL-IN-BEN RAD_SOK ~
FILL-IN-SOKALT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FINN 
     LABEL "I lager":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_LEV 
     LABEL "Huvudleverantör" 
     SIZE 18 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_SPEC 
     LABEL "Spec.mtrl":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_TOTP 
     LABEL "Totalt offertpris":L 
     SIZE 12 BY 1.

DEFINE BUTTON btn_uppant 
     LABEL "Antal":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_IEXC 
     LABEL "Import Exel" 
     SIZE 14 BY 1 TOOLTIP "Enr första kolumnen, antal i andra (blank = 1), lev i tredje (blank = vald leverantör beredning),ev benämning i fjärde.".

DEFINE BUTTON FBTN_OFF 
     LABEL "Offertpris":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK AUTO-END-KEY 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKAPA 
     LABEL "Skapa Best.":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VAL 
     LABEL "Val av mtrl":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-6
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 33.88 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.25 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      bmtrl_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(9)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 20
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>>9.99":U WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 44.5 BY 20
         TITLE "Materiellista huvudleverantör.".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      bmtrl_mtrl.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      bmtrl_mtrl.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 22
      bmtrl_mtrl.Enhet COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      bmtrl_mtrl.BERKVANT COLUMN-LABEL "Antal" FORMAT "->>>>9.99":U
      bmtrl_mtrl.KPRIS COLUMN-LABEL "Kundpris" FORMAT "->>>>>9.99":U
      bmtrl_mtrl.NPRIS COLUMN-LABEL "Netto pris" FORMAT ">>>>>9.99":U
      bmtrl_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(4)":U WIDTH 3
  ENABLE
      bmtrl_mtrl.BERKVANT
      bmtrl_mtrl.KPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 61 BY 20
         TITLE "Materielspecifikation".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_HLEV AT ROW 1.75 COL 1.5
     BRW_MTRL AT ROW 1.75 COL 50.25
     BTN_IEXC AT ROW 6.79 COL 111.5 WIDGET-ID 2
     FBTN_SKAPA AT ROW 8 COL 111.5
     BTN_OVER AT ROW 8.29 COL 46.13
     FBTN_OFF AT ROW 9.08 COL 111.5
     FBTN_VAL AT ROW 10.21 COL 111.5
     FBTN_VISA AT ROW 11.29 COL 111.5
     btn_back AT ROW 12.04 COL 46.13
     FBTN_SKRIV AT ROW 12.42 COL 111.5
     BTN_LEV AT ROW 22.04 COL 23
     CMB_LEV AT ROW 22.08 COL 21 COLON-ALIGNED
     btn_uppant AT ROW 22.08 COL 55.5
     BTN_SPEC AT ROW 22.08 COL 68.5
     BTN_BORT AT ROW 22.08 COL 81.5
     BTN_TOTP AT ROW 22.08 COL 94.5
     BTN_FINN AT ROW 23.5 COL 55.5
     FILL-IN-ENR AT ROW 23.71 COL 21 COLON-ALIGNED
     FILL-IN-BEN AT ROW 24.71 COL 21 COLON-ALIGNED
     RAD_SOK AT ROW 25.92 COL 18 NO-LABEL
     FBTN_OK AT ROW 26.08 COL 94.38
     BTN_AVB AT ROW 26.08 COL 111.5
     FILL-IN-SOKALT AT ROW 26 COL 3.38 NO-LABEL
     RECT-4 AT ROW 23.46 COL 2
     IMAGE-6 AT ROW 23.67 COL 2.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 26.63.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: ? T "?" NO-UNDO temp-db bmtrl_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Materielberedning"
         HEIGHT             = 26.63
         WIDTH              = 125
         MAX-HEIGHT         = 28.54
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.54
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
/* BROWSE-TAB BRW_HLEV IMAGE-6 FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_HLEV FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 50000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BTN_FINN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       btn_uppant:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.mtrltemp.Enr
"mtrltemp.Enr" ? "X(9)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.bmtrl_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bmtrl_mtrl.Enr
"bmtrl_mtrl.Enr" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.bmtrl_mtrl.Benamning
"bmtrl_mtrl.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.bmtrl_mtrl.Enhet
"bmtrl_mtrl.Enhet" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.bmtrl_mtrl.BERKVANT
"bmtrl_mtrl.BERKVANT" "Antal" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.bmtrl_mtrl.KPRIS
"bmtrl_mtrl.KPRIS" "Kundpris" "->>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.bmtrl_mtrl.NPRIS
"bmtrl_mtrl.NPRIS" "Netto pris" ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.bmtrl_mtrl.LEVKOD
"bmtrl_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON START-SEARCH OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör. */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_MTRL IN FRAME FRAME-A /* Materielspecifikation */
DO: 
   IF bmtrl_mtrl.LEVKOD = "0" THEN DO:
      MESSAGE "För att ändra lagerantalet, markera aktuell lev. och klicka på i lager."  
      VIEW-AS ALERT-BOX.      
   END.
   ELSE DO:         
      FIND off_mtrl WHERE off_mtrl.LEVKOD = bmtrl_mtrl.LEV NO-LOCK NO-ERROR.
      IF AVAILABLE off_mtrl THEN DO: 
         MESSAGE "Det finns ett totalt offertpris för aktuell leverantör. Ta bort totalt offertpris om du vill göra förndringar."
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         {muswait.i}           
         
         EMPTY TEMP-TABLE emtrl_mtrl NO-ERROR. 
         EMPTY TEMP-TABLE esok_mtrl  NO-ERROR. 
         EMPTY TEMP-TABLE espec_mtrl NO-ERROR. 
         EMPTY TEMP-TABLE emtrltemp  NO-ERROR. 
         CREATE emtrl_mtrl.
         BUFFER-COPY bmtrl_mtrl TO emtrl_mtrl.
         RUN ANTAL2.W (INPUT 1,
                       INPUT-OUTPUT TABLE emtrl_mtrl,
                       INPUT-OUTPUT TABLE esok_mtrl ,
                       INPUT-OUTPUT TABLE espec_mtrl,
                       INPUT-OUTPUT TABLE emtrltemp).
         FIND FIRST emtrl_mtrl NO-LOCK NO-ERROR.
         BUFFER-COPY emtrl_mtrl TO bmtrl_mtrl.
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(bmtrl_mtrl)).              
         RUN openbdynspec_UI IN brwproc[2].
         RUN lastselectdyn_UI IN brwproc[2].          
         {musarrow.i}
      END. 
   END.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON ROW-DISPLAY OF BRW_MTRL IN FRAME FRAME-A /* Materielspecifikation */
DO:
   DISPLAY bmtrl_mtrl.KPRIS bmtrl_mtrl.BERKVANT WITH BROWSE BRW_MTRL. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bmtrl_mtrl.BERKVANT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bmtrl_mtrl.BERKVANT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF bmtrl_mtrl.BERKVANT IN BROWSE BRW_MTRL /* Antal */
DO:
   entrymtrlantal = TRUE.
   DISPLAY bmtrl_mtrl.BERKVANT WITH BROWSE BRW_MTRL. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bmtrl_mtrl.BERKVANT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF bmtrl_mtrl.BERKVANT IN BROWSE BRW_MTRL /* Antal */
DO:
   entrymtrlantal = FALSE.
   IF bmtrl_mtrl.BERKVANT NE INPUT BROWSE BRW_MTRL bmtrl_mtrl.BERKVANT THEN DO:   
      bmtrl_mtrl.BERKVANT = INPUT BROWSE BRW_MTRL bmtrl_mtrl.BERKVANT.
      DISPLAY bmtrl_mtrl.BERKVANT WITH BROWSE BRW_MTRL. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bmtrl_mtrl.KPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bmtrl_mtrl.KPRIS BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF bmtrl_mtrl.KPRIS IN BROWSE BRW_MTRL /* Kundpris */
DO:
    entrykpris = TRUE.
    DISPLAY bmtrl_mtrl.KPRIS WITH BROWSE BRW_MTRL. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bmtrl_mtrl.KPRIS BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF bmtrl_mtrl.KPRIS IN BROWSE BRW_MTRL /* Kundpris */
DO:
   entrykpris = FALSE.
   IF bmtrl_mtrl.KPRIS NE INPUT BROWSE BRW_MTRL bmtrl_mtrl.KPRIS THEN DO:   
      bmtrl_mtrl.KPRIS = INPUT BROWSE BRW_MTRL bmtrl_mtrl.KPRIS.
      DISPLAY bmtrl_mtrl.KPRIS WITH BROWSE BRW_MTRL. 
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   RUN entryantal_UI.
   MESSAGE "OBS! Vill du spara dina ändringar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE IF NOT svar THEN DO:       
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
   END.                    
   ELSE DO:
      musz = musz.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO:   
   RUN entryantal_UI.
   RUN btnback_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   RUN entryantal_UI.
   MESSAGE "Vill du ta bort markerade poster?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Ta bort?" UPDATE svar AS LOGICAL.
   IF svar THEN DO:
         antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).
         IF bmtrl_mtrl.LEVKOD = "0" THEN DO:
            MESSAGE "För att ta bort lagerantalet, markera aktuell lev. och klicka på i lager. Sätt lager till 0."  
            VIEW-AS ALERT-BOX.      
         END.
         ELSE DO:
            FIND off_mtrl WHERE off_mtrl.LEVKOD = bmtrl_mtrl.LEV NO-LOCK NO-ERROR.
            IF AVAILABLE off_mtrl THEN DO: 
               MESSAGE "Det finns ett totalt offertpris för aktuell leverantör. Ta bort totalt offertpris om du vill göra förändringar."
               VIEW-AS ALERT-BOX.
            END.            
            ELSE DO:                  
               ASSIGN
               kodlev = bmtrl_mtrl.LEVKOD.   
               FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = bmtrl_mtrl.ENR AND bmtrlbuf.LEVKOD = "0" AND 
               bmtrlbuf.BERLEV = kodlev NO-ERROR.
               IF AVAILABLE bmtrlbuf THEN DO:                    
                  DELETE bmtrlbuf.
               END.  
               DELETE bmtrl_mtrl.                           
            END.
         END.      
         IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[2].
         antal_raknare = antal_raknare + 1.   
      END.       
      RUN openbdynspec_UI IN brwproc[2].
      RUN lastselectdyn_UI IN brwproc[2].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FINN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FINN WINDOW-1
ON CHOOSE OF BTN_FINN IN FRAME FRAME-A /* I lager */
DO: 
   RUN entryantal_UI.
   /*ej färdig skall visa lager*/
   IF bmtrl_mtrl.LEVKOD = "0" THEN DO:
      MESSAGE "För att ändra lagerantalet, markera aktuell lev och klicka på i lager." 
      VIEW-AS ALERT-BOX.
   END.
   ELSE DO: 
      FIND off_mtrl WHERE off_mtrl.LEVKOD = bmtrl_mtrl.LEV NO-LOCK NO-ERROR.
      IF AVAILABLE off_mtrl THEN DO: 
         MESSAGE "Det finns ett totalt offertpris för aktuell leverantör. Ta bort totalt offertpris om du vill göra förändringar."
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         {muswait.i} 
         
         EMPTY TEMP-TABLE emtrl_mtrl NO-ERROR. 
         CREATE emtrl_mtrl.
         BUFFER-COPY bmtrl_mtrl TO emtrl_mtrl.
         RUN ILAGER2.W (INPUT-OUTPUT TABLE emtrl_mtrl). 
         FIND FIRST emtrl_mtrl NO-LOCK NO-ERROR.
         BUFFER-COPY emtrl_mtrl TO bmtrl_mtrl.
         
         RUN openbdynspec_UI IN brwproc[2].
         {musarrow.i}
      END.   
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LEV WINDOW-1
ON CHOOSE OF BTN_LEV IN FRAME FRAME-A /* Huvudleverantör */
DO:
   RUN entryantal_UI.
   ASSIGN   
   FILL-IN-BEN:HIDDEN = FALSE
   FILL-IN-ENR:HIDDEN = FALSE
   BTN_LEV:HIDDEN = TRUE
   CMB_LEV:HIDDEN = FALSE
   CMB_LEV:SCREEN-VALUE = huvudlev
   vald_lev = vald_kundlev.   
   APPLY "VALUE-CHANGED" TO CMB_LEV.
   IF posok NE " " THEN DO:
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER WINDOW-1
ON CHOOSE OF BTN_OVER IN FRAME FRAME-A
DO:  
   RUN entryantal_UI.
   FIND FIRST off_mtrl WHERE off_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   IF NOT AVAILABLE off_mtrl THEN DO:
      antal_valda = BRW_HLEV:NUM-SELECTED-ROWS.         
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare).
         /*Niklas personlig spec_mtrl*/
         IF mtrltemp.LEVKOD BEGINS "99" THEN DO:
            FIND FIRST bmtrl_mtrl WHERE bmtrl_mtrl.ENR = mtrltemp.ENR AND bmtrl_mtrl.LEVKOD = "99" AND bmtrl_mtrl.BERLEV = " "
            NO-ERROR.
         END.
         ELSE DO:
            FIND FIRST bmtrl_mtrl WHERE bmtrl_mtrl.ENR = mtrltemp.ENR AND bmtrl_mtrl.LEVKOD = mtrltemp.LEVKOD AND bmtrl_mtrl.BERLEV = " "
            NO-ERROR.
         END.            
         IF AVAILABLE bmtrl_mtrl THEN bmtrl_mtrl.MARK = TRUE.
         ELSE DO:
            CREATE bmtrl_mtrl.
            ASSIGN
            bmtrl_mtrl.KALKNR = valkalknr
            bmtrl_mtrl.ENR = mtrltemp.ENR
            bmtrl_mtrl.BENAMNING = mtrltemp.BENAMNING
            bmtrl_mtrl.ENHET = mtrltemp.ENHET
            bmtrl_mtrl.BERKVANT = 1
            bmtrl_mtrl.KPRIS = mtrltemp.NPRIS
            bmtrl_mtrl.NPRIS = mtrltemp.NPRIS
            bmtrl_mtrl.BPRIS = mtrltemp.BPRIS            
            bmtrl_mtrl.MARK = TRUE.
            /*Niklas personlig spec_mtrl*/
            IF mtrltemp.LEVKOD BEGINS "99" THEN bmtrl_mtrl.LEVKOD = "99".           
            ELSE bmtrl_mtrl.LEVKOD = mtrltemp.LEVKOD.
            IF varforetypval[29] = 1  THEN DO:                 
               /*nettopris beredning inköp*/
               FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE ikmtrltemp THEN DO:                        
                  CREATE ikmtrltemp.
                  BUFFER-COPY mtrltemp TO ikmtrltemp.                  
               END.
            END.
         END.
         antal_raknare = antal_raknare + 1.
      END. 
      RUN openbdynspec_UI IN brwproc[2].
      DEFINE VARIABLE brow AS ROWID NO-UNDO.
      FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.MARK = TRUE NO-LOCK:
         RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(bmtrl_mtrl)).
         ASSIGN 
         brow = ROWID(bmtrl_mtrl)
         bmtrl_mtrl.MARK = FALSE.
      END.
      FIND FIRST bmtrl_mtrl WHERE ROWID(bmtrl_mtrl) = brow NO-LOCK NO-ERROR.
      BRW_HLEV:DESELECT-ROWS().      
   END.
   ELSE DO: 
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR.
      MESSAGE "Det finns ett totalt offertpris för " levtemp.LEVNAMN ".För att lägga till materiel, ta bort offertpris."
      VIEW-AS ALERT-BOX.
   END.                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPEC WINDOW-1
ON CHOOSE OF BTN_SPEC IN FRAME FRAME-A /* Spec.mtrl */
DO:
   RUN entryantal_UI.
   {muswait.i}
   {AVBGOM.I}
   RUN MTRLSANV.W.
   {AVBFRAM.I}
   RUN openbdynspec_UI IN brwproc[2].
   FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.MARK = TRUE NO-LOCK:
      RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(bmtrl_mtrl)).
      bmtrl_mtrl.MARK = FALSE.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TOTP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TOTP WINDOW-1
ON CHOOSE OF BTN_TOTP IN FRAME FRAME-A /* Totalt offertpris */
DO: 
   RUN entryantal_UI.
   IF AVAILABLE bmtrl_mtrl THEN DO:
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(bmtrl_mtrl)).              
   END.
   RUN KALKOPTOT.W (OUTPUT vadkalk).

   RUN offtotmtrl_UI IN kundoffproch (INPUT valkalknr,OUTPUT TABLE off_mtrl).
   IF vadkalk = 1 THEN DO:
      /*ok*/
      RUN hmtoff_UI IN kundoffproch (INPUT valkalknr,OUTPUT FILL-IN-RABATT).
      FOR EACH bmtrl_mtrl:
         bmtrl_mtrl.KPRIS = bmtrl_mtrl.NPRIS * FILL-IN-RABATT / 100.         
      END.
   END.
   IF vadkalk = 2 THEN DO:
      /*bort*/
      RUN hmtoff_UI IN kundoffproch (INPUT valkalknr,OUTPUT FILL-IN-RABATT).
      FOR EACH bmtrl_mtrl:
         bmtrl_mtrl.KPRIS = bmtrl_mtrl.NPRIS.
      END.
   END.
   IF vadkalk = 3 THEN DO:
      /*avbryt*/
      
   END.
   RUN openbdynspec_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_uppant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_uppant WINDOW-1
ON CHOOSE OF btn_uppant IN FRAME FRAME-A /* Antal */
DO: 
   RUN entryantal_UI.
   IF NOT AVAILABLE bmtrl_mtrl THEN DO:
      RETURN NO-APPLY.
   END.
   IF bmtrl_mtrl.LEVKOD = "0" THEN DO:
      MESSAGE "För att ändra lagerantalet, markera aktuell lev och klicka på i lager."  
      VIEW-AS ALERT-BOX.      
   END.
   ELSE DO: 
      ASSIGN
      antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
      antal_raknare = 1.    
      DO WHILE antal_raknare LE antal_valda:   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare). 
         FIND off_mtrl WHERE off_mtrl.LEVKOD = bmtrl_mtrl.LEV NO-LOCK NO-ERROR.
         IF AVAILABLE off_mtrl THEN DO: 
            MESSAGE "Det finns ett totalt offertpris för aktuell leverantör. För att ändra antalet, tabort offertpris."
            VIEW-AS ALERT-BOX.
         END.
         ELSE DO:      
            
            EMPTY TEMP-TABLE emtrl_mtrl NO-ERROR. 
            EMPTY TEMP-TABLE esok_mtrl  NO-ERROR. 
            EMPTY TEMP-TABLE espec_mtrl NO-ERROR. 
            EMPTY TEMP-TABLE emtrltemp  NO-ERROR. 
            CREATE emtrl_mtrl.
            BUFFER-COPY bmtrl_mtrl TO emtrl_mtrl.
            RUN ANTAL2.W (INPUT 1,
                          INPUT-OUTPUT TABLE emtrl_mtrl,
                          INPUT-OUTPUT TABLE esok_mtrl ,
                          INPUT-OUTPUT TABLE espec_mtrl,
                          INPUT-OUTPUT TABLE emtrltemp).
            FIND FIRST emtrl_mtrl NO-LOCK NO-ERROR.
            BUFFER-COPY emtrl_mtrl TO bmtrl_mtrl.
            
            ASSIGN       
            lev = bmtrl_mtrl.LEVKOD.          
            FIND FIRST bmtrlbuf WHERE bmtrlbuf.ENR = bmtrl_mtrl.ENR AND bmtrlbuf.LEVKOD = "0" AND 
            bmtrlbuf.BERLEV = lev NO-ERROR.
            IF AVAILABLE bmtrlbuf THEN DO: 
               antbest = bmtrlbuf.BERKVANT.
               bmtrl_mtrl.BERKVANT = bmtrl_mtrl.BESTANT - antbest.            
            END.                                     
            ELSE DO:
               bmtrl_mtrl.BERKVANT = bmtrl_mtrl.BESTANT.
            END.   
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(bmtrl_mtrl)).              
         END.    
         antal_raknare = antal_raknare + 1.            
      END.                                    
      status-ok = BRW_MTRL:DESELECT-ROWS().      
      RUN openbdynspec_UI IN brwproc[2].
      RUN lastselectdyn_UI IN brwproc[2].          
   END.  
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO:
   RUN entryantal_UI.
   {muswait.i}
   CMB_LEV = INPUT CMB_LEV.
   IF CMB_LEV NE huvudlev THEN DO:
      ASSIGN
      CMB_LEV:HIDDEN = TRUE
      BTN_LEV:HIDDEN = FALSE.
      /*Niklas personlig spec_mtrl*/
      IF CMB_LEV BEGINS globanv THEN DO:            
         vald_lev = "99" + Guru.Konstanter:globanv. 
      END.
      ELSE DO:
         FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV 
         USE-INDEX LEV NO-LOCK NO-ERROR.
         vald_lev = levtemp.LEVKOD. 
      END.      
      IF vald_lev BEGINS "99" THEN DO:        
         RUN initsok_UI (INPUT 2,INPUT "").
      END.      
      
   END.
   ELSE vald_lev = vald_kundlev.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrltemp.LEVKOD = '" + STRING(vald_lev) + "' AND mtrltemp.KALKNR = 0 ").
   RUN openbdynspec_UI IN brwproc[1].
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IEXC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IEXC WINDOW-1
ON CHOOSE OF BTN_IEXC IN FRAME FRAME-A /* Import Exel */
DO:   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE mttemp NO-ERROR. 
   EMPTY TEMP-TABLE felmex NO-ERROR. 
   fildir = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den excelfil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                      
      {muswait.i}             
      RUN MTRLEXELIN.P (INPUT fildir,INPUT vald_lev, OUTPUT TABLE mttemp ).         
      RUN enrvalexc_UI.
      IF varforetypval[29] = 1  THEN DO:                          
         /*nettopris kalkyl*/               
         EMPTY TEMP-TABLE ikmtrltemp NO-ERROR.       
         RUN kalknettomark_UI IN nettoh (INPUT TABLE bmtrl_mtrl, OUTPUT TABLE ikmtrltemp ).              
      END.   
      
      /*IF varforetypval[29] = 1  THEN DO:                 
         RUN linettomark2_UI IN nettoh (INPUT TABLE spec_mtrl, OUTPUT TABLE kmtrltemp APPEND).            
      END.*/
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
      RUN openbdynspec_UI IN brwproc[2].  
      RUN title_UI IN brwproc[2].
      FIND FIRST felmex NO-ERROR.
      IF AVAILABLE felmex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
         {AMERICANEUROPEAN.I}      
         OUTPUT TO VALUE(felfil).         
         PUT "Dessa enr blev ej inlästa från excel . Var god kontrollera!" AT 6.         
         PUT SKIP.         
         FOR EACH felmex:           
            PUT UNFORMATTED felmex.ENR AT 6 felmex.ANTAL AT 19 SUBSTRING(felmex.LEVKOD,1,3) AT 32 SUBSTRING(felmex.LEVNAMN,1,20) AT 42 SUBSTRING(felmex.BENAMNING,1,20) AT 65.  
            PUT SKIP.
         END.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
   END.               
   {musarrow.i}  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OFF WINDOW-1
ON CHOOSE OF FBTN_OFF IN FRAME FRAME-A /* Offertpris */
DO: 
   RUN entryantal_UI.
   {muswait.i}
   {AVBGOM.I}
   RUN OFFERT2.W.
   {AVBFRAM.I}
   RUN openbdynspec_UI IN brwproc[2].
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO: 
   {muswait.i}
   RUN entryantal_UI.
   RUN bestmtrlsparm_UI IN kundoffproch (INPUT FALSE,INPUT varforetypval[1],INPUT valkalknr,INPUT TABLE bmtrl_mtrl,INPUT TABLE off_mtrl).                                                   
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKAPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKAPA WINDOW-1
ON CHOOSE OF FBTN_SKAPA IN FRAME FRAME-A /* Skapa Best. */
DO:
   RUN entryantal_UI.
   {muswait.i} 
   FOR EACH bmtrl_mtrl.
      bmtrl_mtrl.SUMMA = bmtrl_mtrl.NPRIS * bmtrl_mtrl.BERKVANT.
   END.   
   {AVBGOM.I}
   RUN LEVTRP2.W.
   {AVBFRAM.I}
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO: 
   RUN entryantal_UI.
   RUN SKRIVVAL.W (INPUT FALSE).    
   IF musz = TRUE THEN musz = FALSE.   
   ELSE DO:                         
      {muswait.i}
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISABERM2.W.
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:
   RUN entryantal_UI.
   {muswait.i} 
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.
   {AVBGOM.I}   
   RUN VISABERM2.W.
   {AVBFRAM.I}   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:
   {muswait.i}
   {BENHMT2.I}
   felmedd = "".
   EMPTY TEMP-TABLE mtrltemp NO-ERROR.
   RUN benhmt_UI IN mtrlhmtapph (INPUT aosok,INPUT FALSE, INPUT begvar,INPUT vald_lev,
                                 OUTPUT felmedd,OUTPUT TABLE mtrltemp,OUTPUT TABLE satstemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      FIND FIRST mtrltemp WHERE mtrltemp.BENAMNING MATCHES aosok NO-LOCK NO-ERROR.
      IF AVAILABLE mtrltemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrltemp)).
      END.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrltemp.LEVKOD = '" + STRING(vald_lev) + "' AND mtrltemp.KALKNR = 0 ").
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON ANY-KEY OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO: 
   {muswait.i}
   {ENRHMT2.I}
   felmedd = "".
   EMPTY TEMP-TABLE mtrltemp NO-ERROR.
   RUN enrhmt_UI IN mtrlhmtapph (INPUT posok, INPUT begvar, INPUT vald_lev, OUTPUT felmedd, OUTPUT TABLE mtrltemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   ELSE DO:
      FIND FIRST mtrltemp WHERE mtrltemp.ENR MATCHES posok NO-LOCK NO-ERROR.
      IF AVAILABLE mtrltemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrltemp)).
      END.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrltemp.LEVKOD = '" + STRING(vald_lev) + "' AND mtrltemp.KALKNR = 0 ").
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].
      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SOK WINDOW-1
ON VALUE-CHANGED OF RAD_SOK IN FRAME FRAME-A
DO:
   RAD_SOK = INPUT RAD_SOK.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
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
   IF VALID-HANDLE(kundoffproch) THEN DELETE PROCEDURE kundoffproch.
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(nettoh) THEN DELETE PROCEDURE nettoh NO-ERROR.      
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
   {ALLSTARTDYN.I}      
      
   {muswait.i}
   FIND FIRST levtemp NO-ERROR.
   IF NOT AVAILABLE levtemp THEN RUN laddalev_UI.
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
      /*Niklas personlig spec_mtrl*/
      IF levtemp.LEVKOD = "99" THEN DO:
         status-ok = CMB_LEV:ADD-LAST(globanv + " " + levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
      END.
   END.    
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev USE-INDEX LEV NO-LOCK NO-ERROR.
   IF levtemp.LEVKOD = "0" OR levtemp.BORTTAG = TRUE THEN CMB_LEV:ADD-LAST(levtemp.LEVNAMN) IN FRAME {&FRAME-NAME}.
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   huvudlev = levtemp.LEVNAMN.
   bmtrl_mtrl.ENR:LABEL = Guru.Konstanter:genk.
   IF kalkmtrl = TRUE THEN DO:
      RUN bestmtrlhmt_UI IN kundoffproch (INPUT valkalknr,OUTPUT TABLE bmtrl_mtrl,OUTPUT TABLE off_mtrl).           
      FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.LEV NE "0".
         kodlev = bmtrl_mtrl.LEV.   
         FIND bmtrlbuf WHERE bmtrlbuf.ENR = bmtrl_mtrl.ENR AND bmtrlbuf.LEVKOD = "0" AND 
         bmtrlbuf.BERLEV = kodlev NO-ERROR.
         IF AVAILABLE bmtrlbuf THEN DO:
            bmtrl_mtrl.BESTANT = bmtrl_mtrl.BERKVANT + bmtrlbuf.BERKVANT.            
         END.                                                          
         ELSE DO:
            bmtrl_mtrl.BESTANT = bmtrl_mtrl.BERKVANT.
         END. 
      END.
      FOR EACH bmtrl_mtrl WHERE bmtrl_mtrl.LEVKOD = "0".
         bmtrl_mtrl.BESTANT = bmtrl_mtrl.BERKVANT.
      END.          
      IF varforetypval[29] = 1  THEN DO:                          
         /*nettopris kalkyl*/               
         EMPTY TEMP-TABLE ikmtrltemp NO-ERROR.       
         RUN kalknettomark_UI IN nettoh (INPUT TABLE bmtrl_mtrl, OUTPUT TABLE ikmtrltemp ).              
      END.   
   END.
   RUN openbdynspec_UI IN brwproc[2].   
   RUN enable_UI.      
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE
      bmtrl_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE
      bmtrl_mtrl.KPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE
      FBTN_SKAPA:HIDDEN = TRUE
      FBTN_OFF:HIDDEN = TRUE.
   END.
   ELSE DO:
      ASSIGN
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = TRUE
      bmtrl_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = TRUE.
   END.
   FBTN_OFF:HIDDEN = TRUE.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrltemp.LEVKOD = '" + STRING(vald_lev) + "' AND mtrltemp.KALKNR = 0 ").
   RUN openbdynspec_UI IN brwproc[1].
   {FRMSIZE.I}   
   IF globforetag NE "cELPA" THEN DO:
      BTN_FINN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   
   ASSIGN
   BTN_LEV:HIDDEN = TRUE      
   FBTN_VAL:HIDDEN = TRUE
   FBTN_SKRIV:HIDDEN = TRUE.
   Guru.GlobalaVariabler:collefth = ?.
   /*
   Guru.GlobalaVariabler:colrighth = FBTN_SKAPA:HANDLE.
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_OFF:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
/*    Guru.GlobalaVariabler:colrighth = FBTN_VAL:HANDLE.                                                   */
/*    RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  */
*/ 
/*
   Guru.GlobalaVariabler:colrighth = BTN_AVB:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
  */
   Guru.GlobalaVariabler:colrighth = FBTN_VISA:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   BTN_IEXC:LABEL = "Import Excel".
    Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVB:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_OK:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN setdefaultcolbyname_UI IN brwproc[2] (INPUT "ENR").
   RUN DYNARROW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_HLEV:HANDLE, INPUT BRW_MTRL:HANDLE ,
       INPUT ?, INPUT ? , INPUT ?, INPUT ?).
   RUN addmenuitem_UI IN brwproc[3] (INPUT BRW_HLEV:HANDLE,INPUT TRUE,INPUT "Visa beskrivning",INPUT "infoES_UI (INPUT 1)").
   RUN addmenuitem_UI IN brwproc[3] (INPUT BRW_MTRL:HANDLE,INPUT FALSE,INPUT "Visa beskrivning",INPUT "infoES_UI (INPUT 2)").
   RUN settitlenum_UI IN brwproc[1] (INPUT TRUE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN KUNDOFFAPP.P PERSISTENT SET kundoffproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
   END.
   ELSE DO:
      RUN KUNDOFFAPP.P PERSISTENT SET kundoffproch.      
   END.
   IF Guru.Konstanter:appcon THEN RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   ELSE RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnback_UI WINDOW-1 
PROCEDURE btnback_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).                      
      DELETE bmtrl_mtrl.
      RUN valrattright_UI IN brwproc[2] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(bmtrl_mtrl)).
      antal_raknare = antal_raknare + 1.   
   END.
   RUN refreshbrw_UI  IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].

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
  DISPLAY CMB_LEV FILL-IN-ENR FILL-IN-BEN RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-4 IMAGE-6 BRW_HLEV BRW_MTRL BTN_IEXC FBTN_SKAPA BTN_OVER 
         FBTN_OFF FBTN_VAL FBTN_VISA btn_back FBTN_SKRIV BTN_LEV CMB_LEV 
         btn_uppant BTN_SPEC BTN_BORT BTN_TOTP BTN_FINN FILL-IN-ENR FILL-IN-BEN 
         RAD_SOK FBTN_OK BTN_AVB FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enrvalexc_UI C-Win 
PROCEDURE enrvalexc_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN kollev_UI IN mtrlhmtapph (INPUT vald_lev,OUTPUT vlevnamn). 
   FOR EACH mttemp:        
      RUN kollev_UI IN mtrlhmtapph (INPUT mttemp.LEVKOD,OUTPUT lnamn).       
      IF lnamn = "" THEN DO:
         ASSIGN
          vlev = vald_lev
          mttemp.LEVKOD = vald_lev          
          mttemp.LEVNAMN = vlevnamn.
      END.
      ELSE DO: 
         ASSIGN
         vlev = mttemp.LEVKOD.
         mttemp.LEVNAMN = lnamn.
      END.
      EMPTY TEMP-TABLE emtrltemp NO-ERROR.                
      
      RUN hmtskap_UI IN mtrlhmtapph (INPUT mttemp.ENR,INPUT vlev,OUTPUT TABLE emtrltemp). 
      FIND FIRST emtrltemp NO-LOCK NO-ERROR.
      IF AVAILABLE emtrltemp THEN DO:         
         ASSIGN
         antvar = INTEGER(mttemp.ANTAL).         
         RUN over4_UI.
      END.
      ELSE DO:      
         
         CREATE felmex.       
         ASSIGN 
         felmex.ENR = mttemp.ENR
         felmex.ANTAL = mttemp.ANTAL
         felmex.LEVKOD = mttemp.LEVKOD
         felmex.LEVNAMN = mttemp.LEVNAMN                 
         felmex.BENAMNING = mttemp.BENAMNING.                          
      END.  
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryantal_UI WINDOW-1 
PROCEDURE entryantal_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   IF entrymtrlantal = TRUE THEN DO:
      APPLY "LEAVE" TO bmtrl_mtrl.BERKVANT IN BROWSE BRW_MTRL.
      
   END.
    IF entrykpris = TRUE THEN DO:      
      APPLY "LEAVE" TO bmtrl_mtrl.KPRIS IN BROWSE BRW_MTRL.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI WINDOW-1 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT PARAMETER valbrw AS INTEGER NO-UNDO. 
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valenr AS CHARACTER NO-UNDO.
   IF valbrw = 1 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD NO-LOCK NO-ERROR.      
      END.
   END.
   ELSE IF valbrw = 2 THEN DO:   
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF status-ok THEN DO:
         valenr = bmtrl_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = bmtrl_mtrl.LEVKOD NO-LOCK NO-ERROR.      
      END.
   END.
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}
     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initsok_UI WINDOW-1 
PROCEDURE initsok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT  PARAMETER vad AS INTEGER    NO-UNDO.
   DEFINE INPUT PARAMETER sokpa AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   IF vad = 1 THEN DO:
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV ".
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
   END.
   IF vad = 2 THEN DO:
     orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV ".
     tth = TEMP-TABLE mtrltemp:HANDLE.
     EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
     CREATE valsoktemp.
     ASSIGN 
     valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
     valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
     valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
     valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
     valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
     RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddalev_UI WINDOW-1 
PROCEDURE laddalev_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
   tthandle = TEMP-TABLE levtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT "").
   END.
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").

   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:
      vald_kundlev = huvlevtemp.LEVKOD.
   END.
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0"
      AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END.
   vald_lev = vald_kundlev.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over4_UI C-Win 
PROCEDURE over4_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   FIND FIRST bmtrl_mtrl WHERE bmtrl_mtrl.ENR = emtrltemp.ENR AND bmtrl_mtrl.LEVKOD = emtrltemp.LEVKOD 
   NO-LOCK NO-ERROR. 
   IF AVAILABLE bmtrl_mtrl THEN DO:          
      /*inlagt 20121127 lena om två artiklar är översatta till samma artikel*/
      bmtrl_mtrl.BERKVANT = bmtrl_mtrl.BERKVANT + antvar.        
   END.      
   ELSE DO:
      CREATE bmtrl_mtrl.       
      ASSIGN
      bmtrl_mtrl.KALKNR = valkalknr
      bmtrl_mtrl.ENR = emtrltemp.ENR
      bmtrl_mtrl.BENAMNING = emtrltemp.BENAMNING
      bmtrl_mtrl.ENHET = emtrltemp.ENHET
      bmtrl_mtrl.BERKVANT = antvar        
      bmtrl_mtrl.KPRIS = emtrltemp.NPRIS
      bmtrl_mtrl.NPRIS = emtrltemp.NPRIS
      bmtrl_mtrl.BPRIS = emtrltemp.BPRIS         
      bmtrl_mtrl.LEVKOD = emtrltemp.LEVKOD.
      bmtrl_mtrl.MARK = TRUE.      
   
   END.    
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-1 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.   
   IF brwh:NAME = "BRW_HLEV" THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
         IF mtrltemp.KUND = TRUE THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[28].            
         END.
         ELSE IF mtrltemp.KUND = ? THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[38].            
         END.
      END.
   END.
   IF brwh:NAME = "BRW_MTRL" THEN DO:
      IF AVAILABLE bmtrl_mtrl THEN DO:
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = bmtrl_mtrl.LEVKOD AND ikmtrltemp.ENR = bmtrl_mtrl.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:         
            ASSIGN
            bmtrl_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[28]
            bmtrl_mtrl.NPRIS:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[28].          
         END.
         ELSE DO:
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = bmtrl_mtrl.LEVKOD AND ikmtrltemp.ENR = bmtrl_mtrl.ENR AND ikmtrltemp.KALKNR = 0
            AND ikmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
            IF AVAILABLE ikmtrltemp THEN DO:         
               ASSIGN
               bmtrl_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[38]
               bmtrl_mtrl.NPRIS:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[38].          
            END.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

