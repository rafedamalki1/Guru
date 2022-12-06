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

  Created: 04/30/97 -  3:31 pm

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
&Scoped-define SHARED SHARED
{KONVALTEMP.I}    
{KONID.I}    
{LISTMTRL.I}    
{KOPPLINA.I} 
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
{ANNAMN.I}
&Scoped-define NEW NEW
{APPARAT.I}
DEFINE NEW SHARED VARIABLE lin_rowid AS ROWID NO-UNDO.   
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE firstkod AS INTEGER NO-UNDO. 
DEFINE VARIABLE gruppkod AS INTEGER NO-UNDO.
DEFINE VARIABLE typkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE cmbspar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gam_row AS ROWID NO-UNDO.
DEFINE VARIABLE gammal AS LOGICAL NO-UNDO.    
DEFINE VARIABLE finns AS LOGICAL NO-UNDO.  
DEFINE VARIABLE frannr AS INTEGER NO-UNDO.
DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
DEFINE VARIABLE setcolvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE frikortapph AS HANDLE NO-UNDO. 
DEFINE VARIABLE konstvaltapph AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE mtrl_tab NO-UNDO   
   FIELD BENAMNING AS CHARACTER LABEL "Benämning".    
                
DEFINE BUFFER koppbuff FOR kopp_lina.
DEFINE BUFFER koppbuff2 FOR kopp_lina. 
DEFINE BUFFER idbuff FOR kon_id.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_APP

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES apparattemp kon_id kopp_lina mtrl_tab

/* Definitions for BROWSE BRW_APP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_APP apparattemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_APP 
&Scoped-define QUERY-STRING-BRW_APP FOR EACH apparattemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_APP OPEN QUERY BRW_APP FOR EACH apparattemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_APP apparattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_APP apparattemp


/* Definitions for BROWSE BRW_FRAN                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FRAN kon_id.LINNR kon_id.NATNR ~
kon_id.FRI1 kon_id.FRI2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRAN 
&Scoped-define QUERY-STRING-BRW_FRAN FOR EACH kon_id NO-LOCK ~
    BY kon_id.LINNR ~
       BY kon_id.NATNR ~
        BY kon_id.FRI1 ~
         BY kon_id.FRI2
&Scoped-define OPEN-QUERY-BRW_FRAN OPEN QUERY BRW_FRAN FOR EACH kon_id NO-LOCK ~
    BY kon_id.LINNR ~
       BY kon_id.NATNR ~
        BY kon_id.FRI1 ~
         BY kon_id.FRI2.
&Scoped-define TABLES-IN-QUERY-BRW_FRAN kon_id
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRAN kon_id


/* Definitions for BROWSE BRW_LIN                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LIN kopp_lina.BENAMNING kopp_lina.METER 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LIN 
&Scoped-define QUERY-STRING-BRW_LIN FOR EACH kopp_lina NO-LOCK ~
    BY kopp_lina.ENR
&Scoped-define OPEN-QUERY-BRW_LIN OPEN QUERY BRW_LIN FOR EACH kopp_lina NO-LOCK ~
    BY kopp_lina.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_LIN kopp_lina
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LIN kopp_lina


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL mtrl_tab.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH mtrl_tab NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH mtrl_tab NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL mtrl_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL mtrl_tab


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_VAL RAD_VAL BRW_FRAN BRW_APP BRW_MTRL ~
FBTN_SKAPA FBTN_VISA FBTN_SKRIV BRW_LIN BTN_UPP BTN_NER FILL-IN-TYP ~
FILL-IN-SADR FILL-IN-ADR CMB_TYP FILL-IN-APPARAT FILL-IN-AR FILL-IN-SAK ~
FILL-IN-MAX FILL-IN-ANMARK BTN_AVB FILL-IN-GRUPP FILL-IN-ID1 FILL-IN-ID2 ~
FILL-IN-ID3 FILL-IN-ID4 
&Scoped-Define DISPLAYED-OBJECTS CMB_VAL RAD_VAL FILL-IN-TYP FILL-IN-SADR ~
FILL-IN-ADR CMB_TYP FILL-IN-APPARAT FILL-IN-AR FILL-IN-SAK FILL-IN-MAX ~
FILL-IN-ANMARK FILL-IN-GRUPP FILL-IN-ID1 FILL-IN-ID2 FILL-IN-ID3 ~
FILL-IN-ID4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_FRAN 
       MENU-ITEM m_Visa_information LABEL "Visa information".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "" 
     SIZE 4 BY 1.5.

DEFINE BUTTON BTN_UPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5.

DEFINE BUTTON FBTN_SKAPA 
     LABEL "Skapa" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_TYP AS CHARACTER FORMAT "X(6)":U 
     LABEL "Typ" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "Servis","In","Ut" 
     DROP-DOWN-LIST
     SIZE 10.13 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VAL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 33.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ADR AS CHARACTER FORMAT "X(30)":U 
     LABEL "Kabeladress" 
     VIEW-AS FILL-IN 
     SIZE 45.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANMARK AS CHARACTER FORMAT "X(50)":U 
     LABEL "Anmärkning" 
     VIEW-AS FILL-IN 
     SIZE 89.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-APPARAT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Apparat" 
     VIEW-AS FILL-IN 
     SIZE 45.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT ">>>>":U INITIAL ? 
     LABEL "År" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-GRUPP AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26.13 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-ID1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 10 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-ID2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 10 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-ID3 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 10 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-ID4 AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 10 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-MAX AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Max säkring (A)" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SADR AS CHARACTER FORMAT "X(30)":U 
     LABEL "Skåpadress" 
     VIEW-AS FILL-IN 
     SIZE 53.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SAK AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Säkring (A)" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TYP AS CHARACTER FORMAT "X(40)":U 
     LABEL "Skåptyp" 
     VIEW-AS FILL-IN 
     SIZE 26.13 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Skåp", 1,
"Apparat", 2
     SIZE 30.63 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_APP FOR 
      apparattemp SCROLLING.

DEFINE QUERY BRW_FRAN FOR 
      kon_id SCROLLING.

DEFINE QUERY BRW_LIN FOR 
      kopp_lina SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      mtrl_tab SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_APP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_APP WINDOW-1 _STRUCTURED
  QUERY BRW_APP NO-LOCK DISPLAY
      apparattemp.BENAMNING FORMAT "X(60)":U WIDTH 45
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 48.25 BY 6
         TITLE "Apparater".

DEFINE BROWSE BRW_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRAN WINDOW-1 _STRUCTURED
  QUERY BRW_FRAN NO-LOCK DISPLAY
      kon_id.LINNR FORMAT "X(14)":U
      kon_id.NATNR FORMAT "X(14)":U
      kon_id.FRI1 FORMAT ">>>>>>>>>>":U
      kon_id.FRI2 FORMAT ">>>>>>>>>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52.5 BY 14.5.

DEFINE BROWSE BRW_LIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LIN WINDOW-1 _STRUCTURED
  QUERY BRW_LIN NO-LOCK DISPLAY
      kopp_lina.BENAMNING FORMAT "x(256)":U WIDTH 40
      kopp_lina.METER FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 48.25 BY 6.

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      mtrl_tab.BENAMNING FORMAT "x(256)":U WIDTH 45
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 48.25 BY 6
         TITLE "Materiel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     CMB_VAL AT ROW 1.75 COL 1.5 NO-LABEL
     RAD_VAL AT ROW 1.92 COL 54.38 NO-LABEL
     BRW_FRAN AT ROW 3.33 COL 1.5
     BRW_APP AT ROW 3.33 COL 55.75
     BRW_MTRL AT ROW 3.33 COL 55.75
     FBTN_SKAPA AT ROW 7 COL 110.5
     FBTN_VISA AT ROW 8.08 COL 110.5
     FBTN_SKRIV AT ROW 9.21 COL 110.5
     BRW_LIN AT ROW 9.71 COL 55.75
     BTN_UPP AT ROW 10.5 COL 104.88
     BTN_NER AT ROW 13.17 COL 104.88
     FILL-IN-TYP AT ROW 18.42 COL 14.13 COLON-ALIGNED
     FILL-IN-SADR AT ROW 18.42 COL 53.63 COLON-ALIGNED
     FILL-IN-ADR AT ROW 19.96 COL 14.13 COLON-ALIGNED
     CMB_TYP AT ROW 19.96 COL 68.5 COLON-ALIGNED
     FILL-IN-APPARAT AT ROW 21.5 COL 14.13 COLON-ALIGNED
     FILL-IN-AR AT ROW 23.04 COL 14.13 COLON-ALIGNED
     FILL-IN-SAK AT ROW 23.04 COL 34.63 COLON-ALIGNED
     FILL-IN-MAX AT ROW 23.04 COL 59.38 COLON-ALIGNED
     FILL-IN-ANMARK AT ROW 24.63 COL 14 COLON-ALIGNED
     BTN_AVB AT ROW 24.63 COL 110.5
     FILL-IN-GRUPP AT ROW 15.88 COL 58.75 COLON-ALIGNED NO-LABEL
     FILL-IN-ID1 AT ROW 16.92 COL 58.75 COLON-ALIGNED NO-LABEL
     FILL-IN-ID2 AT ROW 16.92 COL 71.63 NO-LABEL
     FILL-IN-ID3 AT ROW 16.92 COL 82.5 NO-LABEL
     FILL-IN-ID4 AT ROW 16.92 COL 91.5 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.38 BY 25.13.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: apparattemp T "?" NO-UNDO temp-db apparattemp
      TABLE: ? T "?" NO-UNDO temp-db kon_id
      TABLE: ? T "?" NO-UNDO temp-db kopp_lina
      TABLE: ? T "?" NO-UNDO temp-db mtrl_tab
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Identifiering linor / kablar"
         HEIGHT             = 25.17
         WIDTH              = 124.5
         MAX-HEIGHT         = 27.54
         MAX-WIDTH          = 124.88
         VIRTUAL-HEIGHT     = 27.54
         VIRTUAL-WIDTH      = 124.88
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
                                                                        */
/* BROWSE-TAB BRW_FRAN RAD_VAL FRAME-A */
/* BROWSE-TAB BRW_APP BRW_FRAN FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_APP FRAME-A */
/* BROWSE-TAB BRW_LIN FBTN_SKRIV FRAME-A */
ASSIGN 
       BRW_APP:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

ASSIGN 
       BRW_FRAN:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_FRAN:HANDLE
       BRW_FRAN:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

/* SETTINGS FOR COMBO-BOX CMB_VAL IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-ID2 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-ID3 IN FRAME FRAME-A
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_APP
/* Query rebuild information for BROWSE BRW_APP
     _TblList          = "Temp-Tables.apparattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.apparattemp.BENAMNING
"apparattemp.BENAMNING" ? "X(60)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_APP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRAN
/* Query rebuild information for BROWSE BRW_FRAN
     _TblList          = "Temp-Tables.kon_id"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kon_id.LINNR|yes,Temp-Tables.kon_id.NATNR|yes,Temp-Tables.kon_id.FRI1|yes,Temp-Tables.kon_id.FRI2|yes"
     _FldNameList[1]   > Temp-Tables.kon_id.LINNR
"kon_id.LINNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_id.NATNR
"kon_id.NATNR" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_id.FRI1
"kon_id.FRI1" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kon_id.FRI2
"kon_id.FRI2" ? ">>>>>>>>>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LIN
/* Query rebuild information for BROWSE BRW_LIN
     _TblList          = "Temp-Tables.kopp_lina"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.kopp_lina.ENR|yes"
     _FldNameList[1]   > Temp-Tables.kopp_lina.BENAMNING
"kopp_lina.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kopp_lina.METER
"kopp_lina.METER" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LIN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.mtrl_tab"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.mtrl_tab.BENAMNING
"mtrl_tab.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_APP
&Scoped-define SELF-NAME BRW_APP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_APP WINDOW-1
ON VALUE-CHANGED OF BRW_APP IN FRAME FRAME-A /* Apparater */
DO:  
   IF RAD_VAL = 2 THEN DO:
      FILL-IN-APPARAT = apparattemp.BENAMNING.
      DISPLAY FILL-IN-APPARAT WITH FRAME {&FRAME-NAME}. 
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FRAN
&Scoped-define SELF-NAME BRW_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FRAN WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_FRAN IN FRAME FRAME-A
DO:  
   {muswait.i}
   RUN info_UI.      
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FRAN WINDOW-1
ON VALUE-CHANGED OF BRW_FRAN IN FRAME FRAME-A
DO:
   FOR EACH kopp_lina WHERE (kopp_lina.NUM1 = kon_id.NUM OR
   kopp_lina.NUM2 = kon_id.NUM) AND kopp_lina.METER > 0 AND kopp_lina.KORTKOD = ?:
      FIND FIRST koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
      AND koppbuff.KORTKOD = kon_id.NUM AND koppbuff.KABNR = kopp_lina.KABNR 
      NO-LOCK NO-ERROR.
      IF AVAILABLE koppbuff THEN DO:
         kopp_lina.KABNR2 = koppbuff.KABNR2.
      END.
   END.           
   RUN setcolindex_UI IN brwproc[4] (INPUT "KABNR2").   
   setcolvar = " WHERE " + "(" + " NUM1 = '" + STRING(kon_id.NUM) + "' OR NUM2 = '" + STRING(kon_id.NUM) + "' " + ")" + " AND KORTKOD = ? AND METER > '" + STRING("0") + "' " .   
   RUN setcolsortvar_UI IN brwproc[4] (INPUT setcolvar).   
   RUN openbdynspec_UI IN brwproc[4].           
   RUN setcolindex_UI IN brwproc[4] (INPUT "").     
   EMPTY TEMP-TABLE mtrl_tab NO-ERROR.    
   FIND FIRST kopp_lina WHERE (kopp_lina.NUM1 = kon_id.NUM OR
   kopp_lina.NUM2 = kon_id.NUM) AND kopp_lina.METER NE 0 AND kopp_lina.KORTKOD = ?
   NO-LOCK NO-ERROR.
   IF AVAILABLE kopp_lina THEN DO: 
      IF RAD_VAL = 1 THEN RUN mtrl_UI.         
      APPLY "VALUE-CHANGED" TO BRW_LIN IN FRAME {&FRAME-NAME}.  
   END.   
   IF RAD_VAL = 1 THEN RUN openbdynspec_UI IN brwproc[2].
   ELSE RUN openbdynspec_UI IN brwproc[3].                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LIN
&Scoped-define SELF-NAME BRW_LIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LIN WINDOW-1
ON VALUE-CHANGED OF BRW_LIN IN FRAME FRAME-A
DO:                
   ASSIGN
   gammal = FALSE
   status-ok = BRW_LIN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   FIND FIRST koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
   AND koppbuff.KORTKOD = kon_id.NUM AND koppbuff.KABNR = kopp_lina.KABNR 
   NO-LOCK NO-ERROR.
   IF AVAILABLE koppbuff THEN DO:                
      ASSIGN                     
      gammal = TRUE
      gam_row = ROWID(koppbuff)
      FILL-IN-TYP = koppbuff.SKAP
      FILL-IN-SADR = koppbuff.SKAPADR  
      FILL-IN-APPARAT = koppbuff.APPARAT
      FILL-IN-ADR = koppbuff.KABADR
      FILL-IN-AR = koppbuff.ARTAL
      FILL-IN-SAK = koppbuff.SAKR
      FILL-IN-MAX = koppbuff.MAXSAKR
      FILL-IN-ANMARK = koppbuff.ANMARK
      CMB_TYP:SCREEN-VALUE = koppbuff.TYP.      
      DISPLAY FILL-IN-TYP WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-SADR WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-APPARAT WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-ADR WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-AR WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-SAK WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-MAX WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-ANMARK WITH FRAME {&FRAME-NAME}.   
      APPLY "VALUE-CHANGED" TO CMB_TYP IN FRAME {&FRAME-NAME}.   
   END. 
   ELSE DO: 
      FIND FIRST koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
      AND koppbuff.KORTKOD NE ? AND koppbuff.KABNR = kopp_lina.KABNR 
      NO-LOCK NO-ERROR.
      IF AVAILABLE koppbuff THEN DO:             
         ASSIGN 
         FILL-IN-TYP = " "        
         FILL-IN-SADR = koppbuff.KABADR
         FILL-IN-APPARAT = ""
         FILL-IN-ADR = koppbuff.SKAPADR
         FILL-IN-AR = koppbuff.ARTAL 
         FILL-IN-SAK = 35     
         FILL-IN-MAX = ?        
         FILL-IN-ANMARK = "".
         IF koppbuff.TYP = "Ut" THEN CMB_TYP:SCREEN-VALUE = "In". 
         ELSE IF koppbuff.TYP = "In" THEN CMB_TYP:SCREEN-VALUE = "Ut".
         ELSE CMB_TYP:SCREEN-VALUE = "Servis".      
         DISPLAY FILL-IN-TYP WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-SADR WITH FRAME {&FRAME-NAME}.
         DISPLAY FILL-IN-APPARAT WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-ADR WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-AR WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-SAK WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-MAX WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-ANMARK WITH FRAME {&FRAME-NAME}.  
         APPLY "VALUE-CHANGED" TO CMB_TYP IN FRAME {&FRAME-NAME}.  
      END. 
      ELSE DO:
         ASSIGN       
         FILL-IN-ADR = " "
         FILL-IN-AR = YEAR(TODAY)
         FILL-IN-SAK = 35     
         FILL-IN-MAX =  ?
         FILL-IN-ANMARK = " "
         CMB_TYP:SCREEN-VALUE = "Servis".      
         DISPLAY FILL-IN-ADR WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-AR WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-SAK WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-MAX WITH FRAME {&FRAME-NAME}. 
         DISPLAY FILL-IN-ANMARK WITH FRAME {&FRAME-NAME}. 
         APPLY "VALUE-CHANGED" TO CMB_TYP IN FRAME {&FRAME-NAME}.        
      END.   
   END.   
   IF kopp_lina.NUM1 = kon_id.NUM THEN DO:  
      FIND FIRST idbuff WHERE idbuff.NUM = kopp_lina.NUM2 NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-ID1 = idbuff.LINNR   
      FILL-IN-ID2 = idbuff.NATNR
      FILL-IN-ID3 = idbuff.FRI1
      FILL-IN-ID4 = idbuff.FRI2.                     
      FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = idbuff.GRUPP NO-LOCK NO-ERROR.
      FILL-IN-GRUPP = konstgrptemp.BENAMNING.
      DISPLAY FILL-IN-GRUPP WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-ID1 WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-ID2 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-ID3 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-ID4 WITH FRAME {&FRAME-NAME}.    
   END.   
   ELSE DO: 
      FIND FIRST idbuff WHERE idbuff.NUM = kopp_lina.NUM1 NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-ID1 = idbuff.LINNR   
      FILL-IN-ID2 = idbuff.NATNR
      FILL-IN-ID3 = idbuff.FRI1
      FILL-IN-ID4 = idbuff.FRI2.  
      FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = idbuff.GRUPP NO-LOCK NO-ERROR.
      FILL-IN-GRUPP = konstgrptemp.BENAMNING.
      DISPLAY FILL-IN-GRUPP WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-ID1 WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-ID2 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-ID3 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-ID4 WITH FRAME {&FRAME-NAME}.
   END.                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON VALUE-CHANGED OF BRW_MTRL IN FRAME FRAME-A /* Materiel */
DO:  
   IF RAD_VAL = 1 THEN DO:
      FILL-IN-TYP = mtrl_tab.BENAMNING.
      DISPLAY FILL-IN-TYP WITH FRAME {&FRAME-NAME}. 
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NER WINDOW-1
ON CHOOSE OF BTN_NER IN FRAME FRAME-A
DO:   
   ASSIGN
   status-ok = BRW_LIN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   frannr = kopp_lina.KABNR2
   lin_rowid = ROWID(kopp_lina)
   finns = FALSE.
   FIND FIRST koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
   AND koppbuff.KORTKOD = kon_id.NUM AND koppbuff.KABNR = kopp_lina.KABNR NO-ERROR.
   IF AVAILABLE koppbuff THEN DO:
      ASSIGN
      finns = TRUE
      gam_row = ROWID(koppbuff).
   END. 
   ASSIGN
   status-ok = BRW_LIN:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}
   tillnr = kopp_lina.KABNR2
   kopp_lina.KABNR2 = frannr.
   FIND FIRST koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
   AND koppbuff.KORTKOD = kon_id.NUM AND koppbuff.KABNR = kopp_lina.KABNR NO-ERROR.
   IF AVAILABLE koppbuff THEN DO:
      koppbuff.KABNR2 = frannr.
   END.   
   FIND kopp_lina WHERE ROWID(kopp_lina) = lin_rowid.
   kopp_lina.KABNR2 = tillnr.
   IF finns = TRUE THEN DO:
      FIND kopp_lina WHERE ROWID(kopp_lina) = gam_row. 
      ASSIGN
      kopp_lina.KABNR2 = tillnr
      finns = FALSE.
   END.     
   RUN setcolindex_UI IN brwproc[4] (INPUT "KABNR2").   
   setcolvar = " WHERE " + "(" + " NUM1 = '" + STRING(kon_id.NUM) + "' OR NUM2 = '" + STRING(kon_id.NUM) + "' " + ")" + " AND KORTKOD = ? AND METER > '" + STRING("0") + "' " .   
   RUN setcolsortvar_UI IN brwproc[4] (INPUT setcolvar).   
   RUN openbdynspec_UI IN brwproc[4].              
   RUN setcolindex_UI IN brwproc[4] (INPUT "").      
   FIND kopp_lina WHERE ROWID(kopp_lina) = lin_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kopp_lina THEN DO:
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(kopp_lina)).
      RUN lastselectdyn_UI IN brwproc[4].
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A
DO:   
   ASSIGN
   status-ok = BRW_LIN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   frannr = kopp_lina.KABNR2
   lin_rowid = ROWID(kopp_lina)
   finns = FALSE.
   FIND FIRST koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
   AND koppbuff.KORTKOD = kon_id.NUM AND koppbuff.KABNR = kopp_lina.KABNR NO-ERROR.
   IF AVAILABLE koppbuff THEN DO:
      ASSIGN
      finns = TRUE
      gam_row = ROWID(koppbuff).
   END. 
   ASSIGN
   status-ok = BRW_LIN:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}
   tillnr = kopp_lina.KABNR2
   kopp_lina.KABNR2 = frannr.    
   FIND FIRST koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
   AND koppbuff.KORTKOD = kon_id.NUM AND koppbuff.KABNR = kopp_lina.KABNR NO-ERROR.
   IF AVAILABLE koppbuff THEN DO:
      koppbuff.KABNR2 = frannr.
   END.   
   FIND kopp_lina WHERE ROWID(kopp_lina) = lin_rowid.
   kopp_lina.KABNR2 = tillnr.
   IF finns = TRUE THEN DO:
      FIND kopp_lina WHERE ROWID(kopp_lina) = gam_row. 
      ASSIGN
      kopp_lina.KABNR2 = tillnr
      finns = FALSE.
   END.   
   RUN setcolindex_UI IN brwproc[4] (INPUT "KABNR2").   
   setcolvar = " WHERE " + "(" + " NUM1 = '" + STRING(kon_id.NUM) + "' OR NUM2 = '" + STRING(kon_id.NUM) + "' " + ")" + " AND KORTKOD = ? AND METER > '" + STRING("0") + "' " .   
   RUN setcolsortvar_UI IN brwproc[4] (INPUT setcolvar).   
   RUN openbdynspec_UI IN brwproc[4].              
   RUN setcolindex_UI IN brwproc[4] (INPUT "").      
   FIND kopp_lina WHERE ROWID(kopp_lina) = lin_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kopp_lina THEN DO:
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(kopp_lina)).
      RUN lastselectdyn_UI IN brwproc[4].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TYP WINDOW-1
ON VALUE-CHANGED OF CMB_TYP IN FRAME FRAME-A /* Typ */
DO:
   CMB_TYP = INPUT CMB_TYP.
   IF CMB_TYP = "Servis" THEN DO:
      ASSIGN
      FILL-IN-SAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-MAX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE IF CMB_TYP = "Ut" THEN DO:
      ASSIGN
      FILL-IN-SAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-MAX:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-SAK = ?
      FILL-IN-MAX = ?
      FILL-IN-SAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-MAX:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VAL WINDOW-1
ON VALUE-CHANGED OF CMB_VAL IN FRAME FRAME-A
DO:
   CMB_VAL = INPUT CMB_VAL.
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VAL 
   NO-LOCK NO-ERROR.          
   FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = konstgrptemp.KONSKOD 
   NO-LOCK NO-ERROR.
   {IDLABELU.I}
   RUN setcolindex_UI IN brwproc[1] (INPUT "LINNR BY NATNR BY FRI1 BY FRI2").           
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE kon_id.GRUPP = " + STRING(konstgrptemp.KONSKOD) + " AND kon_id.ENDKOMB = FALSE ").
   RUN openbdynspec_UI IN brwproc[1].           
   RUN setcolindex_UI IN brwproc[1] (INPUT "").                   
   APPLY "VALUE-CHANGED" TO BRW_FRAN IN FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKAPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKAPA WINDOW-1
ON CHOOSE OF FBTN_SKAPA IN FRAME FRAME-A /* Skapa */
DO:                                                                    
   status-ok = BRW_LIN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   IF INPUT FILL-IN-TYP = "" THEN DO:
      MESSAGE "Skåptyp kan inte vara blank." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-TYP IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.   
   ELSE IF INPUT FILL-IN-SADR = "" THEN DO:
      MESSAGE "Skåpadress kan inte vara blank." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-SADR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END. 
   ELSE IF INPUT FILL-IN-ADR = ? THEN DO:
      MESSAGE "Kabeladress kan inte vara blank." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-ADR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   ELSE IF INPUT FILL-IN-AR = "" THEN DO:
      MESSAGE "Årtal kan inte vara blank." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-AR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.     
   ELSE DO:            
      IF gammal = TRUE THEN DO:
         FIND kopp_lina WHERE ROWID(kopp_lina) = gam_row EXCLUSIVE-LOCK NO-ERROR.   
         ASSIGN
         kopp_lina.SKAP = INPUT FILL-IN-TYP
         kopp_lina.SKAPADR = INPUT FILL-IN-SADR
         kopp_lina.APPARAT = INPUT FILL-IN-APPARAT
         kopp_lina.KABADR = INPUT FILL-IN-ADR
         kopp_lina.ARTAL = INPUT FILL-IN-AR          
         kopp_lina.ANMARK = INPUT FILL-IN-ANMARK
         kopp_lina.TYP = INPUT CMB_TYP.
         IF INPUT CMB_TYP = "In" THEN DO:            
            ASSIGN
            kopp_lina.SAKR = ?
            kopp_lina.MAXSAKR = ?.
         END.
         ELSE DO:
            ASSIGN
            kopp_lina.SAKR = INPUT FILL-IN-SAK
            kopp_lina.MAXSAKR = INPUT FILL-IN-MAX.
         END. 
         FOR EACH koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
         AND koppbuff.KORTKOD = kon_id.NUM AND koppbuff.KABNR NE kopp_lina.KABNR:
            ASSIGN
            koppbuff.SKAP = INPUT FILL-IN-TYP
            koppbuff.SKAPADR = INPUT FILL-IN-SADR.
            IF koppbuff.NUM1 NE kon_id.NUM THEN DO:
               FOR EACH koppbuff2 WHERE (koppbuff2.NUM1 = kon_id.NUM OR koppbuff2.NUM2 = kon_id.NUM)
               AND koppbuff2.KORTKOD NE kon_id.NUM AND koppbuff2.KABNR = koppbuff.KABNR:
                  IF koppbuff2.KORTKOD NE ? THEN
                  ASSIGN
                  koppbuff2.KABADR = INPUT FILL-IN-SADR.
               END.
            END.
            IF koppbuff.NUM2 NE kon_id.NUM THEN DO:
               FOR EACH koppbuff2 WHERE (koppbuff2.NUM1 = kon_id.NUM OR koppbuff2.NUM2 = kon_id.NUM)
               AND koppbuff2.KORTKOD NE kon_id.NUM AND koppbuff2.KABNR = koppbuff.KABNR:
                  IF koppbuff2.KORTKOD NE ? THEN
                  ASSIGN
                  koppbuff2.KABADR = INPUT FILL-IN-SADR.
               END.
            END.
         END.         
      END.
      ELSE DO:
         CREATE koppbuff.
         ASSIGN 
         koppbuff.NUM1 = kopp_lina.NUM1
         koppbuff.NUM2 = kopp_lina.NUM2 
         koppbuff.ENR = kopp_lina.ENR
         koppbuff.BENAMNING = kopp_lina.BENAMNING  
         koppbuff.PRIS = kopp_lina.PRIS
         koppbuff.ENHET = kopp_lina.ENHET 
         koppbuff.LEVKOD = kopp_lina.LEVKOD
         koppbuff.METER = kopp_lina.METER
         koppbuff.LEDARE = kopp_lina.LEDARE
         koppbuff.KABNR = kopp_lina.KABNR 
         koppbuff.KABNR2 = kopp_lina.KABNR2          
         koppbuff.DIAMETER = kopp_lina.DIAMETER
         koppbuff.KORTKOD = kon_id.NUM
         koppbuff.SKAP = INPUT FILL-IN-TYP
         koppbuff.SKAPADR = INPUT FILL-IN-SADR 
         koppbuff.APPARAT = INPUT FILL-IN-APPARAT
         koppbuff.KABADR = INPUT FILL-IN-ADR
         koppbuff.ARTAL = INPUT FILL-IN-AR          
         koppbuff.ANMARK = INPUT FILL-IN-ANMARK
         koppbuff.TYP = INPUT CMB_TYP. 
         IF INPUT CMB_TYP = "In" THEN DO:
            ASSIGN
            koppbuff.SAKR = ?
            koppbuff.MAXSAKR = ?.
         END.
         ELSE DO:
            ASSIGN
            koppbuff.SAKR = INPUT FILL-IN-SAK
            koppbuff.MAXSAKR = INPUT FILL-IN-MAX.            
         END. 
         FOR EACH koppbuff WHERE (koppbuff.NUM1 = kon_id.NUM OR koppbuff.NUM2 = kon_id.NUM)
         AND koppbuff.KORTKOD = kon_id.NUM AND koppbuff.KABNR NE kopp_lina.KABNR:
            ASSIGN
            koppbuff.SKAP = INPUT FILL-IN-TYP
            koppbuff.SKAPADR = INPUT FILL-IN-SADR.
            IF koppbuff.NUM1 NE kon_id.NUM THEN DO:
               FOR EACH koppbuff2 WHERE (koppbuff2.NUM1 = kon_id.NUM OR koppbuff2.NUM2 = kon_id.NUM)
               AND koppbuff2.KORTKOD NE kon_id.NUM AND koppbuff2.KABNR = koppbuff.KABNR:
                  IF koppbuff2.KORTKOD NE ? THEN
                  ASSIGN
                  koppbuff2.KABADR = INPUT FILL-IN-SADR.
               END.
            END.
            IF koppbuff.NUM2 NE kon_id.NUM THEN DO:
               FOR EACH koppbuff2 WHERE (koppbuff2.NUM1 = kon_id.NUM OR koppbuff2.NUM2 = kon_id.NUM)
               AND koppbuff2.KORTKOD NE kon_id.NUM AND koppbuff2.KABNR = koppbuff.KABNR:
                  IF koppbuff2.KORTKOD NE ? THEN
                  ASSIGN
                  koppbuff2.KABADR = INPUT FILL-IN-SADR.
               END.
            END.
         END.
         APPLY "VALUE-CHANGED" TO BRW_LIN IN FRAME {&FRAME-NAME}.
      END.
      MESSAGE "Uppgifter är nu kopplade till markerad kabel/lina"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:
      {muswait.i} 
      ASSIGN
      status-ok = BRW_FRAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      ASSIGN
      lin_rowid = ROWID(kon_id)               
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISAKORTU.W.
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
   {muswait.i}         
   ASSIGN
   status-ok = BRW_FRAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   ASSIGN
   lin_rowid = ROWID(kon_id).
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.
   {AVBGOM.I}   
   RUN VISAKORTU.W.
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-APPARAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-APPARAT WINDOW-1
ON ENTRY OF FILL-IN-APPARAT IN FRAME FRAME-A /* Apparat */
DO:
   IF RAD_VAL = 1 THEN DO:      
      ASSIGN      
      RAD_VAL = 2
      BRW_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      BRW_APP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      DISPLAY RAD_VAL WITH FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO RAD_VAL IN FRAME {&FRAME-NAME}.
   END.   
   ELSE DO:
      musz = musz.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TYP WINDOW-1
ON ENTRY OF FILL-IN-TYP IN FRAME FRAME-A /* Skåptyp */
DO:      
   IF RAD_VAL = 1 THEN DO:
      musz = musz.
   END.   
   ELSE DO:            
      ASSIGN    
      RAD_VAL = 1  
      BRW_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      BRW_APP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      DISPLAY RAD_VAL WITH FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO RAD_VAL IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa_information
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_information WINDOW-1
ON CHOOSE OF MENU-ITEM m_Visa_information /* Visa information */
DO:
   {muswait.i}
   RUN info_UI.      
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-A
DO:
   RAD_VAL = INPUT RAD_VAL.    
   EMPTY TEMP-TABLE mtrl_tab NO-ERROR.    
   IF RAD_VAL = 1 THEN DO:
      RUN mtrl_UI.
      ASSIGN
      BRW_APP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN openbdynspec_UI IN brwproc[2].
   END.   
   ELSE DO:      
      ASSIGN      
      BRW_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      BRW_APP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN openbdynspec_UI IN brwproc[3].           
   END.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_APP
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(frikortapph) THEN DELETE PROCEDURE frikortapph.
   IF VALID-HANDLE(konstvaltapph) THEN DELETE PROCEDURE konstvaltapph.
   
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
   RUN laddaapparat_UI IN frikortapph (OUTPUT TABLE apparattemp).
   ASSIGN
   RAD_VAL = 1
   FILL-IN-AR = YEAR(TODAY)
   status-ok = CMB_VAL:DELETE("0")
   gammal = FALSE.    
   FOR EACH konstgrptemp NO-LOCK:
      FIND FIRST kon_id WHERE kon_id.GRUPP = konstgrptemp.KONSKOD 
      AND kon_id.ENDKOMB = FALSE NO-LOCK NO-ERROR.
      IF AVAILABLE kon_id THEN DO:
         ASSIGN
         status-ok = CMB_VAL:ADD-LAST(konstgrptemp.BENAMNING)IN FRAME {&FRAME-NAME} 
         CMB_VAL:SCREEN-VALUE = konstgrptemp.BENAMNING.   
      END.
   END. 
   ASSIGN
   status-ok = CMB_TYP:DELETE("0")
   CMB_TYP:SCREEN-VALUE = "Servis".   
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
   CMB_VAL = INPUT CMB_VAL.    
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VAL 
   NO-LOCK NO-ERROR.
   FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = konstgrptemp.KONSKOD 
   NO-LOCK NO-ERROR.
   {IDLABELU.I}   
   RUN enable_UI.   
   {BERTITLE.I}
   {FRMSIZE.I}   
   ASSIGN
   BRW_APP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_VAL = INPUT CMB_VAL.    
   APPLY "VALUE-CHANGED" TO CMB_VAL IN FRAME {&FRAME-NAME}.
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
      (INPUT BRW_FRAN:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_APP:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_LIN:HANDLE IN FRAME {&FRAME-NAME}).         
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN FRIKORTAPP.P PERSISTENT SET frikortapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FRIKORTAPP.P PERSISTENT SET frikortapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN KONSTVALTAPP.P PERSISTENT SET konstvaltapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KONSTVALTAPP.P PERSISTENT SET konstvaltapph.
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
  DISPLAY CMB_VAL RAD_VAL FILL-IN-TYP FILL-IN-SADR FILL-IN-ADR CMB_TYP 
          FILL-IN-APPARAT FILL-IN-AR FILL-IN-SAK FILL-IN-MAX FILL-IN-ANMARK 
          FILL-IN-GRUPP FILL-IN-ID1 FILL-IN-ID2 FILL-IN-ID3 FILL-IN-ID4 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE CMB_VAL RAD_VAL BRW_FRAN BRW_APP BRW_MTRL FBTN_SKAPA FBTN_VISA 
         FBTN_SKRIV BRW_LIN BTN_UPP BTN_NER FILL-IN-TYP FILL-IN-SADR 
         FILL-IN-ADR CMB_TYP FILL-IN-APPARAT FILL-IN-AR FILL-IN-SAK FILL-IN-MAX 
         FILL-IN-ANMARK BTN_AVB FILL-IN-GRUPP FILL-IN-ID1 FILL-IN-ID2 
         FILL-IN-ID3 FILL-IN-ID4 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE info_UI WINDOW-1 
PROCEDURE info_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   status-ok = BRW_FRAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   FIND FIRST kon_val WHERE kon_val.NUM = kon_id.NUM AND
   kon_val.KSKAP = FALSE USE-INDEX NUM NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE kon_val THEN RETURN. 
   FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = kon_val.GRUPP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE konstvaltemp THEN DO:
      RUN laddatempen_UI IN konstvaltapph (INPUT kon_val.GRUPP, OUTPUT TABLE konstvaltemp APPEND).
   END.    
   /*
   {AVBGOM.I}      
   RUN BERINFOU2.W (INPUT ROWID(kon_val)). 
   {AVBFRAM.I}
   */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mtrl2_UI WINDOW-1 
PROCEDURE mtrl2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF kon_id.GRUPP = 0 THEN DO:
      IF kon_id.XKORD = ? THEN DO:  
         FOR EACH list_mtrl WHERE list_mtrl.NUM = kon_id.NUM AND 
         list_mtrl.LINKAB = FALSE AND list_mtrl.SKAPMTRL = FALSE:
            FIND FIRST mtrl_tab WHERE mtrl_tab.BENAMNING = list_mtrl.BENAMNING 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE mtrl_tab THEN DO:
               CREATE mtrl_tab.
               ASSIGN
               mtrl_tab.BENAMNING = list_mtrl.BENAMNING.
            END.   
         END.
      END.
      ELSE DO:
         FOR EACH idbuff WHERE idbuff.XKORD = kon_id.XKORD:        
            FOR EACH list_mtrl WHERE list_mtrl.NUM = idbuff.NUM AND 
            list_mtrl.LINKAB = FALSE AND list_mtrl.SKAPMTRL = FALSE:
               FIND FIRST mtrl_tab WHERE mtrl_tab.BENAMNING = list_mtrl.BENAMNING 
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE mtrl_tab THEN DO:
                  CREATE mtrl_tab.
                  ASSIGN
                  mtrl_tab.BENAMNING = list_mtrl.BENAMNING.
               END.   
            END.
         END.   
      END. 
   END.
   ELSE DO:   
      IF kon_id.XKORD = ? THEN DO:  
         FOR EACH list_mtrl WHERE list_mtrl.NUM = kon_id.NUM AND 
         list_mtrl.LINKAB = FALSE:
            FIND FIRST mtrl_tab WHERE mtrl_tab.BENAMNING = list_mtrl.BENAMNING 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE mtrl_tab THEN DO:
               CREATE mtrl_tab.
               ASSIGN
               mtrl_tab.BENAMNING = list_mtrl.BENAMNING.
            END.   
         END.
      END.
      ELSE DO:
         FOR EACH idbuff WHERE idbuff.XKORD = kon_id.XKORD:        
            FOR EACH list_mtrl WHERE list_mtrl.NUM = idbuff.NUM AND 
            list_mtrl.LINKAB = FALSE:
               FIND FIRST mtrl_tab WHERE mtrl_tab.BENAMNING = list_mtrl.BENAMNING 
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE mtrl_tab THEN DO:
                  CREATE mtrl_tab.
                  ASSIGN
                  mtrl_tab.BENAMNING = list_mtrl.BENAMNING.
               END.   
            END.
         END.   
      END. 
   END.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mtrl_UI WINDOW-1 
PROCEDURE mtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF kon_id.GRUPP = 0 THEN DO:
      IF kon_id.XKORD = ? THEN DO:  
         FOR EACH list_mtrl WHERE list_mtrl.NUM = kon_id.NUM AND 
         list_mtrl.LINKAB = FALSE AND list_mtrl.SKAPMTRL = TRUE:
            FIND FIRST mtrl_tab WHERE mtrl_tab.BENAMNING = list_mtrl.BENAMNING 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE mtrl_tab THEN DO:
               CREATE mtrl_tab.
               ASSIGN
               mtrl_tab.BENAMNING = list_mtrl.BENAMNING.
            END.   
         END.
      END.
      ELSE DO:
         FOR EACH idbuff WHERE idbuff.XKORD = kon_id.XKORD:        
            FOR EACH list_mtrl WHERE list_mtrl.NUM = idbuff.NUM AND 
            list_mtrl.LINKAB = FALSE AND list_mtrl.SKAPMTRL = TRUE:
               FIND FIRST mtrl_tab WHERE mtrl_tab.BENAMNING = list_mtrl.BENAMNING 
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE mtrl_tab THEN DO:
                  CREATE mtrl_tab.
                  ASSIGN
                  mtrl_tab.BENAMNING = list_mtrl.BENAMNING.
               END.   
            END.
         END.   
      END. 
   END.
   ELSE DO:   
      IF kon_id.XKORD = ? THEN DO:  
         FOR EACH list_mtrl WHERE list_mtrl.NUM = kon_id.NUM AND 
         list_mtrl.LINKAB = FALSE:
            FIND FIRST mtrl_tab WHERE mtrl_tab.BENAMNING = list_mtrl.BENAMNING 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE mtrl_tab THEN DO:
               CREATE mtrl_tab.
               ASSIGN
               mtrl_tab.BENAMNING = list_mtrl.BENAMNING.
            END.   
         END.
      END.
      ELSE DO:
         FOR EACH idbuff WHERE idbuff.XKORD = kon_id.XKORD:        
            FOR EACH list_mtrl WHERE list_mtrl.NUM = idbuff.NUM AND 
            list_mtrl.LINKAB = FALSE:
               FIND FIRST mtrl_tab WHERE mtrl_tab.BENAMNING = list_mtrl.BENAMNING 
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE mtrl_tab THEN DO:
                  CREATE mtrl_tab.
                  ASSIGN
                  mtrl_tab.BENAMNING = list_mtrl.BENAMNING.
               END.   
            END.
         END.   
      END. 
   END.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

