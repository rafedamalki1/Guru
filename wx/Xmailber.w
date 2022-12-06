&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          RT8               PROGRESS
*/
&Scoped-define WINDOW-NAME    WINDOW-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
DEFINE SHARED VARIABLE detvar AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE datvar LIKE BERMTRL.DATUM NO-UNDO.  
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE vald_lev LIKE MTRL.LEVKOD NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE SHARED VARIABLE valaonr LIKE AONRTAB.AONR NO-UNDO.
DEFINE SHARED VARIABLE valdelnr LIKE AONRTAB.DELNR NO-UNDO.
DEFINE SHARED VARIABLE valort LIKE AONRTAB.ORT NO-UNDO. 
DEFINE SHARED VARIABLE valomrade LIKE AONRTAB.OMRADE NO-UNDO.   
DEFINE SHARED VARIABLE globsids LIKE ANVANDARE.SIDS NO-UNDO. 
DEFINE SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE mailvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE EDITOR_MEDD AS CHARACTER  VIEW-AS EDITOR SCROLLBAR-VERTICAL SIZE 53 BY 9 FONT 25 NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.        
DEFINE VARIABLE arrhjsum LIKE TIDREGITAB.BERANTAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str1 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str0 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE totalt LIKE MTRL.NPRIS NO-UNDO. 
DEFINE VARIABLE totalt2 LIKE MTRL.NPRIS NO-UNDO. 
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVNAMN NO-UNDO.
DEFINE VARIABLE lev LIKE MTRL.LEVKOD NO-UNDO.  
DEFINE VARIABLE sumpris LIKE BERMTRL.PRIS NO-UNDO.    
DEFINE VARIABLE totalsum LIKE BERMTRL.PRIS NO-UNDO.
DEFINE VARIABLE sumantal LIKE BERMTRL.ANTAL NO-UNDO.
DEFINE VARIABLE xkordvar LIKE BERID.XKORD NO-UNDO. 
DEFINE VARIABLE stopvar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE VARIABLE antvar LIKE MTRLBER.ANTAL NO-UNDO. 
DEFINE VARIABLE antvar2 LIKE MTRLBER.ANTAL NO-UNDO.  
DEFINE VARIABLE firstlev LIKE MTRL.LEVKOD NO-UNDO.   
DEFINE VARIABLE mtrlrak AS INTEGER NO-UNDO.  
DEFINE VARIABLE mtrlrak2 AS INTEGER NO-UNDO.
DEFINE VARIABLE radrak AS INTEGER NO-UNDO.   
DEFINE VARIABLE langd AS INTEGER NO-UNDO.  
DEFINE VARIABLE upp AS LOGICAL NO-UNDO.            
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER FORMAT "X(41)" NO-UNDO.
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE bytgrupp AS CHARACTER NO-UNDO. 
DEFINE VARIABLE enrvar LIKE MTRL.ENR NO-UNDO.
DEFINE INPUT  PARAMETER bestoff  AS CHARACTER.
DEFINE VARIABLE utrec AS RECID NO-UNDO.
DEFINE VARIABLE utrec2 AS RECID NO-UNDO.
DEFINE VARIABLE kant AS INTEGER NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.
DEFINE VARIABLE uppvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE sidlangd LIKE ANVANDARE.SIDS NO-UNDO.

{ANMARKD.I}
DEFINE SHARED TEMP-TABLE best_mtrl    
    FIELD ENR LIKE BERMTRL.ENR
    FIELD BENAMNING LIKE BERMTRL.BENAMNING
    FIELD ENHET LIKE BERMTRL.ENHET
    FIELD PRIS LIKE BERMTRL.PRIS 
    FIELD OPRIS LIKE BERMTRL.OPRIS      
    FIELD ANTAL LIKE BERMTRL.ANTAL     
    FIELD BESTANT LIKE BERMTRL.BESTANT    
    FIELD LEVKOD LIKE BERMTRL.LEVKOD 
    FIELD BERLEV LIKE BERMTRL.BERLEV        
    FIELD DBEST LIKE BERMTRL.DBEST
    FIELD DATUM LIKE BERMTRL.DATUM 
    FIELD KLAR LIKE BERMTRL.KLAR   
    INDEX DATUM DATUM ASCENDING
    INDEX ENR IS PRIMARY ENR ASCENDING.  
    
DEFINE SHARED TEMP-TABLE off_mtrl     
    FIELD TOTALT LIKE BERMTRL.PRIS 
    FIELD RABTOT LIKE BERMTRL.PRIS      
    FIELD LEVKOD LIKE BERMTRL.LEVKOD
    FIELD DATUM LIKE BERMTRL.DATUM
    FIELD KLAR LIKE BERMTRL.KLAR.        
    
DEFINE SHARED TEMP-TABLE trp_mtrl    
    FIELD ENR LIKE BERMTRL.ENR
    FIELD BENAMNING LIKE BERMTRL.BENAMNING
    FIELD ENHET LIKE BERMTRL.ENHET
    FIELD PRIS LIKE BERMTRL.PRIS
    FIELD OPRIS LIKE BERMTRL.OPRIS       
    FIELD ANTAL LIKE BERMTRL.ANTAL     
    FIELD BESTANT LIKE BERMTRL.BESTANT    
    FIELD LEVKOD LIKE BERMTRL.LEVKOD 
    FIELD BERLEV LIKE BERMTRL.BERLEV        
    FIELD DBEST LIKE BERMTRL.DBEST    
    INDEX ENR IS PRIMARY ENR ASCENDING.         
    
DEFINE SHARED TEMP-TABLE skapa_mtrl 
   FIELD LEVNAMN LIKE LEVERANTOR.LEVNAMN
   FIELD LKONTAKT LIKE LEVERANTOR.LEVKONTAKT
   FIELD LTELE LIKE LEVERANTOR.LEVTEL
   FIELD LADR LIKE LEVERANTOR.LEVADR
   FIELD LPNR LIKE LEVERANTOR.LEVPNR
   FIELD LORT LIKE LEVERANTOR.LEVORT   
   FIELD FORE LIKE OMRADETAB.NAMN
   FIELD KADR LIKE OMRADETAB.GATUADR  
   FIELD KPNR LIKE OMRADETAB.POSTNR
   FIELD KORT LIKE OMRADETAB.POSTANST
   FIELD BOX AS CHARACTER FORMAT "X(5)"
   FIELD FAX LIKE PERSONALTAB.TELEFON
   FIELD KIKONTAKT LIKE PERSONALTAB.EFTERNAMN                            
   FIELD KITELE LIKE PERSONALTAB.TELEFON
   FIELD KTKONTAKT LIKE PERSONALTAB.EFTERNAMN  
   FIELD KTTELE LIKE PERSONALTAB.TELEFON      
   FIELD DATUM AS DATE
   FIELD MARK AS CHARACTER FORMAT "X(35)"
   FIELD L1 AS CHARACTER FORMAT "X(50)"
   FIELD L2 AS CHARACTER FORMAT "X(50)"
   FIELD L3 AS CHARACTER FORMAT "X(50)"
   FIELD KOM AS CHARACTER FORMAT "X(40)"
   FIELD KUNDNR AS INTEGER FORMAT 9999999999
   FIELD AVIS AS LOGICAL
   FIELD BESTNR AS CHARACTER
   FIELD KIMOBIL LIKE PERSONALTAB.MOBILTEL
   FIELD DEPA LIKE DEPA.BENAMNING
   FIELD LEVTID AS CHARACTER
   FIELD AVISPERS AS CHARACTER.
   
DEFINE NEW SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".

DEFINE NEW SHARED TEMP-TABLE mtrl_temp 
   FIELD NUM LIKE BERMTRL.NUM  
   FIELD ENR LIKE MTRLBER.ENR
   FIELD BENAMNING LIKE MTRLBER.BENAMNING
   FIELD ENHET LIKE MTRLBER.ENHET
   FIELD ANTAL LIKE MTRLBER.ANTAL
   FIELD PRIS LIKE MTRLBER.PRIS      
   FIELD TOTPRIS LIKE MTRLBER.PRIS
   FIELD LEVKOD LIKE MTRLBER.LEVKOD
   FIELD UPPLAG LIKE BERVAL.UPPLAG      
   FIELD GRUPP LIKE BERVAL.KONSKOD 
   FIELD XKORD LIKE BERID.XKORD
   FIELD FORNR LIKE BERID.FORNR
   FIELD LINNR LIKE BERID.LINNR
   FIELD NATNR LIKE BERID.NATNR
   FIELD FRI1 LIKE BERID.FRI1
   FIELD FRI2 LIKE BERID.FRI2
   INDEX ENR IS PRIMARY ENR ASCENDING
   INDEX NUM NUM ASCENDING.       
     
DEFINE TEMP-TABLE mtrl_temp2 
   FIELD NUM LIKE BERMTRL.NUM    
   FIELD ENR LIKE MTRLBER.ENR
   FIELD BENAMNING LIKE MTRLBER.BENAMNING
   FIELD ENHET LIKE MTRLBER.ENHET
   FIELD ANTAL LIKE MTRLBER.ANTAL
   FIELD PRIS LIKE MTRLBER.PRIS 
   FIELD TOTPRIS LIKE MTRLBER.PRIS
   FIELD LEVKOD LIKE MTRLBER.LEVKOD   
   FIELD UPPLAG LIKE BERVAL.UPPLAG 
   FIELD GRUPP LIKE BERVAL.KONSKOD 
   FIELD XKORD LIKE BERID.XKORD 
   FIELD KLAR AS LOGICAL
   FIELD KLAR2 AS LOGICAL
   FIELD FORNR LIKE BERID.FORNR
   FIELD LINNR LIKE BERID.LINNR
   FIELD NATNR LIKE BERID.NATNR
   FIELD FRI1 LIKE BERID.FRI1
   FIELD FRI2 LIKE BERID.FRI2
   INDEX ENR IS PRIMARY LEVKOD ENR ASCENDING
   INDEX LISTA UPPLAG GRUPP LEVKOD KLAR FORNR LINNR NATNR FRI1 FRI2
   INDEX LISTA2 UPPLAG LEVKOD KLAR FORNR LINNR NATNR FRI1 FRI2
   INDEX ENRX ENR UPPLAG XKORD LEVKOD KLAR
   INDEX ENRN ENR UPPLAG NUM LEVKOD KLAR.   
   
DEFINE TEMP-TABLE uppl_temp
   FIELD NUM LIKE BERVAL.NUM 
   FIELD F1 LIKE BERVAL.KTYPKOD 
   FIELD F2 LIKE BERVAL.F2 
   FIELD F3 LIKE BERVAL.F3
   FIELD F4 LIKE BERVAL.F4
   FIELD F5 LIKE BERVAL.F5
   FIELD F6 LIKE BERVAL.F6
   FIELD UPPLAG LIKE BERVAL.UPPLAG
   FIELD GRUPP LIKE BERVAL.KONSKOD 
   FIELD ANMARK LIKE BERVAL.ANMARK  
   INDEX NUM IS PRIMARY NUM ASCENDING.  
   
DEFINE TEMP-TABLE grupp_temp
   FIELD KONSKOD LIKE KONSTGRUPP.KONSKOD
   FIELD BENAMNING LIKE KONSTGRUPP.BENAMNING
   FIELD ORDNING LIKE KONSTGRUPP.ORDNING
   INDEX ORD IS PRIMARY ORDNING ASCENDING.       

DEFINE TEMP-TABLE grupp_temp2
   FIELD KONSKOD LIKE KONSTGRUPP.KONSKOD
   FIELD BENAMNING LIKE KONSTGRUPP.BENAMNING
   FIELD ORDNING LIKE KONSTGRUPP.ORDNING
   INDEX ORD IS PRIMARY ORDNING ASCENDING.     
   
DEFINE TEMP-TABLE id_temp  
   FIELD NUM LIKE BERVAL.NUM 
   FIELD GRUPP LIKE BERVAL.KONSKOD  
   FIELD FORNR LIKE BERID.FORNR
   FIELD LINNR LIKE BERID.LINNR
   FIELD NATNR LIKE BERID.NATNR
   FIELD FRI1 LIKE BERID.FRI1
   FIELD FRI2 LIKE BERID.FRI2 
   FIELD XKORD LIKE BERID.XKORD
   FIELD ENDKOMB LIKE BERID.ENDKOMB
   FIELD FRI3 LIKE BERID.FRI3
   INDEX NUM IS PRIMARY NUM ASCENDING
   INDEX ORD FORNR LINNR NATNR FRI1 FRI2 ASCENDING. 
   
DEFINE TEMP-TABLE lin_upp   
   FIELD METER LIKE BERLINKAB.METER      
   FIELD ENR LIKE BERLINKAB.ENR         
   FIELD BENAMNING LIKE BERLINKAB.BENAMNING 
   FIELD PRIS LIKE BERLINKAB.PRIS
   FIELD ENHET LIKE BERLINKAB.ENHET   
   FIELD TOTMETER LIKE BERLINKAB.TOTMETER
   FIELD UPPLAG LIKE BERLINKAB.UPPLAG 
   FIELD LEVKOD LIKE BERLINKAB.LEVKOD
   FIELD TOTPRIS LIKE BERMTRL.PRIS    
   FIELD KLAR2 AS LOGICAL     
   INDEX ENR ENR ASCENDING.      
   
DEFINE SHARED TEMP-TABLE lev_temp
   FIELD LEVKOD LIKE LEVERANTOR.LEVKOD.   
   
DEFINE TEMP-TABLE skydd_temp    
    FIELD ENR LIKE BERMTRL.ENR
    FIELD BENAMNING LIKE BERMTRL.BENAMNING
    FIELD ENHET LIKE BERMTRL.ENHET
    FIELD PRIS LIKE BERMTRL.PRIS       
    FIELD ANTAL LIKE BERMTRL.ANTAL         
    FIELD LEVKOD LIKE BERMTRL.LEVKOD     
    INDEX ENR IS PRIMARY ENR ASCENDING.
    
DEFINE TEMP-TABLE skydd_temp2    
    FIELD ENR LIKE BERMTRL.ENR
    FIELD BENAMNING LIKE BERMTRL.BENAMNING
    FIELD ENHET LIKE BERMTRL.ENHET
    FIELD PRIS LIKE BERMTRL.PRIS 
    FIELD OPRIS LIKE BERMTRL.OPRIS      
    FIELD ANTAL LIKE BERMTRL.ANTAL 
    FIELD TOTPRIS LIKE MTRLBER.PRIS        
    FIELD LEVKOD LIKE BERMTRL.LEVKOD     
    FIELD KLAR2 AS LOGICAL
    INDEX ENR IS PRIMARY ENR ASCENDING.       
              
DEFINE QUERY mtrlprisq FOR BERMTRL. 
DEFINE QUERY upplq FOR BERVAL. 
DEFINE QUERY berq FOR BERUPP.   
DEFINE QUERY gruppq FOR KONSTGRUPP. 
DEFINE QUERY beridq FOR BERID. 
DEFINE QUERY skyddq FOR KSKYDD.

DEFINE BUFFER idbuff FOR id_temp.
DEFINE BUFFER mtrlbuff FOR mtrl_temp2. 
DEFINE BUFFER gruppbuff FOR grupp_temp.
DEFINE BUFFER linbuff FOR lin_upp.
DEFINE BUFFER skyddbuff FOR skydd_temp2.

DEFINE VARIABLE MED_EDITOR AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 53 BY 11.73
     BGCOLOR 8  NO-UNDO.     

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut 

/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define FIELDS-IN-QUERY-FRAME-VINST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-VINST 
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 12 BY 1.5.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     SIZE 18 BY 1.14
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-35
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 68 BY 2.5
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY BRW_UT FOR tidut SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT QUERY BRW_UT NO-LOCK DISPLAY 
      tidut.ut FORMAT "X(78)"
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 81 BY 18.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BTN_AVS AT ROW 1.5 COL 39.25
     BTN_SKRIV AT ROW 1.5 COL 56.88
     CMB_LEV AT ROW 1.68 COL 13.25 COLON-ALIGNED NO-LABEL
     BRW_UT AT ROW 4.5 COL 2.5
     RECT-35 AT ROW 1 COL 8.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.41
         SIZE 83.75 BY 22.73
         BGCOLOR 8 .

 

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Specifikation av beställning"
         COLUMN             = 8.88
         ROW                = 1.95
         HEIGHT             = 23.14
         WIDTH              = 83.75
         MAX-HEIGHT         = 25.14
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.14
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
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST            = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "rt8.tidut"
     _Options          = "NO-LOCK"
     _OrdList          = ""
     _FldNameList[1]   = rt8.tidut.ut
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _TblList          = ""
     _Options          = "NO-LOCK KEEP-EMPTY"
     _OrdList          = ""
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN SKRIVVAL.W.       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:      
      MESSAGE "OBS! Vill du skriva ut beställning för alla leverantörer?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Utskrift?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         alla = TRUE.
      END.                     
      ELSE DO:
         alla = FALSE.
      END.  
      IF alla = TRUE THEN DO: 
         RUN alla_UI.
      END.
      ELSE DO:
         RUN ut_UI.  
         IF vald_lev = "99" THEN DO:
            RUN ut2_UI. 
         END.   
      END.     
      alla = FALSE.           
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-2
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-VINST
DO:
   FOR EACH tidut:
      DELETE tidut.
   END.  
   ASSIGN                                               
   leverant = INPUT CMB_LEV.                
   FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = LEVERANTOR.LEVKOD.
   FIND FIRST skapa_mtrl.
   ASSIGN
   skapa_mtrl.LEVNAMN = LEVERANTOR.LEVNAMN
   skapa_mtrl.LKONTAKT = LEVERANTOR.LEVKONTAKT
   skapa_mtrl.LTELE = LEVERANTOR.LEVTEL
   skapa_mtrl.LADR = LEVERANTOR.LEVADR
   skapa_mtrl.LPNR = LEVERANTOR.LEVPNR
   skapa_mtrl.LORT = LEVERANTOR.LEVORT.
   str=
"=====================================================================================".         
   RUN huvud_UI.  
   IF detvar = FALSE THEN DO:
      RUN rubrik_UI.
      IF musz = FALSE THEN RUN klar_UI.   
      RUN totalt_UI.         
   END.
   ELSE DO:             
      RUN summa_UI.       
   END.             
   RUN sidfot_UI.
   RUN enable_UI.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


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
   {muswait.i} 
   RUN delete_UI.  
   IF alla = FALSE THEN DO:
      status-ok = CMB_LEV:DELETE("0"). 
      FIND best_mtrl WHERE best_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
      FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = vald_lev NO-LOCK NO-ERROR.     
      ASSIGN       
      status-ok = CMB_LEV:ADD-LAST(LEVERANTOR.LEVNAMN)IN FRAME {&FRAME-NAME}
      CMB_LEV:SCREEN-VALUE = LEVERANTOR.LEVNAMN
      lev = " ".      
      FOR EACH best_mtrl WHERE best_mtrl.LEVKOD NE vald_lev BY best_mtrl.LEVKOD:
         FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = best_mtrl.LEVKOD NO-LOCK NO-ERROR.
         IF LEVERANTOR.LEVKOD NE lev THEN DO:
            ASSIGN  
            status-ok = CMB_LEV:ADD-LAST(LEVERANTOR.LEVNAMN)IN FRAME {&FRAME-NAME}
            lev = best_mtrl.LEVKOD. 
         END.                
      END.                                                     
      FOR EACH tidut:
         DELETE tidut.
      END.  
      str=
"=============================================================================". 
      str1 =
"*****************************************************************************".  
      str0 = 
"-----------------------------------------------------------------------------".     
      RUN huvud_UI.
      IF musz = FALSE THEN RUN klar_UI.   
      RUN skapa_UI.
      RUN summa_UI.                              
      IF musz = TRUE THEN DO:
         musz = FALSE.
         status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.                 
      RUN ut_UI.
      IF vald_lev = "99" THEN DO:
         RUN ut2_UI.
      END.   
/*      MESSAGE "Beställning är nu skickad via mail" VIEW-AS ALERT-BOX.*/
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK.       
   END.
   RUN enable_UI.           
   {musarrow.i}                                                
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anmark_UI WINDOW-2 
PROCEDURE anmark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER anmark AS INTEGER NO-UNDO.
   IF anmark = 1 THEN DO:                  
      CREATE tidut.
      ASSIGN                  
      SUBSTRING(tidut.UT,12) = SUBSTRING(edtext,ednum,edtecken).
   END.  
   ELSE IF anmark = 2 THEN DO:           
      CREATE tidut.
      ASSIGN            
      SUBSTRING(tidut.UT,12) = tidtext.
   END.   
   ELSE IF anmark = 3 THEN DO:           
      CREATE tidut.
      ASSIGN           
      SUBSTRING(tidut.UT,12) = SUBSTRING(edtext,1 + ednum2 * edtecken,edtecken).
   END.                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2 _DEFAULT-DISABLE
PROCEDURE disable_UI :
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2 _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  DISPLAY CMB_LEV 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  ENABLE RECT-35 BTN_AVS BTN_SKRIV CMB_LEV 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
  VIEW WINDOW-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-2 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     /*HUVUD*/ 
   DO TRANSACTION:         
      IF musz = TRUE THEN musz = musz.   
      ELSE DO: 
         FIND FIRST skapa_mtrl NO-LOCK NO-ERROR.
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "<Order>" + CHR(10).
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "<SystemID>GURU</SystemID>" + CHR(10).
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "<Orderhuvud>" + CHR(10).
         CREATE tidut.
         IF bestoff = "beställning" THEN DO:
            ASSIGN SUBSTRING(tidut.UT,1) = "<Dokumenttyp>Beställning</Dokumenttyp>" + CHR(10).
         END.
         IF bestoff = "offert" THEN DO:
            ASSIGN SUBSTRING(tidut.UT,1) = "<Dokumenttyp>Offert</Dokumenttyp>" + CHR(10).
         END.   
         CREATE tidut. 
         ASSIGN SUBSTRING(tidut.UT,1) = "<Datum>"
         SUBSTRING(tidut.UT,8) = STRING(TODAY,"9999/99/99")
         SUBSTRING(tidut.UT,18) = "</Datum>" + CHR(10).
         CREATE tidut. 
         ASSIGN SUBSTRING(tidut.UT,1) = "<Tid>"        
         SUBSTRING(tidut.UT,6) = STRING(TIME,"HH:MM:SS"). 
         SUBSTRING(tidut.UT,14) = "</Tid>" + CHR(10).         
         FIND LEVERANTOR WHERE LEVERANTOR.LEVKOD = vald_lev NO-LOCK NO-ERROR. 
         CREATE tidut.          
         ASSIGN SUBSTRING(tidut.UT,9) = "<Kundnummer>"        
         SUBSTRING(tidut.UT,21) = STRING(skapa_mtrl.KUNDNR,"99999") + "</Kundnummer>" + CHR(10).
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Firma>"        
         SUBSTRING(tidut.UT,16) = SUBSTRING(skapa_mtrl.FORE,1,35) + "</Firma>" + CHR(10).
         IF skapa_mtrl.KIKONTAKT NE "" THEN DO:
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Kontaktperson>"        
            SUBSTRING(tidut.UT,24) = SUBSTRING(skapa_mtrl.KIKONTAKT,1,35) + "</Kontaktperson>" + CHR(10).
         END.
         IF skapa_mtrl.KITELE NE "" THEN DO:   
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Telefon>"        
            SUBSTRING(tidut.UT,18) = STRING(skapa_mtrl.KITELE) + "</Telefon>" + CHR(10).
         END.
         IF skapa_mtrl.FAX NE "" THEN DO:    
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Fax>"        
            SUBSTRING(tidut.UT,14) = STRING(skapa_mtrl.FAX) + "</Fax>" + CHR(10).
         END.   
         IF skapa_mtrl.KIMOBIL NE "" THEN DO:   
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Mobil>"        
            SUBSTRING(tidut.UT,16) = STRING(skapa_mtrl.KIMOBIL) + "</Mobil>" + CHR(10).
         END.   
/*         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Leveransadressat>" +
         STRING(skapa_mtrl.DEPA) + "</Leveransadressat>".*/
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Gatuadress>"        
         SUBSTRING(tidut.UT,21) = SUBSTRING(skapa_mtrl.L1,1,35) + "</Gatuadress>" + CHR(10).
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Postnummer>"        
         SUBSTRING(tidut.UT,21) = STRING(skapa_mtrl.L2,"999 99") + "</Postnummer>" + CHR(10).
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Ort>"        
         SUBSTRING(tidut.UT,14) = SUBSTRING(skapa_mtrl.L3,1,35) + "</Ort>" + CHR(10).
         CREATE tidut.        
         ASSIGN SUBSTRING(tidut.UT,9) = "<ProjektID>"        
         SUBSTRING(tidut.UT,20) = TRIM(STRING(skapa_mtrl.bestnr))
         + "</ProjektID>" + CHR(10).
         CREATE tidut.
         IF skapa_mtrl.LEVTID = "normal" OR skapa_mtrl.LEVTID = "snabb" THEN DO:
            ASSIGN SUBSTRING(tidut.UT,9) = "<Leveranstid>"        
            SUBSTRING(tidut.UT,22) = STRING(skapa_mtrl.LEVTID) + "</Leveranstid>" + CHR(10).
         END.
         ELSE IF skapa_mtrl.LEVTID = "datum" THEN DO:
            ASSIGN SUBSTRING(tidut.UT,9) = "<Leveranstid>"        
            SUBSTRING(tidut.UT,22) = STRING(skapa_mtrl.DATUM,"9999/99/99") + "</Leveranstid>" + CHR(10).
         END.
         IF skapa_mtrl.KOM NE "" THEN DO:
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Kommentarorderhuvud>"        
            SUBSTRING(tidut.UT,30) = STRING(skapa_mtrl.KOM) + "</Kommentarorderhuvud>" + CHR(10).
         END.   
         IF skapa_mtrl.AVIS = TRUE THEN DO: 
            CREATE tidut.         
            ASSIGN SUBSTRING(tidut.UT,9) = "<Avisering>"        
            SUBSTRING(tidut.UT,20) = STRING(skapa_mtrl.AVISPERS)
            + "</Avisering>" + CHR(10).
         END.
         IF skapa_mtrl.MARK NE "" THEN DO:   
            CREATE tidut.
            ASSIGN SUBSTRING(tidut.UT,9) = "<Marke>"        
            SUBSTRING(tidut.UT,16) = STRING(skapa_mtrl.MARK) + "</Marke>" + CHR(10).
         END.   
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,1) = "</Orderhuvud>" + CHR(10).               
       
      END.
   END.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI WINDOW-2 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "<Orderdata>" + CHR(10)        
   mtrlrak = 1.
   mtrlrak2 = 0.
   radrak = 0.
   FOR EACH trp_mtrl WHERE trp_mtrl.LEVKOD = vald_lev AND trp_mtrl.ANTAL > 0: 
      IF trp_mtrl.DBEST NE "RETUR" THEN DO:  
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "<Post " + STRING(mtrlrak) + ">" + CHR(10).
         radrak = radrak + 1.      
         CREATE tidut.
         ASSIGN 
         SUBSTRING(tidut.UT,9) = "<Artikelnr>" + STRING(RIGHT-TRIM(trp_mtrl.ENR)) + "</Artikelnr>" + CHR(10).               
         radrak = radrak + 1.
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Beskrivning>" +        
         STRING(RIGHT-TRIM(trp_mtrl.BENAMNING)) + "</Beskrivning>" + CHR(10). 
         radrak = radrak + 1.     
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Enhet>" + STRING(RIGHT-TRIM(trp_mtrl.ENHET)) + "</Enhet>" + CHR(10). 
         radrak = radrak + 1.
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Antal>" + TRIM(STRING(trp_mtrl.ANTAL))
         + "</Antal>" + CHR(10).
         mtrlrak2 = mtrlrak2 + trp_mtrl.ANTAL.
         radrak = radrak + 1.
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,9) = "<Pris>" + RIGHT-TRIM(STRING(trp_mtrl.OPRIS))
         + "</Pris>" + CHR(10).
         radrak = radrak + 1.
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" THEN DO:
            FIND FIRST BEREDNING WHERE RECID(BEREDNING) = kalkrec NO-LOCK NO-ERROR.            
            IF BEREDNING.AONR NE ? THEN DO:               
               FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.AONR = BEREDNING.AONR AND 
               AONRKONTKOD.DELNR = BEREDNING.DELNR NO-LOCK NO-ERROR.
               IF AVAILABLE AONRKONTKOD THEN DO:
                  CREATE tidut.
                  ASSIGN SUBSTRING(tidut.UT,9) = "<kontosträng1>" + AONRKONTKOD.K1
                  + "</kontosträng1>" + CHR(10).
                  radrak = radrak + 1.
                  CREATE tidut.
                  ASSIGN SUBSTRING(tidut.UT,9) = "<kontosträng2>" + AONRKONTKOD.K2
                  + "</kontosträng2>" + CHR(10).
                  radrak = radrak + 1.
                  CREATE tidut.
                  ASSIGN SUBSTRING(tidut.UT,9) = "<kontosträng3>" + AONRKONTKOD.K3
                  + "</kontosträng3>" + CHR(10).
                  radrak = radrak + 1.
                  CREATE tidut.
                  ASSIGN SUBSTRING(tidut.UT,9) = "<kontosträng4>" + AONRKONTKOD.K4 + AONRKONTKOD.K5
                  + "</kontosträng4>" + CHR(10).
                  radrak = radrak + 1.                             
               END.   
            END.
         END.
         
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "</Post " + STRING(mtrlrak) + ">" + CHR(10).
         radrak = radrak + 1.
         CREATE tidut.
         radrak = radrak + 1.
         mtrlrak = mtrlrak + 1.
      END.      
   END.   
   mtrlrak = mtrlrak - 1.  
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "</Orderdata>" + CHR(10).
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "<Checksumma>" + STRING(radrak + mtrlrak2) 
   + "</Checksumma>" + CHR(10).
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE linor_UI WINDOW-2 
PROCEDURE linor_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   {LINORTRP.I}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE materiel_UI WINDOW-2 
PROCEDURE materiel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = mtrlbuff.ENR
   SUBSTRING(tidut.UT,13) = SUBSTRING(mtrlbuff.BENAMNING,1,35) 
   SUBSTRING(tidut.UT,49) = STRING(mtrlbuff.PRIS,">>>>99.99")     
   SUBSTRING(tidut.UT,59) = mtrlbuff.ENHET
   SUBSTRING(tidut.UT,65) = STRING(mtrlbuff.ANTAL,">>>>9")    
   SUBSTRING(tidut.UT,71) = STRING(mtrlbuff.TOTPRIS,">>>,>>9")
   SUBSTRING(tidut.UT,80) = CHR(10).
   sumpris = sumpris + mtrlbuff.TOTPRIS.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik_UI WINDOW-2 
PROCEDURE rubrik_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = "ENR"                                                                          
   SUBSTRING(tidut.UT,13) = "BENÄMNING"                     
   SUBSTRING(tidut.UT,49) = "NETTOPRIS"         
   SUBSTRING(tidut.UT,59) = "ENHET"
   SUBSTRING(tidut.UT,65) = "ANTAL"
   SUBSTRING(tidut.UT,71) = "SUMMA"
   SUBSTRING(tidut.UT,80) = CHR(10).                         
   CREATE tidut.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sidfot_UI WINDOW-2 
PROCEDURE sidfot_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   CREATE tidut. 
   CREATE tidut.                                   
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Märkning   :"
   SUBSTRING(tidut.UT,13) = STRING(skapa_mtrl.MARK)
   SUBSTRING(tidut.UT,80) = CHR(10).
   CREATE tidut.                                    
   ASSIGN
   SUBSTRING(tidut.UT,1) = "Kommentarer:"
   SUBSTRING(tidut.UT,13) = STRING(skapa_mtrl.KOM)
   SUBSTRING(tidut.UT,80) = CHR(10).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa_UI WINDOW-2 
PROCEDURE skapa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   CREATE tidut. 
   ASSIGN SUBSTRING(tidut.UT,1) = "<Packningsbeskrivning>" + CHR(10).        
   {SKAPATRP.I}         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE summa_UI WINDOW-2 
PROCEDURE summa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   {SUMMATRP.I}      
   CREATE tidut. 
   ASSIGN SUBSTRING(tidut.UT,1) = "</Packningsbeskrivning>" + CHR(10).
   CREATE tidut.
   ASSIGN SUBSTRING(tidut.UT,1) = "</Order>" + CHR(10).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ett_UI WINDOW-2 
PROCEDURE ett_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {ETTTRP.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tva_UI WINDOW-2 
PROCEDURE tva_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {TVATRP.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE id_UI WINDOW-2 
PROCEDURE id_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
  {IDTRP.I}              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE totalt_UI WINDOW-2 
PROCEDURE totalt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    /*SUMMERING AV ALLA ENR*/
   ASSIGN
   totalt = 0 
   totalt2 = 0.  
   FOR EACH trp_mtrl WHERE trp_mtrl.LEVKOD = vald_lev: 
      IF trp_mtrl.DBEST NE "RETUR" THEN DO:
         totalt = totalt + (trp_mtrl.OPRIS * trp_mtrl.ANTAL).      
      END.   
   END. 
   str=
"=====================================================================.=======" + CHR(10).      
   CREATE tidut. 
   CREATE tidut.      
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.
   CREATE tidut.         
   ASSIGN
   SUBSTRING(tidut.UT,58) = "Summa totalt:"
   SUBSTRING(tidut.UT,71) = STRING(totalt,">>>,>>9")
   SUBSTRING(tidut.UT,80) = CHR(10).
   CREATE tidut.         
   SUBSTRING(tidut.UT,71) = "=======" + CHR(10). 
   FIND FIRST off_mtrl WHERE off_mtrl.LEVKOD = vald_lev NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      CREATE tidut.                    
      CREATE tidut.
      SUBSTRING(tidut.UT,71) = "=======" + CHR(10).
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,56) = "Offererat pris:"
      SUBSTRING(tidut.UT,71) = STRING(off_mtrl.RABTOT,">>>,>>9")
      SUBSTRING(tidut.UT,80) = CHR(10).     
   END.                           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skydd_UI WINDOW-2 
PROCEDURE skydd_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {SKYDDTRP.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upplag_UI WINDOW-2 
PROCEDURE upplag_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   IF AVAILABLE BERUPP THEN DO:
      ASSIGN
      retvar = 1
      ednum = 1
      ednum3 = LENGTH(BERUPP.ANMARK)
      retvar = INDEX(BERUPP.ANMARK,CHR(10),ednum)
      edtecken = 50
      edtext = BERUPP.ANMARK
      tidtext = "".  
      {ANMARK2.I}                             
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut2_UI WINDOW-2 
PROCEDURE ut2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      prognamn = "\\GRANGURU\guru_ser\server\PRO8S\gran\".      
      prognamn = prognamn + "berbest" + STRING(skapa_mtrl.bestnr) + ".txt".                        
      OUTPUT TO VALUE(prognamn) APPEND.     
   END.
   ELSE IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SOLE" THEN DO:
       prognamn = "C:\GURU\".      
       prognamn = prognamn + "berbest" + STRING(skapa_mtrl.bestnr) + ".txt".
       OUTPUT TO VALUE(prognamn) APPEND.     
   END.
   FOR EACH trp_mtrl WHERE trp_mtrl.LEVKOD = vald_lev AND
   trp_mtrl.DBEST NE "RETUR": 
      FIND FIRST MTRLSPEC WHERE MTRLSPEC.ENR = trp_mtrl.ENR AND 
      MTRLSPEC.LEVKOD = vald_lev NO-LOCK NO-ERROR.
      IF AVAILABLE MTRLSPEC THEN DO:
         MED_EDITOR = MTRLSPEC.MED.
        /* OUTPUT TO PRINTER PAGE-SIZE VALUE(globsids)
         CONVERT TARGET "iso8859-1". */
         PUT "Kompletterande uppgifter till materiel" AT 6           
         TODAY AT  51
         "=====================================================" AT 6 
         "LEVERANTÖR :     SPECIAL" AT 6    
         "ENR :" AT 6 
         MTRLSPEC.ENR AT 23  
         "BENÄMNING :" AT 6 
         trp_mtrl.BENAMNING AT 23 
         "=====================================================" AT 6 
         MED_EDITOR AT 6.
         OUTPUT CLOSE.  
      END.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alla_UI WINDOW-2 
PROCEDURE alla_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
   FOR EACH tidut:
      DELETE tidut.
   END.   
   FIND FIRST lev_temp WHERE lev_temp.LEVKOD = vald_lev.     
   firstlev = vald_lev.      
   str=
   "=============================================================================".      
   RUN huvud_UI.
   IF detvar = FALSE THEN DO:
      RUN rubrik_UI.
      IF musz = FALSE THEN RUN klar_UI.   
      RUN totalt_UI.         
   END.
   ELSE DO:               
      RUN summa_UI.                              
   END.                             
   RUN sidfot_UI.
   RUN ut_UI.  
   IF vald_lev = "99" THEN DO:
      RUN ut2_UI.
   END.     
   FOR EACH lev_temp WHERE lev_temp.LEVKOD NE firstlev:        
      FOR EACH tidut:
         DELETE tidut.
      END.     
      FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = lev_temp.LEVKOD NO-LOCK NO-ERROR.
      vald_lev = LEVERANTOR.LEVKOD.
      FIND FIRST skapa_mtrl.
      ASSIGN                                          
      skapa_mtrl.LEVNAMN = LEVERANTOR.LEVNAMN
      skapa_mtrl.LKONTAKT = LEVERANTOR.LEVKONTAKT
      skapa_mtrl.LTELE = LEVERANTOR.LEVTEL
      skapa_mtrl.LADR = LEVERANTOR.LEVADR
      skapa_mtrl.LPNR = LEVERANTOR.LEVPNR
      skapa_mtrl.LORT = LEVERANTOR.LEVORT.     
      ASSIGN      
      str=
      "=============================================================================".      
      RUN huvud_UI.
      IF detvar = FALSE THEN DO:
         RUN rubrik_UI.
         IF musz = FALSE THEN RUN klar_UI.   
         RUN totalt_UI.         
      END.
      ELSE DO:                  
         RUN summa_UI.                              
      END.                             
      RUN sidfot_UI.  
      RUN ut_UI.  
      IF vald_lev = "99" THEN DO:
         RUN ut2_UI.
      END.         
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME 

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete_UI WINDOW-2 
PROCEDURE delete_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FOR EACH mtrl_temp:
      DELETE mtrl_temp.
   END.      
   FOR EACH mtrl_temp2:
      DELETE mtrl_temp2.
   END.
   FOR EACH uppl_temp:
      DELETE uppl_temp.
   END.
   FOR EACH grupp_temp:
      DELETE grupp_temp.
   END.   
   FOR EACH grupp_temp2:
      DELETE grupp_temp2.
   END.   
   FOR EACH lin_upp:
      DELETE lin_upp.
   END.   
   FOR EACH skydd_temp:
      DELETE skydd_temp.
   END.   
   FOR EACH skydd_temp2:
      DELETE skydd_temp2.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE depa_UI WINDOW-2 
PROCEDURE depa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {DEPATRP.I} 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nedepa_UI WINDOW-2 
PROCEDURE nedepa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   {NEDEPA.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE depa2_UI WINDOW-2 
PROCEDURE depa2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {DEPA2TRP.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nedepa2_UI WINDOW-2 
PROCEDURE nedepa2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   {NEDEPA2.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE depa3_UI WINDOW-2 
PROCEDURE depa3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {DEPA3TRP.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nedepa3_UI WINDOW-2 
PROCEDURE nedepa3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   {NEDEPA3.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
/*UT*/         
   prognamn = SESSION:TEMP-DIRECTORY.       
   IF bestoff = "beställning" THEN DO:
      prognamn = prognamn + "berbest" + STRING(skapa_mtrl.bestnr) + ".txt".     
      prognamn2 = "berbest" + STRING(skapa_mtrl.bestnr) + ".txt".                   
   END.
   IF bestoff = "offert" THEN DO:
      prognamn = prognamn + "beroff" + STRING(skapa_mtrl.bestnr) + ".txt".   
      prognamn2 = "beroff" + STRING(skapa_mtrl.bestnr) + ".txt".                     
   END.        
   OUTPUT TO VALUE(prognamn).
   FOR EACH tidut:          
     PUT tidut.UT.
     PUT CHR(10).
   END.  
   OUTPUT CLOSE.       
   
/*   IF SEARCH(prognamn) = ? THEN DO:
      MESSAGE "Filen finns ej." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.        */
   EDITOR_MEDD = "".
   FOR EACH tidut:
      EDITOR_MEDD = EDITOR_MEDD + CHR(10) + tidut.UT.
   END.    
   IF Guru.Konstanter:globforetag = "ELPA"  THEN DO:
      RUN EPOST.P (INPUT "",INPUT "Allan_Pettersson@elef.se",INPUT bestoff,INPUT EDITOR_MEDD,
      INPUT "",INPUT "",OUTPUT skick,OUTPUT efel).
      MESSAGE efel VIEW-AS ALERT-BOX.          
      IF skick = TRUE THEN RETURN NO-APPLY.
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND"  THEN DO:    
      IF bestoff = "beställning" THEN DO:
         /*"order@elef.se"*/
         RUN EPOST.P (INPUT "",INPUT "Allan_Pettersson@elef.se",INPUT bestoff,INPUT EDITOR_MEDD,         
         INPUT "",INPUT "",OUTPUT skick,OUTPUT efel).
         MESSAGE efel VIEW-AS ALERT-BOX.   
         IF skick = TRUE THEN RETURN NO-APPLY.       
      END.
      IF bestoff = "offert" THEN DO:
         /*"offert@elef.se"*/
         RUN EPOST.P (INPUT "",INPUT "Allan_Pettersson@elef.se",INPUT bestoff,INPUT EDITOR_MEDD,         
         INPUT "",INPUT "",OUTPUT skick,OUTPUT efel).
         MESSAGE efel VIEW-AS ALERT-BOX.      
         IF skick = TRUE THEN RETURN NO-APPLY.    
      END.

   END.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE BROWSE-NAME
&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
