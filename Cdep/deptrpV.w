&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/22/96 - 10:33 am

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

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{SPECMTRLTEMP.I}
{MTRLTEMP.I}
{LTRPTEMP.I}
{DEPATEMP.I}
{HOPPSEK2W.I}

&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{SKAPAMTRL.I}
DEFINE NEW SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE pris AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE epostvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE bestalld AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vald_lev  AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE SHARED VARIABLE nytt_bestnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE aonrdelnrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leve AS CHARACTER NO-UNDO.   
DEFINE VARIABLE omra AS CHARACTER NO-UNDO.   
DEFINE VARIABLE valomra AS CHARACTER NO-UNDO.  
DEFINE VARIABLE val AS LOGICAL NO-UNDO.  
DEFINE VARIABLE bestoff AS CHARACTER NO-UNDO.
DEFINE VARIABLE knr AS INTEGER NO-UNDO.
DEFINE VARIABLE namn AS CHARACTER NO-UNDO.
DEFINE VARIABLE telf AS CHARACTER NO-UNDO.
DEFINE VARIABLE mtelf AS CHARACTER NO-UNDO.
DEFINE VARIABLE avisp AS CHARACTER NO-UNDO.
DEFINE VARIABLE ganv AS CHARACTER NO-UNDO.
DEFINE VARIABLE epost AS CHARACTER NO-UNDO.
   
DEFINE NEW SHARED TEMP-TABLE lev_temp NO-UNDO
   FIELD LEVKOD AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-LEVNAMN FILL-IN-LKONTAKT ~
FILL-IN-LTELE FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT FBTN_VISA CMB_LEV ~
FBTN_AVSTATUS FBTN_VSTATUS FBTN_MAIL FILL-IN-KUNDNR FILL-IN-BESTNR ~
FILL-IN-FOR FBTN_EDI FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT ~
FILL-IN-FAX FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KIMOBIL ~
FILL-IN-KIEPOST FILL-IN-KTKONTAKT FILL-IN-KTTELE FILL-IN-KTMOBIL ~
FILL-IN-DATUM FILL-IN_EAONR FILL-IN_DELNR FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 ~
FILL-IN-MARK FILL-IN-KOM FILL-IN-AVIS FILL-IN-AVISPERS BTN_AVB FILL-IN-DEPA 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LEVNAMN FILL-IN-LKONTAKT ~
FILL-IN-LTELE FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT CMB_LEV FILL-IN-STATUS ~
FILL-IN-KUNDNR FILL-IN-BESTNR FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX ~
FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX FILL-IN-KIKONTAKT FILL-IN-KITELE ~
FILL-IN-KIMOBIL FILL-IN-KIEPOST FILL-IN-KTKONTAKT FILL-IN-KTTELE ~
FILL-IN-KTMOBIL FILL-IN-DATUM FILL-IN_EAONR FILL-IN_DELNR FILL-IN-L1 ~
FILL-IN-L2 FILL-IN-L3 FILL-IN-MARK FILL-IN-KOM FILL-IN-AVIS ~
FILL-IN-AVISPERS FILL-IN-DEPA 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_AVSTATUS 
     LABEL "Återställ status" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_EDI 
     LABEL "Best.Elektroskandia" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_MAIL 
     LABEL "Best.Onninen" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VSTATUS 
     LABEL "Visa status" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVIS AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Avisering" 
     VIEW-AS FILL-IN 
     SIZE 4.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVISPERS AS CHARACTER FORMAT "X(30)":U 
     LABEL "Namn + telnr" 
     VIEW-AS FILL-IN 
     SIZE 28.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTNR AS CHARACTER FORMAT "X(20)":U 
     LABEL "Best.nr" 
     VIEW-AS FILL-IN 
     SIZE 22.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BOX AS CHARACTER FORMAT "X(4)":U 
     LABEL "Box" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS DATE FORMAT "99/99/99":U 
     LABEL "Leveransdag" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DEPA AS CHARACTER FORMAT "X(35)":U 
     LABEL "Information för depå" 
      VIEW-AS TEXT 
     SIZE 35.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAX AS CHARACTER FORMAT "X(15)":U 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FOR AS CHARACTER FORMAT "X(35)":U 
     LABEL "Företag" 
     VIEW-AS FILL-IN 
     SIZE 36.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KADR AS CHARACTER FORMAT "X(30)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 30.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIEPOST AS CHARACTER FORMAT "X(256)":U 
     LABEL "E-postadress" 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIKONTAKT AS CHARACTER FORMAT "X(50)":U 
     LABEL "Kontaktperson inköp" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KIMOBIL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Mobiltel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KITELE AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOM AS CHARACTER FORMAT "X(80)":U 
     LABEL "Kommentarer" 
     VIEW-AS FILL-IN 
     SIZE 54.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KORT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 16.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KPNR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KTKONTAKT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kontaktperson teknik" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KTMOBIL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Mobiltel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KTTELE AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KUNDNR AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
     LABEL "Kundnummer" 
     VIEW-AS FILL-IN 
     SIZE 15.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L1 AS CHARACTER FORMAT "X(35)":U 
     LABEL "Leveransadress" 
     VIEW-AS FILL-IN 
     SIZE 31.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L2 AS CHARACTER FORMAT "999 99":U 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L3 AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 15.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LADR AS CHARACTER FORMAT "X(20)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 21.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEVNAMN AS CHARACTER FORMAT "X(25)":U 
     LABEL "Leverantör" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LKONTAKT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 26.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LORT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 16.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LPNR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LTELE AS CHARACTER FORMAT "xxxx-xxxxxxx":U 
     LABEL "Tel" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MARK AS CHARACTER FORMAT "X(35)":U 
     LABEL "Märkning" 
     VIEW-AS FILL-IN 
     SIZE 54.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STATUS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83
     FGCOLOR 2 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     FILL-IN-LEVNAMN AT ROW 4.25 COL 21 COLON-ALIGNED
     FILL-IN-LKONTAKT AT ROW 5.42 COL 21 COLON-ALIGNED
     FILL-IN-LTELE AT ROW 5.42 COL 60.38 COLON-ALIGNED
     FILL-IN-LADR AT ROW 6.67 COL 21 COLON-ALIGNED
     FILL-IN-LPNR AT ROW 7.83 COL 21 COLON-ALIGNED
     FILL-IN-LORT AT ROW 7.83 COL 60.38 COLON-ALIGNED
     FBTN_VISA AT ROW 8 COL 96.25
     CMB_LEV AT ROW 9 COL 21 COLON-ALIGNED
     FILL-IN-STATUS AT ROW 9 COL 60.38 COLON-ALIGNED
     FBTN_AVSTATUS AT ROW 9 COL 96.25
     FBTN_VSTATUS AT ROW 10.75 COL 96.25
     FBTN_MAIL AT ROW 12 COL 96.25
     FILL-IN-KUNDNR AT ROW 12.38 COL 21 COLON-ALIGNED
     FILL-IN-BESTNR AT ROW 12.38 COL 60.38 COLON-ALIGNED
     FILL-IN-FOR AT ROW 13.58 COL 21 COLON-ALIGNED
     FBTN_EDI AT ROW 14 COL 96.25
     FILL-IN-KADR AT ROW 14.79 COL 21 COLON-ALIGNED
     FILL-IN-BOX AT ROW 14.79 COL 60.38 COLON-ALIGNED
     FILL-IN-KPNR AT ROW 15.96 COL 21 COLON-ALIGNED
     FILL-IN-KORT AT ROW 15.96 COL 35 COLON-ALIGNED
     FILL-IN-FAX AT ROW 15.96 COL 60.38 COLON-ALIGNED
     FILL-IN-KIKONTAKT AT ROW 17.25 COL 21 COLON-ALIGNED
     FILL-IN-KITELE AT ROW 17.25 COL 60.38 COLON-ALIGNED
     FILL-IN-KIMOBIL AT ROW 18.46 COL 21 COLON-ALIGNED
     FILL-IN-KIEPOST AT ROW 18.46 COL 60.38 COLON-ALIGNED
     FILL-IN-KTKONTAKT AT ROW 19.67 COL 21 COLON-ALIGNED
     FILL-IN-KTTELE AT ROW 19.67 COL 60.38 COLON-ALIGNED
     FILL-IN-KTMOBIL AT ROW 20.88 COL 21 COLON-ALIGNED
     FILL-IN-DATUM AT ROW 22.17 COL 60.38 COLON-ALIGNED
     FILL-IN_EAONR AT ROW 22.25 COL 21 COLON-ALIGNED WIDGET-ID 4 AUTO-RETURN 
     FILL-IN_DELNR AT ROW 22.25 COL 29.88 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     FILL-IN-L1 AT ROW 23.33 COL 21 COLON-ALIGNED
     FILL-IN-L2 AT ROW 23.33 COL 52.75 COLON-ALIGNED NO-LABEL
     FILL-IN-L3 AT ROW 23.33 COL 60.38 COLON-ALIGNED NO-LABEL
     FILL-IN-MARK AT ROW 24.58 COL 21 COLON-ALIGNED
     FILL-IN-KOM AT ROW 25.79 COL 21 COLON-ALIGNED
     FILL-IN-AVIS AT ROW 26.96 COL 21 COLON-ALIGNED
     FILL-IN-AVISPERS AT ROW 26.96 COL 40.5 COLON-ALIGNED
     BTN_AVB AT ROW 26.96 COL 96.25
     FILL-IN-DEPA AT ROW 11.21 COL 21 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112 BY 27.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Leverans och transport"
         HEIGHT             = 28.04
         WIDTH              = 112
         MAX-HEIGHT         = 28.04
         MAX-WIDTH          = 114
         VIRTUAL-HEIGHT     = 28.04
         VIRTUAL-WIDTH      = 114
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-STATUS IN FRAME FRAME-B
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:
   FIND FIRST ltrptemp WHERE ltrptemp.BESTNR = nytt_bestnr AND ltrptemp.LEVKOD = vald_lev
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO: 
      APPLY "LEAVE" TO FILL-IN-KUNDNR.
      RUN levtrp_UI.           
   END.      
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-2
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-B /* Leverantörer */
DO:             
   RUN cmblev_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVSTATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVSTATUS WINDOW-2
ON CHOOSE OF FBTN_AVSTATUS IN FRAME FRAME-B /* Återställ status */
DO:
   MESSAGE "Vill du verkligen ställa tillbaka status till ej beställd?" VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Status" UPDATE svar2 AS LOGICAL.         
   IF svar2 THEN DO:
      FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
      AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
      USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE ltrptemp THEN DO:
         ASSIGN ltrptemp.BESTALLD = "Ej beställd".
         ASSIGN FILL-IN-STATUS = "Ej beställd".
         DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
         sekunder = TIME.
         RUN SEKTIM.P.
         RUN beststatsp_UI IN mtrlbapph (INPUT nytt_bestnr,INPUT vald_depa,INPUT Guru.Konstanter:globanv, INPUT "Ej beställd" ,INPUT vald_lev,INPUT nytid).                                                                                                               
         RUN ltrpsp_UI IN mtrlbapph (INPUT vald_depa ,INPUT vald_lev ,INPUT nytt_bestnr,INPUT TABLE ltrptemp  ).  
         /*CREATE BESTSTAT.
         ASSIGN 
         BESTSTAT.BESTNR = nytt_bestnr
         BESTSTAT.DEP-NR = vald_depa
         BESTSTAT.ANVANDARE = Guru.Konstanter:globanv
         BESTSTAT.BESTALLD = "Ej beställd"
         BESTSTAT.DATUM = TODAY
         BESTSTAT.LEVKOD = vald_lev.
         sekunder = TIME.
         RUN SEKTIM.P.
         BESTSTAT.TID = nytid.
         RELEASE BESTSTAT.                */
      END.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EDI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EDI WINDOW-2
ON CHOOSE OF FBTN_EDI IN FRAME FRAME-B /* Best.Elektroskandia */
DO:           
   {muswait.i}     
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE 
   alla = FALSE
   leverant = INPUT CMB_LEV.
   APPLY "LEAVE" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.     
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD. 
   IF leverant NE "Elektroskandia" THEN DO:      
      MESSAGE "EDI-funktionen är bara i drift för Elektroskandia."
      VIEW-AS ALERT-BOX.     
      RETURN. 
   END.            
   IF FILL-IN-FOR = " " THEN DO:
      MESSAGE "Företag får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF FILL-IN-KIKONTAKT = " " THEN DO:
      MESSAGE "Kontaktperson inköp får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KIKONTAKT IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.
   IF FILL-IN-L1 = " " THEN DO:
      MESSAGE "Leveransadress får ej vara blank."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-L1 IN FRAME {&FRAME-NAME}.  
      RETURN.
   END.    
   IF ( INPUT FILL-IN-DATUM LE TODAY OR
   INPUT FILL-IN-DATUM = "") THEN DO:
      MESSAGE "Datum måste vara större än idag."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.   
   IF FILL-IN_EAONR:HIDDEN = FALSE THEN DO:
      IF FILL-IN_EAONR = "" THEN DO:
         MESSAGE Guru.Konstanter:gaok "Får inte vara blankt."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN_EAONR IN FRAME {&FRAME-NAME}.  
         RETURN.
      END.   
   END.        
   IF INPUT FILL-IN-KUNDNR = "" THEN DO:
      MESSAGE "Felaktigt kundnummer."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END. 
   IF LENGTH(INPUT FILL-IN-KUNDNR) NE 5 THEN DO:
      MESSAGE "Felaktigt kundnummer. Kundnr:et skall vara 5 siffror"
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.  
      
   IF FILL-IN-status = "EDI-beställd"  THEN DO:
      MESSAGE "Beställning är redan utförd med EDI. För att kunna skicka EDI igen återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.      
   IF FILL-IN-BESTNR = " "  THEN DO:
      MESSAGE "Beställningsnummer får ej vara blank."
      VIEW-AS ALERT-BOX.         
      APPLY "ENTRY" TO FILL-IN-BESTNR.
      RETURN. 
   END.
   IF FILL-IN-BESTNR = "0"  THEN DO:
      MESSAGE "Beställningsnummer får ej vara 0."
      VIEW-AS ALERT-BOX.         
      APPLY "ENTRY" TO FILL-IN-BESTNR.
      RETURN. 
   END.   
   IF FILL-IN-status = "Manuellt beställd"  THEN DO:
      MESSAGE "Beställning är redan utförd manuellt. För att kunna skicka EDI återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.      
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:
      APPLY "LEAVE" TO FILL-IN-KUNDNR.
      ASSIGN      
      ltrptemp.BESTALLD = "EDI-beställd".
      RUN levtrp_UI.
      
   END.   
   sekunder = TIME.
   RUN SEKTIM.P.
   RUN beststatsp_UI IN mtrlbapph (INPUT nytt_bestnr,INPUT vald_depa,INPUT Guru.Konstanter:globanv ,INPUT "EDI-beställd" ,INPUT vald_lev,INPUT nytid).
   /*CREATE BESTSTAT.
   ASSIGN 
   BESTSTAT.BESTNR = nytt_bestnr
   BESTSTAT.DEP-NR = vald_depa
   BESTSTAT.ANVANDARE = Guru.Konstanter:globanv
   BESTSTAT.BESTALLD = "EDI-beställd"
   BESTSTAT.DATUM = TODAY
   BESTSTAT.LEVKOD = vald_lev.
   sekunder = TIME.
   RUN SEKTIM.P.
   BESTSTAT.TID = nytid.
   RELEASE BESTSTAT.*/
   APPLY "LEAVE" TO FILL-IN-KUNDNR.
   RUN skapamtrl_UI.   
   ASSIGN   
   skapa_mtrl.BESTNR = STRING(nytt_bestnr)   
   bestoff = "Depåbeställning".  
   epostvar = INPUT FILL-IN-KIEPOST.
   aonrdelnrvar = FILL-IN_EAONR + TRIM(STRING(FILL-IN_DELNR,">>99")). 
   IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "VAST" THEN DO:
      RUN DEPELEKTRO.P (INPUT vald_lev, INPUT aonrdelnrvar).
   END.
   ELSE DO:   
      {AVBGOM.I}
      RUN EDIDEPV.W (INPUT bestoff).
      {AVBFRAM.I}
   END.
   /*
   MESSAGE 
   skapa_mtrl.LEVNAMN skip 
   skapa_mtrl.LKONTAKT skip 
   skapa_mtrl.LTELE skip 
   skapa_mtrl.LADR  skip
   skapa_mtrl.LPNR skip
   skapa_mtrl.LORT skip
   skapa_mtrl.FORE SKIP
   
   skapa_mtrl.KADR skip
   skapa_mtrl.KPNR skip
   skapa_mtrl.KORT skip
   skapa_mtrl.BOX skip
   skapa_mtrl.FAX skip
   skapa_mtrl.KIKONTAKT skip                         
   skapa_mtrl.KITELE skip
   skapa_mtrl.KIMOBIL skip
   skapa_mtrl.KTMOBIL skip
   skapa_mtrl.KTKONTAKT skip
   skapa_mtrl.KTTELE SKIP
   skapa_mtrl.KIEPOST
   skapa_mtrl.DATUM skip
   skapa_mtrl.MARK  skip
   skapa_mtrl.L1 skip
   skapa_mtrl.L2 skip
   skapa_mtrl.L3 skip
   skapa_mtrl.KOM skip
   skapa_mtrl.KUNDNR  skip
   skapa_mtrl.AVIS    skip
   skapa_mtrl.AVISPERS skip
   skapa_mtrl.BESTNR skip
   VIEW-AS ALERT-BOX.
   */
   bestalld = TRUE.
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:
      ASSIGN FILL-IN-STATUS = ltrptemp.BESTALLD.
      DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
   END.   
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MAIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MAIL WINDOW-2
ON CHOOSE OF FBTN_MAIL IN FRAME FRAME-B /* Best.Onninen */
DO:           
   {muswait.i}  
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE 
   alla = FALSE
   leverant = INPUT CMB_LEV.     
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD. 
   IF leverant NE "Onninen" THEN DO:
      MESSAGE "Mailfunktionen är bara i drift för Onninen."
      VIEW-AS ALERT-BOX.     
      RETURN. 
   END.           
   IF ( INPUT FILL-IN-DATUM LE TODAY OR
   INPUT FILL-IN-DATUM = "") THEN DO:
      MESSAGE "Datum måste vara större än idag."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.   
   IF Guru.Konstanter:globforetag = "BORL" THEN DO:
      /*DE HAR KUNDNUMMER 307800*/
      IF ( LENGTH(INPUT FILL-IN-KUNDNR) > 6 OR INPUT FILL-IN-KUNDNR = "" OR INPUT FILL-IN-KUNDNR = 0 ) THEN DO:      
         MESSAGE "Felaktigt kundnummer."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.  
   END.
   ELSE DO:
      IF ( LENGTH(INPUT FILL-IN-KUNDNR) > 5 OR INPUT FILL-IN-KUNDNR = "" OR INPUT FILL-IN-KUNDNR = 0 ) THEN DO:      
         MESSAGE "Felaktigt kundnummer."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
         RETURN.       
      END.  
   END.    
   IF FILL-IN-status = "Mailbeställd" OR FILL-IN-status = "Elektroniskt beställd" THEN DO:
      MESSAGE "Beställning är redan utförd. För att kunna skicka igen,
      återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.      
   IF FILL-IN-status = "Manuellt beställd"  THEN DO:
      MESSAGE "Beställning är redan utförd manuellt. För att kunna skicka mail 
      återställ status"
      VIEW-AS ALERT-BOX.         
      RETURN. 
   END.         
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:
      APPLY "LEAVE" TO FILL-IN-KUNDNR.
      ASSIGN      
      ltrptemp.BESTALLD = "Elektroniskt beställd".
      RUN levtrp_UI.      
   END.   
   sekunder = TIME.
   RUN SEKTIM.P.
   RUN beststatsp_UI IN mtrlbapph (INPUT nytt_bestnr,INPUT vald_depa,INPUT Guru.Konstanter:globanv ,INPUT "Elektroniskt beställd" ,INPUT vald_lev,INPUT nytid).
   /*CREATE BESTSTAT.
   ASSIGN 
   BESTSTAT.BESTNR = nytt_bestnr
   BESTSTAT.DEP-NR = vald_depa
   BESTSTAT.ANVANDARE = Guru.Konstanter:globanv
   BESTSTAT.BESTALLD = "Elektroniskt beställd"
   BESTSTAT.DATUM = TODAY
   BESTSTAT.LEVKOD = vald_lev.
   sekunder = TIME.
   RUN SEKTIM.P.
   BESTSTAT.TID = nytid.
   RELEASE BESTSTAT.*/
   APPLY "LEAVE" TO FILL-IN-KUNDNR.
   RUN skapamtrl_UI.   
   ASSIGN   
   skapa_mtrl.BESTNR = STRING(nytt_bestnr)   
   bestoff = "Beställning"
   epostvar = INPUT FILL-IN-KIEPOST.
   {AVBGOM.I}
   RUN MAILDEPV.W (INPUT bestoff).
   {AVBFRAM.I}
   bestalld = TRUE.
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:
      ASSIGN FILL-IN-STATUS = ltrptemp.BESTALLD.
      DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
   END.   
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-2
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-B /* Visa */
DO:           
   {muswait.i}  
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE 
   alla = FALSE
   leverant = INPUT CMB_LEV. 
   APPLY "LEAVE" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.     
   IF ( INPUT FILL-IN-DATUM LE TODAY OR
   INPUT FILL-IN-DATUM = "") THEN DO:
      MESSAGE "Datum måste vara större än idag."
      VIEW-AS ALERT-BOX.     
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.  
      RETURN. 
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      IF FILL-IN-FOR = " " THEN DO:
         MESSAGE "Företag får ej vara blank."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-FOR IN FRAME {&FRAME-NAME}.  
         RETURN.
      END.
      IF FILL-IN-KIKONTAKT = " " THEN DO:
         MESSAGE "Kontaktperson inköp får ej vara blank."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-KIKONTAKT IN FRAME {&FRAME-NAME}.  
         RETURN.
      END.
      IF FILL-IN-L1 = " " THEN DO:
         MESSAGE "Leveransadress får ej vara blank."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-L1 IN FRAME {&FRAME-NAME}.  
         RETURN.
      END.    
              
      IF INPUT FILL-IN-KUNDNR = "" THEN DO:
         MESSAGE "Felaktigt kundnummer."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.  
         RETURN. 
      END.       
      IF FILL-IN-MARK = "" THEN DO:
         MESSAGE "Märkning får ej vara blank."
         VIEW-AS ALERT-BOX.     
         APPLY "ENTRY" TO FILL-IN-MARK IN FRAME {&FRAME-NAME}.  
         RETURN.
      END.
   END.   
    
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD. 
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:
      APPLY "LEAVE" TO FILL-IN-KUNDNR.
      RUN levtrp_UI.      
   END.   
   APPLY "LEAVE" TO FILL-IN-KUNDNR.
   RUN skapamtrl_UI.
   ASSIGN   
   skapa_mtrl.BESTNR = STRING(nytt_bestnr).   
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN pris = FALSE.
   ELSE DO:   
      MESSAGE "OBS! Vill Ni visa med pris?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Pris?" UPDATE pris.               
   END.
   {AVBGOM.I}
   RUN VISADTRPV.W.
   {AVBFRAM.I}
   {musarrow.i}
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:
      IF ltrptemp.BESTALLD = "Ej beställd" OR ltrptemp.BESTALLD = "" THEN DO:
         MESSAGE "Har du skickat beställningen?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Status" UPDATE svar1 AS LOGICAL.         
         IF svar1 THEN DO:
            FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
            AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
            USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE ltrptemp THEN DO:
               ASSIGN ltrptemp.BESTALLD = "Manuellt beställd".
               ASSIGN FILL-IN-STATUS = ltrptemp.BESTALLD.
               DISPLAY FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
            END.   
            bestalld = TRUE.
            sekunder = TIME.
            RUN SEKTIM.P.
            RUN beststatsp_UI IN mtrlbapph (INPUT nytt_bestnr,INPUT vald_depa,INPUT Guru.Konstanter:globanv, INPUT "Manuellt beställd" ,INPUT vald_lev,INPUT nytid).                                                                                                               
            RUN ltrpsp_UI IN mtrlbapph (INPUT vald_depa ,INPUT vald_lev ,INPUT nytt_bestnr,INPUT TABLE ltrptemp  ).  
            /*CREATE BESTSTAT.
            ASSIGN 
            BESTSTAT.BESTNR = nytt_bestnr
            BESTSTAT.DEP-NR = vald_depa
            BESTSTAT.ANVANDARE = Guru.Konstanter:globanv
            BESTSTAT.BESTALLD = "Manuellt beställd"
            BESTSTAT.DATUM = TODAY
            BESTSTAT.LEVKOD = vald_lev.              
            sekunder = TIME.
            RUN SEKTIM.P.
            BESTSTAT.TID = nytid.

            RELEASE BESTSTAT.*/
         END.
      END.                             
      ELSE IF ltrptemp.BESTALLD = "Elektroniskt beställd" THEN DO:             
         MESSAGE "OBS! Beställningen är redan skickad" VIEW-AS ALERT-BOX.
      END.   
      ELSE IF ltrptemp.BESTALLD = "Manuellt beställd" THEN DO:             
         MESSAGE "OBS! Beställningen är redan manuellt beställd" VIEW-AS ALERT-BOX.
      END.
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VSTATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VSTATUS WINDOW-2
ON CHOOSE OF FBTN_VSTATUS IN FRAME FRAME-B /* Visa status */
DO:
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0 USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
  
   
   RUN VISSTATV.W (INPUT vald_lev, INPUT vald_depa ,INPUT nytt_bestnr).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVIS WINDOW-2
ON LEAVE OF FILL-IN-AVIS IN FRAME FRAME-B /* Avisering */
DO:
  FILL-IN-AVIS = INPUT FILL-IN-AVIS.  
  IF FILL-IN-AVIS = TRUE THEN DO:
     ASSIGN FILL-IN-AVISPERS:HIDDEN = FALSE.
     ENABLE FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
     DISPLAY FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
  END.   
  ELSE ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVIS WINDOW-2
ON MOUSE-SELECT-CLICK OF FILL-IN-AVIS IN FRAME FRAME-B /* Avisering */
DO:
   IF INPUT FILL-IN-AVIS = TRUE THEN FILL-IN-AVIS = FALSE.
   IF INPUT FILL-IN-AVIS = FALSE THEN FILL-IN-AVIS = TRUE.
   DISPLAY FILL-IN-AVIS WITH FRAME {&FRAME-NAME}.
   FILL-IN-AVIS = INPUT FILL-IN-AVIS.  
   IF FILL-IN-AVIS = TRUE THEN DO:
      ASSIGN FILL-IN-AVISPERS:HIDDEN = FALSE.
      ENABLE FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-AVISPERS WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BESTNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BESTNR WINDOW-2
ON LEAVE OF FILL-IN-BESTNR IN FRAME FRAME-B /* Best.nr */
DO:
   FILL-IN-BESTNR = INPUT FILL-IN-BESTNR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-DATUM IN FRAME FRAME-B /* Leveransdag */
DO:
   ASSIGN     
   Guru.GlobalaVariabler:regdatum = FILL-IN-DATUM.
   IF Guru.GlobalaVariabler:regdatum = ? THEN Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-DATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KIEPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KIEPOST WINDOW-2
ON ENTRY OF FILL-IN-KIEPOST IN FRAME FRAME-B /* E-postadress */
DO:
   /*
   FILL-IN-KIEPOST = INPUT FILL-IN-KIEPOST.
   */
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KIEPOST WINDOW-2
ON LEAVE OF FILL-IN-KIEPOST IN FRAME FRAME-B /* E-postadress */
DO:
   FILL-IN-KIEPOST = INPUT FILL-IN-KIEPOST.
   IF FILL-IN-KIEPOST NE "" THEN DO:   
      musz = FALSE.
      RUN EPOSTKOLL.P (INPUT FILL-IN-KIEPOST,OUTPUT musz).   
      IF musz = FALSE THEN DO:
         APPLY "ENTRY" TO FILL-IN-KIEPOST IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.
      musz = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KUNDNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KUNDNR WINDOW-2
ON LEAVE OF FILL-IN-KUNDNR IN FRAME FRAME-B /* Kundnummer */
DO:
   ASSIGN 
   FILL-IN_EAONR = INPUT FILL-IN_EAONR  
   FILL-IN_DELNR = INPUT FILL-IN_DELNR
   FILL-IN-KUNDNR = INPUT FILL-IN-KUNDNR  
   FILL-IN-BESTNR = INPUT FILL-IN-BESTNR
   FILL-IN-LEVNAMN = INPUT FILL-IN-LEVNAMN 
   FILL-IN-LKONTAKT = INPUT FILL-IN-LKONTAKT 
   FILL-IN-LADR = INPUT FILL-IN-LADR 
   FILL-IN-LPNR = INPUT FILL-IN-LPNR 
   FILL-IN-LORT = INPUT FILL-IN-LORT 
   FILL-IN-LTELE = INPUT FILL-IN-LTELE 
   FILL-IN-KUNDNR = INPUT FILL-IN-KUNDNR 
   FILL-IN-FOR = INPUT FILL-IN-FOR 
   FILL-IN-KADR = INPUT FILL-IN-KADR 
   FILL-IN-KPNR = INPUT FILL-IN-KPNR 
   FILL-IN-BOX = INPUT FILL-IN-BOX 
   FILL-IN-KORT = INPUT FILL-IN-KORT 
   FILL-IN-FAX = INPUT FILL-IN-FAX 
   FILL-IN-L1 = INPUT FILL-IN-L1
   FILL-IN-L2 = INPUT FILL-IN-L2 
   FILL-IN-L3 = INPUT FILL-IN-L3 
   FILL-IN-KIKONTAKT = INPUT FILL-IN-KIKONTAKT 
   FILL-IN-KIEPOST = INPUT FILL-IN-KIEPOST 
   FILL-IN-KITELE =  INPUT FILL-IN-KITELE 
   FILL-IN-KTKONTAKT = INPUT FILL-IN-KTKONTAKT 
   FILL-IN-KTTELE = INPUT FILL-IN-KTTELE 
   FILL-IN-KIMOBIL =  INPUT FILL-IN-KIMOBIL
   FILL-IN-KTMOBIL =  INPUT FILL-IN-KTMOBIL 
   FILL-IN-AVISPERS = INPUT FILL-IN-AVISPERS 
   FILL-IN-DATUM = INPUT FILL-IN-DATUM    
   FILL-IN-MARK = INPUT FILL-IN-MARK 
   FILL-IN-KOM = INPUT FILL-IN-KOM 
   FILL-IN-AVIS = INPUT FILL-IN-AVIS.
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
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
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
   {muswait.i}
   {ALLSTARTDYN.I}
   EMPTY TEMP-TABLE lev_temp NO-ERROR.
   
   FILL-IN-KUNDNR:FORMAT = "9999999999".
   {&WINDOW-NAME}:TITLE = "Leverans och transport för beställning nr:" + STRING(nytt_bestnr).
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /*selga = 3*/
      IF vald_kundlev = "3" THEN FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "99999999".
      ELSE FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "9999999".
   END.
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF vald_kundlev = "1" THEN FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "999999".
      ELSE IF vald_kundlev = "12" THEN FILL-IN-KUNDNR:FORMAT IN FRAME {&FRAME-NAME} = "9999999".
   END.
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST spec_mtrl WHERE spec_mtrl.LEVKOD = vald_kundlev NO-LOCK NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO: 
      ASSIGN 
      vald_lev = spec_mtrl.LEVKOD.
      FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD NO-LOCK NO-ERROR. 
      CREATE lev_temp.
      lev_temp.LEVKOD = levtemp.LEVKOD.
      ASSIGN
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN 
      leve = " ".          
      FOR EACH spec_mtrl WHERE spec_mtrl.LEVKOD NE vald_kundlev.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD NO-LOCK NO-ERROR.
         IF levtemp.LEVKOD NE leve THEN DO: 
            CREATE lev_temp.
            lev_temp.LEVKOD = levtemp.LEVKOD.
            ASSIGN  
            status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
            leve = spec_mtrl.LEVKOD. 
         END.                
      END.                                               
   END.
   ELSE DO:
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR. 
      IF AVAILABLE spec_mtrl THEN DO:
         vald_lev = spec_mtrl.LEVKOD.  
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD NO-LOCK NO-ERROR.  
         CREATE lev_temp.
         lev_temp.LEVKOD = levtemp.LEVKOD.
         ASSIGN 
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}            
         CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN 
         leve = spec_mtrl.LEVKOD.                              
         FOR EACH spec_mtrl WHERE spec_mtrl.LEVKOD NE vald_kundlev.
            FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD NO-LOCK NO-ERROR.
            IF levtemp.LEVKOD NE leve THEN DO: 
               CREATE lev_temp.
               lev_temp.LEVKOD = levtemp.LEVKOD.
               ASSIGN  
               status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}               
               leve = spec_mtrl.LEVKOD. 
            END.                
         END.
      END.
      ELSE DO:
         MESSAGE "Det finns inget materiel att skapa beställning på." VIEW-AS ALERT-BOX.
         LEAVE MAIN-BLOCK.
      END.                       
   END.
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa NO-LOCK NO-ERROR. 
   RUN ltrp_UI IN mtrlbapph  (INPUT nytt_bestnr ,INPUT vald_lev,INPUT vald_depa,OUTPUT TABLE ltrptemp).
   FIND FIRST ltrptemp WHERE ltrptemp.BESTNR = nytt_bestnr AND ltrptemp.LEVKOD = vald_lev 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:      
      ASSIGN
      FILL-IN-DEPA = depatemp.BENAMNING
      FILL-IN-LEVNAMN = ltrptemp.LEVNAMN
      FILL-IN-LKONTAKT = ltrptemp.LEVKONTAKT
      FILL-IN-LADR = ltrptemp.LEVADR
      FILL-IN-LPNR = ltrptemp.LEVPNR
      FILL-IN-LORT = ltrptemp.LEVORT
      FILL-IN-LTELE = ltrptemp.LEVTEL
      FILL-IN-KUNDNR = ltrptemp.KUNDNR     
      FILL-IN-BESTNR = STRING(ltrptemp.BESTNR)
      FILL-IN-FOR = ltrptemp.FORE
      FILL-IN-KADR = ltrptemp.KADR
      FILL-IN-KPNR = ltrptemp.KPNR
      FILL-IN-BOX = ltrptemp.BOX
      FILL-IN-KORT = ltrptemp.KORT   
      FILL-IN-FAX = ltrptemp.FAX                   
      FILL-IN-L1 = ltrptemp.L1
      FILL-IN-L2 = ltrptemp.L2 
      FILL-IN-L3 = ltrptemp.L3           
      FILL-IN-KIKONTAKT  = SUBSTRING(ltrptemp.KIKONTAKT,1,50)
      FILL-IN-KIEPOST = SUBSTRING(ltrptemp.KIKONTAKT,52)
      FILL-IN-KITELE = ltrptemp.KITELE  
      FILL-IN-KTKONTAKT = ltrptemp.KTKONTAKT
      FILL-IN-KTTELE = ltrptemp.KTTELE     
      FILL-IN-KIMOBIL = ltrptemp.KIMOBIL
      FILL-IN-KTMOBIL = ltrptemp.KTMOBIL
      FILL-IN-AVISPERS = ltrptemp.AVISPERS 
      FILL-IN-DATUM = ltrptemp.DATUM      
      FILL-IN-MARK = ltrptemp.MARK
      FILL-IN-KOM = ltrptemp.KOM
      FILL-IN-AVIS = ltrptemp.AVIS
      FILL-IN-STATUS = ltrptemp.BESTALLD.   
   END.
   ELSE DO:   
      CREATE ltrptemp.
      ASSIGN
      ltrptemp.BESTNR = nytt_bestnr 
      ltrptemp.LEVKOD = vald_lev
      ltrptemp.BESTALLD = "Ej beställd"
      ltrptemp.DEP-NR = depatemp.DEP-NR.     
      ASSIGN  
      FILL-IN-DEPA = depatemp.BENAMNING.           
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_lev NO-LOCK NO-ERROR. 
      ASSIGN
      FILL-IN-BESTNR = STRING(nytt_bestnr)
      FILL-IN-LEVNAMN = levtemp.LEVNAMN
      FILL-IN-LKONTAKT = levtemp.LEVKONTAKT
      FILL-IN-LTELE = levtemp.LEVTEL
      FILL-IN-LADR = levtemp.LEVADR
      FILL-IN-LPNR = levtemp.LEVPNR
      FILL-IN-LORT = levtemp.LEVORT
      FILL-IN-FOR = depatemp.FIRMA
      FILL-IN-KADR = depatemp.ADRESS
      FILL-IN-KPNR = depatemp.PNR
      FILL-IN-KORT = depatemp.ORT   
      FILL-IN-FAX = depatemp.FAXNR                   
      FILL-IN-L1 = depatemp.LEVADRESS
      FILL-IN-L2 = depatemp.LEVPNR 
      FILL-IN-L3 = depatemp.LEVORT. 
      ASSIGN         
      FILL-IN-DATUM = TODAY + 3.      
      IF WEEKDAY(FILL-IN-DATUM) = 1 THEN FILL-IN-DATUM = FILL-IN-DATUM + 1.
      IF WEEKDAY(FILL-IN-DATUM) = 7 THEN FILL-IN-DATUM = FILL-IN-DATUM + 2.
      
/*       IF Guru.Konstanter:globforetag = "ELPA" THEN DO:                                                 */
/*          FILL-IN-FOR = "Elpool i Umeå AB".                                             */
/*       END.                                                                             */
/*       IF Guru.Konstanter:globforetag = "SOLE"   THEN DO:                        */
/*          FILL-IN-FOR = "Graninge Nät AB".                                              */
/*       END.                                                                             */
/*       IF Guru.Konstanter:globforetag = "SUND" THEN DO:                                                 */
/*          FILL-IN-FOR = "Sundsvall Energi Elnät AB".                                    */
/*       END.                                                                             */
/*       IF Guru.Konstanter:globforetag = "VORD" THEN DO:                                                 */
/*          FILL-IN-FOR = "Vattenfall Service Nord AB".                                   */
/*       END.                                                                             */
/*       IF                                                        */
/*       Guru.Konstanter:globforetag = "VAST"  THEN DO:                            */
/*          FILL-IN-FOR = "Vattenfall Service Syd AB".                                    */
/*       END.                                                                             */
/*       IF Guru.Konstanter:globforetag = "TREC" OR Guru.Konstanter:globforetag = "ALTE" OR Guru.Konstanter:globforetag = "ELCO" THEN DO: */
/*          FILL-IN-FOR = "Transelectric Kraftkonsult AB".                                */
/*       END.                                                                                                                                                                                              */
      ganv = Guru.Konstanter:globanv.
      RUN kundhmt_UI IN mtrlbapph 
      (INPUT vald_depa ,INPUT vald_lev ,INPUT ganv ,INPUT nytt_bestnr, INPUT TABLE ltrptemp,
       OUTPUT knr , OUTPUT namn , OUTPUT telf , OUTPUT mtelf , OUTPUT avisp, OUTPUT EPOST ).                                                                                                               
      IF Guru.Konstanter:globforetag = "SNAT"  THEN FILL-IN-KIKONTAKT = Guru.Konstanter:globanv.       
      ELSE FILL-IN-KIKONTAKT = namn.
       
          
      ASSIGN FILL-IN-KUNDNR = knr
      FILL-IN-STATUS = "Ej beställd"            
      
      FILL-IN-KITELE = telf
      FILL-IN-KIMOBIL = mtelf
      FILL-IN-KIEPOST = epost
      FILL-IN-AVISPERS = avisp.                             
   END.         
   RUN enable_UI.      
   {FRMSIZE.I}  
   
   IF Guru.Konstanter:globforetag = "CELPA"  OR  
   Guru.Konstanter:globforetag = "VAST"  THEN DO:
      DISABLE FILL-IN-BESTNR WITH FRAME {&FRAME-NAME}.
   END.
      
   ASSIGN FILL-IN-DATUM:HIDDEN = FALSE.   
   IF FILL-IN-AVIS = TRUE THEN ASSIGN FILL-IN-AVISPERS:HIDDEN = FALSE.
   ELSE ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.   

   IF Guru.Konstanter:globforetag = "cVAST" OR Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "cGKAL" OR Guru.Konstanter:globforetag = "CBORL" OR
   Guru.Konstanter:globforetag = "ELPA" THEN DO:
      /*Best.Onninen*/
      FBTN_MAIL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.
   ELSE DO:
      FBTN_MAIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.
   IF Guru.Konstanter:globforetag = "cVAST"  OR Guru.Konstanter:globforetag = "cVSAB"  OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      /* Best.Elektroskandia*/
      FBTN_EDI:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      FBTN_EDI:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "VAST" THEN DO:
      FBTN_MAIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      FBTN_EDI:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     
      FOR EACH lev_temp NO-LOCK:
          /*Best.Onninen*/
         IF lev_temp.LEVKOD = "16" THEN  FBTN_MAIL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         /* Best.Elektroskandia*/
         IF lev_temp.LEVKOD = "1" THEN   FBTN_EDI:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
                                     
   END.
   /* Städning Lena 20080820
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      FBTN_EDI:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.      
   ELSE IF Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "TREC" OR 
   Guru.Konstanter:globforetag = "ALTE" OR Guru.Konstanter:globforetag = "VELK" OR  
   Guru.Konstanter:globforetag = "ELCO" OR Guru.Konstanter:globforetag = "SKOG" OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "BORL" OR 
   Guru.Konstanter:globforetag = "TRAS" OR Guru.Konstanter:globforetag = "STRA" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "LECA" THEN DO:
      ASSIGN
      FBTN_EDI:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_MAIL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.*/
   {musarrow.i}
   APPLY "VALUE-CHANGED" TO CMB_LEV.    
   ASSIGN
   FILL-IN-BESTNR:FORMAT = "X(20)" 
   FILL-IN-BOX:FORMAT = "X(4)" 
   FILL-IN-DEPA:FORMAT = "X(35)" 
   FILL-IN-FAX:FORMAT = "X(15)" 
   FILL-IN-KADR:FORMAT = "X(30)" 
   FILL-IN-KIKONTAKT:FORMAT = "X(25)" 
  /* FILL-IN-KIEPOST:FORMAT = "X(15)" */
   FILL-IN-KIMOBIL:FORMAT = "X(15)" 
   FILL-IN-KITELE:FORMAT = "X(15)" 
   FILL-IN-KOM:FORMAT = "X(80)" 
   FILL-IN-KORT:FORMAT = "X(256)" 
   FILL-IN-KPNR:FORMAT = "999 99" 
   FILL-IN-KTKONTAKT:FORMAT = "X(25)" 
   FILL-IN-KTMOBIL:FORMAT = "X(15)" 
   FILL-IN-KTTELE:FORMAT = "X(15)"    
   FILL-IN-L2:FORMAT = "999 99"    
   FILL-IN-LADR:FORMAT = "X(20)" 
   FILL-IN-LEVNAMN:FORMAT = "X(25)" 
   FILL-IN-LKONTAKT:FORMAT = "X(25)" 
   FILL-IN-LORT:FORMAT = "X(256)" 
   FILL-IN-LPNR:FORMAT = "999 99" 
   FILL-IN-LTELE:FORMAT = "xxxx-xxxxxxx"    
   FILL-IN-STATUS:FORMAT = "X(256)". 
   &Scoped-define FORMATNAMN FILL-IN_EAONR   
   {AOFORMAT4.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT2.I}     
   FILL-IN_EAONR:LABEL = Guru.Konstanter:gaok.  
   IF Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "cELPA" {GLOBVES.I} THEN DO:
      ASSIGN     
      FILL-IN-AVISPERS:FORMAT = "X(34)"             
      FILL-IN-FOR:FORMAT = "X(25)"                   
      FILL-IN-L1:FORMAT = "X(25)"       
      FILL-IN-L3:FORMAT = "X(20)"       
      FILL-IN-MARK:FORMAT = "X(20)". 
   END.
   ELSE DO:   
      ASSIGN      
      FILL-IN-AVISPERS:FORMAT = "X(30)"            
      FILL-IN-FOR:FORMAT = "X(35)"             
      FILL-IN-L1:FORMAT = "X(35)"       
      FILL-IN-L3:FORMAT = "X(35)"          
      FILL-IN-MARK:FORMAT = "X(35)". 
   END.

   Guru.GlobalaVariabler:collefth = ?.   
   IF FBTN_VISA:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_VISA:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_AVSTATUS:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_AVSTATUS:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_VSTATUS:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_VSTATUS:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   /*Guru.GlobalaVariabler:collefth = ?.*/
   IF FBTN_MAIL:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_MAIL:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.GlobalaVariabler:collefth = ? THEN DO:
      FBTN_EDI:COLUMN = FBTN_MAIL:COLUMN.  
   END.
   IF FBTN_EDI:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_EDI:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
       
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_EAONR:HIDDEN = TRUE
      FILL-IN_DELNR:HIDDEN = TRUE.
   END.    

   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cmblev_UI WINDOW-2 
PROCEDURE cmblev_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   FIND FIRST ltrptemp WHERE ltrptemp.LEVKOD = vald_lev AND ltrptemp.BESTNR = nytt_bestnr 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:
      APPLY "LEAVE" TO FILL-IN-KUNDNR IN FRAME {&FRAME-NAME}.
      RUN levtrp_UI.      
   END.                   
   leverant = INPUT CMB_LEV.                   
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = leverant NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD.
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa NO-LOCK NO-ERROR.
   FIND FIRST ltrptemp WHERE ltrptemp.BESTNR = nytt_bestnr AND ltrptemp.LEVKOD = vald_lev 
   AND ltrptemp.DEP-NR = vald_depa AND ltrptemp.BERNR = 0
   USE-INDEX BESTNR2 NO-LOCK NO-ERROR.
   IF AVAILABLE ltrptemp THEN DO:      
      ASSIGN
      FILL-IN-DEPA /* IN FRAME {&FRAME-NAME}*/ = depatemp.BENAMNING
      FILL-IN-LEVNAMN = ltrptemp.LEVNAMN
      FILL-IN-LKONTAKT = ltrptemp.LEVKONTAKT
      FILL-IN-LADR = ltrptemp.LEVADR
      FILL-IN-LPNR = ltrptemp.LEVPNR
      FILL-IN-LORT = ltrptemp.LEVORT
      FILL-IN-LTELE = ltrptemp.LEVTEL   
      FILL-IN-KUNDNR = ltrptemp.KUNDNR     
      FILL-IN-FOR = ltrptemp.FORE
      FILL-IN-KADR = ltrptemp.KADR
      FILL-IN-KPNR = ltrptemp.KPNR
      FILL-IN-BOX = ltrptemp.BOX
      FILL-IN-KORT = ltrptemp.KORT
      FILL-IN-FAX = ltrptemp.FAX                   
      FILL-IN-L1 = ltrptemp.L1
      FILL-IN-L2 = ltrptemp.L2 
      FILL-IN-L3 = ltrptemp.L3           
      FILL-IN-KIKONTAKT  = SUBSTRING(ltrptemp.KIKONTAKT,1,50)
      FILL-IN-KIEPOST = SUBSTRING(ltrptemp.KIKONTAKT,52)
      FILL-IN-KITELE = ltrptemp.KITELE  
      FILL-IN-KTKONTAKT = ltrptemp.KTKONTAKT
      FILL-IN-KTTELE = ltrptemp.KTTELE     
      FILL-IN-KIMOBIL = ltrptemp.KIMOBIL
      FILL-IN-KTMOBIL = ltrptemp.KTMOBIL
      FILL-IN-AVISPERS = ltrptemp.AVISPERS
      FILL-IN-DATUM = ltrptemp.DATUM        
      FILL-IN-MARK = ltrptemp.MARK
      FILL-IN-KOM = ltrptemp.KOM
      FILL-IN-AVIS = ltrptemp.AVIS
      FILL-IN-STATUS = ltrptemp.BESTALLD.   
            
      DISPLAY FILL-IN-DEPA FILL-IN-LEVNAMN FILL-IN-LKONTAKT FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT
      FILL-IN-LTELE FILL-IN-KUNDNR FILL-IN-FOR FILL-IN-KADR FILL-IN-KPNR FILL-IN-BOX FILL-IN-KORT
      FILL-IN-FAX FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 FILL-IN-KIKONTAKT FILL-IN-KIEPOST FILL-IN-KITELE FILL-IN-KTKONTAKT
      FILL-IN-KTTELE FILL-IN-KIMOBIL FILL-IN-KTMOBIL FILL-IN-AVISPERS FILL-IN-DATUM FILL-IN-MARK FILL-IN-KOM FILL-IN-AVIS
      FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
      IF FILL-IN-AVIS = TRUE THEN ASSIGN FILL-IN-AVISPERS:HIDDEN = FALSE.
      ELSE ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.      
      FILL-IN-DATUM:HIDDEN = FALSE.      
      IF levtemp.LEVNAMN = "Elektroskandia" THEN DO:
         ASSIGN
         FILL-IN-KTKONTAKT:HIDDEN = TRUE
         FILL-IN-KTTELE:HIDDEN = TRUE 
         FILL-IN-KTMOBIL:HIDDEN = TRUE.
      END.
   END.
   ELSE DO:   
      CREATE ltrptemp.
      ASSIGN
      ltrptemp.BESTNR = nytt_bestnr 
      ltrptemp.LEVKOD = vald_lev
      ltrptemp.BESTALLD = "Ej beställd"
      ltrptemp.DEP-NR = depatemp.DEP-NR.
   
      ASSIGN  
      FILL-IN-BESTNR = STRING(nytt_bestnr)
      FILL-IN-DEPA = depatemp.BENAMNING
      FILL-IN-LEVNAMN  = levtemp.LEVNAMN
      FILL-IN-LKONTAKT = levtemp.LEVKONTAKT
      FILL-IN-LTELE = levtemp.LEVTEL
      FILL-IN-LADR = levtemp.LEVADR
      FILL-IN-LPNR = levtemp.LEVPNR
      FILL-IN-LORT = levtemp.LEVORT
      FILL-IN-FOR = depatemp.FIRMA      
      FILL-IN-KUNDNR = 0
      FILL-IN-KADR = depatemp.ADRESS
      FILL-IN-KPNR = depatemp.PNR
      FILL-IN-KORT = depatemp.ORT   
      FILL-IN-FAX = depatemp.FAXNR                   
      FILL-IN-L1 = depatemp.LEVADRESS
      FILL-IN-L2 = depatemp.LEVPNR 
      FILL-IN-L3 = depatemp.LEVORT      
      FILL-IN-AVIS = FALSE.       
      FILL-IN-STATUS = "Ej beställd".     
      ASSIGN FILL-IN-DATUM:HIDDEN = FALSE.
      ENABLE FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.             
      RUN kundhmt_UI IN mtrlbapph 
      (INPUT depatemp.DEP-NR ,INPUT levtemp.LEVKOD ,INPUT Guru.Konstanter:globanv ,INPUT nytt_bestnr,INPUT TABLE ltrptemp ,OUTPUT knr , OUTPUT namn , OUTPUT telf, OUTPUT mtelf, OUTPUT avisp, OUTPUT epost).                                                                                                               
          
      ASSIGN FILL-IN-KUNDNR = knr
      FILL-IN-STATUS = "Ej beställd"            
      FILL-IN-KIKONTAKT = namn
      FILL-IN-KITELE = telf
      FILL-IN-KIMOBIL = mtelf
      FILL-IN-KIEPOST = epost
      FILL-IN-AVISPERS = avisp.                             
         
      DISPLAY FILL-IN-BESTNR FILL-IN-DEPA FILL-IN-LEVNAMN FILL-IN-LKONTAKT FILL-IN-LADR FILL-IN-LPNR FILL-IN-LORT
      FILL-IN-LTELE FILL-IN-KUNDNR FILL-IN-FOR FILL-IN-KADR FILL-IN-KPNR FILL-IN-BOX FILL-IN-KORT
      FILL-IN-FAX FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KTKONTAKT
      FILL-IN-KTTELE FILL-IN-KIMOBIL FILL-IN-AVISPERS FILL-IN-DATUM FILL-IN-MARK FILL-IN-KOM FILL-IN-AVIS
      FILL-IN-STATUS WITH FRAME {&FRAME-NAME}.
      ASSIGN FILL-IN-AVISPERS:HIDDEN = TRUE.      
      IF levtemp.LEVNAMN = "Elektroskandia" THEN DO:
         ASSIGN
         FILL-IN-KTKONTAKT:HIDDEN = TRUE
         FILL-IN-KTTELE:HIDDEN = TRUE 
         FILL-IN-KTMOBIL:HIDDEN = TRUE.
      END.
   END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-LEVNAMN FILL-IN-LKONTAKT FILL-IN-LTELE FILL-IN-LADR 
          FILL-IN-LPNR FILL-IN-LORT CMB_LEV FILL-IN-STATUS FILL-IN-KUNDNR 
          FILL-IN-BESTNR FILL-IN-FOR FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR 
          FILL-IN-KORT FILL-IN-FAX FILL-IN-KIKONTAKT FILL-IN-KITELE 
          FILL-IN-KIMOBIL FILL-IN-KIEPOST FILL-IN-KTKONTAKT FILL-IN-KTTELE 
          FILL-IN-KTMOBIL FILL-IN-DATUM FILL-IN_EAONR FILL-IN_DELNR FILL-IN-L1 
          FILL-IN-L2 FILL-IN-L3 FILL-IN-MARK FILL-IN-KOM FILL-IN-AVIS 
          FILL-IN-AVISPERS FILL-IN-DEPA 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE FILL-IN-LEVNAMN FILL-IN-LKONTAKT FILL-IN-LTELE FILL-IN-LADR 
         FILL-IN-LPNR FILL-IN-LORT FBTN_VISA CMB_LEV FBTN_AVSTATUS FBTN_VSTATUS 
         FBTN_MAIL FILL-IN-KUNDNR FILL-IN-BESTNR FILL-IN-FOR FBTN_EDI 
         FILL-IN-KADR FILL-IN-BOX FILL-IN-KPNR FILL-IN-KORT FILL-IN-FAX 
         FILL-IN-KIKONTAKT FILL-IN-KITELE FILL-IN-KIMOBIL FILL-IN-KIEPOST 
         FILL-IN-KTKONTAKT FILL-IN-KTTELE FILL-IN-KTMOBIL FILL-IN-DATUM 
         FILL-IN_EAONR FILL-IN_DELNR FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 
         FILL-IN-MARK FILL-IN-KOM FILL-IN-AVIS FILL-IN-AVISPERS BTN_AVB 
         FILL-IN-DEPA 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levtrp_UI WINDOW-2 
PROCEDURE levtrp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   ltrptemp.LEVNAMN = FILL-IN-LEVNAMN
   ltrptemp.LEVKONTAKT = FILL-IN-LKONTAKT 
   ltrptemp.LEVADR = FILL-IN-LADR 
   ltrptemp.LEVPNR = FILL-IN-LPNR 
   ltrptemp.LEVORT = FILL-IN-LORT 
   ltrptemp.LEVTEL = FILL-IN-LTELE 
   ltrptemp.KUNDNR = FILL-IN-KUNDNR 
   ltrptemp.BESTNR = INTEGER(FILL-IN-BESTNR)
   ltrptemp.FORE = FILL-IN-FOR 
   ltrptemp.KADR = FILL-IN-KADR 
   ltrptemp.KPNR = FILL-IN-KPNR 
   ltrptemp.BOX = FILL-IN-BOX 
   ltrptemp.KORT = FILL-IN-KORT 
   ltrptemp.FAX = FILL-IN-FAX 
   ltrptemp.L1 = FILL-IN-L1
   ltrptemp.L2 = FILL-IN-L2 
   ltrptemp.L3 = FILL-IN-L3 
   ltrptemp.KIKONTAKT = ""
   SUBSTRING(ltrptemp.KIKONTAKT,1,50) = FILL-IN-KIKONTAKT 
   SUBSTRING(ltrptemp.KIKONTAKT,52) = FILL-IN-KIEPOST 
   ltrptemp.KITELE =  FILL-IN-KITELE 
   ltrptemp.KTKONTAKT = FILL-IN-KTKONTAKT 
   ltrptemp.KTTELE = FILL-IN-KTTELE 
   ltrptemp.KIMOBIL =  FILL-IN-KIMOBIL
   ltrptemp.KTMOBIL =  FILL-IN-KTMOBIL 
   ltrptemp.AVISPERS = FILL-IN-AVISPERS 
   ltrptemp.DATUM = FILL-IN-DATUM    
   ltrptemp.MARK = FILL-IN-MARK 
   ltrptemp.KOM = FILL-IN-KOM 
   ltrptemp.AVIS = FILL-IN-AVIS.   
   RUN ltrpsp_UI IN mtrlbapph (INPUT vald_depa ,INPUT vald_lev ,INPUT nytt_bestnr,INPUT TABLE ltrptemp  ).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapamtrl_UI WINDOW-2 
PROCEDURE skapamtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH skapa_mtrl.
      DELETE skapa_mtrl.
   END.
   CREATE skapa_mtrl.
   ASSIGN
   skapa_mtrl.LEVNAMN = FILL-IN-LEVNAMN
   skapa_mtrl.LKONTAKT = FILL-IN-LKONTAKT
   skapa_mtrl.LTELE = FILL-IN-LTELE
   skapa_mtrl.LADR = FILL-IN-LADR
   skapa_mtrl.LPNR = FILL-IN-LPNR
   skapa_mtrl.LORT = FILL-IN-LORT
   skapa_mtrl.FORE = FILL-IN-FOR
   skapa_mtrl.KADR = FILL-IN-KADR
   skapa_mtrl.KPNR = FILL-IN-KPNR
   skapa_mtrl.KORT = FILL-IN-KORT
   skapa_mtrl.BOX = FILL-IN-BOX
   skapa_mtrl.FAX = FILL-IN-FAX
   skapa_mtrl.KIKONTAKT = FILL-IN-KIKONTAKT                        
   skapa_mtrl.KITELE = FILL-IN-KITELE
   skapa_mtrl.KIMOBIL = FILL-IN-KIMOBIL
   skapa_mtrl.KTMOBIL = FILL-IN-KTMOBIL
   skapa_mtrl.KTKONTAKT = FILL-IN-KTKONTAKT
   skapa_mtrl.KTTELE = FILL-IN-KTTELE
   skapa_mtrl.KIEPOST = FILL-IN-KIEPOST
   skapa_mtrl.DATUM = FILL-IN-DATUM
   skapa_mtrl.MARK = FILL-IN-MARK 
   skapa_mtrl.L1 = FILL-IN-L1
   skapa_mtrl.L2 = FILL-IN-L2
   skapa_mtrl.L3 = FILL-IN-L3
   skapa_mtrl.KOM = FILL-IN-KOM
   skapa_mtrl.KUNDNR = FILL-IN-KUNDNR 
   skapa_mtrl.AVIS = FILL-IN-AVIS   
   skapa_mtrl.AVISPERS = FILL-IN-AVISPERS
   skapa_mtrl.BESTNR = FILL-IN-BESTNR.     
   /*IF AVAILABLE BEREDNING THEN ASSIGN 
 *    skapa_mtrl.BESTNR = STRING(BEREDNING.AONR) + STRING(BEREDNING.DELNR,Guru.Konstanter:varforetypchar[1]).*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

