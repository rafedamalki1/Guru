&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WFLEXTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WFLEXTID 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/04/06 -  4:27 pm

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
/*OBS!
ANYWHERE SKALL STÅ EFTER VARJE ON F3 OF FRAME FRAME-A ANYWHERE,
ON F4 OF FRAME FRAME-A ANYWHERE osv
  
ANYWHERE FÖRSVINNER VIA UIB



IF ocxvar = TRUE THEN RUN control_load.
skall stå i enable_UI istället för RUN control_load.

EFTER 
&ANALYZE-SUSPEND _CREATE-DYNAMIC
SKALL DET STÅ 
IF ocxvar = TRUE THEN DO:
&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN  
   CREATE CONTROL-FRAME grundata ASSIGN
       

  CREATE CONTROL-FRAME senastetryck ASSIGN
      

&ENDIF
END.                 
IF ocxvar = TRUE FÖRSVINNER VIA UIB


*/
   
   
DEFINE VARIABLE ocxvar AS LOGICAL NO-UNDO.
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN ocxvar = TRUE.
ELSE ocxvar = FALSE.

&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{SOKDEF.I}
/*
DEFINE SHARED VARIABLE alltidmax AS LOGICAL NO-UNDO.
*/
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE flexrec AS RECID NO-UNDO.   
DEFINE NEW SHARED VARIABLE fldrec AS RECID NO-UNDO.  
DEFINE NEW SHARED VARIABLE flexpers AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE delnummer AS INTEGER NO-UNDO.   
DEFINE NEW SHARED VARIABLE valaonrrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE ovut AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE ovkoll AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regstart AS DECIMAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE regslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE onr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE onr2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE dnr2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE dnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE tid AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tra AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE typ AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE tpris AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tid3 AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE manad AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regtotalt AS DECIMAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE perssokrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.  
DEFINE NEW SHARED VARIABLE tidsedrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE tidsedlog AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE andvisrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE anvrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE muszval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE andvisvnr AS INTEGER FORMAT "999" NO-UNDO.  
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE printer AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE printer1 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE NEW SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE NEW SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE NEW SHARED VARIABLE klocka AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkregvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE florsak AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE flrec AS RECID NO-UNDO.

DEFINE NEW SHARED VARIABLE arnr AS INTEGER FORMAT "9999" NO-UNDO. 
DEFINE NEW SHARED VARIABLE manadnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE NEW SHARED VARIABLE manadnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hamtkoll AS DATE NO-UNDO.
hamtkoll = TODAY.


                              

DEFINE VARIABLE val AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE avb-mus AS LOGICAL NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE otim AS INTEGER NO-UNDO.
DEFINE VARIABLE otim2 AS INTEGER NO-UNDO.
DEFINE VARIABLE tiden AS INTEGER NO-UNDO.
DEFINE VARIABLE kollen AS INTEGER NO-UNDO.    
DEFINE VARIABLE kollen2 AS INTEGER NO-UNDO.
DEFINE VARIABLE kollen3 AS INTEGER NO-UNDO.
DEFINE VARIABLE flkoll AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchut AS INTEGER NO-UNDO.
DEFINE VARIABLE tid2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tid4 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE tid5 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE aonr3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dnr3 AS INTEGER NO-UNDO.    
DEFINE VARIABLE pltid AS LOGICAL NO-UNDO.
DEFINE VARIABLE ejtid AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE plus AS INTEGER NO-UNDO.
DEFINE VARIABLE flhjrec AS RECID NO-UNDO.   
DEFINE VARIABLE flsaldo AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO. 
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE flexkvst AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexkvsl AS DECIMAL NO-UNDO. 
DEFINE VARIABLE flexkvslspar AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmost AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmostspar AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmosl AS DECIMAL NO-UNDO. 
DEFINE VARIABLE tidreg AS LOGICAL NO-UNDO.
DEFINE VARIABLE typ3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE tpris3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tidslut AS DECIMAL NO-UNDO. 
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.


DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbconvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gforetagvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE appkolltid AS INTEGER NO-UNDO.
DEFINE VARIABLE titlevar AS CHARACTER NO-UNDO.
DEFINE VARIABLE losenvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE ovtbeord AS CHARACTER NO-UNDO.
DEFINE VARIABLE rmal AS CHARACTER NO-UNDO.
DEFINE VARIABLE antaldatbaser AS INTEGER NO-UNDO.

{VALDBTEMP.I}
{VALDBALL.I}

{SOKSTART.I}
ASSIGN
soktemp.SOKVAL = 86.

{SOKANROP.I}
FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = soktemp.SOKCHAR[1] NO-ERROR.
DEBUGGER:SET-BREAK().
ASSIGN
dbconvar = valdbtemp.DBNAMN
gforetagvar = valdbtemp.GFORETAG
conappvar = valdbtemp.APPCON.
IF Guru.Konstanter:globanv = "FLEX" THEN DO:
   FOR EACH valdbtemp:
      IF valdbtemp.DBNAMN = "UTBI" THEN DELETE valdbtemp.
      IF AVAILABLE valdbtemp THEN DO:
         IF valdbtemp.APPCON NE conappvar THEN DELETE valdbtemp. 
      END.   
   END.
   FOR EACH valdbtemp:
      antaldatbaser = antaldatbaser + 1.     
   END.
   
   IF antaldatbaser > 1 THEN DO:
      IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
      IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
      Guru.Konstanter:appcon = FALSE.
      RUN appkoll_UI.
   END.
END.   
ELSE DO:
   FOR EACH valdbtemp :
      IF valdbtemp.DBNAMN = "UTBI" THEN DELETE valdbtemp.
      IF AVAILABLE valdbtemp THEN DO:
         IF valdbtemp.DBNAMN NE dbconvar THEN DELETE valdbtemp. 
      END.   
   END.
END.   

{TIDAPPDEF.I}
{FLEXAPPDEF.I}
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}
{FLEXTAB.I}
/*{EGENBEN.I}*/
&Scoped-define NEW NEW  
&Scoped-define SHARED SHARED
{DIRDEF.I}
{BLOB.I}
{AVDELNINGTEMPT.I}

DEFINE TEMP-TABLE foreavdelningtemp NO-UNDO LIKE avdelningtemp
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG.
DEFINE TEMP-TABLE foreutsokaonr NO-UNDO LIKE utsokaonr
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG. 
DEFINE TEMP-TABLE foreanvandartemp NO-UNDO LIKE anvandartemp
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG.
DEFINE TEMP-TABLE forepersonaltemp NO-UNDO LIKE personaltemp
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG.
    
DEFINE TEMP-TABLE foreansttemp NO-UNDO LIKE ansttemp
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG.
DEFINE TEMP-TABLE foreflexregtemp NO-UNDO LIKE flexregtemp
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG. 
DEFINE TEMP-TABLE foreflexavttemp NO-UNDO LIKE flexavttemp
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG. 
DEFINE TEMP-TABLE foreflexsaldotemp NO-UNDO LIKE flexsaldotemp
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG. 
DEFINE TEMP-TABLE foreutryckningtemp NO-UNDO LIKE utryckningtemp
   FIELD GFORETAG AS CHARACTER
   INDEX GFORETAG GFORETAG. 
 


/*{FSTART.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-46 RECT-47 FBTN_IN FBTN_LUUT FBTN_OVUT ~
FBTN_ANUT FBTN_FLUT FBTN_GRUND FBTN_AVB FBTN_UT FBTN_LUIN FBTN_OVIN ~
FBTN_ANIN FBTN_FLIN FBTN_HELP BTN_GLOMD FILL-IN-PKOD BTN_LOSEN BTN_PERIOD ~
FILL-IN-LOSEN BTN_SALDO BTN_RAPP BTN_SENASTE 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KLOCKAN FILL-IN-PKOD FILL-IN-LOSEN ~
FILL-IN_FORNAMN FILL-IN_EFTERNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WFLEXTID AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE grundata AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chgrundata AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE senastetryck AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chsenastetryck AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_GLOMD 
     LABEL "Reg av glömd stämpling" 
     SIZE 20.13 BY 2.

DEFINE BUTTON BTN_LOSEN 
     LABEL "Nytt lösenord":L 
     SIZE 14 BY 2.

DEFINE BUTTON BTN_PERIOD 
     LABEL "Reg av period (frånvaro,tj-resa)" 
     SIZE 25.13 BY 2 TOOLTIP "Frånvaro- tjrese-period kan registreras i förhand eller efterhand".

DEFINE BUTTON BTN_RAPP 
     LABEL "Rapport" 
     SIZE 12 BY 2 TOOLTIP "Titta på alla körda/ej körda flextidsregistreringar".

DEFINE BUTTON BTN_SALDO 
     LABEL "Saldo" 
     SIZE 12 BY 2 TOOLTIP "Saldo t.o.m. igår".

DEFINE BUTTON BTN_SENASTE 
     LABEL "Senaste reg" 
     SIZE 12 BY 2 TOOLTIP "Saldo t.o.m. igår".

DEFINE BUTTON FBTN_ANIN 
     LABEL "Annat in / F10":L 
     SIZE 12 BY 2 TOOLTIP "Används vid tjänsteförrättning. Exempelvis om du börjar dagen hos kund.".

DEFINE BUTTON FBTN_ANUT 
     LABEL "Annat ut / F9":L 
     SIZE 12 BY 2 TOOLTIP "Används vid tjänsteförrättning. exempelvis om du avslutar dagen hos kund.".

DEFINE BUTTON FBTN_AVB 
     LABEL "Avsluta":L 
     SIZE 12 BY 2.

DEFINE BUTTON FBTN_FLIN 
     LABEL "Flex in / F12":L 
     SIZE 12 BY 2 TOOLTIP "Vid privata ärenden under dagen, och om man kommer efter 10.00".

DEFINE BUTTON FBTN_FLUT 
     LABEL "Flex ut / F11":L 
     SIZE 12 BY 2 TOOLTIP "Vid privata ärenden under dagen, och om man går för dagen innan 14.00".

DEFINE BUTTON FBTN_GRUND 
     LABEL "Grunddata":L 
     SIZE 12 BY 2 TOOLTIP "Hämtar grunddata för flextidsprogrammen".

DEFINE BUTTON FBTN_HELP 
     LABEL "Hjälp / F1" 
     SIZE 12 BY 2.

DEFINE BUTTON FBTN_IN 
     LABEL "In / F3":L 
     SIZE 12 BY 2 TOOLTIP "Morgonregistrering skall ligga mellan 5.00 och 10.00".

DEFINE BUTTON FBTN_LUIN 
     LABEL "Lunch in / F6":L 
     SIZE 12 BY 2 TOOLTIP "Lunchen skall sluta mellan 10.00 och 14.00".

DEFINE BUTTON FBTN_LUUT 
     LABEL "Lunch ut / F5":L 
     SIZE 12 BY 2 TOOLTIP "Lunchen skall starta mellan 10.00 och 14.00".

DEFINE BUTTON FBTN_OVIN 
     LABEL "Övertid in / F8":L 
     SIZE 12 BY 2 TOOLTIP "På helgen och om du kommer och går på kvällen använd övertidknapparna".

DEFINE BUTTON FBTN_OVUT 
     LABEL "Övertid ut / F7":L 
     SIZE 12 BY 2 TOOLTIP "På helgen och om du kommer och går på kvällen använd övertidknapparna".

DEFINE BUTTON FBTN_UT 
     LABEL "Ut / F4":L 
     SIZE 12 BY 2 TOOLTIP "Kvällsregistrering skall ligga mellan 14.00 och 24.00".

DEFINE VARIABLE FILL-IN-KLOCKAN AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Klockan" 
     VIEW-AS FILL-IN 
     SIZE 11.75 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOSEN AS CHARACTER FORMAT "X(16)":U 
     LABEL "Lösen" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "X(13)" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(14)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE IMAGE IMG_ELPOOL TRANSPARENT
     SIZE 30.88 BY 4.5.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 98.5 BY .5
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 98.5 BY .5
     BGCOLOR 1 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FBTN_IN AT ROW 1.08 COL 15.5
     FBTN_LUUT AT ROW 1.08 COL 27.5
     FBTN_OVUT AT ROW 1.08 COL 39.5
     FBTN_ANUT AT ROW 1.08 COL 51.5
     FBTN_FLUT AT ROW 1.08 COL 63.5
     FBTN_GRUND AT ROW 1.08 COL 75.5
     FBTN_AVB AT ROW 1.08 COL 88
     FBTN_UT AT ROW 3.08 COL 15.5
     FBTN_LUIN AT ROW 3.08 COL 27.5
     FBTN_OVIN AT ROW 3.08 COL 39.5
     FBTN_ANIN AT ROW 3.08 COL 51.5
     FBTN_FLIN AT ROW 3.08 COL 63.5
     FBTN_HELP AT ROW 3.08 COL 88
     BTN_GLOMD AT ROW 5.96 COL 43.38
     FILL-IN-KLOCKAN AT ROW 6 COL 3.88
     FILL-IN-PKOD AT ROW 7.5 COL 11 COLON-ALIGNED
     BTN_LOSEN AT ROW 8.5 COL 28.75
     BTN_PERIOD AT ROW 8.5 COL 43.38
     FILL-IN-LOSEN AT ROW 9 COL 10.88 COLON-ALIGNED BLANK 
     FILL-IN_FORNAMN AT ROW 10.92 COL 12.88 NO-LABEL
     FILL-IN_EFTERNAMN AT ROW 10.92 COL 28.88 NO-LABEL
     BTN_SALDO AT ROW 11 COL 43.38
     BTN_RAPP AT ROW 11.04 COL 56.38
     BTN_SENASTE AT ROW 11.04 COL 69.5
     IMG_ELPOOL AT ROW 5.5 COL 69.13
     RECT-46 AT ROW 5.08 COL 1
     RECT-47 AT ROW 14 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.38 BY 13.79.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WFLEXTID ASSIGN
         HIDDEN             = YES
         TITLE              = "Elpool i Umeå AB Flextid"
         HEIGHT             = 13.83
         WIDTH              = 99.5
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 116
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 116
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WFLEXTID
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-KLOCKAN IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR IMAGE IMG_ELPOOL IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       IMG_ELPOOL:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WFLEXTID)
THEN WFLEXTID:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC
IF ocxvar = TRUE THEN DO:
&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME senastetryck ASSIGN
       FRAME           = FRAME FRAME-A:HANDLE
       ROW             = 5.75
       COLUMN          = 27
       HEIGHT          = 2.25
       WIDTH           = 7.75
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME grundata ASSIGN
       FRAME           = FRAME FRAME-A:HANDLE
       ROW             = 5.75
       COLUMN          = 32
       HEIGHT          = 2.5
       WIDTH           = 10.5
       HIDDEN          = yes
       SENSITIVE       = yes.
      senastetryck:NAME = "senastetryck":U .
/* senastetryck OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      grundata:NAME = "grundata":U .
/* grundata OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      senastetryck:MOVE-AFTER(FBTN_HELP:HANDLE IN FRAME FRAME-A).
      grundata:MOVE-AFTER(senastetryck).

&ENDIF
END.
&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F10 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI.
   IF FBTN_ANIN:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_ANIN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F11 OF FRAME FRAME-A ANYWHERE
DO:
  RUN funk_UI.
  IF FBTN_FLUT:VISIBLE = TRUE THEN
  APPLY "CHOOSE" TO FBTN_FLUT.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F12 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI.
   IF FBTN_FLIN:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_FLIN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F3 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI.
   IF FBTN_IN:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_IN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F4 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI.
   IF FBTN_UT:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_UT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F5 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI.
   IF FBTN_LUUT:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_LUUT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F6 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI.
   IF FBTN_LUIN:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_LUIN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F7 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI. 
   IF FBTN_OVUT:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_OVUT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F8 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI.
   IF FBTN_OVIN:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_OVIN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON F9 OF FRAME FRAME-A ANYWHERE
DO:
   RUN funk_UI.
   IF FBTN_ANUT:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_ANUT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A WFLEXTID
ON HELP OF FRAME FRAME-A ANYWHERE
DO:
   IF FBTN_HELP:VISIBLE = TRUE THEN
   APPLY "CHOOSE" TO FBTN_HELP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GLOMD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GLOMD WFLEXTID
ON CHOOSE OF BTN_GLOMD IN FRAME FRAME-A /* Reg av glömd stämpling */
DO:
   {muswait.i}   
   RUN btnglom_UI.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LOSEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LOSEN WFLEXTID
ON CHOOSE OF BTN_LOSEN IN FRAME FRAME-A /* Nytt lösenord */
DO:
   RUN btnlosen_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PERIOD WFLEXTID
ON CHOOSE OF BTN_PERIOD IN FRAME FRAME-A /* Reg av period (frånvaro,tj-resa) */
DO:      
   RUN btnperiod_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_RAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_RAPP WFLEXTID
ON CHOOSE OF BTN_RAPP IN FRAME FRAME-A /* Rapport */
DO:
   {muswait.i}
   RUN btnrapp_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SALDO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SALDO WFLEXTID
ON CHOOSE OF BTN_SALDO IN FRAME FRAME-A /* Saldo */
DO:
   RUN btnsaldo_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SENASTE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SENASTE WFLEXTID
ON CHOOSE OF BTN_SENASTE IN FRAME FRAME-A /* Senaste reg */
DO:
   RUN btnsenaste_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_ANIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_ANIN WFLEXTID
ON CHOOSE OF FBTN_ANIN IN FRAME FRAME-A /* Annat in / F10 */
DO: 
   {muswait.i}
   RUN btnanin_UI.
   {musarrow.i}             
/*   APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_ANUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_ANUT WFLEXTID
ON CHOOSE OF FBTN_ANUT IN FRAME FRAME-A /* Annat ut / F9 */
DO:
   {muswait.i}
   RUN btnanut_UI.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVB WFLEXTID
ON CHOOSE OF FBTN_AVB IN FRAME FRAME-A /* Avsluta */
DO: 
   RUN appkoll_UI.
   ASSIGN globomr = sparomrade.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FLIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FLIN WFLEXTID
ON CHOOSE OF FBTN_FLIN IN FRAME FRAME-A /* Flex in / F12 */
DO:
   {muswait.i}
   RUN btnflin_UI.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FLUT WFLEXTID
ON CHOOSE OF FBTN_FLUT IN FRAME FRAME-A /* Flex ut / F11 */
DO:
   {muswait.i}
   RUN btnflut_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_GRUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_GRUND WFLEXTID
ON CHOOSE OF FBTN_GRUND IN FRAME FRAME-A /* Grunddata */
DO:
   {muswait.i}
   RUN hamta_UI.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_HELP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_HELP WFLEXTID
ON CHOOSE OF FBTN_HELP IN FRAME FRAME-A /* Hjälp / F1 */
DO:
   {muswait.i}
   RUN btnhelp_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IN WFLEXTID
ON CHOOSE OF FBTN_IN IN FRAME FRAME-A /* In / F3 */
DO:  
   {muswait.i}
   RUN btnin_UI.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_LUIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_LUIN WFLEXTID
ON CHOOSE OF FBTN_LUIN IN FRAME FRAME-A /* Lunch in / F6 */
DO:   
   {muswait.i}
   RUN btnluin_UI.
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_LUUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_LUUT WFLEXTID
ON CHOOSE OF FBTN_LUUT IN FRAME FRAME-A /* Lunch ut / F5 */
DO: 
   {muswait.i}
   RUN btnluut_UI.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OVIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OVIN WFLEXTID
ON CHOOSE OF FBTN_OVIN IN FRAME FRAME-A /* Övertid in / F8 */
DO:
   {muswait.i}
   RUN btnovin_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OVUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OVUT WFLEXTID
ON CHOOSE OF FBTN_OVUT IN FRAME FRAME-A /* Övertid ut / F7 */
DO:
   {muswait.i}
   RUN btnovut_UI.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_UT WFLEXTID
ON CHOOSE OF FBTN_UT IN FRAME FRAME-A /* Ut / F4 */
DO:
   {muswait.i}
   RUN btnut_UI.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KLOCKAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KLOCKAN WFLEXTID
ON ENTRY OF FILL-IN-KLOCKAN IN FRAME FRAME-A /* Klockan */
DO:
   RUN fillklock_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LOSEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LOSEN WFLEXTID
ON LEAVE OF FILL-IN-LOSEN IN FRAME FRAME-A /* Lösen */
DO:
   RUN filllosen_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PKOD WFLEXTID
ON LEAVE OF FILL-IN-PKOD IN FRAME FRAME-A /* Enhet/Sign */
DO:
   RUN fillpkod_UI.        
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME grundata
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL grundata WFLEXTID OCX.Tick
PROCEDURE grundata.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
  /*4:30-5:30*/
  IF TIME >= 16200 AND TIME <= 19800 THEN DO:
     RUN hamta_UI.
  END.
  /*14:30-15:30*/
  ELSE IF TIME >= 52200 AND TIME <= 55800 THEN DO:
     RUN hamta_UI. 
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME senastetryck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL senastetryck WFLEXTID OCX.Tick
PROCEDURE senastetryck.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
   IF appkolltid > TIME THEN appkolltid = TIME.   
   
   IF TIME > appkolltid + 600 THEN RUN appbort_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WFLEXTID 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
DO:
   IF VALID-HANDLE(blobproch) THEN RUN deleteproc_UI IN blobproch.         
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.) */
                  
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
   DEFINE VARIABLE sparfont AS INTEGER NO-UNDO.
   sparfont = varforetypval[20].
   varforetypval[20] = 6.
   {WIN_M_START.I}
   varforetypval[20] = sparfont.
   WFLEXTID:STATUS-AREA = TRUE.
   STATUS INPUT OFF.
   DEFAULT-WINDOW:HIDDEN = TRUE.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      ASSIGN
      FBTN_LUUT:ROW = 3.08 
      FBTN_OVUT:ROW = 3.08 
      FBTN_ANUT:ROW = 3.08 
      FBTN_FLUT:ROW = 3.08     
      FBTN_LUIN:ROW = 1.08 
      FBTN_OVIN:ROW = 1.08 
      FBTN_ANIN:ROW = 1.08 
      FBTN_FLIN:ROW = 1.08. 
   END.
   RUN huvud_UI.
   RUN enable_UI. 
   alltidmax = FALSE.
   /*{FRMSIZEF.I}*/
   {FRMSIZE.I} 
   RUN huvud2_UI.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appbort_UI WFLEXTID 
PROCEDURE appbort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*WFLEXTID:TITLE =  + STRING(TODAY,"99/99/99") + " " + STRING(TIME,"hh:mm").*/
   IF Guru.Konstanter:appcon = FALSE THEN RETURN.
   IF Guru.Konstanter:appcon = TRUE THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
   DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
   Guru.Konstanter:appcon = FALSE.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appkoll_UI WFLEXTID 
PROCEDURE appkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
   /*WFLEXTID:TITLE = STRING(appcon,"TRUE/FALSE") + STRING(TODAY,"99/99/99") + " " + STRING(TIME,"hh:mm").*/
   IF conappvar = "" THEN RETURN.
   IF Guru.Konstanter:appcon = TRUE THEN RETURN.
   CREATE SERVER Guru.Konstanter:apphand.
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT(conappvar,"ELPAO","KAGGEN",gforetagvar) NO-ERROR.
   IF NOT Guru.Konstanter:appcon THEN DO:
      MESSAGE 
      "Det går ej att ansluta appserver." SKIP  
      "Kontakta system ansvarig."    
      SKIP
      ERROR-STATUS:NUM-MESSAGES 
      " fel uppkom vid anslutningen." SKIP 
      "Vill du se dem ?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
      UPDATE view-errs AS LOGICAL .       
      IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
         MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
         ERROR-STATUS:GET-MESSAGE(ivar)
         VIEW-AS ALERT-BOX.
      END.     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnanin_UI WFLEXTID 
PROCEDURE btnanin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/           
   RUN pkoll_UI.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.
   RUN kontroll_UI (INPUT "btnanin_UI").   
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 0 THEN DO:     
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RUN gom_UI (INPUT FALSE).
         RETURN.
      END.
      ELSE IF felmeddtemp.VAL = 1 THEN DO:
         MESSAGE felmeddtemp.FELMEDD
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         DELETE felmeddtemp.
         CASE val:
            WHEN TRUE THEN DO:
               ASSIGN ejtid = TRUE.              
            END.
            WHEN FALSE THEN DO:
               RUN gom_UI (INPUT FALSE).
               RETURN.
            END.   
         END CASE. 
      END.
      ELSE IF felmeddtemp.VAL = 2 THEN DO:
         DELETE felmeddtemp.
         
         RUN AOVAL2.W (INPUT personaltemp.PERSONALKOD).
         
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RUN gom_UI (INPUT FALSE).
            RETURN.
         END. 
      END.
      ELSE IF felmeddtemp.VAL = 3 THEN DO:
         DELETE felmeddtemp.   
      END.      
   END.     
   ASSIGN 
   flexpers = personaltemp.PERSONALKOD   
   aonr3 = onr
   dnr3 = dnr
   typ3 = typ
   tpris3 = tpris.
   musz = FALSE.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      RUN kontroll_UI (INPUT "aforkortin_UI").  
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:      
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         RUN gom_UI (INPUT FALSE).
         DELETE felmeddtemp.
         RETURN.
      END.
   END.
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         IF personaltemp.ANSTALLNING = "Ej tidskrivande personal" THEN musz = musz.
         ELSE musz = TRUE.         
         IF TODAY < 01/01/2002 THEN musz = FALSE.         
      END.                   
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:         
         IF Guru.Konstanter:globanv = "rofo" THEN musz = TRUE.
      END.                   
      IF musz = TRUE THEN DO:
         musz = FALSE.      
         
         RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
         
      END.
      ELSE DO:
         RUN sundao_UI (INPUT "940").         
         ovut = personaltemp.OVERTIDUTTAG.
      END.
   END.
   ELSE DO:
      
      RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
      
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN. 
   END.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "snat" THEN DO:
      IF onr = "160" THEN DO:
         MESSAGE "Arbetstidsförkortning kan ej användas som närvaroprojekt, det vill säga som andra projektet vid Annat In-stämpling." VIEW-AS ALERT-BOX.
         RUN gom_UI (INPUT FALSE).         
         RETURN.
      END.
   END.
   IF Guru.Konstanter:globforetag = "misv" OR Guru.Konstanter:globforetag = "snat" THEN DO:
      IF onr = "161" THEN DO:
         MESSAGE "Arbetstidsförkortning kan ej användas som närvaroprojekt, det vill säga som andra projektet vid Annat In-stämpling." VIEW-AS ALERT-BOX.
         RUN gom_UI (INPUT FALSE).         
         RETURN.
      END.
   END.         
   
   {muswait.i}    
   RUN kontroll_UI (INPUT "btnanin2_UI").   
   {musarrow.i}
   RUN gom_UI (INPUT TRUE). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnanut_UI WFLEXTID 
PROCEDURE btnanut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.
   RUN klockan_UI.
   RUN SEKTIM.P.
   tid5 = nytid.   
   regdatum = TODAY.
   RUN REGVEC.P.
   RUN REGDAG.P.
   pkod = personaltemp.PERSONALKOD.
   {SLFLARBW.I}
   
   /*hårdkodning för att slippa ändra flexreg.MOSLUT efter sommartid OBS- måste ändras till vintertid en gång till*/
   ASSIGN
   flexmost = flexregtemp.MOSTART
   flexmosl = flexregtemp.MOSLUT
   flexkvsl = flexregtemp.KVSLUT.
   IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN DO:
      IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.               
      ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
      ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.       
   END.
   ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN DO:
      IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
      ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
      ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.
   END.
   ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN DO: 
      IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
      ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
      ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.
   END.   
   
   RUN deltid2_UI.
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF tid5 < flexmost THEN tid5 = flexmost.
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).           
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 4,INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   IF sok2 = 1 THEN DO:
      MESSAGE "Du har inte stämplat in idag." VIEW-AS ALERT-BOX.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END. 
   
   RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      RUN kontroll_UI (INPUT "aforkortut_UI").     
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:      
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.      
         DELETE felmeddtemp.
         RUN gom_UI (INPUT FALSE).
         RETURN.
      END.
   END.
   {muswait.i}   
   RUN kontroll_UI (INPUT "btnanut_UI").
   
   {musarrow.i}
   RUN gom_UI (INPUT TRUE).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnflin_UI WFLEXTID 
PROCEDURE btnflin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.   
   regdatum = TODAY.       
   kollen = 0.
   RUN klockan_UI.
   RUN SEKTIM.P.
   tid5 = nytid.
   regdatum = TODAY.
   RUN REGVEC.P.
   RUN REGDAG.P.
   pkod = personaltemp.PERSONALKOD.
   {SLFLARBW.I}
   IF regstart = regslut THEN DO:
      MESSAGE "Du har inte ordinarie arbetstid idag." VIEW-AS ALERT-BOX.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.     

   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
      flexkvst = flexregtemp.KVSTART.
      flexkvsl = flexregtemp.KVSLUT.
      IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND 
      MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND 
      DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND 
      DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN DO: 
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
   END.
   RUN deltid_UI.
   IF tid5 < regstart THEN DO:                                                         
      MESSAGE "Du kan inte registrera flex in innan ordinarie arbetstid!" SKIP
              "Använd knappen In istället" VIEW-AS ALERT-BOX.
      RUN gom_UI (INPUT FALSE).
      RETURN.   
   END.       
   ELSE IF tid5 > flexkvsl THEN DO:                                                         
      MESSAGE "Registreringen ligger utanför flexramen. Vill du registrera ändå?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
      CASE val2:
         WHEN FALSE THEN DO:
            RUN gom_UI (INPUT FALSE).
            RETURN.
         END.   
      END CASE.         
   END.     
   ejtid = FALSE.
   RUN kontroll_UI (INPUT "btnflin_UI").
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 0 THEN DO:     
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN.
      END.
      ELSE IF felmeddtemp.VAL = 1 THEN DO:
         MESSAGE felmeddtemp.FELMEDD
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         DELETE felmeddtemp.
         CASE val:
            WHEN TRUE THEN DO:
               ASSIGN ejtid = TRUE.                         
            END.
            WHEN FALSE THEN DO:
               RETURN.
            END.   
         END CASE. 
      END.
   END.   
   IF kollen = 0 THEN DO:   
      /* HÅRDKODA AONR FÖR FLEXTID*/
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" THEN DO:         
         RUN sundao_UI (INPUT "155").
         
      END.      
      ELSE DO:
         
         RUN AOVAL2.W (INPUT personaltemp.PERSONALKOD).   
         
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RUN gom_UI (INPUT FALSE).
         RETURN.
      END.   
   END.                              
   ASSIGN
   aonr3 = onr
   dnr3 = dnr
   typ3 = typ
   tpris3 = tpris
   musz = FALSE.      
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         IF personaltemp.ANSTALLNING = "Ej tidskrivande personal" THEN musz = musz.
         ELSE musz = TRUE.
         IF TODAY < 01/01/2002 THEN musz = FALSE.
         /*IF Guru.Konstanter:globanv = "ELPAO" OR Guru.Konstanter:globanv = "TEST" THEN musz = TRUE.*/
      END.             
      IF Guru.Konstanter:globforetag = "elpa" AND personaltemp.personalkod = "120" THEN musz = TRUE.
      IF Guru.Konstanter:globforetag = "elpa" AND personaltemp.personalkod = "140" THEN musz = TRUE.
      IF musz = TRUE THEN DO:
         musz = FALSE.      
 
         RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
 
      END.
      ELSE DO:
         RUN sundao_UI (INPUT "940").
         ovut = personaltemp.OVERTIDUTTAG.      
      END.
   END.
   ELSE DO:
      
      RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
      
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.  
   END.   
   {muswait.i}
   RUN kontroll_UI (INPUT "btnflin2_UI").
   {musarrow.i}
   RUN gom_UI (INPUT TRUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnflut_UI WFLEXTID 
PROCEDURE btnflut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.
   RUN klockan_UI.
   RUN SEKTIM.P.
   tid5 = nytid.     
   RUN REGVEC.P.
   RUN REGDAG.P.
   pkod = personaltemp.PERSONALKOD.
   {SLFLARBW.I}
   IF Guru.Konstanter:globforetag = "elpa" THEN.
   ELSE DO:
      FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
      IF AVAILABLE flexregtemp THEN DO:   
         IF tid5 < flexregtemp.MOSTART THEN tid5 = flexregtemp.MOSTART.
         IF tid5 > regslut THEN DO:                                                         
            MESSAGE "Du kan inte registrera flex ut efter ordinarie arbetstid! Använd ut-knappen" VIEW-AS ALERT-BOX.
            RUN gom_UI (INPUT FALSE).
            RETURN.         
         END.
      END.   
   END.
   RUN kontroll_UI (INPUT "btnflut_UI").
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 0 THEN DO:     
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN.
      END.
      ELSE IF felmeddtemp.VAL = 1 THEN DO:
         MESSAGE felmeddtemp.FELMEDD
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         DELETE felmeddtemp.
         CASE val:            
            WHEN FALSE THEN DO:               
               RUN gom_UI (INPUT FALSE).
               RETURN.
            END.   
         END CASE.          
      END.
   END.  
   /* HÅRDKODA AONR FÖR FLEX*/
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
      RUN sundao_UI (INPUT "155").
                   
   END.      
   ELSE DO:
     
     RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
     
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.   
   {muswait.i}
   RUN kontroll_UI (INPUT "btnflut2_UI").
   {musarrow.i}
   RUN gom_UI (INPUT TRUE).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnglom_UI WFLEXTID 
PROCEDURE btnglom_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   RUN pkoll_UI.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      IF AVAILABLE personaltemp THEN DO:
         /*IF PERSONALTAB.OMRADE = "SEAB K" THEN DO:
            musz = TRUE.
            MESSAGE "Knappen används ej av Korsta" VIEW-AS ALERT-BOX.
         END.   */
      END.   
   END.
   IF Guru.Konstanter:globforetag = "SUFL" THEN DO:
      IF AVAILABLE personaltemp THEN DO:
         IF personaltemp.PERSONALKOD  = "HB" THEN DO:
            musz = TRUE.
            MESSAGE "Knappen används ej av Korsta" VIEW-AS ALERT-BOX.
         END.   
      END.   
   END.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.      
   {muswait.i}
   ASSIGN
   vart = "NYA"
   bdatum = TODAY - 1.
   regdatum = bdatum.   
   IF Guru.Konstanter:globforetag = "cELPA" THEN DO:
      
      RUN GLOMDFU.W (INPUT personaltemp.PERSONALKOD).
      
   END.
   ELSE DO:       
      
      RUN GLOMDF.W (INPUT personaltemp.PERSONALKOD).
      
   END.
   {musarrow.i}             
   RUN gom_UI (INPUT TRUE).      
   ASSIGN
   musz = FALSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnhelp_UI WFLEXTID 
PROCEDURE btnhelp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*    IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:                                                                    */
/*       FILE-INFO:FILE-NAME = "guru.r".                                                                               */
/*       tempdir = SUBSTRING(FILE-INFO:FULL-PATHNAME, 1, R-INDEX(FILE-INFO:FULL-PATHNAME,"CSTART") - 1).               */
/*       MESSAGE "Filnamn:" FILE-INFO:FILE-NAME SKIP "Sökväg:" FILE-INFO:FULL-PATHNAME SKIP tempdir VIEW-AS ALERT-BOX. */
/* /*       MESSAGE SESSION:CLIENT-TYPE SKIP PROPATH SKIP THIS-PROCEDURE:FILE-NAME SKIP */                             */
/* /*           VIEW-AS ALERT-BOX.                                                      */                             */
/*    END.                                                                                                             */
/*    ELSE DO:                                                                                                         */
/*       RUN pathfinder_UI (INPUT "WTID", OUTPUT tempdir).                                                             */
/* /*       FILE-INFO:FILE-NAME = THIS-PROCEDURE:FILE-NAME. */                                                         */
/*                                                                                                                     */
/* /*       MESSAGE THIS-PROCEDURE:FILE-NAME SKIP FILE-INFO:FULL-PATHNAME. */                                          */
/*    END.  
                                                                                                           */
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
   {FINNSDYNBLOB.I}
   IF VALID-HANDLE(blobproch) THEN DO:
      RUN blobkor_UI IN blobproch (INPUT "Flexhjälp.Doc", INPUT "", OUTPUT felmedd).
      FIND FIRST blobinfotemp WHERE blobinfotemp.FILNAMN = "Flexhjälp.Doc" NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE blobinfotemp THEN DO:
         kommando = SEARCH("Flexhjälp.Doc").
         IF kommando = ? THEN DO:
            MESSAGE "Hittade inte Flexhjälp.Doc" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
         RUN OPENDOC.P (kommando,"","",NO).
         felmedd = "".
      END.
      IF felmedd NE "" THEN MESSAGE felmedd VIEW-AS ALERT-BOX.      
   END.
   ELSE DO:
      kommando = SEARCH("Flexhjälp.Doc").
      IF kommando = ? THEN DO:
         MESSAGE "Hittade inte Flexhjälp.Doc" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      RUN OPENDOC.P (kommando,"","",NO).
   END.
   IF VALID-HANDLE(blobproch) THEN RUN deleteproc_UI IN blobproch.         
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnin_UI WFLEXTID 
PROCEDURE btnin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END. 
   RUN klockan_UI.
   RUN SEKTIM.P.
   tid5 = nytid.    
   musz = FALSE.
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE flexregtemp THEN DO:
      MESSAGE "Inget flexavtal finns upplagt." VIEW-AS ALERT-BOX.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.        
   regdatum = TODAY.   
   RUN REGVEC.P.
   RUN REGDAG.P.
   pkod = personaltemp.PERSONALKOD.
   {SLFLARBW.I}    
   ejtid = FALSE.
   IF regstart = regslut THEN DO:
      MESSAGE "Du har inte ordinarie arbetstid idag." VIEW-AS ALERT-BOX.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.     
   RUN kontroll_UI (INPUT "btnin_UI").
   
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 0 THEN DO:     
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN.
      END.
      ELSE IF felmeddtemp.VAL = 1 THEN DO:
         MESSAGE felmeddtemp.FELMEDD
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         DELETE felmeddtemp.
         CASE val:            
            WHEN TRUE THEN ASSIGN ejtid = TRUE.
            WHEN FALSE THEN DO:               
               RUN gom_UI (INPUT FALSE).
               RETURN.
            END.   
         END CASE. 
      END.
   END.    
   IF AVAILABLE flexregtemp THEN DO:         
      ASSIGN
      flexmost = flexregtemp.MOSTART
      flexmosl = flexregtemp.MOSLUT.  
      /*hårdkodning för att slippa ändra flexreg.MOSLUT efter sommartid OBS- måste ändras till vintertid en gång till omgjort 20130521 Lena*/
      IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.               
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.       
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN DO: 
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.
      END.
      ELSE DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                       
         ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 7.3 THEN ASSIGN flexmosl = 8.  
      END.  
      RUN deltid2_UI.      
      IF tid5 > flexmosl THEN DO:               
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
            MESSAGE "Du är utanför flexramen använd knappen Flex in istället" VIEW-AS ALERT-BOX.
            musz = TRUE.
         END.
         ELSE IF Guru.Konstanter:globforetag = "elpa" THEN DO:
            
            RUN FELKNAPP.W (INPUT 1).
            
            musz = TRUE.
         END.
         ELSE DO:                                              
            ASSIGN florsak = "".
            RUN ORSAK.W.     
         END.   
      END.       
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.
   musz = FALSE.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO: 
      /*IF Guru.Konstanter:globforetag = "elpa" THEN.*/
      RUN salkoll_UI. /*problem med wrx */
   END.
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:     
         IF personaltemp.ANSTALLNING = "Ej tidskrivande personal" THEN musz = musz.
         ELSE musz = TRUE.
         IF TODAY < 01/01/2002  THEN musz = FALSE.         
      END.             
      IF Guru.Konstanter:globforetag = "elpa" AND personaltemp.personalkod = "120" THEN musz = TRUE.
      IF Guru.Konstanter:globforetag = "elpa" AND personaltemp.personalkod = "TEST2" THEN musz = TRUE.
      IF musz = TRUE THEN DO:
         musz = FALSE.      
         
         RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
         
      END.
      ELSE DO:
         RUN sundao_UI (INPUT "940").         
         ovut = personaltemp.OVERTIDUTTAG.      
      END.
   END.
   ELSE DO:
      
      RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
      
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.   
   {muswait.i}
   RUN kontroll_UI (INPUT "btnin2_UI").
   {musarrow.i}
   RUN gom_UI (INPUT TRUE).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnlosen_UI WFLEXTID 
PROCEDURE btnlosen_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   &Scoped-define FRAME-NAME FRAME-A
  IF INPUT FRAME {&FRAME-NAME} FILL-IN-LOSEN  = "" THEN DO:
      MESSAGE "Lösen kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-LOSEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.   
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE anvandartemp THEN DO:
      MESSAGE "Enhet/Sign finns inte." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE IF anvandartemp.AV-LOSEN NE INPUT FILL-IN-LOSEN THEN DO:
      MESSAGE "Lösenord felaktigt." VIEW-AS ALERT-BOX.
      FILL-IN-LOSEN = "".
      APPLY "ENTRY" TO FILL-IN-LOSEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END. 
   
   persrec = RECID(personaltemp).
   anvrec = RECID(anvandartemp).
   {muswait.i}
   {AVBGOM.I}
   RUN NYLOSEN.W (INPUT personaltemp.PERSONALKOD,INPUT-OUTPUT TABLE anvandartemp).
   {AVBFRAM.I}
   RUN gom_UI (INPUT FALSE).   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnluin_UI WFLEXTID 
PROCEDURE btnluin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.
   RUN klockan_UI.
   RUN SEKTIM.P.
   tid5 = nytid.
   
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
      IF tid5 < flexregtemp.LUSTART OR tid5 > flexregtemp.LUSLUT THEN DO:                                                         
         MESSAGE "Registreringen ligger utanför flexramen. Vill du registrera ändå?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         CASE val:
            WHEN FALSE THEN DO:
               RUN gom_UI (INPUT FALSE).
               RETURN.
            END.   
         END CASE. 
      END.   
   END.             
   RUN kontroll_UI (INPUT "btnluin_UI").
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 0 THEN DO:     
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RUN gom_UI (INPUT FALSE).
         RETURN.
      END.
      ELSE IF felmeddtemp.VAL = 1 THEN DO:
         MESSAGE felmeddtemp.FELMEDD
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
         DELETE felmeddtemp.
         CASE val:            
            WHEN FALSE THEN DO:               
               RUN gom_UI (INPUT FALSE).
               RETURN.
            END.   
         END CASE. 
      END.
   END.    
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      RUN sundao_UI (INPUT "940").
      ovut = personaltemp.OVERTIDUTTAG.      
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         IF Guru.Konstanter:globanv = "ELPAO" OR Guru.Konstanter:globanv = "TEST" THEN musz = TRUE.
      END.         
      IF musz = TRUE THEN DO:
         musz = FALSE.
         
         RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
         
      END.
   END.
   ELSE DO:
      RUN AOVAL.W (INPUT personaltemp.PERSONALKOD).
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.   
   {muswait.i}
   RUN kontroll_UI (INPUT "btnluin2_UI").
   {musarrow.i}
   RUN gom_UI (INPUT TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnluut_UI WFLEXTID 
PROCEDURE btnluut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.
   RUN klockan_UI.
   RUN SEKTIM.P.
   tid5 = nytid.
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
      IF tid5 < flexregtemp.LUSTART OR tid5 > flexregtemp.LUSLUT THEN DO:                                                         
         MESSAGE "Registreringen ligger utanför flexramen. Vill du registrera ändå?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         CASE val:
            WHEN FALSE THEN DO:
               RUN gom_UI (INPUT FALSE).
               RETURN.
            END.   
         END CASE. 
      END.   
   END.    
   RUN kontroll_UI (INPUT "btnluut_UI").
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RUN gom_UI (INPUT FALSE).
      RETURN.      
   END.    
   {muswait.i}
    RUN gom_UI (INPUT TRUE).
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnovin_UI WFLEXTID 
PROCEDURE btnovin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.
   regdatum = TODAY.
   RUN REGVEC.P.
   RUN REGDAG.P.
   pkod = personaltemp.PERSONALKOD.
   {SLUTARBW.I}
   RUN klockan_UI.
   RUN SEKTIM.P.
   ASSIGN tid = nytid.
   
   IF regstart NE regslut THEN DO:
      IF tid GE regstart AND tid < regslut THEN DO:
         MESSAGE "Du är inom ordinarie arbetstid. Registrera in istället!" VIEW-AS ALERT-BOX.
         RUN gom_UI (INPUT FALSE).
         RETURN.
      END.   
   END.   
   
   ovkoll = 1.
   
   RUN AOVALU.W (INPUT personaltemp.PERSONALKOD,INPUT tid, INPUT regstart ,INPUT regslut, OUTPUT rmal).  /*SKALL DET VARA NYTT AONR PÅ ÖVERTIDEN? */   
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.    
   END.   
   ovkoll = 0.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.   
   {muswait.i}
   RUN kontroll_UI (INPUT "btnovin_UI").
   {musarrow.i}
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FELTEXTW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
   END.
   ELSE DO:
      RUN FELTEXTW.P  
      (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
   END. 
   FOR EACH felmeddtemp:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
   END.
   RUN gom_UI (INPUT TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnovut_UI WFLEXTID 
PROCEDURE btnovut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END. 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      IF AVAILABLE personaltemp THEN DO:
         IF personaltemp.OMRADE = "SVAB" THEN DO:
            musz = TRUE.
            MESSAGE "Knappen används ej av Mittsverige vatten" VIEW-AS ALERT-BOX.
         END.   
      END.   
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.      
   regdatum = TODAY.
   RUN REGVEC.P.
   RUN REGDAG.P.
   pkod = personaltemp.PERSONALKOD.
   {SLUTARBW.I}
   RUN klockan_UI.
   RUN SEKTIM.P.
   ASSIGN tid5 = nytid.  
   /*tid5 = 17.5. */
   IF regstart NE regslut THEN DO:
      IF tid5 GE regstart AND tid5 < regslut THEN DO:
         MESSAGE "Du är inom ordinarie arbetstid och kan inte stämpla övertid!" VIEW-AS ALERT-BOX.
         RUN gom_UI (INPUT FALSE).
         RETURN.
      END.   
   END.
   RUN kontroll_UI (INPUT "btnovut_UI").
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RUN gom_UI (INPUT FALSE).
      RETURN.      
   END.    
   IF kollen3 = 1 THEN DO:
      kollen3 = 0.
      IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
         RUN sundao_UI (INPUT "940").
         ovut = personaltemp.OVERTIDUTTAG.               
      END.
      ELSE DO:
         
         ovkoll = 1.         
         RUN AOVALU.W (INPUT personaltemp.PERSONALKOD,INPUT tid5, INPUT regstart ,INPUT regslut, OUTPUT rmal).  /*SKALL DET VARA NYTT AONR PÅ ÖVERTIDEN? */
         florsak = rmal.
         ovkoll = 0.
         
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RUN gom_UI (INPUT FALSE).
         RETURN.
      END.   
      ASSIGN aonr3 = onr
      dnr3 = dnr
      typ3 = typ
      tpris3 = tpris
      kollen2 = 1.
   END.   
   {muswait.i}
   RUN kontroll_UI (INPUT "btnovut2_UI").  
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RUN gom_UI (INPUT FALSE).
      RETURN.      
   END.    
   {musarrow.i}
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FELTEXTW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
   END.
   ELSE DO:
      RUN FELTEXTW.P  
      (INPUT Guru.Konstanter:globanv,OUTPUT TABLE felmeddtemp,OUTPUT musz).     
   END. 
   FOR EACH felmeddtemp:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
   END.
   RUN gom_UI (INPUT TRUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnperiod_UI WFLEXTID 
PROCEDURE btnperiod_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pkoll_UI.  
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
   END.
   ELSE DO:        
      persrec = RECID(personaltemp).
      {muswait.i}
      
      RUN PERIOD.W (INPUT personaltemp.PERSONALKOD).
      
      RUN gom_UI (INPUT TRUE).
      {musarrow.i}
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnrapp_UI WFLEXTID 
PROCEDURE btnrapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
   END.
   ELSE DO:        
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:         
         {AVBGOM.I} 
         RUN VAlmanBtn.w.
         IF musz = FALSE THEN RUN SALMAN2.W (INPUT personaltemp.PERSONALKOD).
         {AVBFRAM.I}
         musz = FALSE.         
      END.
      ELSE DO:      
         val = TRUE.
         MESSAGE "Vill du se rapport för ej körd flextid klicka på JA." SKIP
         "Vill du se rapport för förra körningen av flextid klicka på NEJ." VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO-CANCEL UPDATE val.
         
         {AVBGOM.I}
         CASE val:
            WHEN TRUE THEN DO:
               manad = 1. 
               RUN FLRAPP2.W (INPUT personaltemp.PERSONALKOD).                   
            END.
            WHEN FALSE THEN DO:
               manad = 0.            
               RUN FLRAPP2.W (INPUT personaltemp.PERSONALKOD).                   
            END.
         END CASE.
         {AVBFRAM.I}
      END.
      
      {musarrow.i}
      RUN gom_UI (INPUT FALSE).
   END.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnsaldo_UI WFLEXTID 
PROCEDURE btnsaldo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).      
   END.
   ELSE DO:
      RUN flextab_UI (INPUT 5,INPUT personaltemp.PERSONALKOD).
      FIND FIRST flexsaldotemp WHERE flexsaldotemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexsaldotemp THEN DO:
         {muswait.i}
         nytid = flexsaldotemp.ACCFLEX.
         /*ACCUMULERAD FLEX + PERIODENS FLEX + EJKÖRD FLEX*/
         RUN TIMSEK.P.
         ASSIGN
         flsaldo = sekunder
         nytid = flexsaldotemp.PERIODFLEX.
         RUN TIMSEK.P.
         flsaldo = sekunder + flsaldo.
         nytid = flexsaldotemp.EJKORDFLEX.
         RUN TIMSEK.P.
         sekunder = sekunder + flsaldo.
         RUN FSEKTIM.P.
         {musarrow.i}
         MESSAGE "Du har saldo " STRING(fnytid,"->>9.99") " TIMMAR."  VIEW-AS ALERT-BOX.
         RUN gom_UI (INPUT FALSE).
      END.   
      ELSE DO:
         MESSAGE "Inget flexsaldo är ännu kört."  VIEW-AS ALERT-BOX.
         RUN gom_UI (INPUT FALSE).   
      END.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnsenaste_UI WFLEXTID 
PROCEDURE btnsenaste_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).      
   END.
   ELSE DO:       
      {muswait.i}                
      MESSAGE "Vill du visa senaste registrering klicka på JA." SKIP
      "Vill du ändra en ut registrering till Övertid ut klicka på NEJ." VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL UPDATE val3 AS LOGICAL.      
      CASE val3:
         WHEN TRUE THEN DO:            
            {AVBGOM.I}
            RUN ALLSIST.W.
            {AVBFRAM.I}         
            RUN gom_UI (INPUT FALSE).            
         END.   
         WHEN FALSE THEN DO:            
            {AVBGOM.I}            
            RUN UTOVUT.W (INPUT personaltemp.PERSONALKOD).
            {AVBFRAM.I}         
            RUN gom_UI (INPUT FALSE).            
         END.   
      END CASE.         
      {musarrow.i}
     /* {muswait.i}
      {AVBGOM.I}
      RUN ALLSIST.W.
      {AVBFRAM.I}         
      RUN gom_UI (INPUT FALSE).
      {musarrow.i}*/
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnut_UI WFLEXTID 
PROCEDURE btnut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN pkoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.
   flkoll = 1.
   RUN klockan_UI.
   RUN SEKTIM.P.
   tid5 = nytid.  
   regdatum = TODAY.
   RUN REGVEC.P.
   RUN REGDAG.P.
   pkod = personaltemp.PERSONALKOD.
   {SLUTARBW.I}
   IF regstart = regslut THEN DO:
      MESSAGE "Du har inte ordinarie arbetstid idag." VIEW-AS ALERT-BOX.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.     
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
      flexkvst = flexregtemp.KVSTART.
      flexkvsl = flexregtemp.KVSLUT.
      IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND 
       MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND 
      DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.
      ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND 
       DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN DO: 
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
      END.      
      RUN deltid_UI.                     
      ASSIGN
      sok1 = ansttemp.KOD
      sok4 = STRING(regdatum).
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      IF sok2 = 1 THEN DO:
         flexkvst = regslut.
      END.    
      
      IF tid5 < flexkvst THEN DO:                        
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
            MESSAGE "Du är utanför flexramen använd knappen Flex ut istället" VIEW-AS ALERT-BOX.
            musz = TRUE.
         END.
         ELSE IF Guru.Konstanter:globforetag = "elpa" THEN DO:
            
            RUN FELKNAPP.W (INPUT 2).
            
            musz = TRUE.
         END.
         ELSE DO:
            florsak = "".
            RUN ORSAK.W.                                 
         END.   
      END.
         
   END. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN gom_UI (INPUT FALSE).
      RETURN.
   END.      
   RUN kontroll_UI (INPUT "btnut_UI").  
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RUN gom_UI (INPUT FALSE).
      RETURN.      
   END.    
   {musarrow.i} 
   RUN gom_UI (INPUT TRUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load WFLEXTID  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "pstimerFLEXTIDU.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chgrundata = grundata:COM-HANDLE
    UIB_S = chgrundata:LoadControls( OCXFile, "grundata":U)
    chsenastetryck = senastetryck:COM-HANDLE
    UIB_S = chsenastetryck:LoadControls( OCXFile, "senastetryck":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "pstimerFLEXTIDU.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deltid2_UI WFLEXTID 
PROCEDURE deltid2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}                  
   IF personaltemp.DELTID = TRUE THEN DO:
      sok1 = personaltemp.ANSTALLNING.
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 5,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 5,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.   
      IF sok2 = 1 THEN sok2 = sok2.
      ELSE DO:
         sekunder = sok5.
         RUN SEKTIM.P.
         IF nytid < regstart THEN DO:             
            flexmostspar = flexmost.
            nytid = flexmosl.
            RUN TIMSEK.P.
            seku = sekunder.
            nytid = regstart.
            RUN TIMSEK.P.
            sekunder = seku - sok5 + sekunder.
            RUN SEKTIM.P.
            flexmosl = nytid.
            nytid = flexmost.
            RUN TIMSEK.P.
            seku = sekunder.
            nytid = regstart.
            RUN TIMSEK.P.            
            sekunder = seku - sok5 + sekunder.
            RUN SEKTIM.P.
            flexmost = nytid.            
            IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:               
               IF personaltemp.PERSONALKOD = "SEMGN" THEN DO:
                  flexmost = flexmostspar.
               END.
            END.
         END.
      END.        
   END.      

   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deltid_UI WFLEXTID 
PROCEDURE deltid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}   
   IF personaltemp.DELTID = TRUE THEN DO:
      sok1 = personaltemp.ANSTALLNING.
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 6,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 6,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.   
      IF sok2 = 1 THEN sok2 = sok2.
      ELSE DO:
         musz = FALSE.
         FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
         IF AVAILABLE flexregtemp THEN DO:            
            IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND 
            MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN musz = TRUE.
            ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND 
            DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN musz = TRUE.               
            ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND 
            DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN musz = TRUE.
         END.
         IF musz = TRUE THEN DO:
            musz = FALSE.
            IF sok4 NE "" THEN DO:
               nytid = DECIMAL(sok4).
            END.
            ELSE DO:
               sekunder = sok5.
               RUN SEKTIM.P.
            END.
         END.
         ELSE DO:         
            sekunder = sok5.
            RUN SEKTIM.P.
         END.
         IF nytid > regslut THEN DO: 
            flexkvslspar = flexkvsl.
            nytid = flexkvsl.
            RUN TIMSEK.P.
            seku = sekunder.
            nytid = regslut.
            RUN TIMSEK.P.
            sekunder = seku - sok5 + sekunder.
            RUN SEKTIM.P.
            flexkvsl = nytid.
            nytid = flexkvst.
            RUN TIMSEK.P.
            seku = sekunder.
            nytid = regslut.
            RUN TIMSEK.P.
            sekunder = seku - sok5 + sekunder.
            RUN SEKTIM.P.
            flexkvst = nytid.
            IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
               flexkvsl = flexkvslspar.
               /*IF personaltemp.PERSONALKOD = "LMN" THEN DO:
                  flexkvsl = flexkvslspar.
               END.
               IF personaltemp.PERSONALKOD = "SEMGN" THEN DO:
                  flexkvsl = flexkvslspar.
               END.
               IF personaltemp.PERSONALKOD = "SEABN" THEN DO:
                  flexkvsl = flexkvslspar.
               END.*/
            END.
         END.   
      END.        
   END.   
   ASSIGN
   sok1 = ansttemp.KOD
   sok4 = STRING(regdatum).
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   IF sok2 = 1 THEN DO:
      flexkvst = regslut.
   END.    
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WFLEXTID  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WFLEXTID)
  THEN DELETE WIDGET WFLEXTID.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WFLEXTID  _DEFAULT-ENABLE
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
  IF ocxvar = TRUE THEN RUN control_load.
  DISPLAY FILL-IN-KLOCKAN FILL-IN-PKOD FILL-IN-LOSEN FILL-IN_FORNAMN 
          FILL-IN_EFTERNAMN 
      WITH FRAME FRAME-A IN WINDOW WFLEXTID.
  ENABLE RECT-46 RECT-47 FBTN_IN FBTN_LUUT FBTN_OVUT FBTN_ANUT FBTN_FLUT 
         FBTN_GRUND FBTN_AVB FBTN_UT FBTN_LUIN FBTN_OVIN FBTN_ANIN FBTN_FLIN 
         FBTN_HELP BTN_GLOMD FILL-IN-PKOD BTN_LOSEN BTN_PERIOD FILL-IN-LOSEN 
         BTN_SALDO BTN_RAPP BTN_SENASTE 
      WITH FRAME FRAME-A IN WINDOW WFLEXTID.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillklock_UI WFLEXTID 
PROCEDURE fillklock_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN appkoll_UI.
   RUN klockan_UI.   
   RUN SEKTIM.P.
   FILL-IN-KLOCKAN = nytid.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filllosen_UI WFLEXTID 
PROCEDURE filllosen_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "CELPA" THEN musz = musz.
   ELSE DO:
      ASSIGN      
      FILL-IN_EFTERNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_FORNAMN:HIDDEN = FALSE.      
      DISPLAY  FILL-IN-KLOCKAN FILL-IN_EFTERNAMN FILL-IN_FORNAMN  WITH FRAME {&FRAME-NAME}.
   END.
   ASSIGN
   FILL-IN-LOSEN = INPUT FRAME {&FRAME-NAME} FILL-IN-LOSEN.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN ASSIGN FILL-IN-KLOCKAN:HIDDEN = TRUE.
   RUN appkoll_UI.
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillpkod_UI WFLEXTID 
PROCEDURE fillpkod_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEBUGGER:SET-BREAK().
   FIND FIRST forepersonaltemp WHERE forepersonaltemp.PERSONALKOD = TRIM(INPUT FRAME {&FRAME-NAME} FILL-IN-PKOD) NO-LOCK NO-ERROR.      
   IF AVAILABLE forepersonaltemp THEN DO:
      IF forepersonaltemp.GFORETAG NE gforetagvar THEN DO:
         gforetagvar = forepersonaltemp.GFORETAG.
         RUN rattdb_UI (INPUT gforetagvar).
        
         FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = gforetagvar NO-LOCK NO-ERROR.
         dbconvar = valdbtemp.DBNAMN.        
         RUN hamtafix_UI.
      END.  
   END.    
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = TRIM(INPUT FRAME {&FRAME-NAME} FILL-IN-PKOD) NO-LOCK NO-ERROR.
   {muswait.i}
   IF AVAILABLE personaltemp THEN DO: 
      RUN appkoll_UI.
      /*sok4 = personaltemp.PERSONALKOD.
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 11,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
       END.
       ELSE DO:
          RUN FLEXTIDH.P 
          (INPUT 11,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
          INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      IF sok1 NE "X" THEN DO:
         ASSIGN     
         FILL-IN_DATUM = DATE(sok4)
         FILL-IN_TID   = sok5
         FILL-IN_KNAPP = sok3
         FILL-IN_AONR  = sok1 
         FILL-IN_DELNR = sok2.
      END.   */
      RUN klockan_UI.
      RUN SEKTIM.P.
      ASSIGN
      FILL-IN_EFTERNAMN  = personaltemp.EFTERNAMN
      FILL-IN_FORNAMN = personaltemp.FORNAMN
      FILL-IN-KLOCKAN = nytid
      persrec = RECID(personaltemp).
      regdatum = TODAY.
      RUN REGVEC.P.
      RUN REGDAG.P.
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.  
      FIND FIRST utryckningtemp WHERE utryckningtemp.KOD = ansttemp.KOD
      USE-INDEX UT NO-LOCK NO-ERROR.  
      FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEX NO-LOCK 
      NO-ERROR.  
      IF AVAILABLE flexregtemp AND flexregtemp.LOSEN = FALSE THEN DO:
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "CELPA" THEN musz = musz.
         ELSE DO:
            ASSIGN            
            FILL-IN_EFTERNAMN:HIDDEN = FALSE
            FILL-IN_FORNAMN:HIDDEN = FALSE.            
            DISPLAY FILL-IN-KLOCKAN FILL-IN_EFTERNAMN FILL-IN_FORNAMN  WITH FRAME {&FRAME-NAME}.
         END.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "MISV" THEN ASSIGN FILL-IN-KLOCKAN:HIDDEN = TRUE. 
      END.
      

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flextab_UI WFLEXTID 
PROCEDURE flextab_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER vem AS CHARACTER NO-UNDO.
   EMPTY TEMP-TABLE eansttemp        NO-ERROR. 
   EMPTY TEMP-TABLE eflexregtemp     NO-ERROR. 
   EMPTY TEMP-TABLE eflexavttemp     NO-ERROR. 
   EMPTY TEMP-TABLE eflexsaldotemp   NO-ERROR. 
   EMPTY TEMP-TABLE eutryckningtemp  NO-ERROR.       
   IF vem = "" THEN DO:
      /*se hamtasub_UI.*/
      /*
      EMPTY TEMP-TABLE ansttemp        NO-ERROR. 
      EMPTY TEMP-TABLE flexregtemp     NO-ERROR. 
      EMPTY TEMP-TABLE flexavttemp     NO-ERROR. 
      EMPTY TEMP-TABLE flexsaldotemp   NO-ERROR. 
      EMPTY TEMP-TABLE utryckningtemp  NO-ERROR.       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT vadgora,INPUT vem,INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
         INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
      END.
      ELSE DO:
         RUN FLEXTAB.P 
         (INPUT vadgora,INPUT vem,INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
         INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
      END.    
      */ 
   END.
   ELSE DO:     
      FOR EACH flexsaldotemp WHERE flexsaldotemp.PERSONALKOD = vem:
         CREATE eflexsaldotemp.
         BUFFER-COPY flexsaldotemp TO eflexsaldotemp.
         DELETE flexsaldotemp.
      END.
      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT vadgora,INPUT vem,INPUT-OUTPUT TABLE eansttemp,INPUT-OUTPUT TABLE eflexregtemp,
         INPUT-OUTPUT TABLE eflexavttemp,INPUT-OUTPUT TABLE eflexsaldotemp,INPUT-OUTPUT TABLE eutryckningtemp).      
      END.
      ELSE DO:
         RUN FLEXTAB.P 
         (INPUT vadgora,INPUT vem,INPUT-OUTPUT TABLE eansttemp,INPUT-OUTPUT TABLE eflexregtemp,
         INPUT-OUTPUT TABLE eflexavttemp,INPUT-OUTPUT TABLE eflexsaldotemp,INPUT-OUTPUT TABLE eutryckningtemp).      
      END.     
   END.
   
   IF vem = "" THEN DO:      
   END.
   ELSE DO:
      FOR EACH eansttemp:
         CREATE ansttemp.
         BUFFER-COPY eansttemp TO ansttemp.
         DELETE eansttemp.
      END.
      FOR EACH eflexregtemp:
         CREATE flexregtemp.
         BUFFER-COPY eflexregtemp TO flexregtemp.
         DELETE eflexregtemp.
      END.
      FOR EACH eflexavttemp:
         CREATE flexavttemp.
         BUFFER-COPY eflexavttemp TO flexavttemp.
         DELETE eflexavttemp.
      END.
      FOR EACH eflexsaldotemp:
         CREATE flexsaldotemp.
         BUFFER-COPY eflexsaldotemp TO flexsaldotemp.
         DELETE eflexsaldotemp.
      END.
      FOR EACH eutryckningtemp:
         CREATE utryckningtemp.
         BUFFER-COPY eutryckningtemp TO utryckningtemp.
         DELETE eutryckningtemp.
      END.     
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flyttknapp_UI WFLEXTID 
PROCEDURE flyttknapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   
   ENABLE FBTN_IN WITH FRAME {&FRAME-NAME}. 
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?
   Guru.GlobalaVariabler:colrighth = FBTN_IN:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:   
      ENABLE FBTN_LUUT WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = FBTN_LUUT:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globanv = "elpao" THEN DO:   
      ENABLE FBTN_OVUT WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = FBTN_OVUT:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   ENABLE FBTN_ANUT WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = FBTN_ANUT:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).

   ENABLE FBTN_FLUT WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = FBTN_FLUT:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      ENABLE FBTN_GRUND WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = FBTN_GRUND:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   /*ENABLE FBTN_AVB WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = FBTN_AVB:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).*/

   Guru.GlobalaVariabler:collefth = ?.
   ENABLE FBTN_UT WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = FBTN_UT:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:   
      ENABLE FBTN_LUIN WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = FBTN_LUIN:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:   
      ENABLE FBTN_OVIN WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = FBTN_OVIN:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   ENABLE FBTN_ANIN WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = FBTN_ANIN:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).

   ENABLE FBTN_FLIN WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = FBTN_FLIN:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   

   /*ENABLE FBTN_HELP WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = FBTN_HELP:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE funk_UI WFLEXTID 
PROCEDURE funk_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF FOCUS:NAME = "FILL-IN-PKOD" THEN DO:
      APPLY "LEAVE" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
   END.      
   IF FOCUS:NAME = "FILL-IN-LOSEN" THEN DO:
      APPLY "LEAVE" TO FILL-IN-LOSEN IN FRAME {&FRAME-NAME}.
   END. 
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gom_UI WFLEXTID 
PROCEDURE gom_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER tackvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE hjw AS LOGICAL NO-UNDO.
   CURRENT-WINDOW = {&WINDOW-NAME}.   
   DEFAULT-WINDOW:HIDDEN = TRUE.
   IF tackvar = TRUE THEN DO:   
      /*
      hjw = DEFAULT-WINDOW:HIDDEN.
      DEFAULT-WINDOW:HIDDEN = TRUE.
      PAUSE 3 MESSAGE "Tack för din registrering".
      DEFAULT-WINDOW:HIDDEN = hjw. 
      */
      IF DEFAULT-WINDOW:HIDDEN = TRUE THEN PAUSE 3 MESSAGE "Tack för din registrering".
   END.
   ASSIGN
   FILL-IN-PKOD = ""
   FILL-IN_EFTERNAMN = ""
   FILL-IN_FORNAMN = ""   
   FILL-IN-LOSEN = "" .
   /*WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW PAUSE 2.*/
   ASSIGN   
   FILL-IN_EFTERNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_FORNAMN:HIDDEN = TRUE.    
   IF AVAILABLE flexregtemp THEN DO:
      IF flexregtemp.LOSEN = FALSE THEN DO:
         FILL-IN-LOSEN:HIDDEN = TRUE.
         BTN_LOSEN:HIDDEN = TRUE.
      END.    
      ELSE DO:
         FILL-IN-LOSEN:HIDDEN = FALSE.
         BTN_LOSEN:HIDDEN = FALSE.
         DISPLAY FILL-IN-LOSEN WITH FRAME {&FRAME-NAME}.
      END.
   END.
   DISPLAY FILL-IN-PKOD WITH FRAME {&FRAME-NAME}.
   APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}. 
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI WFLEXTID 
PROCEDURE hamta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE anvdator AS CHARACTER NO-UNDO.  
   {muswait.i}    
   /*RUN  Guru.Konstanter:appcon9.P (INPUT "ELPAO",INPUT "KAGGEN",INPUT gforetagvar).*/
     
   RUN appkoll_UI.
   WFLEXTID:TITLE = "Hämtar data! Vänta!".
   EMPTY TEMP-TABLE avdelningtemp NO-ERROR. 
   EMPTY TEMP-TABLE utsokaonr NO-ERROR. 
   EMPTY TEMP-TABLE anvandartemp NO-ERROR. 
   EMPTY TEMP-TABLE personaltemp NO-ERROR.
   IF AVAILABLE uppvaltemp THEN uppvaltemp.ANVANDARE = "".
   FOR EACH valdbtemp WHERE valdbtemp.GFORETAG = gforetagvar: 
      RUN hamtasub_UI.      
   END.  
   FOR EACH valdbtemp WHERE valdbtemp.GFORETAG NE gforetagvar: 
      RUN rattdb_UI (INPUT valdbtemp.GFORETAG).
           
      dbconvar = valdbtemp.DBNAMN.                          
      RUN hamtasub_UI.
   END. 
   FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = gforetagvar NO-LOCK NO-ERROR.
   IF dbconvar = valdbtemp.DBNAMN THEN.
   ELSE RUN rattdb_UI (INPUT gforetagvar).
      FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = gforetagvar NO-LOCK NO-ERROR.
   dbconvar = valdbtemp.DBNAMN. 
       
   RUN hamtafix_UI. 
   FIND FIRST uppvaltemp NO-LOCK NO-ERROR.
   /*
   RUN flextab_UI (INPUT 4,INPUT "").
   */  
   IF Guru.Konstanter:appcon THEN DO: 
      RUN INLOAPI.P (OUTPUT anvdator).
      ASSIGN
      datornamn = TRIM(SUBSTRING(anvdator,25,20)).
      IF ocxvar = TRUE THEN RUN GRUNDHKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT datornamn).
   END.
   
   WFLEXTID:TITLE = titlevar.
   /* + " Grunddata inläst " + STRING(TODAY,"99/99/99") + " " + STRING(TIME,"hh:mm").*/
   {musarrow.i}             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamtasub_UI WFLEXTID 
PROCEDURE hamtasub_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE avdelningtemp NO-ERROR. 
   EMPTY TEMP-TABLE utsokaonr NO-ERROR. 
   EMPTY TEMP-TABLE anvandartemp NO-ERROR. 
   EMPTY TEMP-TABLE personaltemp NO-ERROR.
   IF AVAILABLE uppvaltemp THEN uppvaltemp.ANVANDARE = "".
   FOR EACH foreavdelningtemp  WHERE foreavdelningtemp.GFORETAG = valdbtemp.GFORETAG:
      DELETE foreavdelningtemp.      
   END. 
   FOR EACH foreutsokaonr WHERE foreutsokaonr.GFORETAG = valdbtemp.GFORETAG:
      DELETE foreutsokaonr.      
   END.   
   FOR EACH foreanvandartemp WHERE foreanvandartemp.GFORETAG = valdbtemp.GFORETAG:
      DELETE foreanvandartemp.      
   END.      
   FOR EACH forepersonaltemp WHERE forepersonaltemp.GFORETAG = valdbtemp.GFORETAG:
      DELETE forepersonaltemp.       
   END.   
   IF Guru.Konstanter:appcon THEN DO:              
      RUN AVDHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT-OUTPUT TABLE avdelningtemp).
      EMPTY TEMP-TABLE utsokaonr NO-ERROR. 
      RUN AOHMTALLT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT " " ,INPUT-OUTPUT TABLE uppvaltemp, INPUT-OUTPUT TABLE utsokaonr, INPUT TABLE valsoktemp).
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT   (INPUT 3,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
      
   END.
   ELSE DO:      
      RUN AVDHMT.P (INPUT-OUTPUT TABLE avdelningtemp).
      EMPTY TEMP-TABLE utsokaonr NO-ERROR. 
      RUN AOHMTALLT.P (INPUT " " ,INPUT-OUTPUT TABLE uppvaltemp, INPUT-OUTPUT TABLE utsokaonr, INPUT TABLE valsoktemp).                  
      RUN ANVSKAP.P (INPUT 3,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).      
   END.
   FOR EACH avdelningtemp:
      CREATE foreavdelningtemp.
      BUFFER-COPY avdelningtemp TO foreavdelningtemp.
      foreavdelningtemp.GFORETAG = valdbtemp.GFORETAG.
   END. 
   FOR EACH utsokaonr:
      CREATE foreutsokaonr.
      BUFFER-COPY utsokaonr TO foreutsokaonr.
      foreutsokaonr.GFORETAG = valdbtemp.GFORETAG.
   END.   
   FOR EACH anvandartemp:
      CREATE foreanvandartemp.
      BUFFER-COPY anvandartemp TO foreanvandartemp.
      foreanvandartemp.GFORETAG = valdbtemp.GFORETAG.
   END.      
   FOR EACH personaltemp:
      CREATE forepersonaltemp.
      BUFFER-COPY personaltemp TO forepersonaltemp.
      forepersonaltemp.GFORETAG = valdbtemp.GFORETAG.
   END. 
   EMPTY TEMP-TABLE ansttemp        NO-ERROR. 
   EMPTY TEMP-TABLE flexregtemp     NO-ERROR. 
   EMPTY TEMP-TABLE flexavttemp     NO-ERROR. 
   EMPTY TEMP-TABLE flexsaldotemp   NO-ERROR. 
   EMPTY TEMP-TABLE utryckningtemp  NO-ERROR. 
   FOR EACH foreansttemp WHERE foreansttemp.GFORETAG = valdbtemp.GFORETAG:
      DELETE foreansttemp.      
   END. 
   FOR EACH foreflexregtemp WHERE foreflexregtemp.GFORETAG = valdbtemp.GFORETAG:
      DELETE foreflexregtemp.      
   END.   
   FOR EACH foreflexavttemp WHERE foreflexavttemp.GFORETAG = valdbtemp.GFORETAG:
      DELETE foreflexavttemp.      
   END.      
   FOR EACH foreflexsaldotemp WHERE foreflexsaldotemp.GFORETAG = valdbtemp.GFORETAG:
      DELETE foreflexsaldotemp.       
   END.  
   FOR EACH foreutryckningtemp WHERE foreutryckningtemp.GFORETAG = valdbtemp.GFORETAG:
      DELETE foreutryckningtemp.       
   END.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
   END.
   ELSE DO:
      RUN FLEXTAB.P 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
   END.    
   FOR EACH ansttemp:
      CREATE foreansttemp.
      BUFFER-COPY ansttemp TO foreansttemp.
      foreansttemp.GFORETAG = valdbtemp.GFORETAG.
   END.  
   FOR EACH flexregtemp:
      CREATE foreflexregtemp.
      BUFFER-COPY flexregtemp TO foreflexregtemp.
      foreflexregtemp.GFORETAG = valdbtemp.GFORETAG.
   END. 
   FOR EACH flexavttemp:
      CREATE foreflexavttemp.
      BUFFER-COPY flexavttemp TO foreflexavttemp.
      foreflexavttemp.GFORETAG = valdbtemp.GFORETAG.
   END.   
   FOR EACH flexsaldotemp:
      CREATE foreflexsaldotemp.
      BUFFER-COPY flexsaldotemp TO foreflexsaldotemp.
      foreflexsaldotemp.GFORETAG = valdbtemp.GFORETAG.
   END.      
   FOR EACH utryckningtemp:
      CREATE foreutryckningtemp.
      BUFFER-COPY utryckningtemp TO foreutryckningtemp.
      foreutryckningtemp.GFORETAG = valdbtemp.GFORETAG.
   END.                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamtafix_UI WFLEXTID 
PROCEDURE hamtafix_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE avdelningtemp NO-ERROR. 
   EMPTY TEMP-TABLE utsokaonr NO-ERROR. 
   EMPTY TEMP-TABLE anvandartemp NO-ERROR. 
   EMPTY TEMP-TABLE personaltemp NO-ERROR.
   FOR EACH foreavdelningtemp  WHERE foreavdelningtemp.GFORETAG = gforetagvar:
      CREATE avdelningtemp.
      BUFFER-COPY foreavdelningtemp TO avdelningtemp.      
   END. 
   FOR EACH foreutsokaonr WHERE foreutsokaonr.GFORETAG = gforetagvar:
      CREATE utsokaonr.
      BUFFER-COPY foreutsokaonr TO utsokaonr.      
   END.   
   FOR EACH foreanvandartemp WHERE foreanvandartemp.GFORETAG = gforetagvar:
      CREATE anvandartemp.
      BUFFER-COPY foreanvandartemp TO anvandartemp.      
   END.      
   FOR EACH forepersonaltemp WHERE forepersonaltemp.GFORETAG = gforetagvar:
      CREATE personaltemp.
      BUFFER-COPY forepersonaltemp TO personaltemp.      
   END.  
   EMPTY TEMP-TABLE ansttemp        NO-ERROR. 
   EMPTY TEMP-TABLE flexregtemp     NO-ERROR. 
   EMPTY TEMP-TABLE flexavttemp     NO-ERROR. 
   EMPTY TEMP-TABLE flexsaldotemp   NO-ERROR. 
   EMPTY TEMP-TABLE utryckningtemp  NO-ERROR.
   FOR EACH foreansttemp WHERE foreansttemp.GFORETAG = gforetagvar:
      CREATE ansttemp.
      BUFFER-COPY foreansttemp TO ansttemp.      
   END.  
   FOR EACH foreflexregtemp WHERE foreflexregtemp.GFORETAG = gforetagvar:
      CREATE flexregtemp.
      BUFFER-COPY foreflexregtemp TO flexregtemp.      
   END. 
   FOR EACH foreflexavttemp WHERE foreflexavttemp.GFORETAG = gforetagvar:
      CREATE flexavttemp.
      BUFFER-COPY foreflexavttemp TO flexavttemp.      
   END.   
   FOR EACH foreflexsaldotemp WHERE foreflexsaldotemp.GFORETAG = gforetagvar:
      CREATE flexsaldotemp.
      BUFFER-COPY foreflexsaldotemp TO flexsaldotemp.      
   END.      
   FOR EACH utryckningtemp WHERE foreansttemp.GFORETAG = gforetagvar:
      CREATE foreutryckningtemp.
      BUFFER-COPY utryckningtemp TO foreutryckningtemp.
      foreutryckningtemp.GFORETAG = valdbtemp.GFORETAG.
   END.        
   
                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud2_UI WFLEXTID 
PROCEDURE huvud2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   ASSIGN sparomrade = globomr.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN ASSIGN FILL-IN-KLOCKAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR  Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
      FIND FIRST flexregtemp USE-INDEX FLEXREG NO-LOCK NO-ERROR.
      IF AVAILABLE flexregtemp THEN DO:
         musz = FALSE.
         IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN musz = TRUE.
         ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN musz = TRUE.     
         ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN musz = TRUE. 
         IF musz = TRUE THEN DO:
            musz = FALSE.
            &Scoped-define FRAME-NAME FRAME-A
            FBTN_IN:TOOLTIP IN FRAME {&FRAME-NAME} = "Morgonregistrering skall ligga mellan 7.00 och 8.30".
            FBTN_FLIN:TOOLTIP = "Vid privata ärenden under dagen, och om man kommer efter 8.30".
            FBTN_FLUT:TOOLTIP = "Vid privata ärenden under dagen, och om man går för dagen innan 15.30".
            FBTN_UT:TOOLTIP = "Kvällsregistrering skall ligga mellan 15.30 och 18.00".
         END.
         ELSE DO:
            FBTN_IN:TOOLTIP = "Morgonregistrering skall ligga mellan 7.00 och 9.00".
            FBTN_FLIN:TOOLTIP = "Vid privata ärenden under dagen, och om man kommer efter 9.00".
            FBTN_FLUT:TOOLTIP = "Vid privata ärenden under dagen, och om man går för dagen innan 16.00".
            FBTN_UT:TOOLTIP = "Kvällsregistrering skall ligga mellan 16.00 och 18.00".
         END. 
      END.         
   END.         
   &Scoped-define FRAME-NAME FRAME-A
   my1hand = FILL-IN-PKOD:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN-LOSEN:MOVE-AFTER-TAB-ITEM(my1hand).
   FIND FIRST flexregtemp NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
     IF flexregtemp.LOSEN = FALSE THEN DO:
        FILL-IN-LOSEN:HIDDEN = TRUE.
        BTN_LOSEN:HIDDEN = TRUE.
     END.   
     IF flexregtemp.FLEXARB = FALSE THEN DO:
        ASSIGN
        FBTN_FLIN:HIDDEN = TRUE
        FBTN_FLUT:HIDDEN = TRUE.
      END.  
   END. 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "SUFL" THEN DO:    
      ASSIGN
      FBTN_LUIN:HIDDEN = TRUE
      FBTN_LUUT:HIDDEN = TRUE
      FBTN_OVIN:HIDDEN = TRUE
      FBTN_OVUT:HIDDEN = TRUE.
      BTN_SENASTE:HIDDEN = TRUE.
   END.
   IF  Guru.Konstanter:globforetag = "LULE" THEN DO:    
      ASSIGN      
      FBTN_OVIN:HIDDEN = TRUE
      FBTN_OVUT:HIDDEN = TRUE.
      BTN_SENASTE:HIDDEN = TRUE.
   END.
   
   IF Guru.Konstanter:globanv = "ELPAO" OR Guru.Konstanter:globanv = "sss" THEN DO:      
      FBTN_OVUT:HIDDEN = FALSE.      
   END.
   /*till ps-timer
   IF TIME >= 16200 AND TIME <= 19800 THEN RUN hamta_UI.
   */
   DISPLAY FILL-IN-KLOCKAN WITH FRAME {&FRAME-NAME}.
   APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
   {musarrow.i}
   {WIN_M_SLUT.I}
      
   IF Guru.Konstanter:globforetag = "elpa" THEN FBTN_GRUND:HIDDEN = FALSE.
   ELSE  FBTN_GRUND:HIDDEN = TRUE.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:    
      ASSIGN      
      FILL-IN_EFTERNAMN:HIDDEN = TRUE 
      FILL-IN_FORNAMN:HIDDEN = TRUE.       
   END.
   RUN flyttknapp_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WFLEXTID 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {&WINDOW-NAME}:MAX-BUTTON         = NO.
   {&WINDOW-NAME}:RESIZE             = NO.
   {muswait.i}
   CREATE uppvaltemp.
   ASSIGN         
   uppvaltemp.ENDBEST = ?  
   uppvaltemp.DELNRKOLL = TRUE
   uppvaltemp.VISPERAR = TRUE
   uppvaltemp.STARTDATUM = DATE(01,01,YEAR(TODAY))
   uppvaltemp.SLUTDATUM  = TODAY
   uppvaltemp.AVSLUTSTART = DATE(01,01,YEAR(TODAY))
   uppvaltemp.AVSLUTSLUT = TODAY
   uppvaltemp.TILLFALLFAST = 3
   SUBSTRING(uppvaltemp.PROJEKTOR,1,20) = "ALLA"
   uppvaltemp.BEREDARE = "ALLA"
   uppvaltemp.ARBANSVARIG = "ALLA"
   uppvaltemp.BESTNAMN = "ALLA"
   uppvaltemp.OMRNAMN = ""
   uppvaltemp.OMRADE = "Alla"
   uppvaltemp.BESTID = "ALLA"
   uppvaltemp.AVDNAMN = "ALLA"
   uppvaltemp.AVDNR = "ALLA"
   uppvaltemp.FAKTTYP = "ALLA"
   uppvaltemp.PAAV = 1.  
   
   
   titlevar = WFLEXTID:TITLE.
   RUN hamta_UI.
   /*
   {FINNSDYNBLOB.I}
   RUN blobimage_UI IN blobproch (INPUT "elpumea.jpg", IMG_ELPOOL:HANDLE IN FRAME {&FRAME-NAME}).
   FIND FIRST blobinfotemp WHERE blobinfotemp.FILNAMN = "elpumea.jpg" NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE blobinfotemp THEN IMG_ELPOOL:LOAD-IMAGE("BILDER\elpumea.jpg").
   IF IMG_ELPOOL:HANDLE = ? THEN IMG_ELPOOL:LOAD-IMAGE("BILDER\elpumea.jpg").
   END.        
   ELSE IMG_ELPOOL:LOAD-IMAGE("BILDER\elpumea.jpg").
   IF VALID-HANDLE(blobproch) THEN RUN deleteproc_UI IN blobproch.         
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
  */
   /*
   IMG_ELPOOL:LOAD-IMAGE("BILDER\elpool_guru.gif").
   */
  
   IMG_ELPOOL:LOAD-IMAGE ("BILDER\elpoollogo.png") IN FRAME {&FRAME-NAME}.
   FBTN_AVB:LOAD-IMAGE("BILDER\xbtn_favs.gif").
   IMG_ELPOOL:HIDDEN = FALSE.
   
   ovkoll = 0.
   RUN klockan_UI.
   RUN SEKTIM.P.
   FILL-IN-KLOCKAN = nytid.   
   /*&Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok. 
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}*/
                                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klockan_UI WFLEXTID 
PROCEDURE klockan_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 12,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 12,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   sekunder = sok2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontroll_UI WFLEXTID 
PROCEDURE kontroll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER program AS CHARACTER NO-UNDO.

   {FLEXAPPIN.I}  
   flexapp.PERSONALKOD = personaltemp.PERSONALKOD.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLEXSKAP2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT program,INPUT-OUTPUT TABLE flexapp,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN FLEXSKAP2.P 
      (INPUT program,INPUT-OUTPUT TABLE flexapp,OUTPUT TABLE felmeddtemp).      
   END.
   {FLEXAPPUT.I}       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pkoll_UI WFLEXTID 
PROCEDURE pkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN FILL-IN-PKOD = TRIM(INPUT FRAME {&FRAME-NAME} FILL-IN-PKOD).
   ASSIGN FILL-IN-LOSEN = INPUT FILL-IN-LOSEN.
   /*här rätt databas connectad ?*/
   RUN appkoll_UI.
   IF FILL-IN-PKOD = "" THEN DO:
      MESSAGE "Enhet/Sign kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      musz = TRUE.
      RETURN.
   END.   
   ELSE DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD =  FILL-IN-PKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE personaltemp THEN DO:
         MESSAGE "Enhet/Sign finns inte." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.
      ELSE DO:
         FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
         IF flexavttemp.FLEXTID = FALSE THEN DO:
            MESSAGE "Enhet/Sign har inte flextidsavtal." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
            musz = TRUE.
            RETURN.
         END.
      END.   
   END.   
   IF flexregtemp.LOSEN = TRUE THEN DO:
      IF FILL-IN-LOSEN = "" THEN DO:
         MESSAGE "Lösen kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.   
      ELSE DO:
         FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE anvandartemp THEN DO:
            losenvar = anvandartemp.AV-LOSEN.           
         END.         
         IF NOT AVAILABLE anvandartemp THEN DO:
            MESSAGE "Enhet/Sign finns inte." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
            musz = TRUE.
            RETURN.
         END.
         ELSE IF anvandartemp.AV-LOSEN NE INPUT FILL-IN-LOSEN THEN DO:
            FILL-IN-LOSEN = "".
            MESSAGE "Lösenord felaktigt." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
            musz = TRUE.
            RETURN.
         END.
         ELSE IF losenvar = anvandartemp.PERSONALKOD THEN DO:            
            /*kolla också att längden är mer än 4 tecken*/
            MESSAGE "Lösen får inte vara samma som användarnamn. Byt lösen och gör om registrering!" VIEW-AS ALERT-BOX.
            {AVBGOM.I}
            RUN NYLOSEN.W (INPUT personaltemp.PERSONALKOD,INPUT-OUTPUT TABLE anvandartemp).
            {AVBFRAM.I}
            RUN gom_UI (INPUT FALSE).    
            musz = TRUE.
            RETURN.
         END.
         ELSE IF LENGTH(losenvar) < 4 THEN DO:            
            /*kolla också att längden är mer än 4 tecken*/
            MESSAGE "Lösen måste vara minst 4 tecken långt. Byt lösen och gör om registrering!" VIEW-AS ALERT-BOX.
            {AVBGOM.I}
            RUN NYLOSEN.W (INPUT personaltemp.PERSONALKOD,INPUT-OUTPUT TABLE anvandartemp).
            {AVBFRAM.I}
            RUN gom_UI (INPUT FALSE).  
            musz = TRUE.
            RETURN.
         END.
      END.   
   END.   
   EMPTY TEMP-TABLE felmeddtemp  NO-ERROR. 
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE anvandartemp THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 3,INPUT personaltemp.PERSONALKOD,INPUT anvandartemp.ANVANDARE,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 3,INPUT personaltemp.PERSONALKOD,INPUT anvandartemp.ANVANDARE,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.         
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.
   END.
   IF hamtkoll + 14 < TODAY THEN DO:
      hamtkoll = TODAY.
      RUN hamta_UI. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rattdb_UI WFLEXTID 
PROCEDURE rattdb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER rattdbvar AS CHARACTER NO-UNDO.
  IF Guru.Konstanter:appcon THEN DO:   
      RUN DBDISCONN.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT dbconvar).
      RUN Guru.Konstanter:appcon9.P ON Guru.Konstanter:apphand TRANSACTION (INPUT "ELPAO",INPUT "KAGGEN",INPUT rattdbvar).
   END.
   ELSE DO:
      RUN DBDISCONN.p (INPUT dbconvar).
      RUN Guru.Konstanter:appcon9.P  (INPUT "ELPAO",INPUT "KAGGEN",INPUT rattdbvar).
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE salkoll_UI WFLEXTID 
PROCEDURE salkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN flextab_UI (INPUT 5,INPUT personaltemp.PERSONALKOD).
   FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   FIND FIRST flexsaldotemp WHERE flexsaldotemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE flexsaldotemp THEN DO:      
      nytid = flexsaldotemp.ACCFLEX.
      /*ACCUMULERAD FLEX + PERIODENS FLEX + EJKÖRD FLEX*/
      RUN TIMSEK.P.
      ASSIGN
      flsaldo = sekunder
      nytid = flexsaldotemp.PERIODFLEX.
      RUN TIMSEK.P.
      flsaldo = sekunder + flsaldo.
      nytid = flexsaldotemp.EJKORDFLEX.
      RUN TIMSEK.P.
      sekunder = sekunder + flsaldo.
      RUN FSEKTIM.P.
      IF fnytid GE 38 THEN DO:
          RUN FLEXMED.W (INPUT 1, INPUT fnytid).
      END.
      IF fnytid < - 8 THEN DO:
          RUN FLEXMED.W (INPUT 2 , INPUT fnytid).
      END.                  
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sundao_UI WFLEXTID 
PROCEDURE sundao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER sokao AS CHARACTER NO-UNDO.
   IF varforetypval[4] = 1 THEN DO:
      ASSIGN
      sok1 = sokao
      sok2 = 0
      sok3 = ""
      sok4 = personaltemp.PERSONALKOD
      sok5 = 0.
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 2,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.                  
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 2
      soktemp.SOKCHAR[1] = sokao
      soktemp.SOKINT[1] = 0
      soktemp.SOKCHAR[2] = personaltemp.PERSONALKOD
      soktemp.SOKCHAR[3] = sok3
      soktemp.SOKCHAR[4] = personaltemp.BEFATTNING 
      soktemp.SOKDATE[1] = TODAY.
      {SOKANROP.I}
      ASSIGN
      onr = sokao
      dnr = 0
      typ = soktemp.SOKCHAR[3] 
      tpris = soktemp.SOKDECI[1]
      tra = 0.
   END.
   ELSE DO:        
      ASSIGN
      sok1 = sokao
      sok2 = 0
      sok3 = ""
      sok4 = personaltemp.PERSONALKOD
      sok5 = 0.
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 2,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.                  
      ASSIGN
      onr = sokao
      dnr = 0
      typ = sok3 
      tpris =  sok5
      tra = 0.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

