&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: Returlösewin.w

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
DEFINE VARIABLE alltok AS LOGICAL NO-UNDO.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS GLOBFORETAG Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS GLOBFORETAG 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE GLOBFORETAG AS CHARACTER FORMAT "X(256)":U 
     LABEL "GLOBFORETAG" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     GLOBFORETAG AT ROW 3.25 COL 31 COLON-ALIGNED WIDGET-ID 2
     Btn_OK AT ROW 10 COL 32
     Btn_Cancel AT ROW 10 COL 50
     SPACE(0.13) SKIP(1.15)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "retur lösen test"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* retur lösen test */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    RUN ReturSmtp_UI (INPUT "anders@elpool.se", INPUT "test", OUTPUT alltok).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME GLOBFORETAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL GLOBFORETAG Dialog-Frame
ON LEAVE OF GLOBFORETAG IN FRAME Dialog-Frame /* GLOBFORETAG */
DO:
   Guru.Konstanter:globforetag = INPUT Guru.Konstanter:globforetag.
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
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY GLOBFORETAG 
      WITH FRAME Dialog-Frame.
  ENABLE GLOBFORETAG Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturSmtp_UI Dialog-Frame 
PROCEDURE ReturSmtp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER emailretur AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyLosen AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER oSuccessfulvar AS LOGICAL NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */
   DEFINE VARIABLE mailhub         AS CHARACTER NO-UNDO. /*smtpserver*/                                                                                                                                                  
   DEFINE VARIABLE EmailTo         AS CHARACTER NO-UNDO. /*till en eller flera ,*/                                                                                                                                       
   DEFINE VARIABLE EmailFrom       AS CHARACTER NO-UNDO. /*från en eller flera ,*/                                                                                                                                       
   DEFINE VARIABLE EmailCC         AS CHARACTER NO-UNDO. /*copia till 0 eller flera ,*/                                                                                                                                  
   DEFINE VARIABLE Attachmentstyp     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE LocalFiles      AS CHARACTER NO-UNDO. /*filer 0 eller flera ,*/                                                                                                                                       
   DEFINE VARIABLE Subject         AS CHARACTER NO-UNDO. /*ämne*/                                                                                                                                                        
   DEFINE VARIABLE Bodysmtp        AS CHARACTER NO-UNDO. /*body*/                                                                                                                                                        
   DEFINE VARIABLE MIMEHeader      AS CHARACTER NO-UNDO. /*MIMEHeader  CHARACTER - [type=<mimetype>][:CHARACTERset=<chrset>][:filetype=<type>]*/                                                       
   DEFINE VARIABLE BodyType        AS CHARACTER NO-UNDO. /*BodyType text om du skapar html direkt i bodyn eller file om du skapar en htmlfil först*/                                                                  
   DEFINE VARIABLE Importance      AS INTEGER NO-UNDO.   /*INT - Importance flag for the mail header,of the message sent. Valid values include 0 to 3, 1 = HIGH; 3 = Low */
   DEFINE VARIABLE L_DoAUTH        AS LOGICAL NO-UNDO.   /*LOGICAL - yes if authentication is requiered*/                     
   DEFINE VARIABLE C_AuthType      AS CHARACTER NO-UNDO. /*CHAR - Type of authentication. Currently supported types:base64 */ 
   DEFINE VARIABLE C_User          AS CHARACTER NO-UNDO. /*CHAR - The name of the SMTP server user*/                          
   DEFINE VARIABLE C_Password      AS CHARACTER NO-UNDO. /*CHAR - The password of the SMTP server user*/                      
   DEFINE VARIABLE oSuccessful     AS LOGICAL NO-UNDO.   /*Om true är allt ok*/                                                                                                                                                            
   DEFINE VARIABLE vMessage        AS CHARACTER NO-UNDO. /*meddelande i klartext hur sändninge gick*/


   DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE servervar AS CHARACTER NO-UNDO.
   IF GLOBFORETAG  = "MISV"  THEN DO:
      ASSIGN
      franvar = CHR(119) + CHR(101) + CHR(98) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(109) + CHR(105) + CHR(116) + CHR(116) + CHR(115) + CHR(118) + CHR(101) + CHR(114) + CHR(105) + CHR(103) + CHR(101) + CHR(118) + CHR(97) + CHR(116) + CHR(116) + CHR(101) + CHR(110) + CHR(46) + CHR(115) + CHR(101) 
       /*webguru@mittsverigevatten.se*/
      servervar = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50).
   END.
   ELSE IF GLOBFORETAG  = "SUND" THEN DO:
      ASSIGN
      franvar = CHR(119) + CHR(101) + CHR(98) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(115) + CHR(117) + CHR(110) + CHR(100) + CHR(115) + CHR(118) + CHR(97) + CHR(108) + CHR(108) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101). /*  webguru@sundsvallenergi.se*/
      servervar = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50).
   END.
   ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      {SMTPFRANELPOOL.I} /* @guru.sundsvallelnat.se*/
      emailretur = "victoria.rosengren@sundsvallelnat.se".
      franvar = CHR(64) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(46) + CHR(115) + CHR(117) + CHR(110) + CHR(100) + CHR(115) + CHR(118) + CHR(97) + CHR(108) + CHR(108) + CHR(101) + CHR(108) + CHR(110) + CHR(97) + CHR(116) + CHR(46) + CHR(115) + CHR(101) .
   END.
   ELSE IF Guru.Konstanter:globforetag = "cUMEA" THEN DO:
      ASSIGN 
      franvar = CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(117) + CHR(109) + CHR(101) + CHR(97) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) 
      servervar = CHR(111) + CHR(100) + CHR(101) + CHR(110) + CHR(46) + CHR(117) + CHR(109) + CHR(101) + CHR(97) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) .
   END.
   ELSE IF Guru.Konstanter:globforetag = "gkal"  THEN DO:
      ASSIGN   
      franvar = CHR(119) + CHR(101) + CHR(98) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(107) + CHR(97) + CHR(108) + CHR(109) + CHR(97) + CHR(114) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) 
      servervar = CHR(109) + CHR(97) + CHR(105) + CHR(108) + CHR(105) + CHR(110) + CHR(116).   
   END.
   ELSE IF Guru.Konstanter:globforetag = "BODE"  THEN DO:
      ASSIGN   
      franvar = "Guru@bodensenergi.se". 
      servervar = "bodensenergi-se.mail.protection.outlook.com".   
   END.
   
   ELSE DO:
      /*
      ASSIGN
   franvar = "elpool.ume@elpool.se"
  
   */
      ASSIGN
      franvar = "elpool.ume@elpool.se"
      servervar = CHR(115) + CHR(109) + CHR(116) + CHR(112) + CHR(46) + CHR(116) + CHR(101) + CHR(108) + CHR(101) + CHR(99) + CHR(111) + CHR(109) + CHR(51) + CHR(46) + CHR(110) + CHR(101) + CHR(116) .
      C_Password = CHR(122) + CHR(120) + CHR(61) + CHR(98) + CHR(112) + CHR(48) + CHR(113) + CHR(114).
      C_User = CHR(101) + CHR(108) + CHR(112) + CHR(111) + CHR(111) + CHR(108) + CHR(46) + CHR(117) + CHR(109) + CHR(101) + CHR(64) + CHR(101) + CHR(108) + CHR(112) + CHR(111) + CHR(111) + CHR(108) + CHR(46) + CHR(115) + CHR(101) .
      L_DoAUTH = TRUE.
      C_AuthType = "base64".
      
   END.
   MESSAGE "S" servervar "T" emailretur "F" franvar
   VIEW-AS ALERT-BOX.
   ASSIGN 
      mailhub             = servervar     
      EmailTo             = emailretur
      EmailFrom           = franvar
      EmailCC             = " " 
      Attachmentstyp      = ""
      LocalFiles          = ""
      Subject             = "Guru inloggning " 
      Bodysmtp            =  "Ditt nya engångs lösenord " + nyLosen
      MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
                            
      BodyType            = "".     
     
          
     {AMERICANEUROPEAN.I}
  
   
      RUN SMTPMAIL3.P  
      (INPUT mailhub,        /*smtpserver*/                                                                                    
      INPUT EmailTo,        /*till en eller flera ,*/                                                                         
      INPUT EmailFrom,      /*från en eller flera ,*/                                                                         
      INPUT EmailCC,        /*copia till 0 eller flera ,*/                                                                    
      INPUT Attachmentstyp, /*[type=<mimetype>][:CHARACTERset=<chrset>][:filetype=<type>]*/                                   
      INPUT LocalFiles,     /*filer 0 eller flera ,*/                                                                         
      INPUT Subject,        /*ämne*/                                                                                          
      INPUT Bodysmtp,       /*body*/                                                                                          
      INPUT MIMEHeader,     /*[type=<mimetype>][:CHARACTERset=<chrset>][:filetype=<type>]*/                                                                                          
      INPUT BodyType,       /*text om du skapar html direkt i bodyn eller file om du skapar en htmlfil först*/                
      INPUT Importance,     /*INT - Importance flag for the mail header,of the message sent. Valid values include 0 to 3, 1 = HIGH; 3 = Low */
      INPUT L_DoAUTH,       /*LOGICAL - yes if authentication is requiered*/
      INPUT C_AuthType,     /*CHAR - Type of authentication. Currently supported types:base64 */
      INPUT C_User,         /*CHAR - The name of the SMTP server user*/
      INPUT C_Password,     /*CHAR - The password of the SMTP server user*/
      OUTPUT oSuccessful,   /*Om true är allt ok*/                                                                            
      OUTPUT vMessage).     /*meddelande i klartext hur sändninge gick*/
   
      {EUROPEANAMERICAN.I}
      MESSAGE vMessage
      VIEW-AS ALERT-BOX.
      oSuccessfulvar = oSuccessful.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

