
/*------------------------------------------------------------------------
    File        : Returlosen.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Nov 16 16:33:19 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*   
    IF  globforetag = "sund" THEN EmailFrom = "webguru@sundsvallenergi.se".
         IF globforetag = "SNAT" THEN EmailFrom = "@guru.sundsvallelnat.se".
         IF globforetag = "MISV" THEN EmailFrom = "webguru@mittsverigevatten.se".
  */       
DEFINE VARIABLE restuserh AS HANDLE NO-UNDO.
DEFINE VARIABLE GLOBFORETAG AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE felmeddtemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL     AS INTEGER.

FUNCTION felmedd RETURNS CHARACTER(INPUT felnrmed AS INTEGER) :   
   IF felnrmed = 1 THEN RETURN "Lösen ordet kan inte vara blankt!".
   IF felnrmed = 2 THEN RETURN "Lösen får inte vara samma som användarnamn/Sign. Byt lösen!".
   IF felnrmed = 3 THEN RETURN "Lösen måste vara minst " + STRING(Guru.Konstanter:varforetypval[56]) +  " tecken. Byt lösen!".
   IF felnrmed = 4 THEN RETURN "Nytt lösen får inte vara samma som gammalt lösen Byt lösen!".
   IF felnrmed = 5 THEN DO:
      RETURN "För många försök. Du är nu spärrad! Ring Elpool 090/184540".
   END.
   IF felnrmed = 6 THEN RETURN "Felaktig inloggning!". 
   IF felnrmed = 7 THEN RETURN {LOSENKOLLFEL7.I}.  
   IF felnrmed = 8 THEN RETURN "MacAdress fel! Ring Elpool 090-184544 ".
   IF felnrmed = 9 THEN RETURN {LOSENKOLLFEL12.I}.
   IF felnrmed = 10 THEN RETURN {LOSENKOLLFEL13.I}.
END FUNCTION.
DEFINE VARIABLE alltok AS LOGICAL NO-UNDO.
 RUN ReturSmtp_UI (INPUT "marten.bring@one-nordic.se", INPUT "", OUTPUT alltok).

/* ********************  Preprocessor Definitions  ******************** */
PROCEDURE ReturMail_UI :
           DEFINE VARIABLE alltok AS LOGICAL NO-UNDO.
           DEFINE VARIABLE nyLosen AS CHARACTER NO-UNDO.
           DEFINE VARIABLE meddelandevar AS CHARACTER NO-UNDO.
            nyLosen = STRING(CHR(66) + CHR(89) + CHR(84) + CHR(65) + CHR(46) + CHR(78) + CHR(85)) + STRING(TIME).
            RUN BytLosen_UI (INPUT "rofo", INPUT nyLosen).
            RUN ReturSmtp_UI (INPUT "lena@elpool.se", INPUT nyLosen , OUTPUT alltok).
            IF alltok = TRUE THEN meddelandevar = "Mail är skickad! Använd det nya lösenordet, som du måste byta direkt!".
            ELSE meddelandevar = felmedd(10).
            RETURN.

   
END PROCEDURE.


PROCEDURE ReturSmtp_UI :
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

GLOBFORETAG = "ONENO".

   DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE servervar AS CHARACTER NO-UNDO.
   IF GLOBFORETAG = "ONENO" OR GLOBFORETAG = "ONENOUTBI" THEN DO:
      ASSIGN
      franvar = CHR(110) + CHR(111) + CHR(45) + CHR(114) + CHR(101) + CHR(112) + CHR(108) + CHR(121) + CHR(45) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(111) + CHR(110) + CHR(101) + CHR(45) + CHR(110) + CHR(111) + CHR(114) + CHR(100) + CHR(105) + CHR(99) + CHR(46) + CHR(115) + CHR(101).  
       /*webguru@mittsverigevatten.se*/
      servervar = CHR(115) + CHR(109) + CHR(116) + CHR(112) + CHR(46) + CHR(111) + CHR(110) + CHR(101) + CHR(46) + CHR(108) + CHR(111) + CHR(99) + CHR(97) + CHR(108). 
   END.
   ELSE IF GLOBFORETAG  = "MISV"  THEN DO:
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
   ELSE IF GLOBFORETAG = "SNAT" THEN DO:
      {SMTPFRANELPOOL.I} /* @guru.sundsvallelnat.se*/
      franvar = CHR(64) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(46) + CHR(115) + CHR(117) + CHR(110) + CHR(100) + CHR(115) + CHR(118) + CHR(97) + CHR(108) + CHR(108) + CHR(101) + CHR(108) + CHR(110) + CHR(97) + CHR(116) + CHR(46) + CHR(115) + CHR(101) .
   END.
   ELSE IF GLOBFORETAG = "UMEA" THEN DO:
      ASSIGN 
      franvar = CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(117) + CHR(109) + CHR(101) + CHR(97) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) 
      servervar = CHR(111) + CHR(100) + CHR(101) + CHR(110) + CHR(46) + CHR(117) + CHR(109) + CHR(101) + CHR(97) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) .
   END.
   ELSE IF GLOBFORETAG = "gkal"  THEN DO:
      ASSIGN   
      franvar = CHR(119) + CHR(101) + CHR(98) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(107) + CHR(97) + CHR(108) + CHR(109) + CHR(97) + CHR(114) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) 
      servervar = CHR(109) + CHR(97) + CHR(105) + CHR(108) + CHR(105) + CHR(110) + CHR(116).   
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
   MESSAGE servervar GLOBFORETAG
   VIEW-AS ALERT-BOX.
   ASSIGN 
      mailhub             = servervar     
      EmailTo             = emailretur
      EmailFrom           = franvar
      EmailCC             = " " 
      Attachmentstyp      = ""
      LocalFiles          = ""
      Subject             = "TEST Guru inloggning " 
      Bodysmtp            =  "Ditt nya engångs lösenord." + nyLosen + CHR(10) + " Ange Använar och Lösen tryck sedan Byt lösenord och ange ett eget lösenord! "
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
      oSuccessfulvar = oSuccessful.
      
      MESSAGE oSuccessful vMessage
      VIEW-AS ALERT-BOX.
  
END PROCEDURE.
/*
 PROCEDURE BytLosen_UI :
   /*
   IF typkoll = 3 THEN DO TRANSACTION:
   */
   
   DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER uppdatlosen AS CHARACTER NO-UNDO.
   DO TRANSACTION:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv 
      USE-INDEX ANDV EXCLUSIVE-LOCK NO-ERROR.
      IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN  ASSIGN ANVANDARE.AV-LOSEN = Guru.Konstanter:SaltRetur(uppdatlosen).
      ELSE ASSIGN ANVANDARE.AV-LOSEN = uppdatlosen.
   END.
   FIND CURRENT ANVANDARE NO-LOCK.    
   RUN RESTUSERS.P PERSISTENT SET restuserh. 
   
   RUN restBytLosenord_UI IN restuserh(INPUT ANVANDARE.PERSONALKOD,INPUT ANVANDARE.AV-LOSEN, OUTPUT TABLE felmeddtemp). 
   RUN avslutarestUsr_UI IN restuserh.
   DELETE PROCEDURE restuserh NO-ERROR.
   restuserh = ?.

END PROCEDURE.  
 */        