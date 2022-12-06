/*EPOST.P*/
DEFINE INPUT  PARAMETER Profile  AS CHARACTER.      /* Profile name for sender */
DEFINE INPUT  PARAMETER Address  AS CHARACTER.      /* Email address of recipient */
DEFINE INPUT  PARAMETER Amne  AS CHARACTER.      /* Subject of email */
DEFINE INPUT  PARAMETER Body     AS CHARACTER.      /* Body text */
DEFINE INPUT  PARAMETER Filpath  AS CHARACTER.      /* Name of file to attach */
DEFINE INPUT  PARAMETER globanv2 AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER globforetag2 AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER Ereslut  AS LOGICAL.        /* Email status  */
DEFINE OUTPUT PARAMETER Emailtxt AS CHARACTER.      /* Status txt  */
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE avsandare  AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE avsandarepost AS CHARACTER NO-UNDO.
&Scoped-define NEW   
&Scoped-define SHARED
{GLOBVAR2DEL1.I}
{ANVPERS.I}
{SMTPDEF3.I}
ASSIGN
globanv = globanv2.

/*GG 20060208 - För dom som sitter på citrixlösning...funkar ej MAPI epostfunktionen*/
IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
   /*sund pga citrix*/
   /*  gammal adress !! IF Guru.Konstanter:globforetag = "SUND" THEN mailhub  = "172.16.79.249".
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN mailhub  = "130.1.27.253".
   ELSE IF Guru.Konstanter:globforetag = "MISV" THEN mailhub  = "130.1.27.253".
   */   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN mailhub  = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50) .
   ELSE IF Guru.Konstanter:globforetag = "MISV" THEN mailhub  = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50).
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN mailhub  = CHR(115) + CHR(109) + CHR(116) + CHR(112) + CHR(46) + CHR(116) + CHR(101) + CHR(108) + CHR(101) + CHR(99) + CHR(111) + CHR(109) + CHR(51) + CHR(46) + CHR(110) + CHR(101) + CHR(116) .

   RUN EPOSTKOLL.P (INPUT Address,OUTPUT musz).
   IF musz = FALSE THEN DO:
      MESSAGE "Felaktig mottagaradress!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE DO: 
      musz = FALSE.
      ASSIGN
      globanv2 = ""
      avsandare = "".
      IF Guru.Konstanter:appfel = FALSE THEN DO: 
         RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).

         ASSIGN
         globanv2 = TRIM(outanvanv).
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 1,INPUT globanv2,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
         END.
         ELSE DO:
            RUN ANVSKAP.P 
            (INPUT 1,INPUT globanv2,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
         END.
         FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = globanv2 NO-LOCK NO-ERROR.
         IF AVAILABLE anvandartemp THEN DO:
            avsandare = anvandartemp.PERSONALKOD.
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 3,INPUT avsandare,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
            END.
            ELSE DO:
               RUN ANVSKAP.P 
               (INPUT 3,INPUT avsandare,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
            END.
            FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = avsandare NO-LOCK NO-ERROR.
            IF AVAILABLE personaltemp THEN DO:
               ASSIGN
               avsandare = personaltemp.EFTERNAMN + " " + personaltemp.FORNAMN
               avsandarepost = SUBSTRING(personaltemp.PERSONSOK,20).
            END.            
         END.
         ELSE avsandare = globanv2.
         IF avsandarepost NE "" THEN do:
            IF EmailCC = "" THEN EmailCC = avsandarepost.            
         END.

         IF Filpath NE "" THEN DO:
            /*Maska fram filnamnet*/
            DEFINE VARIABLE pos AS INTEGER NO-UNDO.
            DEFINE VARIABLE pos2 AS INTEGER NO-UNDO.
            DEFINE VARIABLE startpos AS INTEGER NO-UNDO.
            DEFINE VARIABLE filvar AS CHARACTER NO-UNDO.
            Filpath = REPLACE(Filpath,"\","/").
            startpos = INDEX(Filpath,".",LENGTH(Filpath) - 5).
            pos = 0.
            REPEAT :
               pos2 = pos.
               pos = pos + 1.
               pos = INDEX(Filpath,"/",pos).                                        
               IF pos = 0 THEN LEAVE.
            END.
            pos = pos2 + 1.
            IF pos GE 0 THEN filvar = SUBSTRING(Filpath,pos,LENGTH(Filpath)).
            ELSE filvar = Filpath.
            /*Slut mask*/
         END.
         
         IF Filpath NE "" THEN DO:
            Attachmentstyp = filvar.
         END.
         ELSE Attachmentstyp = "".
         IF avsandarepost NE "" THEN do:
            IF INDEX(avsandare,"@",1) = 0 THEN avsandare = avsandarepost.
         END.

         ASSIGN
         EmailTo             = Address 
         EmailFrom           = avsandare
         LocalFiles          = Filpath
         Subject             = Amne
         Bodysmtp            = Body
         MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
         BodyType            = "".
         status-ok = SESSION:SET-WAIT-STATE("GENERAL").
         IF  Guru.Konstanter:globforetag = "sund" THEN EmailFrom = "webguru@sundsvallenergi.se".
         IF Guru.Konstanter:globforetag = "SNAT" THEN EmailFrom = "@guru.sundsvallelnat.se".
         IF Guru.Konstanter:globforetag = "MISV" THEN EmailFrom = "webguru@mittsverigevatten.se".
         RUN smtpmail_UI (INPUT FALSE).
         status-ok = SESSION:SET-WAIT-STATE("").
        /* MESSAGE EmailTo vMessage VIEW-AS ALERT-BOX.*/
         IF oSuccessful = TRUE THEN DO TRANSACTION:
            oSuccessful = FALSE.               
         END.            
         musz = FALSE.
      END.      
   END.          
END.
ELSE DO:
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT globanv,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 1,INPUT globanv,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = globanv NO-LOCK NO-ERROR.
   IF AVAILABLE anvandartemp THEN DO:
      avsandare = anvandartemp.PERSONALKOD.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 3,INPUT avsandare,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
      END.
      ELSE DO:
         RUN ANVSKAP.P 
         (INPUT 3,INPUT avsandare,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
      END.
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = avsandare
      NO-LOCK NO-ERROR.
      IF AVAILABLE personaltemp THEN DO:
         ASSIGN
         avsandare = personaltemp.EFTERNAMN + " " + personaltemp.FORNAMN
         avsandarepost = SUBSTRING(personaltemp.PERSONSOK,20).
      END.
      ELSE DO:
         ASSIGN
         avsandare = " "
         avsandarepost = " ".
      END.
      IF globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN avsandare = " ".
   END.
   
   DEFINE VARIABLE TextPtr AS MEMPTR NO-UNDO.
   IF Filpath = "" THEN body = body.
   ELSE body = body + CHR(10) + CHR(10) + " ".
   SET-SIZE(TextPtr) =  60000.
   PUT-STRING(TextPtr,1) = body.
   RUN EPOSTMAPI.P (INPUT  Address,
                    INPUT  avsandarepost,
                    INPUT  "",
                    INPUT  Amne,
                    INPUT  TextPtr,                 
                    INPUT  Filpath,
                    INPUT  avsandare,
                    OUTPUT Ereslut,
                    OUTPUT Emailtxt
                    ).
   SET-SIZE(TextPtr) = 0.
   /* RUN EPOSTMAPI.P (INPUT  Address,  */
   /*                  INPUT  "",       */
   /*                  INPUT  "",       */
   /*                  INPUT  Amne,  */
   /*                  INPUT  Body,     */
   /*                  INPUT  "",       */
   /*                  INPUT  Filpath,  */
   /*                  INPUT  avsandare */
   /*                  ).               */
   RETURN.
END.
