/*SMTPEXTRA.I*/

/*Anders Olsson Elpool i Umeå AB  7 jun 2017 14:49:32 
används inte 
*/

IF globforetag = "cELPA" OR globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" THEN DO:
      /*sund pga citrix*/
      IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "MISV" THEN mailhub  = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50).
      
      RUN EPOSTKOLL.P (INPUT FILL-IN_MOTTAGARE,OUTPUT musz).
      IF musz = FALSE THEN DO:
         APPLY "ENTRY" TO FILL-IN_MOTTAGARE IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
      ELSE DO: 
         musz = FALSE.
         DEFINE VARIABLE globanv2 AS CHARACTER NO-UNDO.
         DEFINE VARIABLE avsandare  AS CHARACTER NO-UNDO.
         ASSIGN
         globanv2 = ""
         avsandare = "".
         IF Guru.Konstanter:appfel = FALSE THEN DO: 
            RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).

            ASSIGN
            globanv2 = TRIM(outanvanv).
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 1,INPUT Guru.Konstanter:globanv2,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
            END.
            ELSE DO:
               RUN ANVSKAP.P 
               (INPUT 1,INPUT Guru.Konstanter:globanv2,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
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
                  avsandare = personaltemp.EFTERNAMN + " " + personaltemp.FORNAMN.
               END.
               ELSE avsandare = " ".
               IF globanv2 = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN avsandare = " ".
            END.
            ASSIGN 
/*             mailhub = CHR(115) + CHR(109) + CHR(116) + CHR(112) + CHR(46) + CHR(116) + CHR(101) + CHR(108) + CHR(101) + CHR(99) + CHR(111) + CHR(109) + CHR(51) + CHR(46) + CHR(110) + CHR(101) + CHR(116)  172.16.79.249 
                                    130.1.27.253*/
            EmailTo             = FILL-IN_MOTTAGARE 
            EmailFrom           = avsandare
            EmailCC             = ""
            Attachmentstyp      = ""
            LocalFiles          = ""
            Subject             = FILL-IN-AMNE
            Bodysmtp            = EDITOR_MEDD
            MIMEHeader          = "type=text/html:charset=iso-8859-1:filetype=ascii"
            BodyType            = "".
            status-ok = SESSION:SET-WAIT-STATE("GENERAL").
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
   /*GG slut*/
