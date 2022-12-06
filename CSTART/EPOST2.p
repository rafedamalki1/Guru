/*EPOST2.P*/
DEFINE VARIABLE radantal     AS INTEGER NO-UNDO.
DEFINE VARIABLE profilvar    AS CHARACTER NO-UNDO.

{TIDUTTT.I}

DEFINE TEMP-TABLE HEAD
   FIELD UT AS CHARACTER.
   
DEFINE TEMP-TABLE Body1
   FIELD UT AS CHARACTER.
   
DEFINE TEMP-TABLE Body2
   FIELD UT AS CHARACTER.
   
DEFINE TEMP-TABLE Body3
   FIELD UT AS CHARACTER.

DEFINE TEMP-TABLE Body4
   FIELD UT AS CHARACTER.
   
DEFINE TEMP-TABLE Body5
   FIELD UT AS CHARACTER.      
   

DEFINE INPUT  PARAMETER Profile  AS CHARACTER.      /* Profile name for sender */
DEFINE INPUT  PARAMETER Address  AS CHARACTER.      /* Email address of recipient */
DEFINE INPUT  PARAMETER Amne  AS CHARACTER.      /* Subject of email */
DEFINE INPUT  PARAMETER TABLE FOR tidut.            /* Body text */
DEFINE INPUT  PARAMETER Filpath  AS CHARACTER.      /* Name of file to attach */
DEFINE INPUT  PARAMETER globanv2 AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER globforetag2 AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER Ereslut  AS LOGICAL.        /* Email status  */
DEFINE OUTPUT PARAMETER Emailtxt AS CHARACTER.      /* Status txt  */   
DEFINE VARIABLE avsandare  AS CHARACTER NO-UNDO.
DEFINE VARIABLE Bodyvar    AS CHARACTER.
DEFINE VARIABLE Body2var   AS CHARACTER.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE avsandarepost AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.
&Scoped-define NEW   
&Scoped-define SHARED
{GLOBVAR2DEL1.I}
{ANVPERS.I}
{SMTPDEF3.I}
ASSIGN
globanv = globanv2.

FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   CREATE Body1.
   B1:
   FOR EACH tidut:      
      IF LENGTH(Body1.UT,"CHARACTER") < 24900 THEN DO:
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            Body1.UT = Body1.UT  + TRIM(tidut.UT) + CHR(13) + CHR(10).
         END.
         ELSE DO:
            Body1.UT = Body1.UT  + TRIM(tidut.UT) + CHR(10).
         END.   
         DELETE tidut. 
      END.
      ELSE LEAVE B1.      
   END.
END.      
FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   CREATE Body2.
   B2:
   FOR EACH tidut:    
      IF LENGTH(Body2.UT,"CHARACTER") < 24900 THEN DO:
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            Body2.UT = Body2.UT  + TRIM(tidut.UT) + CHR(13) + CHR(10).
         END.
         ELSE DO:
            Body2.UT = Body2.UT  + TRIM(tidut.UT) + CHR(10).
         END.   
         DELETE tidut.
      END. 
      ELSE LEAVE B2.
   END.
END.
FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   CREATE Body3.
   B3:
   FOR EACH tidut:    
      IF LENGTH(Body3.UT,"CHARACTER") < 24900 THEN DO:
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            Body3.UT = Body3.UT  + TRIM(tidut.UT) + CHR(13) + CHR(10).
         END.
         ELSE DO:
            Body3.UT = Body3.UT  + TRIM(tidut.UT) + CHR(10).
         END.   
         DELETE tidut.
      END. 
      ELSE LEAVE B3.
   END.
END.
FIND FIRST body1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE body1 THEN CREATE body1.
FIND FIRST body2 NO-LOCK NO-ERROR.
IF NOT AVAILABLE body2 THEN CREATE body2.
FIND FIRST body3 NO-LOCK NO-ERROR.
IF NOT AVAILABLE body3 THEN CREATE body3.

FIND FIRST tidut NO-LOCK NO-ERROR.
IF AVAILABLE tidut THEN DO:
   MESSAGE "Ett fel har uppstått kontakta Anders Olsson Elpool 090/184540. Ange EDI-fel"
   VIEW-AS ALERT-BOX TITLE "Medelande".
   RETURN.
END.
DEFINE VARIABLE TextPtr AS MEMPTR NO-UNDO.
DEFINE VARIABLE langd1 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd3 AS INTEGER NO-UNDO.
ASSIGN
langd1 = LENGTH(Body1.UT)
langd2 = LENGTH(Body2.UT)
langd3 = LENGTH(Body3.UT).

SET-SIZE(TextPtr) = langd1 + langd2 + langd3 + 1.
PUT-STRING(TextPtr,1) = body1.UT.
IF body2.UT NE "" THEN
PUT-STRING(TextPtr,langd1 + 1) = body2.UT.
IF body3.UT NE "" THEN
PUT-STRING(TextPtr,langd1 + langd2 + 1) = body3.UT.

IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
   /*sund pga citrix*/   
   /*  gammal adress !! IF Guru.Konstanter:globforetag = "SUND" THEN mailhub  = "172.16.79.249".  
   IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" ) THEN mailhub  = "130.1.27.253".
   ELSE IF Guru.Konstanter:globforetag = "MISV" THEN mailhub  = "130.1.27.253".*/
   IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" ) THEN mailhub  = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50).
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
         IF avsandarepost NE "" THEN DO:
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
         Bodysmtp            = STRING(TextPtr)
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
         SET-SIZE(TextPtr) = 0.
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
      ELSE do:
         ASSIGN
         avsandare = " " 
         avsandarepost = " ".
      END.
   END.
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
   /*                  INPUT  body1.UT, */
   /*                  INPUT  body2.UT, */
   /*                  INPUT  Filpath,  */
   /*                  INPUT  avsandare */
   /*                  ).               */
   RETURN.
END.
