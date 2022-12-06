 /*LOSENKOLLWEB.P
 
  Y:\Fasta\GDPR\inlogg.xlsx 
 EJIPCHK innebär att det är bara användaren pkod som stoppas och kollas
 
*/


{AppSpringSetInfo.I} 
&Scoped-define NEW NEW 
DEFINE TEMP-TABLE kollwebuser NO-UNDO
   FIELD APPID AS CHARACTER
   FIELD IDUSER AS CHARACTER
   FIELD SOKCHAR2 AS CHARACTER
   INDEX APPID IS PRIMARY APPID.

DEFINE VARIABLE antaldatoruser AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE xsektemp
  FIELD AV-LEVEL AS INTEGER
  FIELD MENYVART AS CHARACTER
  FIELD SEK AS LOGICAL EXTENT 20
  INDEX XSEK IS PRIMARY MENYVART AV-LEVEL.
{VARFORETYP.I}
DEFINE VARIABLE vMessage        AS CHARACTER NO-UNDO. /*meddelande i klartext hur sändninge gick*/


 
DEFINE VARIABLE MacAddDb AS CHARACTER NO-UNDO.
DEFINE VARIABLE ComputerName AS CHARACTER NO-UNDO.
RUN PROVAG.P.
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.

 IF Guru.Konstanter:globforetag = "xxxx" THEN DO:
 END.
 ELSE DO:
    Guru.Konstanter:SaltData(). 
 END. 
 
RUN STYREAPP.P (INPUT Guru.Konstanter:globforetag, INPUT-OUTPUT varforetypval, INPUT-OUTPUT varforetypchar, INPUT TRUE).
{STYREAPPLADD.I}   
DEFINE VARIABLE restuserh AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE felmeddtemp 
   FIELD FELMEDD AS CHARACTER
   FIELD VAL     AS INTEGER.
FUNCTION felmedd RETURNS CHARACTER(INPUT felnrmed AS INTEGER) :   
   IF felnrmed = 1 THEN RETURN "Lösenordet kan inte vara blankt!".
   IF felnrmed = 2 THEN RETURN "Lösenordet får inte vara samma som användarnamn/Sign. Byt lösenord!".
   IF felnrmed = 3 THEN RETURN "Lösenordet måste vara minst " + STRING(Guru.Konstanter:varforetypval[56]) +  " tecken. Byt lösenord!".
   IF felnrmed = 4 THEN RETURN "Nytt lösenord får inte vara samma som gammalt lösenord! Byt lösenord!".
   IF felnrmed = 5 THEN DO:
      IF Guru.Konstanter:globforetag = "VAST" THEN DO:
         RETURN "För många försök. Du är nu spärrad! Kontakta din Guruadministratör eller Skicka mail till guru.support@vattenfall.com".
      END.
      ELSE  RETURN "För många försök. Du är nu spärrad! Kontakta din Guruadministratör eller Ring Elpool 090/184540".
      /*Anders Olsson Elpool i Umeå AB  7 okt 2022 10:40:23 
      går detta att företagsstyra? guru.support@vattenfall.com 
      Internservice@msva.se
      mittsverige
      */  
   END.
   IF felnrmed = 6 THEN RETURN "Felaktig inloggning!". /* antal försök kvar*/ 
   IF felnrmed = 7 THEN RETURN {LOSENKOLLFEL7.I}.  
   IF felnrmed = 8 THEN RETURN "MacAdress fel! Kontakta din Guruadministratör eller Ring Elpool 090/184540 ".
   IF felnrmed = 9 THEN RETURN {LOSENKOLLFEL12.I}.
   IF felnrmed = 10 THEN RETURN {LOSENKOLLFEL13.I}.
   IF felnrmed = 11 THEN RETURN {LOSENKOLLFEL14.I}.
   IF felnrmed = 12 THEN RETURN {LOSENKOLLFEL15.I}.
   IF felnrmed = 13 THEN RETURN "Du försöker ansluta Guru från ett icke giltigt nätverk!".
   IF felnrmed = 14 THEN RETURN "Din användare är inte giltig!".
END FUNCTION.

PROCEDURE AppSpringSet_UI :
   DEFINE INPUT  PARAMETER AppSpringSetStart AS CHARACTER {AppServerInfoExtent.i} NO-UNDO.
   Guru.Konstanter:AppSpringSet = AppSpringSetStart. 
END PROCEDURE.

/*laddar klientes macadress*/
PROCEDURE MacAddStart_UI :
   DEFINE INPUT  PARAMETER InMac AS CHARACTER NO-UNDO.
   MacAddDb = InMac.
END PROCEDURE.
/*laddar klientdatorns namn*/ 
PROCEDURE ComputerNameStart_UI :
   DEFINE INPUT  PARAMETER datorname AS CHARACTER NO-UNDO.
   ComputerName = datorname.
END PROCEDURE.
/*sätter undantag från spärrar eller tvärt om beroende på företag om appserver*/
PROCEDURE VitlistningStart_UI :
   DEFINE OUTPUT PARAMETER alltok AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE helpkoll AS CHARACTER NO-UNDO.
   IF Guru.Konstanter:AppSpringSet[1] = "xxxBODEN" THEN DO:
      /*VITLISTA
      OUTPUT TO inip.txt APPEND. 
      PUT UNFORMATTED "ip  " Guru.Konstanter:AppSpringSet[3].
      OUTPUT CLOSE.
      */
      helpkoll = SUBSTRING(Guru.Konstanter:AppSpringSet[3],1,11).
      IF helpkoll = CHR(49) + CHR(57) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(56) + CHR(46) + CHR(54) + CHR(56) + CHR(46) OR     
      helpkoll = CHR(49) + CHR(57) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(56) + CHR(46) + CHR(54) + CHR(48) + CHR(46) OR
      helpkoll = CHR(56) + CHR(49) + CHR(46) + CHR(50) + CHR(50) + CHR(54) + CHR(46) + CHR(50) + CHR(50) + CHR(57) + CHR(46) + CHR(49) + CHR(48) + CHR(57)  
      
      THEN DO:
         helpkoll = SUBSTRING(Guru.Konstanter:AppSpringSet[3],12,3).
         IF INTEGER(helpkoll) >=1 AND INTEGER(helpkoll) <= 254 THEN DO.  
            alltok = TRUE.
            RETURN.
         END.   
      END.   
      alltok = FALSE.
      meddelandevar = felmedd(13).
      RETURN. 
   END.
   ELSE DO:
      alltok = TRUE.
      RETURN.
   END.       
END PROCEDURE.

/*skickar ett nytt lösen via mail LÄGGER ÄVEN UT TEXT I LOGGFILEN returmail.txt*/
PROCEDURE ReturMail_UI :
   /*
   RUN ReturMail_UI IN losenwebh (INPUT app_server_info[5],INPUT app_server_info[6], OUTPUT mailok).
   */
   DEFINE INPUT  PARAMETER datoruser AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltok AS LOGICAL NO-UNDO.
   DEFINE VARIABLE  returmail AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nyLosen AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   kommando = Guru.Konstanter:guruvar + "returmail.txt". 
   IF Ganv = datoruser THEN.
   ELSE RETURN.
   FIND FIRST ANVANDARE  WHERE ANVANDARE.ANVANDARE = Ganv NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:
      IF ANVANDARE.AV-LEVEL = 0 THEN.
      ELSE DO:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD =  ANVANDARE.PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALTAB THEN DO:
            returmail = TRIM(SUBSTRING(PERSONALTAB.PERSONSOK,20)).
            IF INDEX(returmail,"@") = 0 THEN DO:
               meddelandevar = felmedd(12).
               RETURN.
            END.   
            nyLosen = STRING(CHR(66) + CHR(89) + CHR(84) + CHR(65) + CHR(46) + CHR(78) + CHR(85)) + STRING(TIME).
            RUN BytLosen_UI (INPUT Ganv, INPUT nyLosen).
            RUN ReturSmtp_UI (INPUT returmail,INPUT nyLosen,OUTPUT alltok).
            IF alltok = TRUE THEN meddelandevar = felmedd(11).
            ELSE meddelandevar = felmedd(10).
            
            
            
           /*se  AppSpringSetInfo.I */             
            
            OUTPUT TO VALUE(kommando) APPEND.
            PUT UNFORMATTED TODAY " "  Guru.Konstanter:AppSpringSet[1] " " Guru.Konstanter:AppSpringSet[2] " " Guru.Konstanter:AppSpringSet[3] " " datoruser " " Ganv " " meddelandevar  " " vMessage SKIP.
            OUTPUT CLOSE. 
            RETURN.
         END.  
      END.
   END.
   
   
END PROCEDURE.

PROCEDURE ReSetAntalIpUserCheck_UI :
   DEFINE INPUT PARAMETER Computer_LanIP AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER datoruser AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER GuruAnvandare AS CHARACTER NO-UNDO.
   DO TRANSACTION:
     
      FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "Ip" AND WEBUSERDATE.IDUSER = Computer_LanIP AND WEBUSERDATE.SOKCHAR[2] = datoruser 
      EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE WEBUSERDATE THEN DELETE WEBUSERDATE.
      /*Anders Olsson Elpool i Umeå AB  6 sep 2022 16:29:47 
      WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.IDUSER = GuruAnvandare
      WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.IDUSER = datoruser
      MÅSTE TAS BORT FÖR DE SOM HAR DIREKTINLOGGNING UTAN KOLL PÅ MAC dvs GKAL OCH ATS
      */
      FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.IDUSER = GuruAnvandare EXCLUSIVE-LOCK NO-ERROR.   
      IF AVAILABLE WEBUSERDATE THEN DELETE WEBUSERDATE.
      
      FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.IDUSER = datoruser EXCLUSIVE-LOCK NO-ERROR.   
      
      IF AVAILABLE WEBUSERDATE THEN DELETE WEBUSERDATE.
      FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "DatorAnv" AND WEBUSERDATE.IDUSER = datoruser EXCLUSIVE-LOCK NO-ERROR.   
      IF AVAILABLE WEBUSERDATE THEN DELETE WEBUSERDATE.
      FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "MacAdd" AND WEBUSERDATE.IDUSER = MacAddDb EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE WEBUSERDATE THEN DELETE WEBUSERDATE.
   END.
   FOR EACH WEBUSERDATE WHERE WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.FORETAG = datoruser AND WEBUSERDATE.SOKCHAR[2] = datoruser  AND 
   WEBUSERDATE.SOKCHAR[3] = Computer_LanIP  EXCLUSIVE-LOCK:
      DELETE WEBUSERDATE.
   END.
   
   RELEASE WEBUSERDATE NO-ERROR.

END PROCEDURE.  
/*antal försök räknas upp räknar upp*/
PROCEDURE AntalIpUserCheck_UI :
   /*POSTERNA RÄKNAS UPP*/
   DEFINE INPUT PARAMETER Computer_LanIP AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER datoruser AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER GuruAnvandare AS CHARACTER NO-UNDO.
   antaldatoruser = datoruser.
   
   DO TRANSACTION:
      {EJIPCHK.I} {EJIPCHKSERVER.I}
      /*Anders Olsson Elpool i Umeå AB  12 okt 2022 09:24:38 
      VITLISTA AV FLEX OCH DEPÅ SYSTEM 
       EJIPCHK = CHR(69) + CHR(74) + CHR(73) + CHR(80) + CHR(67) + CHR(72) + CHR(75)
      */
      IF Computer_LanIP NE CHR(69) + CHR(74) + CHR(73) + CHR(80) + CHR(67) + CHR(72) + CHR(75) THEN DO:
         FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "Ip" AND WEBUSERDATE.IDUSER = Computer_LanIP AND WEBUSERDATE.SOKCHAR[2] = datoruser 
         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE WEBUSERDATE THEN CREATE WEBUSERDATE. 
         ASSIGN 
         WEBUSERDATE.APPID = "Ip"  
         WEBUSERDATE.IDUSER = Computer_LanIP
         WEBUSERDATE.FORETAG = datoruser
         WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 1)
         WEBUSERDATE.SOKCHAR[2] = datoruser
         WEBUSERDATE.SOKCHAR[3] = Computer_LanIP
         WEBUSERDATE.SOKCHAR[5] = GuruAnvandare
         WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME))
         WEBUSERDATE.SOKCHAR[6] = ComputerName.
         {EJMACCHK.I}
         /*Anders Olsson Elpool i Umeå AB  12 okt 2022 09:28:24 
         VITLISTA AV VISSA MACADD 
         */
         IF MacAddDb = "" OR MacAddDb = CHR(69) + CHR(74) + CHR(77) + CHR(65) + CHR(67) + CHR(67) + CHR(72) + CHR(75) THEN.
         ELSE DO:
            FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "MacAdd" AND WEBUSERDATE.IDUSER = MacAddDb EXCLUSIVE-LOCK NO-ERROR.   
            IF NOT AVAILABLE WEBUSERDATE THEN CREATE WEBUSERDATE.
            ASSIGN 
            WEBUSERDATE.APPID = "MacAdd"  
            WEBUSERDATE.IDUSER = MacAddDb
            WEBUSERDATE.FORETAG = datoruser
            WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 1)
            WEBUSERDATE.SOKCHAR[2] = datoruser
            WEBUSERDATE.SOKCHAR[3] = MacAddDb
            WEBUSERDATE.SOKCHAR[5] = GuruAnvandare
            WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME))
            WEBUSERDATE.SOKCHAR[6] = ComputerName.
         END.
         FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "DatorAnv" AND WEBUSERDATE.IDUSER = datoruser EXCLUSIVE-LOCK NO-ERROR.   
         IF NOT AVAILABLE WEBUSERDATE THEN CREATE WEBUSERDATE.
         ASSIGN 
         WEBUSERDATE.APPID = "DatorAnv"  
         WEBUSERDATE.IDUSER = datoruser
         WEBUSERDATE.FORETAG = datoruser
         WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 1)
         WEBUSERDATE.SOKCHAR[2] = datoruser
         WEBUSERDATE.SOKCHAR[3] = Computer_LanIP
         WEBUSERDATE.SOKCHAR[5] = GuruAnvandare
         WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME))
         WEBUSERDATE.SOKCHAR[6] = ComputerName.
         
         IF GuruAnvandare NE "" THEN DO:
            FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.IDUSER = GuruAnvandare EXCLUSIVE-LOCK NO-ERROR.   
            
            IF NOT AVAILABLE WEBUSERDATE THEN CREATE WEBUSERDATE.
            ASSIGN 
            WEBUSERDATE.APPID = "GuruAnv"  
            WEBUSERDATE.IDUSER = GuruAnvandare
            WEBUSERDATE.FORETAG = datoruser
            WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 1)
            WEBUSERDATE.SOKCHAR[2] = datoruser
            WEBUSERDATE.SOKCHAR[3] = Computer_LanIP
            WEBUSERDATE.SOKCHAR[5] = GuruAnvandare
            WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME))
            WEBUSERDATE.SOKCHAR[6] = ComputerName.
         END.
        /*Anders Olsson Elpool i Umeå AB  12 okt 2022 09:29:14 
        KORS SPÄRR AV USER
         GuruAnv  GuruAnvandare
         DatorAnv datoruser
         
          GuruAnv  datoruser
         DatorAnv GuruAnvandare
         */
      END.
      ELSE DO:
                                                                                       /*FLEX OCH DEPÅ PKOD*/ 
         FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.IDUSER = GuruAnvandare EXCLUSIVE-LOCK NO-ERROR.   
         IF NOT AVAILABLE WEBUSERDATE THEN CREATE WEBUSERDATE.
         ASSIGN 
         WEBUSERDATE.APPID = "GuruAnv"  
         WEBUSERDATE.IDUSER = GuruAnvandare
         WEBUSERDATE.FORETAG = datoruser
         WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 1)
         WEBUSERDATE.SOKCHAR[2] = datoruser
         WEBUSERDATE.SOKCHAR[3] = Computer_LanIP
         WEBUSERDATE.SOKCHAR[5] = GuruAnvandare
         WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME))
         WEBUSERDATE.SOKCHAR[6] = ComputerName.
      END.        
   END.
   RELEASE WEBUSERDATE NO-ERROR.

END PROCEDURE.  
PROCEDURE senast_UI :
   DEFINE INPUT  PARAMETER ftxt AS CHARACTER NO-UNDO.
   IF AVAILABLE WEBUSERDATE THEN DO TRANSACTION:
      FIND CURRENT WEBUSERDATE EXCLUSIVE-LOCK NO-ERROR.
      WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME)).
      WEBUSERDATE.SOKCHAR[5] = WEBUSERDATE.SOKCHAR[5] + " " + ftxt. 
   END.
   ELSE RETURN.
   FIND CURRENT WEBUSERDATE NO-LOCK NO-ERROR.   
END PROCEDURE. 
/*koll så att max inte överskrids för ip mac datoranv guruanv*/ 
PROCEDURE MaxIpUserCheck_UI :
   DEFINE INPUT PARAMETER Computer_LanIP AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER datoruser AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER InuserGuruDator AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltOk AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "Ip" AND WEBUSERDATE.IDUSER = Computer_LanIP  AND WEBUSERDATE.SOKCHAR[2] = datoruser 
   NO-LOCK NO-ERROR.
   IF AVAILABLE WEBUSERDATE THEN DO:
      RUN SokAntalKvar_UI (INPUT WEBUSERDATE.APPID, INPUT WEBUSERDATE.IDUSER, INPUT WEBUSERDATE.SOKCHAR[2]).
      IF INTEGER(WEBUSERDATE.SOKCHAR[1]) >= Guru.Konstanter:varforetypval[52] THEN DO:
         meddelandevar = felmedd(5).
         alltOk = FALSE.
         RUN senast_UI ("IP").   
         DO TRANSACTION:
            CREATE WEBUSERDATE.
            ASSIGN 
            WEBUSERDATE.APPID = "ELPK"  
            WEBUSERDATE.IDUSER = datoruser
            WEBUSERDATE.FORETAG = InuserGuruDator
            WEBUSERDATE.SOKCHAR[1] = STRING(0).
            WEBUSERDATE.SOKCHAR[2] = MacAddDb.
            WEBUSERDATE.SOKCHAR[3] = Computer_LanIP.
            WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME)).
            WEBUSERDATE.SOKCHAR[5] = "Elpool Granskar IP".
            WEBUSERDATE.SOKCHAR[6] = ComputerName.
         END.
         RELEASE WEBUSERDATE NO-ERROR. 
         RETURN.
      END.   
   END.
   
   FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.IDUSER = InuserGuruDator NO-LOCK NO-ERROR.
   IF AVAILABLE WEBUSERDATE THEN DO:
      RUN SokAntalKvar_UI (INPUT WEBUSERDATE.APPID, INPUT WEBUSERDATE.IDUSER, INPUT WEBUSERDATE.SOKCHAR[2]).
      IF INTEGER(WEBUSERDATE.SOKCHAR[1]) >= Guru.Konstanter:varforetypval[52] THEN DO:
         meddelandevar = felmedd(5).
         alltOk = FALSE.   
         RUN senast_UI ("GA").
         /*spärrar datauser om guruanvändare utnyttjas*/
         DO TRANSACTION:
            FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "DatorAnv" AND WEBUSERDATE.IDUSER = InuserGuruDator EXCLUSIVE-LOCK NO-ERROR.   
            IF NOT AVAILABLE WEBUSERDATE THEN CREATE WEBUSERDATE.
            
            
            ASSIGN 
            WEBUSERDATE.APPID = "DatorAnv"  
            WEBUSERDATE.IDUSER = InuserGuruDator
            WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 99).
            WEBUSERDATE.SOKCHAR[2] = datoruser.
            WEBUSERDATE.SOKCHAR[3] = Computer_LanIP.
            WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME)).
            WEBUSERDATE.SOKCHAR[5] = "Datoruser via Guruser 1".
            WEBUSERDATE.SOKCHAR[6] = ComputerName.
            FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "DatorAnv" AND WEBUSERDATE.IDUSER = datoruser EXCLUSIVE-LOCK NO-ERROR.   
            IF NOT AVAILABLE WEBUSERDATE THEN CREATE WEBUSERDATE.
            
            ASSIGN 
            WEBUSERDATE.APPID = "DatorAnv"  
            WEBUSERDATE.IDUSER = datoruser
            WEBUSERDATE.FORETAG = datoruser
            WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 99).
            WEBUSERDATE.SOKCHAR[2] = datoruser.
            WEBUSERDATE.SOKCHAR[3] = Computer_LanIP.
            WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME)).
            WEBUSERDATE.SOKCHAR[5] = "Datoruser via Guruser 2".
            WEBUSERDATE.SOKCHAR[6] = ComputerName.
         END.
         RELEASE WEBUSERDATE NO-ERROR. 
         RETURN.
      END. 
       
   END.
   IF MacAddDb = "" THEN.
   ELSE DO:
      FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "MacAdd" AND WEBUSERDATE.IDUSER = MacAddDb NO-LOCK NO-ERROR.
      IF AVAILABLE WEBUSERDATE THEN DO:
         RUN SokAntalKvar_UI (INPUT WEBUSERDATE.APPID, INPUT WEBUSERDATE.IDUSER, INPUT WEBUSERDATE.SOKCHAR[2]).
         IF INTEGER(WEBUSERDATE.SOKCHAR[1]) >= Guru.Konstanter:varforetypval[52] THEN DO:
            meddelandevar = felmedd(5).
            alltOk = FALSE.   
            RUN senast_UI ("MA").
            DO TRANSACTION:
               CREATE WEBUSERDATE.
               ASSIGN 
               WEBUSERDATE.APPID = "ELPK"  
               WEBUSERDATE.IDUSER = datoruser
               WEBUSERDATE.FORETAG = InuserGuruDator
               WEBUSERDATE.SOKCHAR[1] = STRING(0).
               WEBUSERDATE.SOKCHAR[2] = MacAddDb.
               WEBUSERDATE.SOKCHAR[3] = Computer_LanIP.
               WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME)).
               WEBUSERDATE.SOKCHAR[5] = "Elpool Granskar MAC".
               WEBUSERDATE.SOKCHAR[6] = ComputerName.
            END.
            RELEASE WEBUSERDATE NO-ERROR. 
            RETURN.
         END.   
      END.
   END.
   
   FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "DatorAnv" AND WEBUSERDATE.IDUSER = datoruser NO-LOCK NO-ERROR.
   IF AVAILABLE WEBUSERDATE THEN DO:
      RUN SokAntalKvar_UI (INPUT WEBUSERDATE.APPID, INPUT WEBUSERDATE.IDUSER, INPUT WEBUSERDATE.SOKCHAR[2]).
      IF INTEGER(WEBUSERDATE.SOKCHAR[1]) >= Guru.Konstanter:varforetypval[52] THEN DO:
         meddelandevar = felmedd(5).
         alltOk = FALSE. 
         RUN senast_UI ("DA").
         /*spärrar befintlig guru användare också!*/   
         FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = datoruser  NO-LOCK NO-ERROR.
         IF AVAILABLE ANVANDARE THEN DO TRANSACTION:
            FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = "GuruAnv" AND WEBUSERDATE.IDUSER = datoruser EXCLUSIVE-LOCK NO-ERROR.   
            IF NOT AVAILABLE WEBUSERDATE THEN CREATE WEBUSERDATE.
            ASSIGN 
            WEBUSERDATE.APPID = "GuruAnv"  
            WEBUSERDATE.IDUSER = datoruser
            WEBUSERDATE.FORETAG = datoruser
            WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 99).
            WEBUSERDATE.SOKCHAR[2] = datoruser.
            WEBUSERDATE.SOKCHAR[3] = Computer_LanIP.
            WEBUSERDATE.SOKCHAR[4] = STRING(DATETIME (TODAY,MTIME)).
            WEBUSERDATE.SOKCHAR[5] = "GuruAnv via datoruser".
            WEBUSERDATE.SOKCHAR[6] = ComputerName.
         END.
         RELEASE WEBUSERDATE NO-ERROR.    
         RETURN.
      END.   
   END.
   alltOk = TRUE.
    
END PROCEDURE.  

PROCEDURE NylosenReglerKoll_UI :
   DEFINE INPUT PARAMETER GuruAnvandare AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER gaLosen AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nypkod AS CHARACTER NO-UNDO.  
   DEFINE OUTPUT PARAMETER felNr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltOk AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   /*Anders Olsson Elpool i Umeå AB  7 feb 2019 15:23:54 
   får endast köras från ANDANV.W 
   */
   IF gaLosen = GuruAnvandare OR gaLosen = nypkod THEN DO:
      felNr = 2.
      meddelandevar = felmedd(felNr).
      IF meddelandevar = "" THEN. 
      ELSE DO: 
         alltOk = FALSE.      
         RETURN.
      END.     
   END.
   RUN losenReglerKoll_UI (INPUT GuruAnvandare,INPUT-OUTPUT gaLosen,INPUT "", OUTPUT felNr, OUTPUT alltOk,OUTPUT meddelandevar).
   IF meddelandevar = {LOSENKOLLFEL12.I} THEN  alltOk = TRUE. 
END PROCEDURE. 
/*kollar att ditt nya lösen sätts rätt gäller även ny upplägg av users */
PROCEDURE losenReglerKoll_UI :
   DEFINE INPUT PARAMETER GuruAnvandare AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT  PARAMETER gaLosen AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyLosen AS CHARACTER NO-UNDO.  
   DEFINE OUTPUT PARAMETER felNr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltOk AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   
   alltOk = FALSE.    
   /*kollar att ditt befintliga lösen är ok*/ 
   IF nyLosen = "" THEN DO:
      RUN ReglerKoll_UI (INPUT GuruAnvandare,INPUT gaLosen,OUTPUT meddelandevar, OUTPUT felNr).
      IF meddelandevar = "" THEN alltOk = TRUE.
      ELSE alltOk = FALSE.      
      RETURN.  
   END.   
   /*kollar att ditt nya lösen är ok*/
   IF nyLosen NE "" THEN DO:
      IF gaLosen = nyLosen THEN DO:
         felNr = 4.
         meddelandevar = felmedd(felNr).
         RETURN.
      END.
      
      IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
         IF Guru.Konstanter:SaltRetur(gaLosen) = Guru.Konstanter:SaltRetur(nyLosen) THEN DO:
             felNr = 4.
             meddelandevar = felmedd(felNr).
             RETURN.
         END.
      END.
        
      RUN ReglerKoll_UI (INPUT GuruAnvandare,INPUT nyLosen,OUTPUT meddelandevar, OUTPUT felNr).
      IF meddelandevar = "" THEN DO:
         alltOk = TRUE.
         RUN BytLosen_UI (INPUT GuruAnvandare, INPUT nyLosen).
         gaLosen = nyLosen.
      END.
      ELSE alltOk = FALSE.      
      RETURN.  
      
   END.
END PROCEDURE.
/*kollar att ditt nya lösen sätts rätt gäller även ny upplägg av users */
PROCEDURE ReglerKoll_UI :
   DEFINE INPUT PARAMETER GuruAnvandare AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER passwd AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER felNr AS INTEGER NO-UNDO.
   DEFINE VARIABLE anvPkod AS CHARACTER NO-UNDO.
   DEFINE VARIABLE PwdKryptonit AS CHARACTER NO-UNDO.
   RUN finnsKoppladPkod_UI (INPUT GuruAnvandare, OUTPUT anvPkod).
 
   IF passwd = "" THEN DO:
      felNr = 1.
      meddelandevar = felmedd(felNr). 
      RETURN.      
   END.
   IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
      /* passwd ÄR I DETTA LÄGE KRYPTERAT VIA losenReglerKoll_UI*/ 
      IF passwd = Guru.Konstanter:SaltRetur("")  THEN DO:
         felNr = 1.
         meddelandevar = felmedd(felNr).
         RETURN.       
      END.
   END.
      
   IF passwd = GuruAnvandare OR passwd = anvPkod THEN DO:
      felNr = 2.
      meddelandevar = felmedd(felNr).
      RETURN.
   END.
    
   IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
       /* passwd ÄR I DETTA LÄGE KRYPTERAT VIA losenReglerKoll_UI*/ 
      IF passwd = Guru.Konstanter:SaltRetur(GuruAnvandare) OR passwd = Guru.Konstanter:SaltRetur(anvPkod) THEN DO:
         felNr = 2.
         meddelandevar = felmedd(felNr).
         RETURN.
      END.
   END.
      
   IF LENGTH(passwd) < Guru.Konstanter:varforetypval[56] THEN DO:
      felNr = 3.
      meddelandevar = felmedd(felNr).
      RETURN.
   END.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "misv"  THEN.
   ELSE DO:
      IF passwd BEGINS CHR(66) + CHR(89) + CHR(84) + CHR(65) + CHR(46) + CHR(78) + CHR(85) THEN DO:
         felNr = 9.   
         meddelandevar = felmedd(felNr).
         RETURN.
      END.
      IF passwd BEGINS CHR(66) + CHR(89) + CHR(84) + CHR(65)  THEN DO:
         felNr = 9.   
         meddelandevar = felmedd(felNr).
         RETURN.
      END.
   END.
   IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:   
       /* passwd ÄR I DETTA LÄGE KRYPTERAT VIA losenReglerKoll_UI*/  
      PwdKryptonit =  Guru.Konstanter:SaltRetur(CHR(66) + CHR(89) + CHR(84) + CHR(65) + CHR(46) + CHR(78) + CHR(85)).
      IF passwd BEGINS Guru.Konstanter:SaltRetur(CHR(66) + CHR(89) + CHR(84) + CHR(65) + CHR(46) + CHR(78) + CHR(85)) THEN DO:
         felNr = 9.   
         meddelandevar = felmedd(felNr).
         RETURN.
      END.
       PwdKryptonit = Guru.Konstanter:SaltRetur(CHR(66) + CHR(89) + CHR(84) + CHR(65)).
      IF passwd BEGINS Guru.Konstanter:SaltRetur(CHR(66) + CHR(89) + CHR(84) + CHR(65)) THEN DO:
         felNr = 9.   
         meddelandevar = felmedd(felNr).
         RETURN.
      END.
   END.
   
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "misv"  THEN.
   ELSE DO:
      IF passwd = CHR(66) + CHR(89) + CHR(84) + CHR(65) OR passwd = CHR(66) + CHR(89) + CHR(84) + CHR(78) + CHR(85) OR passwd = CHR(66) + CHR(89) + CHR(84) + CHR(65) + CHR(78) + CHR(85) THEN DO:
         felNr = 9.   
         meddelandevar = felmedd(felNr).
         RETURN.
      END.
      IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
         IF passwd = Guru.Konstanter:SaltRetur(CHR(66) + CHR(89) + CHR(84) + CHR(65)) OR passwd = Guru.Konstanter:SaltRetur(CHR(66) + CHR(89) + CHR(84) + CHR(78) + CHR(85)) OR passwd = Guru.Konstanter:SaltRetur(CHR(66) + CHR(89) + CHR(84) + CHR(78) + CHR(85)) THEN DO:
            felNr = 9.   
            meddelandevar = felmedd(felNr).
            RETURN.
         END.
      END.      
   END.
   
END PROCEDURE.

   
PROCEDURE finnsKoppladPkod_UI :
  /* IF typkoll = 99 THEN DO:*/
  DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER Ganvpkod AS CHARACTER NO-UNDO.
  FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv  
  USE-INDEX ANDV NO-LOCK NO-ERROR.
  IF AVAILABLE ANVANDARE THEN DO:
     Ganvpkod = ANVANDARE.PERSONALKOD.
  END.     
  
  
END PROCEDURE. 
 /*    
  är användaren spärrad via datum,antal försök räknas upp,  är lösenordet rätt om rätt så tas antal försök bort
  skriver även till loggfil INFELOG.TXT
  */ 
PROCEDURE RattLosen_UI :
   /*IF typkoll = 1 THEN DO:*/
   DEFINE INPUT PARAMETER Computer_LanIP AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER datoruser AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER gaLosen AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltOk AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE antalkvar AS INTEGER NO-UNDO.
   /*antal försök räknas upp*/
   RUN AntalIpUserCheck_UI (INPUT Computer_LanIP,INPUT datoruser,INPUT Ganv). 
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv  NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO: 
      IF ANVANDARE.SPARRAD = TRUE THEN DO:
         IF ANVANDARE.SPARR-DATUMTID = ? THEN DO:
            alltOk = FALSE.
            meddelandevar = felmedd(14).
            RETURN.
         END.
         IF ANVANDARE.SPARR-DATUMTID <= NOW THEN DO:
            alltOk = FALSE.
            meddelandevar = felmedd(14).
            RETURN.
         END.    
      END.
   END.
   IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
      
      IF Guru.Konstanter:SaltRetur(gaLosen) = ? THEN RELEASE ANVANDARE NO-ERROR.
      ELSE DO:
         FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv AND 
         ANVANDARE.AV-LOSEN = Guru.Konstanter:SaltRetur(gaLosen) USE-INDEX ANDV NO-LOCK NO-ERROR.
      END.   
   END.   
   ELSE DO:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv AND 
      ANVANDARE.AV-LOSEN = gaLosen USE-INDEX ANDV NO-LOCK NO-ERROR.
   END.
   
   IF AVAILABLE ANVANDARE THEN DO:
      /*Anders Olsson Elpool i Umeå AB  7 nov 2022 15:06:46 
      GURUSUPPORT 
      */

      IF ANVANDARE.ANVANDARE = CHR(71) + CHR(85) + CHR(82) + CHR(85) + CHR(83) + CHR(85) + CHR(80) + CHR(80) + CHR(79) + CHR(82) + CHR(84) THEN DO:
         FIND FIRST EXTRADATA WHERE EXTRADATA.PROGRAM = ANVANDARE.ANVANDARE  AND EXTRADATA.HUVUDCH = Guru.Konstanter:AppSpringSet[5] NO-LOCK NO-ERROR.
         IF NOT AVAILABLE EXTRADATA THEN DO:
            alltOk = FALSE.
            meddelandevar = felmedd(14).
            RETURN.
         END.
      END.   
      IF datoruser NE Ganv THEN DO:
         /*lägger in i fellog*/
         RUN inFelLog_UI (INPUT Computer_LanIP, INPUT datoruser, INPUT Ganv).
      END.   
      RUN ReSetAntalIpUserCheck_UI (INPUT Computer_LanIP,INPUT datoruser,INPUT Ganv).
      alltOk = TRUE.
      RETURN. 
   END.
   
   IF NOT AVAILABLE ANVANDARE THEN DO:
      alltOk = FALSE.   
      RUN AntalKvar_UI (OUTPUT antalkvar).
        
      meddelandevar = felmedd(6) + " Antal försök kvar: " + STRING(antalkvar).
      
      RETURN.
   END.
   
END PROCEDURE.
PROCEDURE SokAntalKvar_UI :
   DEFINE INPUT  PARAMETER aid AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER idu AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER sko2 AS CHARACTER NO-UNDO.
   FIND FIRST kollwebuser WHERE kollwebuser.APPID = aid AND kollwebuser.IDUSER = idu AND kollwebuser.SOKCHAR2 = sko2 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kollwebuser THEN DO:
      CREATE kollwebuser.
   END.
   ASSIGN 
   kollwebuser.APPID = aid 
   kollwebuser.IDUSER = idu 
   kollwebuser.SOKCHAR2 = sko2. 
END PROCEDURE.   
PROCEDURE AntalKvar_UI :
   DEFINE OUTPUT PARAMETER antalkvar AS INTEGER NO-UNDO.
   DEBUGGER:SET-BREAK().
   antalkvar = 1.
   FOR EACH kollwebuser WHERE NO-LOCK:
      
      FIND FIRST WEBUSERDATE WHERE WEBUSERDATE.APPID = kollwebuser.APPID AND WEBUSERDATE.IDUSER = kollwebuser.IDUSER  AND WEBUSERDATE.SOKCHAR[2] = kollwebuser.SOKCHAR2 
      NO-LOCK NO-ERROR.
      IF AVAILABLE WEBUSERDATE THEN DO:
         IF INTEGER(WEBUSERDATE.SOKCHAR[1]) > antalkvar THEN antalkvar = INTEGER(WEBUSERDATE.SOKCHAR[1]).
      END. 
      
   END.
   FOR EACH  WEBUSERDATE WHERE WEBUSERDATE.FORETAG = antaldatoruser NO-LOCK:
      IF INTEGER(WEBUSERDATE.SOKCHAR[1]) > antalkvar THEN antalkvar = INTEGER(WEBUSERDATE.SOKCHAR[1]).
   END.
   antalkvar = Guru.Konstanter:varforetypval[52] - antalkvar.
END PROCEDURE.
PROCEDURE inFelLog_UI :
   DEFINE INPUT PARAMETER Computer_LanIP AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER datoruser AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   {AMERICANEUROPEAN.I}
   
   kommando = Guru.Konstanter:guruvar + "INFELOG.TXT".
   
   OUTPUT TO VALUE(kommando) APPEND.
   PUT UNFORMATTED Computer_LanIP " ip " datoruser " NT-LOG " Ganv " GURU-LOGG " Guru.Konstanter:globforetag " FÖRETAG " STRING(DATETIME (TODAY,MTIME)) SKIP.
   
   OUTPUT CLOSE.
  {EUROPEANAMERICAN.I}    
   
END PROCEDURE.  

/*automatisk inloggnong via   Guru.Konstanter:AppSpringSet[6]  = GuruAnvandare
          reset av antal försök
           Guru.Konstanter:AppSpringSet[3]  = Computer_LanIP
           Guru.Konstanter:AppSpringSet[5]  = datoruser
           Guru.Konstanter:AppSpringSet[6]  = GuruAnvandare
   */
PROCEDURE AutoSpringLogin_UI :
   /*AUTOLOGIN */ 
  
   DEFINE INPUT PARAMETER Computer_LanIP AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER datoruser AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltOk AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   /*Anders Olsson Elpool i Umeå AB  12 okt 2022 09:53:30 
   STOPPAR ELPAO = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) 
   och GURUSUPPORT
   */
   
   IF datoruser = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) OR datoruser = CHR(71) + CHR(85) + CHR(82) + CHR(85) + CHR(83) + CHR(85) + CHR(80) + CHR(80) + CHR(79) + CHR(82) + CHR(84) THEN  DO:
      alltOk = FALSE.
      meddelandevar = {LOSENKOLLFEL8.I}.   
      RETURN.
   END.
   
   
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv 
   USE-INDEX ANDV NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:
      IF ANVANDARE.SPARRAD = TRUE THEN DO:
         IF ANVANDARE.SPARR-DATUMTID <= NOW THEN DO:
            alltOk = FALSE.
            meddelandevar = felmedd(14).
            RETURN.
         END.    
      END.
      RUN ReSetAntalIpUserCheck_UI (INPUT Computer_LanIP,INPUT datoruser,INPUT Ganv).
      alltOk = TRUE. 
   END.
   ELSE DO:
       alltOk = FALSE.
       meddelandevar = {LOSENKOLLFEL8.I}.
       RUN inFelLog_UI (INPUT Computer_LanIP, INPUT datoruser, INPUT Ganv).
   END.    
   
END PROCEDURE.
/*körs för alla så att globvar sätts*/
PROCEDURE AutoLogin_UI :
   /*AUTOLOGIN/ 
      IF typkoll = 2 THEN DO:
         från REGLERFORLOSEN.P
   */
   
   DEFINE INPUT PARAMETER Computer_LanIP AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER datoruser AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER gaLosen AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER globallao AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER globallpers AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER globavd AS INTEGER NO-UNDO. 
   DEFINE OUTPUT PARAMETER globomr AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER globstorb AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER globstorh AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER globniv AS INTEGER NO-UNDO. 
   DEFINE OUTPUT PARAMETER globpersnamn AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER globanvpkod AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER globanvavdnr AS INTEGER NO-UNDO. 
   DEFINE OUTPUT PARAMETER globjid AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltOk AS LOGICAL NO-UNDO.
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv 
   USE-INDEX ANDV NO-LOCK NO-ERROR.
   
   
   
   IF AVAILABLE ANVANDARE THEN DO:
      RUN ReSetAntalIpUserCheck_UI (INPUT Computer_LanIP,INPUT datoruser,INPUT Ganv).
      ASSIGN 
      globanvavdnr = ANVANDARE.AVDELNINGNR
      globanvpkod = ANVANDARE.PERSONALKOD
      gaLosen = ANVANDARE.AV-LOSEN
      globstorb = ANVANDARE.SIDL      /*bredd*/
      globstorh = ANVANDARE.SIDS     /*höjd*/
      globniv = ANVANDARE.AV-LEVEL
      Guru.Konstanter:globniv = ANVANDARE.AV-LEVEL
      globallpers = ANVANDARE.ALLPERS
      globallao = ANVANDARE.ALLAONR
      globpersnamn = ANVANDARE.AV-NAMN   .     
      IF ANVANDARE.PERSONALKOD NE "" THEN DO:
          FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD
          USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
          IF AVAILABLE PERSONALTAB THEN DO:
             globomr = PERSONALTAB.OMRADE.
             FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE =  PERSONALTAB.OMRADE NO-LOCK NO-ERROR.
             IF AVAILABLE OMRADETAB THEN DO:
                FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR  = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
                IF AVAILABLE AVDELNING THEN DO:
                   globavd = AVDELNING.AVDELNINGNR.
                   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
                   IF AVAILABLE JURPERS THEN DO:
                      globjid = JURPERS.JUDID.               
                   END.   
                END.   
            END.
         END.
      END.
      ELSE DO:
         FIND FIRST OMRADETAB WHERE NO-LOCK NO-ERROR.
         IF AVAILABLE OMRADETAB THEN globomr = OMRADETAB.OMRADE.
      END.   
      IF Ganv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN globjid = "".
      alltOk = TRUE. 
   END.
   ELSE alltOk = FALSE.
END PROCEDURE.
/*Anders Olsson Elpool i Umeå AB  13 sep 2019 14:56:16 
   Mac sätts bara om datoruser = GuruAnvandare
   */
PROCEDURE MacSett_UI :
   DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER MacAdd AS CHARACTER NO-UNDO.
   IF MacAdd = "" THEN RETURN.
   IF LENGTH(MacAdd) < 6 THEN RETURN.
   FIND FIRST EXTRADATA  WHERE EXTRADATA.PROGRAM = "MACADD" AND EXTRADATA.HUVUDCH = Ganv AND  EXTRADATA.SOKCHAR[1] = MacAdd 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE EXTRADATA THEN DO TRANSACTION:
      CREATE EXTRADATA.
      ASSIGN
      EXTRADATA.PROGRAM = "MACADD"
      EXTRADATA.HUVUDCH = Ganv
      EXTRADATA.HUVUDINT =  ?
      EXTRADATA.SOKCHAR[1] = MacAdd 
      EXTRADATA.SOKDATE[1] = TODAY.   
   END.
   ELSE DO TRANSACTION:
     FIND CURRENT EXTRADATA EXCLUSIVE-LOCK NO-ERROR. 
     EXTRADATA.SOKDATE[1] = TODAY.
   END.  
   RELEASE EXTRADATA NO-ERROR.
END PROCEDURE.
/*obs! meddelandevar = "QUIT". obs! om MAN tillfälligt måste stoppa användare i guru eller något annat, men INTE alla  */
PROCEDURE StoppVillkor_UI:
   DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltOk AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   /*
   IF Guru.Konstanter:AppSpringSet[1] = "ONENO" OR  Guru.Konstanter:AppSpringSet[1] = "ONENOUTBI"  THEN DO:
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv  NO-LOCK NO-ERROR.
      IF AVAILABLE ANVANDARE THEN DO:
         IF  ANVANDARE.AV-LEVEL = 13 OR ANVANDARE.AV-LEVEL = 0 THEN.
         ELSE DO:
            meddelandevar = "QUIT".
            alltOk = FALSE.
            RETURN.
         END.   
      END.
           
   END.
   */
   alltOk = TRUE.
   RETURN.
END PROCEDURE.
/* tar bort blanka macadresser kollar datoranvändare och macadd finns att lösen är enligt reglerna*/
PROCEDURE MacKontroll_UI :
   DEFINE INPUT PARAMETER Ganv AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER MacAdd AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER alltOk AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER meddelandevar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE felNr AS INTEGER NO-UNDO.   
   DEFINE VARIABLE hjlosen AS CHARACTER NO-UNDO.
   FOR EACH  EXTRADATA  WHERE EXTRADATA.PROGRAM = "MACADD" AND EXTRADATA.HUVUDCH = Ganv AND  EXTRADATA.SOKCHAR[1] = "" EXCLUSIVE-LOCK:
      DELETE EXTRADATA.
   END.
   FOR EACH  EXTRADATA  WHERE EXTRADATA.PROGRAM = "MACADD" AND EXTRADATA.HUVUDCH = Ganv AND  LENGTH(EXTRADATA.SOKCHAR[1]) < 6 EXCLUSIVE-LOCK:
      DELETE EXTRADATA.
   END.
   /*får logga in med blank macadd*/
   FIND FIRST EXTRADATA  WHERE EXTRADATA.PROGRAM = "MACADD" AND EXTRADATA.HUVUDCH = Ganv AND  EXTRADATA.SOKCHAR[1] = MacAdd 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE EXTRADATA THEN DO:
       /*får logga in med blank macadd*/
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ATS" THEN.
      ELSE DO:
         meddelandevar = felmedd(7).
         alltOk = FALSE.
         RETURN.
      END.   
   END.
  
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Ganv  NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:
      hjlosen = ANVANDARE.AV-LOSEN.
      IF Guru.Konstanter:globforetag = "lule" THEN.
      ELSE DO:
         /* lösen ska vara enligt reglerna*/ 
         
         RUN losenReglerKoll_UI (INPUT Ganv,INPUT-OUTPUT hjlosen ,INPUT "", OUTPUT felNr, OUTPUT alltOk,OUTPUT meddelandevar).  
         IF alltOk = FALSE THEN DO: 
            meddelandevar = felmedd(9).
            RETURN.
         END.   
      END.
   END.   
   alltOk = TRUE.
   RETURN.
END PROCEDURE.
/*gör bytet av lösen*/
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
   IF ANVANDARE.ANVANDARE = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN.
   ELSE DO:    
      RUN RESTUSERS.P PERSISTENT SET restuserh. 
      RUN restBytLosenord_UI IN restuserh(INPUT ANVANDARE.PERSONALKOD,INPUT ANVANDARE.AV-LOSEN, OUTPUT TABLE felmeddtemp). 
      RUN avslutarestUsr_UI IN restuserh.
      DELETE PROCEDURE restuserh NO-ERROR.
      restuserh = ?.
   END.
END PROCEDURE.
/*sätterkonstanter på appserver*/
PROCEDURE KonstanterSet :
   DEFINE INPUT PARAMETER kval AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER valueval AS CHARACTER NO-UNDO.
   IF kval = "globforetag" THEN Guru.Konstanter:globforetag = valueval.
   IF kval = "globanv" THEN Guru.Konstanter:globanv = valueval.
   IF kval = "gaok" THEN Guru.Konstanter:gaok = valueval.
   IF kval = "gdelnrk" THEN Guru.Konstanter:gdelnrk = valueval.
   IF kval = "gfore" THEN Guru.Konstanter:GForetag = valueval.
   IF kval = "globsprak" THEN Guru.Konstanter:globsprak = INTEGER(valueval).
   
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
   


   DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE servervar AS CHARACTER NO-UNDO.
   IF Guru.Konstanter:AppSpringSet[1] = "ONENO" OR Guru.Konstanter:AppSpringSet[1] = "ONENOUTBI" THEN DO:
      ASSIGN
      franvar = CHR(110) + CHR(111) + CHR(45) + CHR(114) + CHR(101) + CHR(112) + CHR(108) + CHR(121) + CHR(45) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(111) + CHR(110) + CHR(101) + CHR(45) + CHR(110) + CHR(111) + CHR(114) + CHR(100) + CHR(105) + CHR(99) + CHR(46) + CHR(115) + CHR(101).  
       /*webguru@mittsverigevatten.se*/
      servervar = CHR(115) + CHR(109) + CHR(116) + CHR(112) + CHR(46) + CHR(111) + CHR(110) + CHR(101) + CHR(46) + CHR(108) + CHR(111) + CHR(99) + CHR(97) + CHR(108). 
   END.
   ELSE IF Guru.Konstanter:AppSpringSet[1]  = "MISV"  THEN DO:
      ASSIGN
      franvar = CHR(119) + CHR(101) + CHR(98) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(109) + CHR(105) + CHR(116) + CHR(116) + CHR(115) + CHR(118) + CHR(101) + CHR(114) + CHR(105) + CHR(103) + CHR(101) + CHR(118) + CHR(97) + CHR(116) + CHR(116) + CHR(101) + CHR(110) + CHR(46) + CHR(115) + CHR(101) 
       /*webguru@mittsverigevatten.se*/
      servervar = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50).
   END.
   ELSE IF Guru.Konstanter:AppSpringSet[1]  = "SUND" THEN DO:
      ASSIGN
      franvar = CHR(119) + CHR(101) + CHR(98) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(115) + CHR(117) + CHR(110) + CHR(100) + CHR(115) + CHR(118) + CHR(97) + CHR(108) + CHR(108) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101). /*  webguru@sundsvallenergi.se*/
      servervar = CHR(49) + CHR(55) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(46) + CHR(50) + CHR(53) + CHR(52) + CHR(46) + CHR(50) + CHR(50) + CHR(50).
   END.
   ELSE IF Guru.Konstanter:AppSpringSet[1] = "SNAT" THEN DO:
      {SMTPFRANELPOOL.I} /* @guru.sundsvallelnat.se*/
      franvar = CHR(64) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(46) + CHR(115) + CHR(117) + CHR(110) + CHR(100) + CHR(115) + CHR(118) + CHR(97) + CHR(108) + CHR(108) + CHR(101) + CHR(108) + CHR(110) + CHR(97) + CHR(116) + CHR(46) + CHR(115) + CHR(101) .
   END.
   ELSE IF Guru.Konstanter:AppSpringSet[1] = "cUMEA" THEN DO:
      /*denna fubkar ej men elpool varianten funkar.*/
      ASSIGN 
      franvar = CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(117) + CHR(109) + CHR(101) + CHR(97) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) 
      servervar = CHR(111) + CHR(100) + CHR(101) + CHR(110) + CHR(46) + CHR(117) + CHR(109) + CHR(101) + CHR(97) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) .
   END.
   ELSE IF Guru.Konstanter:AppSpringSet[1] = "gkal"  THEN DO:
      ASSIGN   
      franvar = CHR(119) + CHR(101) + CHR(98) + CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(107) + CHR(97) + CHR(108) + CHR(109) + CHR(97) + CHR(114) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101) 
      servervar = CHR(109) + CHR(97) + CHR(105) + CHR(108) + CHR(105) + CHR(110) + CHR(116).   
   END.
   ELSE IF Guru.Konstanter:globforetag = "ccBODE"  THEN DO:
      ASSIGN   
      /*
      franvar = "Guru@bodensenergi.se". 
      */
      franvar = CHR(71) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(98) + CHR(111) + CHR(100) + CHR(101) + CHR(110) + CHR(115) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(46) + CHR(115) + CHR(101). 
      /*bodensenergi-se.mail.protection.outlook.com
      */
      servervar = CHR(98) + CHR(111) + CHR(100) + CHR(101) + CHR(110) + CHR(115) + CHR(101) + CHR(110) + CHR(101) + CHR(114) + CHR(103) + CHR(105) + CHR(45) + CHR(115) + CHR(101) + CHR(46) + CHR(109) + CHR(97) + CHR(105) + CHR(108) + CHR(46) + CHR(112) + CHR(114) + CHR(111) + CHR(116) + CHR(101) + CHR(99) + CHR(116) + CHR(105) + CHR(111) + CHR(110) + CHR(46) + CHR(111) + CHR(117) + CHR(116) + CHR(108) + CHR(111) + CHR(111) + CHR(107) + CHR(46) + CHR(99) + CHR(111) + CHR(109). 
   END.
   ELSE DO:
      ASSIGN
      /*
      franvar = CHR(101) + CHR(108) + CHR(112) + CHR(111) + CHR(111) + CHR(108) + CHR(46) + CHR(117) + CHR(109) + CHR(101) + CHR(64) + CHR(101) + CHR(108) + CHR(112) + CHR(111) + CHR(111) + CHR(108) + CHR(46) + CHR(115) + CHR(101) 
      */
      /*guru@elpool.se*/
      franvar = CHR(103) + CHR(117) + CHR(114) + CHR(117) + CHR(64) + CHR(101) + CHR(108) + CHR(112) + CHR(111) + CHR(111) + CHR(108) + CHR(46) + CHR(115) + CHR(101).
      servervar = CHR(115) + CHR(109) + CHR(116) + CHR(112) + CHR(46) + CHR(116) + CHR(101) + CHR(108) + CHR(101) + CHR(99) + CHR(111) + CHR(109) + CHR(51) + CHR(46) + CHR(110) + CHR(101) + CHR(116) .
      C_Password = CHR(122) + CHR(120) + CHR(61) + CHR(98) + CHR(112) + CHR(48) + CHR(113) + CHR(114).
      /*elpool.ume@elpool.se*/
      C_User = CHR(101) + CHR(108) + CHR(112) + CHR(111) + CHR(111) + CHR(108) + CHR(46) + CHR(117) + CHR(109) + CHR(101) + CHR(64) + CHR(101) + CHR(108) + CHR(112) + CHR(111) + CHR(111) + CHR(108) + CHR(46) + CHR(115) + CHR(101) .
      L_DoAUTH = TRUE.
      C_AuthType = "base64".
   END.
   ASSIGN 
      mailhub             = servervar     
      EmailTo             = emailretur
      EmailFrom           = franvar
      EmailCC             = " " 
      Attachmentstyp      = ""
      LocalFiles          = ""
      Subject             = "Guru inloggning " 
      Bodysmtp            =  "Ditt nya engångs lösenord. " + nyLosen + CHR(10) + " Ange AnvändarId och Lösen tryck sedan Byt lösenord och ange ett eget lösenord! " 
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

  
END PROCEDURE.

{LOSENREGLERSEK.I}
