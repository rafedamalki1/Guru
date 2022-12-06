
/*------------------------------------------------------------------------
    File        : SPRINGKONTROLLINFO.i
    inloggning
    Purpose     : SPRINGKONTROLL.P 
    Syntax      :KÖRS FRÅN AppSpringDbCon.P OCH  AppDarwinPlusDbCon.P
    Description :kör en massa kontroller mot   LOSENKOLLWEB.p

/*laddar klientes macadress*/
RUN MacAddStart_UI IN losenwebh (INPUT app_server_info[4]).
/*laddar klientdatorns namn*/ 
RUN ComputerNameStart_UI IN losenwebh (INPUT app_server_info[9]).
/*sätter undantag från spärrar eller tvärt om beroende på företag om appserver*/
RUN VitlistningStart_UI IN losenwebh (OUTPUT alltOk,OUTPUT meddelandevar).
/* MaxIpUserCheck_UI koll så att max inte överskrids för ip mac datoranv guruanv*/ 
RUN MaxIpUserCheck_UI IN losenwebh (INPUT  app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[6],OUTPUT alltOk,OUTPUT meddelandevar).
/*för att app_server_info[5] ska sökas som Guruanv*/
RUN MaxIpUserCheck_UI IN losenwebh (INPUT  app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[5],OUTPUT alltOk,OUTPUT meddelandevar).
/*skickar ett nytt lösen via mail LÄGGER ÄVEN UT TEXT I LOGGFILEN returmail.txt*/
 RUN ReturMail_UI IN losenwebh (INPUT app_server_info[5],INPUT app_server_info[6], OUTPUT meddelandevar, OUTPUT alltOk).
/* tar bort blanka macadresser kollar datoranvändare och macadd finns att lösen är enligt reglerna*/ 
RUN MacKontroll_UI IN losenwebh (INPUT app_server_info[5], INPUT app_server_info[4], OUTPUT alltOk,OUTPUT meddelandevar).
/*om MAN tillfälligt måste stoppa användare i guru eller något annat, men INTE alla*/
RUN StoppVillkor_UI IN losenwebh (INPUT Guru.Konstanter:AppSpringSet[5], OUTPUT alltOk,OUTPUT meddelandevar).
/*har anget lösenord eller måste ange lösen*/
IF varforetypval51 = 1 OR app_server_info[7] NE "" THEN DO: 
    /*    
  är användaren spärrad via datum,antal försök räknas upp räknar upp,  är lösenordet rätt om rätt så tas antal försök bort
  skriver även till loggfil INFELOG.TXT
  */ 
   RUN RattLosen_UI IN losenwebh (INPUT app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[6], INPUT app_server_info[7], OUTPUT alltOk,OUTPUT meddelandevar).
   /*kollar att ditt nya lösen sätts rätt gäller även ny upplägg av users */
   RUN losenReglerKoll_UI IN losenwebh (INPUT app_server_info[6],INPUT-OUTPUT app_server_info[7],INPUT app_server_info[8], OUTPUT felNr, OUTPUT alltOk,OUTPUT meddelandevar).
   /*får du komma in*/  
   RUN RattLosen_UI IN losenwebh (INPUT app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[6], INPUT app_server_info[7], OUTPUT alltOk,OUTPUT meddelandevar).
END. 
  /*automatisk inloggnong via   Guru.Konstanter:AppSpringSet[6]  = GuruAnvandare
          reset av antal försök
           Guru.Konstanter:AppSpringSet[3]  = Computer_LanIP
           Guru.Konstanter:AppSpringSet[5]  = datoruser
           Guru.Konstanter:AppSpringSet[6]  = GuruAnvandare
   */
   
RUN AutoSpringLogin_UI IN losenwebh (INPUT app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[6],OUTPUT alltOk,OUTPUT meddelandevar).
IF alltOk = FALSE THEN DO:
      /*antal försök räknas upp räknar upp*/
      RUN AntalIpUserCheck_UI IN losenwebh (INPUT Guru.Konstanter:AppSpringSet[3] ,INPUT app_server_info[5],INPUT Guru.Konstanter:AppSpringSet[6] ).
      
/*Anders Olsson Elpool i Umeå AB  13 sep 2019 14:56:16  Mac sätts bara om datoruser = GuruAnvandare   */
RUN MacSett_UI IN losenwebh (INPUT app_server_info[6], INPUT app_server_info[4]).
*/
