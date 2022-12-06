
/*------------------------------------------------------------------------
    File        : SPRINGKONTROLLINFO.i
    inloggning
    Purpose     : SPRINGKONTROLL.P 
    Syntax      :K�RS FR�N AppSpringDbCon.P OCH  AppDarwinPlusDbCon.P
    Description :k�r en massa kontroller mot   LOSENKOLLWEB.p

/*laddar klientes macadress*/
RUN MacAddStart_UI IN losenwebh (INPUT app_server_info[4]).
/*laddar klientdatorns namn*/ 
RUN ComputerNameStart_UI IN losenwebh (INPUT app_server_info[9]).
/*s�tter undantag fr�n sp�rrar eller tv�rt om beroende p� f�retag om appserver*/
RUN VitlistningStart_UI IN losenwebh (OUTPUT alltOk,OUTPUT meddelandevar).
/* MaxIpUserCheck_UI koll s� att max inte �verskrids f�r ip mac datoranv guruanv*/ 
RUN MaxIpUserCheck_UI IN losenwebh (INPUT  app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[6],OUTPUT alltOk,OUTPUT meddelandevar).
/*f�r att app_server_info[5] ska s�kas som Guruanv*/
RUN MaxIpUserCheck_UI IN losenwebh (INPUT  app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[5],OUTPUT alltOk,OUTPUT meddelandevar).
/*skickar ett nytt l�sen via mail L�GGER �VEN UT TEXT I LOGGFILEN returmail.txt*/
 RUN ReturMail_UI IN losenwebh (INPUT app_server_info[5],INPUT app_server_info[6], OUTPUT meddelandevar, OUTPUT alltOk).
/* tar bort blanka macadresser kollar datoranv�ndare och macadd finns att l�sen �r enligt reglerna*/ 
RUN MacKontroll_UI IN losenwebh (INPUT app_server_info[5], INPUT app_server_info[4], OUTPUT alltOk,OUTPUT meddelandevar).
/*om MAN tillf�lligt m�ste stoppa anv�ndare i guru eller n�got annat, men INTE alla*/
RUN StoppVillkor_UI IN losenwebh (INPUT Guru.Konstanter:AppSpringSet[5], OUTPUT alltOk,OUTPUT meddelandevar).
/*har anget l�senord eller m�ste ange l�sen*/
IF varforetypval51 = 1 OR app_server_info[7] NE "" THEN DO: 
    /*    
  �r anv�ndaren sp�rrad via datum,antal f�rs�k r�knas upp r�knar upp,  �r l�senordet r�tt om r�tt s� tas antal f�rs�k bort
  skriver �ven till loggfil INFELOG.TXT
  */ 
   RUN RattLosen_UI IN losenwebh (INPUT app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[6], INPUT app_server_info[7], OUTPUT alltOk,OUTPUT meddelandevar).
   /*kollar att ditt nya l�sen s�tts r�tt g�ller �ven ny uppl�gg av users */
   RUN losenReglerKoll_UI IN losenwebh (INPUT app_server_info[6],INPUT-OUTPUT app_server_info[7],INPUT app_server_info[8], OUTPUT felNr, OUTPUT alltOk,OUTPUT meddelandevar).
   /*f�r du komma in*/  
   RUN RattLosen_UI IN losenwebh (INPUT app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[6], INPUT app_server_info[7], OUTPUT alltOk,OUTPUT meddelandevar).
END. 
  /*automatisk inloggnong via   Guru.Konstanter:AppSpringSet[6]  = GuruAnvandare
          reset av antal f�rs�k
           Guru.Konstanter:AppSpringSet[3]  = Computer_LanIP
           Guru.Konstanter:AppSpringSet[5]  = datoruser
           Guru.Konstanter:AppSpringSet[6]  = GuruAnvandare
   */
   
RUN AutoSpringLogin_UI IN losenwebh (INPUT app_server_info[3],INPUT app_server_info[5],INPUT app_server_info[6],OUTPUT alltOk,OUTPUT meddelandevar).
IF alltOk = FALSE THEN DO:
      /*antal f�rs�k r�knas upp r�knar upp*/
      RUN AntalIpUserCheck_UI IN losenwebh (INPUT Guru.Konstanter:AppSpringSet[3] ,INPUT app_server_info[5],INPUT Guru.Konstanter:AppSpringSet[6] ).
      
/*Anders Olsson Elpool i Ume� AB  13 sep 2019 14:56:16  Mac s�tts bara om datoruser = GuruAnvandare   */
RUN MacSett_UI IN losenwebh (INPUT app_server_info[6], INPUT app_server_info[4]).
*/
