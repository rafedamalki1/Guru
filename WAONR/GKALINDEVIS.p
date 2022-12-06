/*GKALINDEVIS.P*/   
/*
SMÅLAND
KONTO "68400"  TAS EJ MED.
KONTO SOM BÖRJAR PÅ 3 ÄR INTÄKT
KONTO >= "44000" AND KONTO <= "46000" ÄR MATERIAL
KONTO SOM BÖRJAR PÅ  5 ÄR ÖVRIGT
KONTO >= "60000" AND KONTO <= "64900" ÄR ÖVRIGT
KONTO >= "65000" AND KONTO <= "66190" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "69100" AND KONTO <= "69999" ÄR ÖVRIGT
KONTO >= "73200" AND KONTO <= "76990" ÄR ÖVRIGT
Allt annat ÄR ÖVRIGT. 

KALMAR
KONTO "65922"  TAS EJ MED.
KONTO "68300"  TAS EJ MED.
KONTO SOM BÖRJAR PÅ 3 ÄR INTÄKT
KONTO SOM BÖRJAR PÅ 4 ÄR MATRIEL
KONTO SOM BÖRJAR PÅ 5 ÄR ÖVRIGT
KONTO >= "60000" AND KONTO <= "65800" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "65900" AND KONTO <= "65915" ÄR ÖVRIGT
KONTO >= "65917" AND KONTO <= "65921" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "65923" AND KONTO <= "66190" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "69100" AND KONTO <= "69999" ÄR ÖVRIGT OBS! Jag har ingen post för tjänst. Ett alternati kan vara 
                                                     Personalkostnader.    
KONTO >= "73200" AND KONTO <= "76990" ÄR ÖVRIGT
KONTO >= "97041" ÄR MATRIEL
KONTO >= "97461" ÄR MATRIEL
Allt annat ÄR ÖVRIGT. 

*/
 
DEFINE VARIABLE tider AS CHARACTER NO-UNDO. 
DEFINE VARIABLE bokdatum AS DATE NO-UNDO. 
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE rad LIKE KOSTREG.RADNR NO-UNDO.
DEFINE VARIABLE iaonrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE utprogkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                

DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(78)" LABEL "File" NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER FORMAT "x(78)" LABEL "Directory" NO-UNDO.
DEFINE VARIABLE ftpanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftplord AS CHARACTER NO-UNDO. 
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(132)".  
DEFINE TEMP-TABLE kostemp 
   FIELD ANVANDARE  LIKE KOSTREG.ANVANDARE 
   FIELD AONR       LIKE KOSTREG.AONR 
   FIELD BENAMNING  LIKE KOSTREG.BENAMNING 
   FIELD BETDATUM   LIKE KOSTREG.BETDATUM 
   FIELD BOKKONTO   LIKE KOSTREG.BOKKONTO 
   FIELD DELNR      LIKE KOSTREG.DELNR 
   FIELD FAKBES     LIKE KOSTREG.FAKBES 
   FIELD FAKTNR     LIKE KOSTREG.FAKTNR 
   FIELD FAKTURERAD LIKE KOSTREG.FAKTURERAD 
   FIELD INKOMST    LIKE KOSTREG.INKOMST 
   FIELD KOSTAUTO   LIKE KOSTREG.KOSTAUTO 
   FIELD LEVKOD     LIKE KOSTREG.LEVKOD 
   FIELD MASKKOST   LIKE KOSTREG.MASKKOST 
   FIELD MOMS       LIKE KOSTREG.MOMS 
   FIELD MTRL       LIKE KOSTREG.MTRL 
   FIELD OVRKR      LIKE KOSTREG.OVRKR 
   FIELD PERSKOST   LIKE KOSTREG.PERSKOST 
   FIELD RADNR      LIKE KOSTREG.RADNR 
   FIELD REGDATUM   LIKE KOSTREG.REGDATUM 
   FIELD TRAKTKOST  LIKE KOSTREG.TRAKTKOST
   INDEX KOST IS PRIMARY AONR DELNR RADNR.
DEFINE VARIABLE kostrakn AS INTEGER NO-UNDO.
DEFINE VARIABLE antalfiler  AS INTEGER NO-UNDO.   
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE servervar AS CHARACTER LABEL "Smtp Server" NO-UNDO.
DEFINE VARIABLE franvar AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE eposttemp NO-UNDO
   FIELD EPOST AS CHARACTER
   FIELD MEDD AS CHARACTER
   INDEX EPOST EPOST.

DEFINE VARIABLE motagandemaskin AS CHARACTER NO-UNDO.  /*ftp maskin */
DEFINE VARIABLE portremot AS INTEGER NO-UNDO.  /*ftp maskin */
DEFINE VARIABLE remotLogin  AS CHARACTER NO-UNDO.  /*login*/               
DEFINE VARIABLE remotpasswd AS CHARACTER NO-UNDO.  /*lösen*/
DEFINE VARIABLE motagandekat AS CHARACTER NO-UNDO.  /*underkatalog */
DEFINE VARIABLE motagandeextra AS CHARACTER NO-UNDO.  /*extra kommandon som ska köras, tex browsa ner i strukturen */
DEFINE VARIABLE ftpcommando_filnamn  AS CHARACTER NO-UNDO.  /* namn på ftpfil */ 
DEFINE VARIABLE filurval AS CHARACTER NO-UNDO. /* Om man vill filtrera ut filer att lista */
DEFINE VARIABLE dirformat  AS INTEGER NO-UNDO.  /* hur får vi tillbaka diren? */
DEFINE VARIABLE localpath AS CHARACTER NO-UNDO. /* var ska filen läggas? */
DEFINE VARIABLE localfilename  AS CHARACTER NO-UNDO.  /* Ska filen döpas om efter hämtning? */
 DEFINE VARIABLE temdir AS CHARACTER NO-UNDO.
 temdir = SESSION:TEMP-DIRECTORY.
 {SESSIONTEMPDIR.I}.
 IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN temdir = webclienttempdir. 


   
{STARTFORAPP.I}
{SMTPDEF3.I}   
{AMERICANEUROPEAN.I}
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.   
IF Guru.Konstanter:globforetag = "ELPA" THEN DO:        
   ASSIGN
   prognamn = "\\pc112\DELAD\PRO9\guru\import\"
   progkopia = "\\pc112\DELAD\PRO9\guru\import\". 
END.
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   
   ASSIGN
   prognamn = "D:\DELAD\server\PRO9s\import\"
   progkopia = "d:\DELAD\server\PRO9s\import\imkopia\".    
    
END.
IF Guru.Konstanter:globforetag = "gkal"  THEN DO:
   /*mailint
mailint.energi-miljo.se
192.168.65.43
gamla
192.168.65.76 mailint
   */
  
   ASSIGN   
   servervar = "mailint".   
END.

DEFINE TEMP-TABLE felmeddftptemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.
   
/*   
IF Guru.Konstanter:globforetag = "ELPA" THEN Guru.Konstanter:globforetag = Guru.Konstanter:globforetag.
ELSE DO:
   
   /*VILKA FILER FINNS /*InGuru InHR InEconoma*/ */
   RUN FTPDIR.P (INPUT "gep", INPUT "vsf17ggr", 
                 INPUT "/InGuru",INPUT "DAGJ*.txt",
                 INPUT "192.168.69.34", 
                 OUTPUT TABLE felmeddftptemp,
                 OUTPUT TABLE provag).
                 /*Anders Olsson Elpool i Umeå AB  30 maj 2022 13:22:42 
                    192.168.69.35 SKA BYTAS
                   */ 
   OUTPUT TO D:\delad\server\PRO9S\autotid.txt APPEND.
   FOR EACH felmeddftptemp:
      PUT UNFORMATTED felmeddftptemp.FELMEDD SKIP.
   END.
   OUTPUT CLOSE.
  
   EMPTY TEMP-TABLE felmeddftptemp NO-ERROR. 
   antalfiler = 0. 
   FOR EACH provag:
      IF provag.VAG NE "" THEN DO:
         /*HÄMTA FIL*/
         ASSIGN   
        ftpanv =  CHR(103) + CHR(101) + CHR(112) 
        ftplord = CHR(118) + CHR(115) + CHR(102) + CHR(49) + CHR(55) + CHR(103) + CHR(103) + CHR(114) . 
        RUN FTPFILE.P (INPUT ftpanv, 
                        INPUT ftplord, INPUT FALSE, INPUT 1,
                         INPUT prognamn + provag.VAG, INPUT "/InGuru/" + provag.VAG,
                         INPUT "192.168.69.34", OUTPUT TABLE felmeddftptemp).      
         OUTPUT TO D:\delad\server\PRO9S\autotid.txt APPEND.
         FOR EACH felmeddftptemp:
            PUT UNFORMATTED provag.VAG felmeddftptemp.FELMEDD SKIP.
         END.
         
         OUTPUT CLOSE.
         FIND FIRST felmeddftptemp NO-ERROR.  
         IF AVAILABLE felmeddftptemp THEN DO:
            IF felmeddftptemp.FELMEDD = 'Fil mottagen...' THEN DO:
               EMPTY TEMP-TABLE felmeddftptemp NO-ERROR.
               /*TA BORT REMOTEFILE*/
               ASSIGN   
        ftpanv =  CHR(103) + CHR(101) + CHR(112) 
        ftplord = CHR(118) + CHR(115) + CHR(102) + CHR(49) + CHR(55) + CHR(103) + CHR(103) + CHR(114) . 
        RUN FTPFILE.P (INPUT ftpanv, 
                        INPUT ftplord, INPUT ?, INPUT 1,
                               INPUT "", INPUT "/InGuru/" + provag.VAG,
                               INPUT "192.168.69.34", OUTPUT TABLE felmeddftptemp).
                               /*Anders Olsson Elpool i Umeå AB  30 maj 2022 13:22:42 
                    192.168.69.35 SKA BYTAS
                   */ 
               antalfiler = antalfiler + 1.                                         
            END.
         
            OUTPUT TO D:\delad\server\PRO9S\autotid.txt APPEND.
            PUT antalfiler SKIP.
            FOR EACH felmeddftptemp:
               PUT UNFORMATTED provag.VAG felmeddftptemp.FELMEDD SKIP.
            END.
            
            OUTPUT CLOSE.
         END.
         EMPTY TEMP-TABLE felmeddftptemp NO-ERROR. 
      END.
   END.
END.
*/
OUTPUT TO D:\delad\server\PRO9S\autotid.txt APPEND.
            
PUT UNFORMATTED "inläsning av hämtade filer 1 " SKIP.
OUTPUT CLOSE.
tider = REPLACE(STRING(TIME,"HH:MM"),":","").             
progque = prognamn + "KALGN6M7.Q".
INPUT FROM OS-DIR(prognamn) NO-ECHO.
REPEAT:
   /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
   SET filnamn dirlist attrlist.
   IF filnamn MATCHES "DAGJ*.txt" THEN DO:      
      prognamnvar = dirlist. 
      RUN startin_UI.
      RUN kalmar_UI.
      utprogkopia = progkopia + filnamn.
   /*   RUN ftpdel_UI (INPUT filnamn).*/
      utprogkopia = REPLACE(utprogkopia,".txt",tider + ".txt").
      OS-RENAME VALUE(dirlist) VALUE(utprogkopia).      
   END.
   /*
   IF filnamn MATCHES "DAGJ*.txt" THEN DO:
      prognamnvar = dirlist. 
      RUN startin_UI.
      RUN smaland_UI.
      utprogkopia = progkopia + filnamn.
      OS-RENAME VALUE(dirlist) VALUE(utprogkopia).
   END.        
   */
END.
OUTPUT TO D:\delad\server\PRO9S\autotid.txt APPEND.
            
PUT UNFORMATTED "inläsning av hämtade filer 2 " SKIP.
OUTPUT CLOSE.








EMPTY TEMP-TABLE eposttemp NO-ERROR.

IF Guru.Konstanter:globforetag = "gkal" THEN DO:   
   IF antalfiler > 0 THEN DO:         
      CREATE eposttemp.
      ASSIGN     
      eposttemp.EPOST = "Lina.Dryselius@kalmarenergi.se;Sophie.Ljungstedt@kalmarenergi.se".      
      eposttemp.MEDD =  STRING(antalfiler) + " filer har lästs in " + STRING(TODAY,"99999999") + ". Antal kostnadregistreringar som har skapats av dessa filer är " + string(kostrakn)  + CHR(10).                
   END.
END.
FOR EACH eposttemp:        
   ASSIGN 
   mailhub             = servervar     
   EmailTo             = eposttemp.EPOST 
   EmailFrom           = "webguru@kalmarenergi.se"
   EmailCC             = ""
   Attachmentstyp      = ""
   LocalFiles          = ""
   Subject             = "Inlästa filer från Devis" + STRING(TODAY,"99999999") 
   Bodysmtp            = eposttemp.MEDD
   MIMEHeader          = "type=text/plain/html:charset=iso-8859-1:filetype=ascii"
   BodyType            = "".
   RUN smtpmail_UI (INPUT FALSE).
   IF oSuccessful = TRUE THEN DO TRANSACTION:
      oSuccessful = FALSE.               
   END.      
   ELSE DO:
      IF Guru.Konstanter:globforetag = "gkal"  THEN DO:
         OUTPUT TO D:\delad\server\PRO9S\inlasfilerfel.txt APPEND.            
         PUT UNFORMATTED TODAY " " vMessage " " servervar " " eposttemp.EPOST " " franvar SKIP.
      END.         
      OUTPUT CLOSE.    
   END.
END.


{EUROPEANAMERICAN.I}

PROCEDURE ftpdel_UI :
   DEFINE INPUT  PARAMETER delfil AS CHARACTER NO-UNDO.
   
   motagandemaskin = "192.168.69.35".
   portremot = 2121.
   remotLogin = CHR(103) + CHR(101) + CHR(112). .
   remotpasswd = CHR(118) + CHR(115) + CHR(102) + CHR(49) + CHR(55) + CHR(103) + CHR(103) + CHR(114) .
   motagandekat = "InGuru".
   motagandeextra = "".
   ftpcommando_filnamn = "DEVIS".
   filurval =  delfil.
   dirformat = 0.
   localpath = "D:\DELAD\server\PRO9s\Import".
   localfilename =  "".
   
    RUN FTPDIRGETDEL_OSC.P   (INPUT  "DEL",
                      INPUT  motagandemaskin,  
                      INPUT  portremot,
                      INPUT  remotLogin ,                 
                      INPUT  remotpasswd,  
                      INPUT  motagandekat,  
                      INPUT  motagandeextra,  
                      INPUT  ftpcommando_filnamn,   
                      INPUT  filurval, 
                      INPUT  dirformat,
                      INPUT  localpath, 
                      INPUT  localfilename).
    
    
END PROCEDURE.

PROCEDURE startin_UI:
   EMPTY TEMP-TABLE tidin NO-ERROR. 
   kommando = "D:\DELAD\KLIENT\PRO9\dlc\bin\quoter.exe". 
       
   /*kommando = SEARCH("quoter.exe").*/
   OS-DELETE SILENT VALUE(progque).
   IF kommando = ? THEN RETURN.
   ELSE OS-COMMAND SILENT VALUE(kommando) VALUE(prognamnvar) > VALUE(progque).      
   INPUT FROM VALUE(progque) NO-ECHO CONVERT TARGET "iso8859-1" SOURCE "ibm850".
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.   
      REPEAT:
         IF INDEX(words,'"',1) = 0 THEN LEAVE.
         words = REPLACE(words,'"',' ').
      END.
      CREATE tidin.   
      ASSIGN tidin.TIN = words.   
   END.
   INPUT CLOSE.     
  
END PROCEDURE.

PROCEDURE kalmar_UI.
   
   FOR EACH tidin:   
      IF SUBSTRING(tidin.TIN,142,6) = "" THEN DO:
         DELETE tidin.
         NEXT. /*AONR*/
      END.
      /*Elnät 101,210,301,302*/
      /*Småländsk 101,210,341,342*/
      /*Värme 101,210,311,312*/
      /*SÄVSJÖ 101 210 328 361 362 364 */
      IF SUBSTRING(tidin.TIN,25,3) = "101" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "210" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "301" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "302" THEN  musz = musz.      
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "311" THEN  musz = musz.      
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "312" THEN  musz = musz.      
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "341" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "342" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "328" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "361" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "362" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "364" THEN  musz = musz.
      ELSE DO:
         DELETE tidin.
         NEXT.
      END.
      IF SUBSTRING(tidin.TIN,130,5) = "68400" OR  
         SUBSTRING(tidin.TIN,130,5) = "65922" OR
         SUBSTRING(tidin.TIN,130,5) = "68300" THEN DO: 
         DELETE tidin.
         NEXT.
      END.
      rad = rad + 1.                                       
      RUN aokoll_UI.
      IF AVAILABLE AONRTAB THEN DO:
         RUN kostreg_UI.
         IF AONRTAB.OMRADE BEGINS "SE" THEN DO:
            IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
               ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,77,15)) * -1.
            END.
            ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) = "4001" THEN DO:
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
            END.
            ELSE IF KOSTREG.BOKKONTO >= "40020"  AND KOSTREG.BOKKONTO <= "40030" THEN DO:
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
            END. 
            ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "4" THEN DO:
               ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
            END.
            ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "5" THEN DO:
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
            END. 
            ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "6" THEN DO: 
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "73200" AND KOSTREG.BOKKONTO <= "76990" THEN DO: 
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "97041" THEN DO: 
               ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "97461" THEN DO: 
               ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END.
            ELSE IF KOSTREG.BOKKONTO >= "97462" THEN DO: 
               ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END.
           
         END.
         ELSE DO:   
            IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
               ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,77,15)) * -1.
            END.  
            ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "4" THEN DO:
               ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
            END. 
            ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "5" THEN DO:
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "60000" AND KOSTREG.BOKKONTO <= "65800" THEN DO: 
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "65900" AND KOSTREG.BOKKONTO <= "65915" THEN DO: 
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "65917" AND KOSTREG.BOKKONTO <= "65921" THEN DO: 
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END.
           
            /* 65922 KOSTREG istället för MASKINENTREP förslag Wenche 20181108
            läs in 65923 istället för att klara övergång*/
            
            ELSE IF KOSTREG.BOKKONTO = "65923" AND KOSTREG.REGDATUM GE 12/01/2018 THEN DO: 
               ASSIGN KOSTREG.MASKKOST = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "65924" AND KOSTREG.BOKKONTO <= "66190" THEN DO: 
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "69100" AND KOSTREG.BOKKONTO <= "69999" THEN DO: 
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "73200" AND KOSTREG.BOKKONTO <= "76990" THEN DO: 
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "97041" THEN DO: 
               ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END. 
            ELSE IF KOSTREG.BOKKONTO >= "97461" THEN DO: 
               ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END.
            ELSE DO:
               ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
            END.
         END.   
      END.
      DELETE tidin.
   END.
   
END PROCEDURE.
PROCEDURE kostreg_UI:
   FIND LAST KOSTREG WHERE KOSTREG.AONR = AONRTAB.AONR AND 
   KOSTREG.DELNR = AONRTAB.DELNR
   USE-INDEX KOST NO-LOCK NO-ERROR.  
   rad = 1.                                                                      
   IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1. 
   IF INTEGER(SUBSTRING(tidin.TIN,12,2)) > 12  THEN SUBSTRING(tidin.TIN,12,2) = "12".
   bokdatum = DATE(INTEGER(SUBSTRING(tidin.TIN,12,2)),01,INTEGER(SUBSTRING(tidin.TIN,8,4))).
  /* IF bokdatum <= TODAY - 35 THEN bokdatum = TODAY.*/  
   CREATE KOSTREG.
   ASSIGN  
   KOSTREG.RADNR = rad
   KOSTREG.AONR = AONRTAB.AONR
   KOSTREG.DELNR = AONRTAB.DELNR
   KOSTREG.REGDATUM = bokdatum 
   KOSTREG.BETDATUM = TODAY
   KOSTREG.BENAMNING = SUBSTRING(tidin.TIN,41,36) 
   KOSTREG.BOKKONTO = SUBSTRING(tidin.TIN,130,5)
   KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,16,9)
   KOSTREG.FAKTURERAD = ?
   KOSTREG.LEVKOD = ""
   SUBSTRING(KOSTREG.ANVANDARE,1,12) = "EKONOMdevis"
   KOSTREG.KOSTAUTO = TRUE. 
   VALIDATE KOSTREG.
   kostrakn = kostrakn + 1.
END PROCEDURE.
PROCEDURE aokoll_UI:
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,142,6) AND AONRTAB.DELNR = 0
   USE-INDEX AONR NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE AONRTAB THEN DO:
      IF SUBSTRING(tidin.TIN,142,1) = "0" THEN DO:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,143,5) AND AONRTAB.DELNR = 0 
         
         USE-INDEX AONR NO-LOCK NO-ERROR.   
      END.
   END.
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      IF NOT AVAILABLE AONRTAB THEN DO TRANSACTION:
         CREATE AONRTAB.
         ASSIGN
         AONRTAB.AONR = SUBSTRING(tidin.TIN,142,6).
         AONRTAB.ORT = "IN FRÅN DEVIS".
      END.
   END.
   
END PROCEDURE.
/*
PROCEDURE smaland_UI.
   FOR EACH tidin:   
      IF SUBSTRING(tidin.TIN,141,6) = "" THEN DO:
         DELETE tidin.
         NEXT. /*AONR*/
      END.
      /*Småländsk 101,210,341,342*/
      IF SUBSTRING(tidin.TIN,25,3) = "101" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "210" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "341" THEN  musz = musz.
      ELSE IF SUBSTRING(tidin.TIN,25,3) = "342" THEN  musz = musz.
      ELSE DO:
         DELETE tidin.
         NEXT.
      END.
      rad = rad + 1.                                                                      
      RUN aokoll_UI.     
      IF AVAILABLE AONRTAB THEN DO:
         RUN kostreg_UI.
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.  
         ELSE IF KOSTREG.BOKKONTO >= "44000" AND KOSTREG.BOKKONTO <= "46000" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
         END. 
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "5" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)). 
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "60000" AND KOSTREG.BOKKONTO <= "64900" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END. 
         ELSE IF KOSTREG.BOKKONTO >= "65000" AND KOSTREG.BOKKONTO <= "66190" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
         ELSE IF KOSTREG.BOKKONTO >= "69100" AND KOSTREG.BOKKONTO <= "69999" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
         ELSE IF KOSTREG.BOKKONTO >= "73200" AND KOSTREG.BOKKONTO <= "76990" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
         ELSE DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,77,15)).
         END.
      END.
      DELETE tidin.
   END.   
END PROCEDURE.
*/
