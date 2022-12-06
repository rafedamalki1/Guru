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
   prognamn = "D:\DELAD\KLIENT\PRO9\GURU\IMPORT\"
   progkopia = "D:\DELAD\KLIENT\PRO9\GURU\IMPORT\imkopia\".    
    
END.
DEFINE TEMP-TABLE felmeddftptemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.
DEFINE TEMP-TABLE provag
   FIELD VAGNR AS INTEGER
   FIELD VAG AS CHARACTER
   INDEX VAGNR IS PRIMARY VAGNR.

/*
kommando = "D:\DELAD\KLIENT\PRO9\dlc\bin\quoter.exe  D:\DELAD\KLIENT\PRO9\GURU\IMPORT\DAGJ2014091715.txt > D:\DELAD\KLIENT\PRO9\GURU\IMPORT\KWK.Q".
    OS-COMMAND SILENT VALUE(kommando).
    */ 
      

OUTPUT TO D:\DELAD\KLIENT\PRO9\GURU\IMPORT\autotid.txt APPEND.
         
PUT UNFORMATTED "inläsning av hämtade filer 1 " SKIP.
OUTPUT CLOSE.
tider = REPLACE(STRING(TIME,"HH:MM"),":","").             
progque = prognamn + "GN6M7.Q".
INPUT FROM OS-DIR(prognamn) NO-ECHO.
REPEAT:
   /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
   SET filnamn dirlist attrlist.
   OUTPUT TO D:\DELAD\KLIENT\PRO9\GURU\IMPORT\autotid.txt APPEND.
            
PUT UNFORMATTED filnamn dirlist attrlist SKIP.
OUTPUT CLOSE.
   IF filnamn MATCHES "DAGJ*.txt" THEN DO:      
      prognamnvar = dirlist. 
      RUN startin_UI.
      utprogkopia = progkopia + filnamn.
      utprogkopia = REPLACE(utprogkopia,".txt",tider + ".txt").
      OS-RENAME VALUE(dirlist) VALUE(utprogkopia).      
   END.
   
END.
OUTPUT TO D:\DELAD\KLIENT\PRO9\GURU\IMPORT\autotid.txt APPEND.
            
PUT UNFORMATTED "inläsning av hämtade filer 2 " SKIP.
OUTPUT CLOSE.

{EUROPEANAMERICAN.I}
PROCEDURE startin_UI:
   EMPTY TEMP-TABLE tidin NO-ERROR. 
   OS-DELETE SILENT VALUE(progque).   
   OUTPUT TO D:\DELAD\KLIENT\PRO9\GURU\IMPORT\autotid.txt APPEND.
            
PUT UNFORMATTED "inläsning av hämtade filer 3 " SKIP.
OUTPUT CLOSE.    
   
   
   OUTPUT TO D:\DELAD\KLIENT\PRO9\GURU\IMPORT\autotid.txt APPEND.
            

PUT UNFORMATTED prognamnvar SKIP.
PUT UNFORMATTED progque SKIP.
OUTPUT CLOSE.

   kommando = "D:\DELAD\KLIENT\PRO9\dlc\bin\quoter.exe". 
   OS-COMMAND SILENT VALUE(kommando) VALUE(prognamnvar) > VALUE(progque).      
   
END PROCEDURE.

