/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: EKGADACCESS.p
      Comment: <comment>
      
   */


DEF VAR hAccess       AS COM-HANDLE NO-UNDO.
DEF VAR hCurrdb       AS COM-HANDLE NO-UNDO.
DEF VAR hTable        AS COM-HANDLE NO-UNDO.
DEF VAR hTable2        AS COM-HANDLE NO-UNDO.
DEF VAR hfield        AS COM-HANDLE NO-UNDO.
DEF VAR l-database    AS CHAR       NO-UNDO INIT "C:\Program\SIBSQL\AvCad\Dbf\AvCadx.mdb".
DEF VAR avcdatabase    AS CHAR       NO-UNDO INIT "C:\Program\SIBSQL\AvCad\Dbf\AvCadx.mdb".
DEF VAR l-textformat  AS INT        NO-UNDO INIT 10.
DEF VAR l-fieldLength AS INT        NO-UNDO INIT 40.
DEF VAR l-Result      AS LOG        NO-UNDO.
DEFINE VARIABLE com AS CHARACTER NO-UNDO.
DEFINE VARIABLE com2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE st AS INTEGER NO-UNDO.
DEFINE VARIABLE sl AS INTEGER NO-UNDO.
DEFINE VARIABLE path AS CHARACTER.
DEFINE VARIABLE idnum AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER NO-UNDO.
DEFINE VARIABLE counter AS INTEGER NO-UNDO.
DEFINE VARIABLE odbch AS HANDLE NO-UNDO.
DEFINE VARIABLE rakres AS INTEGER NO-UNDO.
DEFINE VARIABLE siffra AS LOGICAL NO-UNDO.
DEFINE VARIABLE ekgklog  AS INTEGER NO-UNDO.
DEFINE VARIABLE klgklass AS CHARACTER NO-UNDO.
DEFINE VARIABLE ekgklog2  AS INTEGER NO-UNDO.
DEFINE VARIABLE klgklass2 AS CHARACTER NO-UNDO.

   
{ODBCTEMP.I}
{EKGSKAPTEMP.I}
DEFINE TEMP-TABLE feltemp NO-UNDO
   FIELD FELTEXT AS CHARACTER.
 

                          

DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE odbcvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE avcadok AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.

RUN load_UI.

PROCEDURE load_UI :
   
   DEBUGGER:SET-BREAK().
   l-database = "C:\svenen\EKGINDATA\EKGIN.mdb".
   path = SEARCH(l-database).
   IF path NE ? THEN DO:
      CREATE "Access.Application" hAccess CONNECT TO l-database NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         RUN odbc_UI (OUTPUT avcadok).
         IF avcadok = FALSE THEN DO:
            MESSAGE "Kunde inte ladda databasen, kontakta Elpool på 090-184540. Ange:" l-database VIEW-AS ALERT-BOX.
            RETURN.
         END.
         avcadok = TRUE.
      END.
      ELSE DO:
         hAccess:VISIBLE = NO.
         hCurrdb    = hAccess:CurrentDb.
         IF NOT VALID-HANDLE(hCurrdb) THEN DO:
            RUN odbc_UI (OUTPUT avcadok).
             IF avcadok = FALSE THEN DO:
               MESSAGE "Kunde inte ladda databasen, kontakta Elpool på 090-184540. Ange:" l-database VIEW-AS ALERT-BOX.
               RETURN.
            END.
            avcadok = TRUE.

         END.
         avcadok = TRUE.
      END.
      IF avcadok = TRUE THEN RUN tab_UI.
      RETURN.
   END.
   
   
END PROCEDURE.


PROCEDURE odbc_UI :
   DEFINE OUTPUT PARAMETER avcadok AS LOGICAL NO-UNDO.
   RUN ODBCACESS.P PERSISTENT SET odbch (INPUT "EKG",
                                 INPUT "localhost",
                                 INPUT "",
                                 INPUT "",
                                 OUTPUT hAccess,
                                 OUTPUT TABLE feltemp).
   FIND FIRST feltemp WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE feltemp THEN DO:
      MESSAGE feltemp.FELTEXT VIEW-AS ALERT-BOX.
      DELETE PROCEDURE odbch NO-ERROR.
      avcadok = FALSE.
      RETURN.
   END.
   avcadok = TRUE.
   odbcvar = TRUE.
END PROCEDURE.
PROCEDURE tab_UI :
   
   DEFINE VARIABLE htableS AS COM-HANDLE NO-UNDO.
   DEBUGGER:SET-BREAK().
   IF odbcvar = TRUE THEN DO:
            
      assign
      ekgklog = 1
      klgklass = "KLG1"
      ekgklog2 = 2
      klgklass2 = "KLG2".
      
      /*/* rensprogram*/
              
      RUN rensskarptabellklg_UI(INPUT ekgklog, INPUT klgklass, INPUT ekgklog2, INPUT klgklass2).*/
      
      CREATE ekgsubkattemp.
      ASSIGN
      ekgsubkattemp.EKGSUBID = ekgklog
      ekgsubkattemp.EBRKAT = klgklass
      ekgsubkattemp.BENAMNING = "EBR-Kostnadskatalog"
      ekgsubkattemp.ARTAL = 2013
      ekgsubkattemp.LASTA = FALSE      
      ekgsubkattemp.ANVANDS = FALSE.
      
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "GRUNDDATA"
      sqltab.FALTANT = 6
      sqltab.FALT[1] = "RESURSNUMMER"
      sqltab.FALT[2] = "RESURS"
      sqltab.FALT[3] = "PRIS".
      sqltab.FALT[4] = "EA".
      sqltab.FALT[5] = "Anmärkning".
      sqltab.FALT[6] = "Timsumma".
      kommando = "GRUNDDATA". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN resurs_UI (INPUT ekgklog, INPUT klgklass,INPUT ekgklog2, INPUT klgklass2).
      /*ekgaccess.p 
      ekgresurstemp      från databas 1-40,37,38,39,101   KLG1 KLG2
      ekgresurpristemp   från databas 1-40,37,38,39,101   KLG1 KLG2
      ekgresursnivatemp  for each ekgresurstemp 1-40,101  KLG1 KLG2
      
      ekg2access.p 
      ekgresurstemp      60-88                            KLG1 KLG2
      ekgresurpristemp   60-88                            KLG1 KLG2
      ekgresursnivatemp  60-88                            KLG1 KLG2
      */ 
      
      RUN paslag_UI (INPUT ekgklog, INPUT klgklass).
      RUN regel_UI (INPUT ekgklog, INPUT klgklass).
      RUN niva_UI (INPUT ekgklog, INPUT klgklass).
      RUN resursniva_UI (INPUT ekgklog, INPUT klgklass,INPUT ekgklog2, INPUT klgklass2).
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "Artikelregister"
      sqltab.FALTANT = 8
      sqltab.FALT[1] = "Artnr"
      sqltab.FALT[2] = "Benämning"
      sqltab.FALT[3] = "Enhet"
      sqltab.FALT[4] = "Pris"
      sqltab.FALT[5] = "Kalkylpris"
      sqltab.FALT[6] = "Baspris"
      sqltab.FALT[7] = "Låsuppdatering"
      sqltab.FALT[8] = "Används".
      
      kommando = "Artikelregister". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN mtrl_UI(INPUT ekgklog, INPUT klgklass).
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P5"
      sqltab.FALTANT = 22
      sqltab.FALT[1] = "Löpnr"
      sqltab.FALT[2] = "Arbete"
      sqltab.FALT[3] = "Enhet"
      sqltab.FALT[4] = "Beredtimmar"
      sqltab.FALT[5] = "DEtimmar"
      sqltab.FALT[6] = "Maskintimmar"
      sqltab.FALT[7] = "Kommentar"
      sqltab.FALT[8] = "EAMaskin"
      sqltab.FALT[9] = "EASpecial"
      sqltab.FALT[10] = "EATillägg"
      sqltab.FALT[11] = "Resurs1"
      sqltab.FALT[12] = "Resurs1-tid"
      sqltab.FALT[13] = "Resurs2"
      sqltab.FALT[14] = "Resurs2-tid"
      sqltab.FALT[15] = "Resurs3"
      sqltab.FALT[16] = "Resurs3-tid"
      sqltab.FALT[17] = "Resurs4"
      sqltab.FALT[18] = "Resurs4-tid"
      sqltab.FALT[19] = "Resurs5"
      sqltab.FALT[20] = "Resurs5-tid"
      sqltab.FALT[21] = "Resurs6"
      sqltab.FALT[22] = "Resurs6-tid".      
      kommando = "P5". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p5_UI(INPUT ekgklog, INPUT klgklass).
      
      
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P4"
      sqltab.FALTANT = 22
      sqltab.FALT[1] = "Löpnr"
      sqltab.FALT[2] = "Frekvenser"
      sqltab.FALT[3] = "Arbete"
      sqltab.FALT[4] = "Enhet"
      sqltab.FALT[5] = "Beredtimmar"
      sqltab.FALT[6] = "DEtimmar"
      sqltab.FALT[7] = "Maskintimmar"
      sqltab.FALT[8] = "EA"
      sqltab.FALT[9] = "Kommentar"
      sqltab.FALT[10] = "EAMaskin"
      sqltab.FALT[11] = "EASpecial"
      sqltab.FALT[12] = "EATillägg"
      sqltab.FALT[13] = "Resurs1"
      sqltab.FALT[14] = "Resurs1-tid"
      sqltab.FALT[15] = "Resurs2"
      sqltab.FALT[16] = "Resurs2-tid"
      sqltab.FALT[17] = "Resurs3"
      sqltab.FALT[18] = "Resurs3-tid"
      sqltab.FALT[19] = "Resurs4"
      sqltab.FALT[20] = "Resurs4-tid"
      sqltab.FALT[21] = "Resurs5"
      sqltab.FALT[22] = "Resurs5-tid"
      sqltab.FALT[23] = "Resurs6"
      sqltab.FALT[24] = "Resurs6-tid".      
      kommando = "P4". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p4_UI(INPUT ekgklog, INPUT klgklass).
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P4P5"
      sqltab.FALTANT = 4
      sqltab.FALT[1] = "Postid"
      sqltab.FALT[2] = "P4kod"
      sqltab.FALT[3] = "P5kod"
      sqltab.FALT[4] = "Antal".
            
      kommando = "P4p5". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p4p5_UI(INPUT ekgklog, INPUT klgklass).
      
      
      FOR EACH ekgp4temp WHERE ekgp4temp.FREKVENS = TRUE:
         
         /* Om frekvens ibockat, men ingen frekvens upplagd skall monttimmar och bertimmar läggas till  ex 43110004 43110005 45100121   
         måste göras efter att frekvener skapats*/
         FIND FIRST ekgp4frekvenstemp  WHERE ekgp4frekvenstemp.P4ARBKOD = ekgp4temp.P4ARBKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp4frekvenstemp  THEN DO:         
            IF ekgp4temp.MONTTIM > 0 THEN DO: 
               CREATE ekgp4resurstemp.                           
               ASSIGN 
               ekgp4resurstemp.EKGSUBID  = ekgklog
               ekgp4resurstemp.EBRKAT = klgklass
               ekgp4resurstemp.P4ARBKOD = ekgp4temp.P4ARBKOD      
               ekgp4resurstemp.P4LOPNR = 0
               ekgp4resurstemp.RESURSNR = 1 
               ekgp4resurstemp.ANTAL = ekgp4temp.MONTTIM.
            END.
            IF ekgp4temp.BERTIM > 0 THEN DO: 
               CREATE ekgp4resurstemp.
               ASSIGN 
               ekgp4resurstemp.EKGSUBID  = ekgklog
               ekgp4resurstemp.EBRKAT = klgklass
               ekgp4resurstemp.P4ARBKOD = ekgp4temp.P4ARBKOD      
               ekgp4resurstemp.P4LOPNR = 0
               ekgp4resurstemp.RESURSNR = 17 
               ekgp4resurstemp.ANTAL = ekgp4temp.BERTIM.
            END.
            ekgp4temp.FREKVENS = FALSE.
         END.
      END.  
      FOR EACH ekgp4temp WHERE ekgp4temp.FREKVENS = FALSE:
         /*lägg ej upp frekvensposter om frekvens är urbockad. Ok Peter????*/
         FOR EACH ekgp4frekvenstemp  WHERE ekgp4frekvenstemp.P4ARBKOD = ekgp4temp.P4ARBKOD:
            DELETE ekgp4frekvenstemp.
         END.                 
      END.   
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P3P4"
      sqltab.FALTANT = 4
      sqltab.FALT[1] = "Postid"
      sqltab.FALT[2] = "P3kod"
      sqltab.FALT[3] = "P4kod"
      sqltab.FALT[4] = "Antal".
            
      kommando = "P3p4". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p3p4_UI(INPUT ekgklog, INPUT klgklass).
      
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P3"
      sqltab.FALTANT = 27
      sqltab.FALT[1] = "Löpnr"
      sqltab.FALT[2] = "Frekvenser"
      sqltab.FALT[3] = "Arbete"
      sqltab.FALT[4] = "Enhet"
      sqltab.FALT[5] = "Beredtimmar"
      sqltab.FALT[6] = "DEtimmar"
      sqltab.FALT[7] = "Maskintimmar"
      sqltab.FALT[8] = "EA"
      sqltab.FALT[9] = "Kommentar"
      sqltab.FALT[10] = "Antalman"
      sqltab.FALT[11] = "Tidsunderl"
      sqltab.FALT[12] = "Justering beredning"
      sqltab.FALT[13] = "EAMaskin"
      /*klg2 sqltab.FALT[13] = "Utrustning"*/
      sqltab.FALT[14] = "EATillägg"
      sqltab.FALT[15] = "EASpecial"
      sqltab.FALT[16] = "Resurs1"
      sqltab.FALT[17] = "Resurs1-tid"
      sqltab.FALT[18] = "Resurs2"
      sqltab.FALT[19] = "Resurs2-tid"
      sqltab.FALT[20] = "Resurs3"
      sqltab.FALT[21] = "Resurs3-tid"
      sqltab.FALT[22] = "Resurs4"
      sqltab.FALT[23] = "Resurs4-tid"
      sqltab.FALT[24] = "Resurs5"
      sqltab.FALT[25] = "Resurs5-tid"
      sqltab.FALT[26] = "Resurs6"
      sqltab.FALT[27] = "Resurs6-tid".      
      kommando = "P3". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p3_UI(INPUT ekgklog, INPUT klgklass).
      
      FOR EACH ekgp3temp WHERE ekgp3temp.FREKVENS = FALSE:
         /*lägg ej upp frekvensposter om frekvens är urbockad. Ok Peter????*/
         FOR EACH ekgp3frekvenstemp  WHERE ekgp3frekvenstemp.P3ARBKOD = ekgp3temp.P3ARBKOD AND ekgp3frekvenstemp.P3LOPNR = ekgp3temp.P3LOPNR:
            DELETE ekgp3frekvenstemp.
         END.                 
      END.
      
      /*
      FOR EACH ekgp3temp WHERE ekgp3temp.FREKVENS = TRUE:
         /*Om frekvens ibockat, men ingen frekvens upplagd skall monttimmar  läggas till   ex 825 11  */
         FIND FIRST ekgp3frekvenstemp  WHERE ekgp3frekvenstemp.P3ARBKOD = ekgp3temp.P3ARBKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp3frekvenstemp  THEN DO:         
            IF ekgp3temp.MONTTIM > 0 THEN DO: 
               CREATE ekgp3resurstemp.
               ASSIGN 
               ekgp4resurstemp.EKGSUBID  = ekgklog
               ekgp4resurstemp.EBRKAT = klgklass               
               ekgp3resurstemp.P3ARBKOD = ekgp3temp.P3ARBKOD     
               ekgp3resurstemp.P3LOPNR = ekgp3temp.P3LOPNR                                             
               ekgp3resurstemp.RESURSNR = 1 
               ekgp3resurstemp.ANTAL = ekgp3temp.MONTTIM.               
            END.            
         END.
      END.*/                  
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P2"
      sqltab.FALTANT = 32
      sqltab.FALT[1] = "Löpnr"
      sqltab.FALT[2] = "Frekvenser"
      sqltab.FALT[3] = "Arbete"
      sqltab.FALT[4] = "Enhet"
      sqltab.FALT[5] = "Beredtimmar"
      sqltab.FALT[6] = "DEtimmar"
      sqltab.FALT[7] = "Maskintimmar"
      sqltab.FALT[8] = "EA"      
      sqltab.FALT[9] = "Arbetekost"
      sqltab.FALT[10] = "Materielkost"
      sqltab.FALT[11] = "Maskinkost"
      sqltab.FALT[12] = "Specialkost"
      sqltab.FALT[13] = "Övrigt"
      sqltab.FALT[14] = "Summakr"      
      sqltab.FALT[15] = "Kommentar"      
      sqltab.FALT[16] = "Justering beredning"
      sqltab.FALT[17] = "EAMaskin"
      /*klg2 sqltab.FALT[13] = "Utrustning"*/
      sqltab.FALT[18] = "EATillägg"
      sqltab.FALT[19] = "EASpecial"
      sqltab.FALT[20] = "Resurs1"
      sqltab.FALT[21] = "Resurs1-tid"
      sqltab.FALT[22] = "Resurs2"
      sqltab.FALT[23] = "Resurs2-tid"
      sqltab.FALT[24] = "Resurs3"
      sqltab.FALT[25] = "Resurs3-tid"
      sqltab.FALT[26] = "Resurs4"
      sqltab.FALT[27] = "Resurs4-tid"
      sqltab.FALT[28] = "Resurs5"
      sqltab.FALT[29] = "Resurs5-tid"
      sqltab.FALT[30] = "Resurs6"
      sqltab.FALT[31] = "Resurs6-tid"
      sqltab.FALT[32] = "ÖvrigaKostnaderutövermaskiner".      
      kommando = "P2". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p2_UI(INPUT ekgklog, INPUT klgklass).
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P2P3"
      sqltab.FALTANT = 4
      sqltab.FALT[1] = "Postid"
      sqltab.FALT[2] = "P2kod"
      sqltab.FALT[3] = "P3kod"
      sqltab.FALT[4] = "Antal".
            
      kommando = "P2p3". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p2p3_UI(INPUT ekgklog, INPUT klgklass).
      
      FOR EACH ekgp2temp WHERE ekgp2temp.FREKVENS = FALSE:
         /*lägg ej upp frekvensposter om frekvens är urbockad. Ok Peter????*/
         FOR EACH ekgp2frekvenstemp  WHERE ekgp2frekvenstemp.P2ARBKOD = ekgp2temp.P2ARBKOD AND ekgp2frekvenstemp.P2LOPNR = ekgp2temp.P2LOPNR:
            DELETE ekgp2frekvenstemp.
         END.                 
      END.
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "Materialfrekvens"
      sqltab.FALTANT = 4
      sqltab.FALT[1] = "Postid"
      sqltab.FALT[2] = "P2löpnr"
      sqltab.FALT[3] = "Artikelnummer"
      sqltab.FALT[4] = "Antal".
            
      kommando = "Materialfrekvens". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN Materialfrekvens_UI(INPUT ekgklog, INPUT klgklass).
      
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P1"
      sqltab.FALTANT = 21
      sqltab.FALT[1] = "Löpnr"
      sqltab.FALT[2] = "Frekvenser"
      sqltab.FALT[3] = "Arbete"
      sqltab.FALT[4] = "Enhet"
      sqltab.FALT[5] = "Beredtimmar"
      sqltab.FALT[6] = "Justering beredning"
      sqltab.FALT[7] = "DEtimmar"
      sqltab.FALT[8] = "Maskintimmar"
      sqltab.FALT[9] = "EA"
      sqltab.FALT[10] = "Summakr"
      sqltab.FALT[11] = "Kommentar"
      sqltab.FALT[12] = "JusteringsKostnader"                              
      sqltab.FALT[13] = "Arbetekost"
      sqltab.FALT[14] = "Materielkost"
      sqltab.FALT[15] = "Maskinkost"
      sqltab.FALT[16] = "Specialkost"      
      sqltab.FALT[17] = "JusteringArbeteKost"
      sqltab.FALT[18] = "JusteringMaterialKost"
      sqltab.FALT[19] = "JusteringMaskinKost"        
      sqltab.FALT[20] = "Övrigt"      
      sqltab.FALT[21] = "JusteringÖvrigt".
            
            
      kommando = "P1". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p1_UI(INPUT ekgklog, INPUT klgklass).
      
      EMPTY TEMP-TABLE sqltab NO-ERROR. 
      CREATE sqltab.
      ASSIGN
      sqltab.TABNAMN = "P1P2"
      sqltab.FALTANT = 4
      sqltab.FALT[1] = "Postid"
      sqltab.FALT[2] = "P1kod"
      sqltab.FALT[3] = "P2kod"
      sqltab.FALT[4] = "Antal".
            
      kommando = "P1p2". 
      RUN hamtap_UI IN odbch (INPUT kommando,INPUT TABLE sqltab,OUTPUT TABLE esqldat).
      RUN p1p2_UI(INPUT ekgklog, INPUT klgklass).
      
      
      /*ta bort frekvenser utan träff i katalog*/
      FOR EACH ekgp1frekvenstemp :
         FIND FIRST ekgp1temp  WHERE ekgp1temp.P1ARBKOD =  ekgp1frekvenstemp.P1ARBKOD  AND ekgp1temp.P1LOPNR = ekgp1frekvenstemp.P1LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp1temp THEN DO:
            DELETE ekgp1frekvenstemp.
            /*DISPLAY "ejfrek1p1" ekgp1frekvenstemp.P1ARBKOD ekgp1frekvenstemp.P1lopnr.*/
         END.
         ELSE DO:
            FIND FIRST ekgp2temp  WHERE ekgp2temp.P2ARBKOD =  ekgp1frekvenstemp.P2ARBKOD  AND ekgp2temp.P2LOPNR = ekgp1frekvenstemp.P2LOPNR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ekgp2temp THEN DO:
               DELETE ekgp1frekvenstemp.
               /*DISPLAY "ejfrek1p2" ekgp1frekvenstemp.P1ARBKOD ekgp1frekvenstemp.P1lopnr.*/
            END.
         END.   
      END.
      /*RENSA NÄR BÅDE P2 OCH P3 SKAPADE*/
      FOR EACH ekgp2frekvenstemp :
         FIND FIRST ekgp2temp  WHERE ekgp2temp.P2ARBKOD =  ekgp2frekvenstemp.P2ARBKOD  AND ekgp2temp.P2LOPNR = ekgp2frekvenstemp.P2LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp2temp THEN DO:
            /*borde tas bort!!*/
            DELETE ekgp2frekvenstemp.
            /*DISPLAY "ejfrek2p2" ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2lopnr.*/
         END.
      END.
      FOR EACH ekgp2frekvenstemp :
         FIND FIRST ekgp3temp  WHERE ekgp3temp.P3ARBKOD =  ekgp2frekvenstemp.P3ARBKOD  AND ekgp3temp.P3LOPNR = ekgp2frekvenstemp.P3LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp3temp THEN DO:
            /*borde tas bort!!*/
            DELETE ekgp2frekvenstemp.
            /*DISPLAY "ejfrek2p3" ekgp2frekvenstemp.P3ARBKOD ekgp2frekvenstemp.P3lopnr ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2lopnr.*/
         END.
      END.
      
      FOR EACH ekgp3frekvenstemp :
         FIND FIRST ekgp3temp  WHERE ekgp3temp.P3ARBKOD =  ekgp3frekvenstemp.P3ARBKOD  AND ekgp3temp.P3LOPNR = ekgp3frekvenstemp.P3LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp3temp THEN DO:
            /*borde tas bort!!*/
            DELETE ekgp3frekvenstemp.
            /*DISPLAY "ejfrek2p2" ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2lopnr.*/
         END.
      END.
      FOR EACH ekgp3frekvenstemp :
         FIND FIRST ekgp4temp  WHERE ekgp4temp.P4ARBKOD =  ekgp3frekvenstemp.P4ARBKOD  AND ekgp4temp.P4LOPNR = ekgp3frekvenstemp.P4LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp4temp THEN DO:
            /*borde tas bort!!*/
            DELETE ekgp3frekvenstemp.
            /*DISPLAY "ejfrek2p3" ekgp2frekvenstemp.P3ARBKOD ekgp2frekvenstemp.P3lopnr ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2lopnr.*/
         END.
      END.
      
      FOR EACH ekgp4frekvenstemp :
         FIND FIRST ekgp4temp  WHERE ekgp4temp.P4ARBKOD =  ekgp4frekvenstemp.P4ARBKOD  AND ekgp4temp.P4LOPNR = ekgp4frekvenstemp.P4LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp4temp THEN DO:
            /*borde tas bort!!*/
            DELETE ekgp4frekvenstemp.
            /*DISPLAY "ejfrek2p2" ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2lopnr.*/
         END.
      END.
      FOR EACH ekgp4frekvenstemp :
         FIND FIRST ekgp5temp  WHERE ekgp5temp.P5ARBKOD =  ekgp4frekvenstemp.P5ARBKOD  AND ekgp5temp.P5LOPNR = ekgp4frekvenstemp.P5LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp5temp THEN DO:
            /*borde tas bort!!*/
            DELETE ekgp4frekvenstemp.
            /*DISPLAY "ejfrek2p3" ekgp2frekvenstemp.P3ARBKOD ekgp2frekvenstemp.P3lopnr ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2lopnr.*/
         END.
      END.
      
      
      
      RUN skapskarptabell_UI.
      
       
      
            
   END.
   
   IF VALID-HANDLE(hCurrdb) THEN DO:
      RELEASE OBJECT htable NO-ERROR.
      htable = ?.       
   END.   
   ELSE DO:
      RUN close_UI.
   END.   
END.
PROCEDURE skapskarptabell_UI :
MESSAGE "skapskarptabell_UI".
  
   FOR EACH ekgsubkattemp:
      CREATE ekgsubkat.
      BUFFER-COPY ekgsubkattemp  TO ekgsubkat.      
   END.
   FOR EACH ekgresurstemp:
      CREATE ekgresurs.
      BUFFER-COPY ekgresurstemp  TO ekgresurs.      
   END.
   FOR EACH ekgresurspristemp:
      CREATE ekgresurspris.
      BUFFER-COPY ekgresurspristemp TO ekgresurspris.      
   END.
   FOR EACH ekgpaslagtemp :
      CREATE ekgpaslag.
      BUFFER-COPY ekgpaslagtemp TO ekgpaslag.      
   END.
   FOR EACH ekgnivatemp:
      CREATE ekgniva.
      BUFFER-COPY ekgnivatemp TO ekgniva.      
   END.
   FOR EACH ekgresursnivatemp:
      CREATE ekgresursniva.
      BUFFER-COPY ekgresursnivatemp TO ekgresursniva.      
   END.
   FOR EACH ekgresurspaslagtemp:
      CREATE ekgresurspaslag.
      BUFFER-COPY ekgresurspaslagtemp TO ekgresurspaslag.      
   END.
   FOR EACH ekgregeltemp:
      CREATE ekgregel.
      BUFFER-COPY ekgregeltemp  TO ekgregel.      
   END.
   FOR EACH ekgmtrltemp:
      CREATE ekgmtrl.
      BUFFER-COPY ekgmtrltemp TO ekgmtrl.      
   END.
   FOR EACH ekgp5temp:
      CREATE ekgp5.
      BUFFER-COPY ekgp5temp TO ekgp5.      
   END.
   FOR EACH ekgp5resurstemp:
      CREATE ekgp5resurs.
      BUFFER-COPY ekgp5resurstemp  TO ekgp5resurs.      
   END.
   FOR EACH ekgp4temp:
      CREATE ekgp4.
      BUFFER-COPY ekgp4temp TO ekgp4.      
   END.
   FOR EACH ekgp4resurstemp:
      CREATE ekgp4resurs.
      BUFFER-COPY ekgp4resurstemp  TO ekgp4resurs.      
   END.
   FOR EACH ekgp4frekvenstemp:
      CREATE ekgp4frekvens.
      BUFFER-COPY ekgp4frekvenstemp  TO ekgp4frekvens.      
   END.
   FOR EACH ekgp3arbkodtemp:
      CREATE ekgp3arbkod.
      BUFFER-COPY ekgp3arbkodtemp TO ekgp3arbkod.      
   END.
   FOR EACH ekgp3temp:
      CREATE ekgp3.
      BUFFER-COPY ekgp3temp TO ekgp3.      
   END.
   FOR EACH ekgp3resurstemp:
      CREATE ekgp3resurs.
      BUFFER-COPY ekgp3resurstemp  TO ekgp3resurs.      
   END.
   FOR EACH ekgp3frekvenstemp:
      CREATE ekgp3frekvens.
      BUFFER-COPY ekgp3frekvenstemp  TO ekgp3frekvens.      
   END.
   FOR EACH ekgp2arbkodtemp:
      CREATE ekgp2arbkod.
      BUFFER-COPY ekgp2arbkodtemp TO ekgp2arbkod.      
   END.
   FOR EACH ekgp2temp:
      CREATE ekgp2.
      BUFFER-COPY ekgp2temp TO ekgp2.      
   END.
   FOR EACH ekgp2resurstemp:
      CREATE ekgp2resurs.
      BUFFER-COPY ekgp2resurstemp  TO ekgp2resurs.      
   END.
   FOR EACH ekgp2frekvenstemp:
      CREATE ekgp2frekvens.
      BUFFER-COPY ekgp2frekvenstemp  TO ekgp2frekvens.      
   END.
   FOR EACH ekgp2mtrltemp:
      CREATE ekgp2mtrl.
      BUFFER-COPY ekgp2mtrltemp TO ekgp2mtrl.      
   END.
   FOR EACH ekgp1arbkodtemp:
      CREATE ekgp1arbkod.
      BUFFER-COPY ekgp1arbkodtemp TO ekgp1arbkod.      
   END.
   FOR EACH ekgp1temp:
      CREATE ekgp1.
      BUFFER-COPY ekgp1temp TO ekgp1.      
   END.
   
   FOR EACH ekgp1frekvenstemp:
      CREATE ekgp1frekvens.
      BUFFER-COPY ekgp1frekvenstemp  TO ekgp1frekvens.      
   END.
   
END PROCEDURE.



PROCEDURE rensskarptabellklg_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER tempsubid2 AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr2 AS CHARACTER NO-UNDO.
   
   FOR EACH ekgsubkat WHERE  ekgsubkat.EKGSUBID  = tempsubid AND  ekgsubkat.EBRKAT = tempebr:
      DELETE ekgsubkat.           
   END.
   FOR EACH EKGRESURS :  
      IF ekgresurs.RESURSNR GE 1 AND ekgresurs.RESURSNR LE 39 THEN DO:       
         DELETE ekgresurs.
      END.
      ELSE IF ekgresurs.RESURSNR = 101 THEN DO:       
         DELETE ekgresurs.
      END.                
   END.
   
   
   FOR EACH ekgresurspris WHERE  ekgresurspris.EKGSUBID  = tempsubid AND  ekgresurspris.EBRKAT = tempebr:
      /*Ta bort resurs 1-39 och 101 för både klg1 och klg2*/
      IF ekgresurspris.RESURSNR GE 1 AND ekgresurspris.RESURSNR LE 39 THEN DO:       
         DELETE ekgresurspris.
      END.
      ELSE IF ekgresurspris.RESURSNR = 101 THEN DO:       
         DELETE ekgresurspris.
      END.         
   END.
   FOR EACH ekgresurspris WHERE  ekgresurspris.EKGSUBID  = tempsubid2 AND  ekgresurspris.EBRKAT = tempebr2:
      /*Ta bort resurs 1-39 och 101 för både klg1 och klg2*/
      IF ekgresurspris.RESURSNR GE 1 AND ekgresurspris.RESURSNR LE 39 THEN DO:       
         DELETE ekgresurspris.
      END.
      ELSE IF ekgresurspris.RESURSNR = 101 THEN DO:       
         DELETE ekgresurspris.
      END.         
   END.
   FOR EACH ekgresursniva WHERE  ekgresurspris.EKGSUBID  = tempsubid AND  ekgresurspris.EBRKAT = tempebr:
      /*Ta bort resurs 1-39 och 101 för både klg1 och klg2*/
      IF ekgresursniva.RESURSNR GE 1 AND ekgresursniva.RESURSNR LE 39 THEN DO:       
         DELETE ekgresursniva.
      END.
      ELSE IF ekgresursniva.RESURSNR = 101 THEN DO:       
         DELETE ekgresursniva.
      END.         
   END.
   FOR EACH ekgresursniva WHERE  ekgresurspris.EKGSUBID  = tempsubid2 AND  ekgresurspris.EBRKAT = tempebr2:
      /*Ta bort resurs 1-39 och 101 för både klg1 och klg2*/
      IF ekgresursniva.RESURSNR GE 1 AND ekgresursniva.RESURSNR LE 39 THEN DO:       
         DELETE ekgresursniva.
      END.
      ELSE IF ekgresursniva.RESURSNR = 101 THEN DO:       
         DELETE ekgresursniva.
      END.         
   END.
   FOR EACH ekgregel WHERE  ekgregel.EKGSUBID  = tempsubid AND  ekgregel.EBRKAT = tempebr:
      DELETE ekgregel.           
   END.
   FOR EACH ekgpaslag WHERE  ekgpaslag.EKGSUBID  = tempsubid AND  ekgpaslag.EBRKAT = tempebr:      
      DELETE ekgpaslag.      
   END.
   FOR EACH ekgniva WHERE  ekgniva.EKGSUBID  = tempsubid AND  ekgniva.EBRKAT = tempebr:   
      DELETE ekgniva.      
   END.
   
   FOR EACH ekgresurspaslag WHERE  ekgresurspaslag.EKGSUBID  = tempsubid AND  ekgresurspaslag.EBRKAT = tempebr:
      DELETE ekgresurspaslag.     
   END.
   
   FOR EACH ekgmtrl WHERE  ekgmtrl.EKGSUBID  = tempsubid AND  ekgmtrl.EBRKAT = tempebr:
      DELETE ekgmtrl.           
   END.
   FOR EACH ekgp5 WHERE  ekgp5.EKGSUBID  = tempsubid AND  ekgp5.EBRKAT = tempebr:
      DELETE ekgp5.     
   END.
   FOR EACH ekgp5resurs WHERE  ekgp5resurs.EKGSUBID  = tempsubid AND  ekgp5resurs.EBRKAT = tempebr:
      DELETE ekgp5resurs.           
   END.
   FOR EACH ekgp4 WHERE  ekgp4.EKGSUBID  = tempsubid AND  ekgp4.EBRKAT = tempebr:
      DELETE ekgp4.           
   END.
   FOR EACH ekgp4resurs WHERE  ekgp4resurs.EKGSUBID  = tempsubid AND  ekgp4resurs.EBRKAT = tempebr:
      DELETE ekgp4resurs.           
   END.
   FOR EACH ekgp4frekvens WHERE  ekgp4frekvens.EKGSUBID  = tempsubid AND  ekgp4frekvens.EBRKAT = tempebr:
      DELETE ekgp4frekvens.           
   END.
   FOR EACH ekgp3arbkod WHERE  ekgp3arbkod.EKGSUBID  = tempsubid AND  ekgp3arbkod.EBRKAT = tempebr:
      DELETE ekgp3arbkod.       
   END.
   FOR EACH ekgp3 WHERE  ekgp3.EKGSUBID  = tempsubid AND  ekgp3.EBRKAT = tempebr:
      DELETE ekgp3.       
   END.
   FOR EACH ekgp3resurs WHERE  ekgp3resurs.EKGSUBID  = tempsubid AND  ekgp3resurs.EBRKAT = tempebr:
      DELETE ekgp3resurs.           
   END.
   FOR EACH ekgp3frekvens WHERE ekgp3frekvens.EKGSUBID  = tempsubid AND  ekgp3frekvens.EBRKAT = tempebr:
      DELETE ekgp3frekvens.            
   END.
   FOR EACH ekgp2arbkod WHERE  ekgp2arbkod.EKGSUBID  = tempsubid AND  ekgp2arbkod.EBRKAT = tempebr:
      DELETE ekgp2arbkod.       
   END.
   FOR EACH ekgp2 WHERE  ekgp2.EKGSUBID  = tempsubid AND  ekgp2.EBRKAT = tempebr:
      DELETE ekgp2.            
   END.
   FOR EACH ekgp2resurs WHERE  ekgp2resurs.EKGSUBID  = tempsubid AND  ekgp2resurs.EBRKAT = tempebr:
      DELETE ekgp2resurs.           
   END.
   FOR EACH ekgp2frekvens WHERE  ekgp2frekvens.EKGSUBID  = tempsubid AND  ekgp2frekvens.EBRKAT = tempebr:
      DELETE ekgp2frekvens.            
   END.
   FOR EACH ekgp2mtrl WHERE  ekgp2mtrl.EKGSUBID  = tempsubid AND  ekgp2mtrl.EBRKAT = tempebr:
      DELETE ekgp2mtrl.           
   END.
   FOR EACH ekgp1arbkod WHERE  ekgp1arbkod.EKGSUBID  = tempsubid AND  ekgp1arbkod.EBRKAT = tempebr:
      DELETE ekgp1arbkod.       
   END.
   FOR EACH ekgp1 WHERE  ekgp1.EKGSUBID  = tempsubid AND  ekgp1.EBRKAT = tempebr:
      DELETE ekgp1.           
   END.
   
   FOR EACH ekgp1frekvens WHERE  ekgp1frekvens.EKGSUBID  = tempsubid AND  ekgp1frekvens.EBRKAT = tempebr:
      DELETE ekgp1frekvens.           
   END.
   
END PROCEDURE.

PROCEDURE p1p2_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      CREATE ekgp1frekvenstemp.
      ASSIGN    
      ekgp1frekvenstemp.EKGSUBID  = tempsubid
      ekgp1frekvenstemp.EBRKAT = tempebr
      ekgp1frekvenstemp.P1ARBKOD = SUBSTRING(esqldat.DATAFALT[2],1,4)      
      ekgp1frekvenstemp.P1LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[2],5,2))
      ekgp1frekvenstemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[3],1,3)
      ekgp1frekvenstemp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[3],4,2))
      ekgp1frekvenstemp.ANTAL = DECIMAL(esqldat.DATAFALT[4]).                               
   END.
   /*FOR EACH ekgp1frekvenstemp :
      FIND FIRST ekgp1temp  WHERE ekgp1temp.P1ARBKOD =  ekgp1frekvenstemp.P1ARBKOD  AND ekgp1temp.P1LOPNR = ekgp1frekvenstemp.P1LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp1temp THEN DO:
         DELETE ekgp1frekvenstemp.
         /*DISPLAY "ejfrek1p1" ekgp1frekvenstemp.P1ARBKOD ekgp1frekvenstemp.P1lopnr.*/
      END.
      ELSE DO:
         FIND FIRST ekgp2temp  WHERE ekgp2temp.P2ARBKOD =  ekgp1frekvenstemp.P2ARBKOD  AND ekgp2temp.P2LOPNR = ekgp1frekvenstemp.P2LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp2temp THEN DO:
            DELETE ekgp1frekvenstemp.
            /*DISPLAY "ejfrek1p2" ekgp1frekvenstemp.P1ARBKOD ekgp1frekvenstemp.P1lopnr.*/
         END.
      END.   
   END.*/
       
  /* FOR EACH ekgp1frekvenstemp BY ekgp1frekvenstemp.P1ARBKOD:
      DISPLAY "1FREK" ekgp1frekvenstemp.P1ARBKOD ekgp1frekvenstemp.P1LOPNR ekgp1frekvenstemp.P2ARBKOD ekgp1frekvenstemp.P2LOPNR ekgp1frekvenstemp.ANTAL.
   END.*/    
   MESSAGE "p1p2".
END PROCEDURE.






PROCEDURE p1_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      IF INTEGER(SUBSTRING(esqldat.DATAFALT[1],5,2)) = 0 THEN DO:
         /*ARBETSKOD löpnr 00*/
         CREATE ekgp1arbkodtemp.
         ASSIGN    
         ekgp1arbkodtemp.EKGSUBID  = tempsubid
         ekgp1arbkodtemp.EBRKAT = tempebr
         ekgp1arbkodtemp.P1ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,4)               
         ekgp1arbkodtemp.BENAMNING = esqldat.DATAFALT[3]         
         ekgp1arbkodtemp.ANMARKNING = esqldat.DATAFALT[11].
      END.
      ELSE DO:
         CREATE ekgp1temp.
         ASSIGN    
         ekgp1temp.EKGSUBID  = tempsubid
         ekgp1temp.EBRKAT = tempebr
         ekgp1temp.P1ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,4)      
         ekgp1temp.P1LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],5,2))
         ekgp1temp.BENAMNING = esqldat.DATAFALT[3]
         ekgp1temp.ENHET = esqldat.DATAFALT[4]
         ekgp1temp.ANMARKNING = esqldat.DATAFALT[11]
         ekgp1temp.FREKVENS = LOGICAL(esqldat.DATAFALT[2])
         ekgp1temp.MASKTIM = DECIMAL(esqldat.DATAFALT[8])
         ekgp1temp.EAMASK = 0 
         ekgp1temp.EAUTRUST = 0 
         ekgp1temp.EAOVRIGT = 0
         ekgp1temp.EA = DECIMAL(esqldat.DATAFALT[9])
         ekgp1temp.MONTTIM = DECIMAL(esqldat.DATAFALT[7])
         ekgp1temp.BERTIM = DECIMAL(esqldat.DATAFALT[5])      
         ekgp1temp.UTRUSTTIM = 0     
         ekgp1temp.ARBETEKOST = DECIMAL(esqldat.DATAFALT[13])
         ekgp1temp.MATERIELKOST = DECIMAL(esqldat.DATAFALT[14])
         ekgp1temp.MASKINKOST = DECIMAL(esqldat.DATAFALT[15])
         ekgp1temp.UTRUSTKOST = DECIMAL(esqldat.DATAFALT[16])
         ekgp1temp.OVRIGT = DECIMAL(esqldat.DATAFALT[20])
         ekgp1temp.SUMMA = DECIMAL(esqldat.DATAFALT[10]).
         IF ekgp1temp.BERTIM = ? THEN ekgp1temp.BERTIM = 0.
         IF ekgp1temp.MONTTIM = ? THEN ekgp1temp.MONTTIM = 0.
         IF ekgp1temp.MASKTIM = ? THEN ekgp1temp.MASKTIM = 0.
         IF ekgp1temp.BENAMNING = ? THEN ekgp1temp.BENAMNING = "".
                                         
      END.
   END.        
   /*FOR EACH ekgp1temp BY ekgp1temp.P1ARBKOD:
      DISPLAY "ekgp1temp" ekgp1temp.P1ARBKOD ekgp1temp.P1LOPNR ekgp1temp.BENAMNING ekgp1temp.BERTIM ekgp1temp.MONTTIM ekgp1temp.MASKTIM.
   END.*/    
   MESSAGE "p1".
   
END PROCEDURE.


PROCEDURE materialfrekvens_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   /*OUTPUT TO c:\ekgmtrlfrekvensejfyställigarbkod.txt.*/
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      RUN isSiffra_UI (INPUT SUBSTRING(esqldat.DATAFALT[2],4,1), OUTPUT siffra ).
      IF siffra = TRUE THEN DO:
         CREATE ekgp2mtrltemp.
         ASSIGN    
         ekgp2mtrltemp.EKGSUBID  = tempsubid
         ekgp2mtrltemp.EBRKAT = tempebr
         ekgp2mtrltemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[2],1,3)      
         ekgp2mtrltemp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[2],4,2))
         ekgp2mtrltemp.ARTNR = esqldat.DATAFALT[3]      
         ekgp2mtrltemp.ANTAL = DECIMAL(esqldat.DATAFALT[4]).
      END.
      /*ELSE DO:
         DISPLAY esqldat.DATAFALT[1]  esqldat.DATAFALT[2] esqldat.DATAFALT[3] esqldat.DATAFALT[4].
      END.*/                                     
   END.     
   /*OUTPUT CLOSE.*/
   /*OUTPUT TO c:\ekgmtrlfrekvensejarbkodKLG1.txt.*/
   FOR EACH ekgp2mtrltemp :
      FIND FIRST ekgp2temp  WHERE ekgp2temp.P2ARBKOD =  ekgp2mtrltemp.P2ARBKOD  AND ekgp2temp.P2LOPNR = ekgp2mtrltemp.P2LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp2temp THEN DO:
         FIND FIRST ekgp2  WHERE ekgp2.P2ARBKOD =  ekgp2mtrltemp.P2ARBKOD  AND ekgp2.P2LOPNR = ekgp2mtrltemp.P2LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp2 THEN DO:
            /*borde tas bort*/
            /*DISPLAY ekgp2mtrltemp.P2ARBKOD ekgp2mtrltemp.P2lopnr ekgp2mtrltemp.artnr.*/
            DELETE ekgp2mtrltemp.
         END.   
      END.
   END.   
   /*OUTPUT CLOSE.*/
   /*OUTPUT TO c:\ekgmtrlfrekvensejartikelKLG1.txt.*/
   FOR EACH ekgp2mtrltemp :   
      FIND FIRST ekgmtrltemp  WHERE ekgmtrltemp.ARTNR =  ekgp2mtrltemp.ARTNR  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgmtrltemp THEN DO:
         FIND FIRST ekgmtrl  WHERE ekgmtrl.ARTNR =  ekgp2mtrltemp.ARTNR  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgmtrl THEN DO:
            /*DISPLAY  ekgp2mtrltemp.P2ARBKOD ekgp2mtrltemp.P2lopnr ekgp2mtrltemp.artnr.*/
            DELETE ekgp2mtrltemp.
         END.   
      END.             
   END.
   OUTPUT CLOSE.
   
   /*FOR EACH ekgp2mtrltemp BY ekgp2mtrltemp.P2ARBKOD:
      DISPLAY "2FREK" ekgp2mtrltemp.P2ARBKOD ekgp2mtrltemp.P2LOPNR ekgp2mtrltemp.ARTNR ekgp2mtrltemp.ANTAL.
   END.*/    
   MESSAGE "materielfrek".
END PROCEDURE.


PROCEDURE p2p3_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      CREATE ekgp2frekvenstemp.
      ASSIGN    
      ekgp2frekvenstemp.EKGSUBID  = tempsubid
      ekgp2frekvenstemp.EBRKAT = tempebr
      ekgp2frekvenstemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[2],1,3)      
      ekgp2frekvenstemp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[2],4,2))
      ekgp2frekvenstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[3],1,3)
      ekgp2frekvenstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[3],4,2))
      ekgp2frekvenstemp.ANTAL = DECIMAL(esqldat.DATAFALT[4]).                               
   END.     
   /*/*RENSA NÄR BÅDE P2 OCH P3 SKAPADE*/
   FOR EACH ekgp2frekvenstemp :
      FIND FIRST ekgp2temp  WHERE ekgp2temp.P2ARBKOD =  ekgp2frekvenstemp.P2ARBKOD  AND ekgp2temp.P2LOPNR = ekgp2frekvenstemp.P2LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp2temp THEN DO:
         /*borde tas bort!!*/
         DELETE ekgp2frekvenstemp.
         /*DISPLAY "ejfrek2p2" ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2lopnr.*/
      END.
   END.
   FOR EACH ekgp2frekvenstemp :
      FIND FIRST ekgp3temp  WHERE ekgp3temp.P3ARBKOD =  ekgp2frekvenstemp.P3ARBKOD  AND ekgp3temp.P3LOPNR = ekgp2frekvenstemp.P3LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp3temp THEN DO:
         /*borde tas bort!!*/
         DELETE ekgp2frekvenstemp.
         /*DISPLAY "ejfrek2p3" ekgp2frekvenstemp.P3ARBKOD ekgp2frekvenstemp.P3lopnr ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2lopnr.*/
      END.
   END.*/
      
   
   /*FOR EACH ekgp2frekvenstemp BY ekgp2frekvenstemp.P2ARBKOD:
      DISPLAY "2FREK" ekgp2frekvenstemp.P2ARBKOD ekgp2frekvenstemp.P2LOPNR ekgp2frekvenstemp.P3ARBKOD ekgp2frekvenstemp.P3LOPNR ekgp2frekvenstemp.ANTAL.
   END.*/    
   MESSAGE "p2p3".
   
END PROCEDURE.



PROCEDURE p2_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      IF INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2)) = 0 THEN DO:
         /*ARBETSKOD löpnr 00*/
         CREATE ekgp2arbkodtemp.
         ASSIGN    
         ekgp2arbkodtemp.EKGSUBID  = tempsubid
         ekgp2arbkodtemp.EBRKAT = tempebr
         ekgp2arbkodtemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)               
         ekgp2arbkodtemp.BENAMNING = esqldat.DATAFALT[3]         
         ekgp2arbkodtemp.ANMARKNING = esqldat.DATAFALT[15].
      END.
      ELSE DO:   
           
         CREATE ekgp2temp.
         ASSIGN    
         ekgp2temp.EKGSUBID  = tempsubid
         ekgp2temp.EBRKAT = tempebr
         ekgp2temp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
         ekgp2temp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
         ekgp2temp.BENAMNING = esqldat.DATAFALT[3]
         ekgp2temp.ENHET = esqldat.DATAFALT[4]
         ekgp2temp.ANMARKNING = esqldat.DATAFALT[15]
         ekgp2temp.FREKVENS = LOGICAL(esqldat.DATAFALT[2])
         ekgp2temp.MASKTIM = DECIMAL(esqldat.DATAFALT[7])
         ekgp2temp.EAMASK = DECIMAL(esqldat.DATAFALT[17])
         ekgp2temp.EAUTRUST = DECIMAL(esqldat.DATAFALT[19])
         ekgp2temp.EAOVRIGT = DECIMAL(esqldat.DATAFALT[18])
         ekgp2temp.EA = DECIMAL(esqldat.DATAFALT[8])
         ekgp2temp.MONTTIM = DECIMAL(esqldat.DATAFALT[6])
         ekgp2temp.BERTIM = DECIMAL(esqldat.DATAFALT[5])      
         ekgp2temp.UTRUSTTIM = 0     
         ekgp2temp.ARBETEKOST = DECIMAL(esqldat.DATAFALT[9])
         ekgp2temp.MATERIELKOST = DECIMAL(esqldat.DATAFALT[10])
         ekgp2temp.MASKINKOST = DECIMAL(esqldat.DATAFALT[11])
         ekgp2temp.UTRUSTKOST = DECIMAL(esqldat.DATAFALT[12])
         ekgp2temp.OVRIGT = DECIMAL(esqldat.DATAFALT[13])
         ekgp2temp.SUMMA = DECIMAL(esqldat.DATAFALT[14]).
         IF ekgp2temp.BERTIM = ? THEN ekgp2temp.BERTIM = 0.
         IF ekgp2temp.MONTTIM = ? THEN ekgp2temp.MONTTIM = 0.
         IF ekgp2temp.MASKTIM = ? THEN ekgp2temp.MASKTIM = 0.
         IF ekgp2temp.BENAMNING = ? THEN ekgp2temp.BENAMNING = "".
        
         
         /*"Tillkommande kostnad" övriga kostnader, läggs istället på resurs 61 "Övriga kostnader 100 kr"  ("ÖvrigaKostnaderutövermaskiner")*/      
         IF DECIMAL(esqldat.DATAFALT[32]) > 0 THEN DO:
            CREATE ekgp2resurstemp.
            ASSIGN 
            ekgp2resurstemp.EKGSUBID  = tempsubid
            ekgp2resurstemp.EBRKAT = tempebr
            ekgp2resurstemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
            ekgp2resurstemp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
            ekgp2resurstemp.RESURSNR = 61 
            ekgp2resurstemp.ANTAL = DECIMAL(esqldat.DATAFALT[32]) / 100.
         END.  
        
        
         IF ekgp2temp.FREKVENS = TRUE THEN DO:
            /*tillkommande bereartimmar, tidigare Justering beredrtimmar*/      
            IF DECIMAL(esqldat.DATAFALT[16]) NE 0 THEN DO: 
               CREATE ekgp2resurstemp.
               ASSIGN 
               ekgp2resurstemp.EKGSUBID  = tempsubid
               ekgp2resurstemp.EBRKAT = tempebr
               ekgp2resurstemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
               ekgp2resurstemp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
               ekgp2resurstemp.RESURSNR = 101 
               ekgp2resurstemp.ANTAL = DECIMAL(esqldat.DATAFALT[16]).
               
            END.
         END.         
         IF ekgp2temp.FREKVENS = FALSE THEN DO:
            /*bara om INTE frekvens får monttimmar och bertimmar läggas till
              bara om INTE frekvens -lägg upp resursen*/
            IF ekgp2temp.MONTTIM > 0 THEN DO: 
               CREATE ekgp2resurstemp.
               ASSIGN 
               ekgp2resurstemp.EKGSUBID  = tempsubid
               ekgp2resurstemp.EBRKAT = tempebr
               ekgp2resurstemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
               ekgp2resurstemp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
               ekgp2resurstemp.RESURSNR = 1 
               ekgp2resurstemp.ANTAL = ekgp2temp.MONTTIM.
            END.
            IF ekgp2temp.BERTIM > 0 THEN DO: 
               CREATE ekgp2resurstemp.
               ASSIGN 
               ekgp2resurstemp.EKGSUBID  = tempsubid
               ekgp2resurstemp.EBRKAT = tempebr
               ekgp2resurstemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
               ekgp2resurstemp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
               ekgp2resurstemp.RESURSNR = 17 
               ekgp2resurstemp.ANTAL = ekgp2temp.BERTIM.
            END.          
                     
            rakres = 20.
            DO WHILE rakres < 32:
               IF INTEGER(esqldat.DATAFALT[rakres]) > 0 THEN DO:
                  IF DECIMAL(esqldat.DATAFALT[rakres + 1]) > 0 THEN DO: 
                     CREATE ekgp2resurstemp.
                     ASSIGN 
                     ekgp2resurstemp.EKGSUBID  = tempsubid
                     ekgp2resurstemp.EBRKAT = tempebr
                     ekgp2resurstemp.P2ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
                     ekgp2resurstemp.P2LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
                     ekgp2resurstemp.RESURSNR = INTEGER(esqldat.DATAFALT[rakres]) 
                     ekgp2resurstemp.ANTAL = DECIMAL(esqldat.DATAFALT[rakres + 1]).
                  END.   
               END.
               rakres = rakres + 2.
            END.
         END.
      END.                          
   END.     
   /*FOR EACH ekgp2resurstemp BY ekgp2resurstemp.P2ARBKOD:
      DISPLAY "2RESURS" ekgp2resurstemp.P2ARBKOD ekgp2resurstemp.P2LOPNR ekgp2resurstemp.RESURSNR ekgp2resurstemp.ANTAL.
   END.*/    
   MESSAGE "p2".
   
END PROCEDURE.



PROCEDURE p3p4_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      CREATE ekgp3frekvenstemp.
      ASSIGN    
      ekgp3frekvenstemp.EKGSUBID  = tempsubid
      ekgp3frekvenstemp.EBRKAT = tempebr
      ekgp3frekvenstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[2],1,3)      
      ekgp3frekvenstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[2],4,2))
      ekgp3frekvenstemp.P4ARBKOD = esqldat.DATAFALT[3]
      ekgp3frekvenstemp.P4LOPNR = 0
      ekgp3frekvenstemp.ANTAL = DECIMAL(esqldat.DATAFALT[4]).                               
   END.
   /* /*OBS! Rensning måste göras efter att både ekgp3temp och ekgp3frekvenstemp har skapats*/
   FOR EACH ekgp3frekvenstemp :
      FIND FIRST ekgp3temp  WHERE ekgp3temp.P3ARBKOD =  ekgp3frekvenstemp.P3ARBKOD  AND ekgp3temp.P3LOPNR = ekgp3frekvenstemp.P3LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp3temp THEN DO:
         DISPLAY "ejfrek3" ekgp3frekvenstemp.P3ARBKOD ekgp3frekvenstemp.P3lopnr.
      END.
   END.     
   FOR EACH ekgp3frekvenstemp BY ekgp3frekvenstemp.P3ARBKOD:
      DISPLAY "3FREK" ekgp3frekvenstemp.P3ARBKOD ekgp3frekvenstemp.P3LOPNR ekgp3frekvenstemp.P4ARBKOD ekgp3frekvenstemp.P4LOPNR ekgp3frekvenstemp.ANTAL.
   END.*/   
   MESSAGE "p3p4". 
   
END PROCEDURE.


PROCEDURE p3_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      IF INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2)) = 0 THEN DO:
         /*ARBETSKOD löpnr 00*/
         CREATE ekgp3arbkodtemp.
         ASSIGN    
         ekgp3arbkodtemp.EKGSUBID  = tempsubid
         ekgp3arbkodtemp.EBRKAT = tempebr
         ekgp3arbkodtemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)               
         ekgp3arbkodtemp.BENAMNING = esqldat.DATAFALT[3]         
         ekgp3arbkodtemp.ANMARKNING = esqldat.DATAFALT[9].
      END.
      ELSE DO:   
         CREATE ekgp3temp.
         ASSIGN    
         ekgp3temp.EKGSUBID  = tempsubid
         ekgp3temp.EBRKAT = tempebr
         ekgp3temp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
         ekgp3temp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
         ekgp3temp.BENAMNING = esqldat.DATAFALT[3]
         ekgp3temp.ENHET = esqldat.DATAFALT[4]
         ekgp3temp.ANMARKNING = esqldat.DATAFALT[9]
         ekgp3temp.FREKVENS = LOGICAL(esqldat.DATAFALT[2])
         ekgp3temp.MASKTIM = DECIMAL(esqldat.DATAFALT[7])
         ekgp3temp.EAMASK = DECIMAL(esqldat.DATAFALT[13])
         ekgp3temp.EAUTRUST = DECIMAL(esqldat.DATAFALT[15])
         ekgp3temp.EAOVRIGT = DECIMAL(esqldat.DATAFALT[14])
         ekgp3temp.EA = DECIMAL(esqldat.DATAFALT[8])
         ekgp3temp.MONTTIM = DECIMAL(esqldat.DATAFALT[6])
         ekgp3temp.BERTIM = DECIMAL(esqldat.DATAFALT[5])
         ekgp3temp.ANTALMAN = INTEGER(esqldat.DATAFALT[10])
         ekgp3temp.TIDSUNDER = esqldat.DATAFALT[11]
         ekgp3temp.UTRUSTTIM = 0.
         IF ekgp3temp.BERTIM = ? THEN ekgp3temp.BERTIM = 0.
         IF ekgp3temp.MONTTIM = ? THEN ekgp3temp.MONTTIM = 0.
         IF ekgp3temp.MASKTIM = ? THEN ekgp3temp.MASKTIM = 0.
         IF ekgp3temp.BENAMNING = ? THEN ekgp3temp.BENAMNING = "".
         
         
               
         IF ekgp3temp.FREKVENS = FALSE THEN DO:
            /*bara om INTE frekvens får monttimmar och bertimmar läggas till
              bara om INTE frekvens -lägg upp resursen*/
            IF ekgp3temp.MONTTIM > 0 THEN DO: 
               CREATE ekgp3resurstemp.
               ASSIGN 
               ekgp3resurstemp.EKGSUBID  = tempsubid
               ekgp3resurstemp.EBRKAT = tempebr
               ekgp3resurstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
               ekgp3resurstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
               ekgp3resurstemp.RESURSNR = 1 
               ekgp3resurstemp.ANTAL = ekgp3temp.MONTTIM.
            END.
            IF ekgp3temp.BERTIM > 0 THEN DO: 
               CREATE ekgp3resurstemp.
               ASSIGN 
               ekgp3resurstemp.EKGSUBID  = tempsubid
               ekgp3resurstemp.EBRKAT = tempebr
               ekgp3resurstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
               ekgp3resurstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
               ekgp3resurstemp.RESURSNR = 17 
               ekgp3resurstemp.ANTAL = ekgp3temp.BERTIM.
            END.          
                     
            rakres = 16.
            DO WHILE rakres < 28:
               IF INTEGER(esqldat.DATAFALT[rakres]) > 0 THEN DO:
                  IF DECIMAL(esqldat.DATAFALT[rakres + 1]) > 0 THEN DO: 
                     CREATE ekgp3resurstemp.
                     ASSIGN 
                     ekgp3resurstemp.EKGSUBID  = tempsubid
                     ekgp3resurstemp.EBRKAT = tempebr
                     ekgp3resurstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
                     ekgp3resurstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
                     ekgp3resurstemp.RESURSNR = INTEGER(esqldat.DATAFALT[rakres]) 
                     ekgp3resurstemp.ANTAL = DECIMAL(esqldat.DATAFALT[rakres + 1]).
                  END.   
               END.
               rakres = rakres + 2.
            END.
         END.
         ELSE DO:
            /*av någon outgrundlig anledning ligger "tillkommande bertim"  i "bertim" . Detta gör att även när det inte är frekvens måste beredarresur läggas upp*/
            IF ekgp3temp.BERTIM NE 0 THEN DO: 
               CREATE ekgp3resurstemp.
               ASSIGN 
               ekgp3resurstemp.EKGSUBID  = tempsubid
               ekgp3resurstemp.EBRKAT = tempebr
               ekgp3resurstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
               ekgp3resurstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
               ekgp3resurstemp.RESURSNR = 101 
               ekgp3resurstemp.ANTAL = ekgp3temp.BERTIM.
            END.
            
            
            /*p3p4 frek körs nu innan   flyttat till efter att p3p4 frekvens skapats*/
            /*Om frekvens ibockat, men ingen frekvens upplagd skall monttimmar  läggas till   ex 825 11  */
            FIND FIRST ekgp3frekvenstemp  WHERE ekgp3frekvenstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3) AND ekgp3frekvenstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ekgp3frekvenstemp  THEN DO:         
               IF ekgp3temp.MONTTIM > 0 THEN DO: 
                  CREATE ekgp3resurstemp.
                  ASSIGN 
                  ekgp3resurstemp.EKGSUBID  = tempsubid
                  ekgp3resurstemp.EBRKAT = tempebr
                  ekgp3resurstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
                  ekgp3resurstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))                                             
                  ekgp3resurstemp.RESURSNR = 1 
                  ekgp3resurstemp.ANTAL = ekgp3temp.MONTTIM.
                  
               END.
               /*även resurs ska tas med om frekvens felaktigt i bockad*/
               rakres = 16.
               DO WHILE rakres < 28:
                  IF INTEGER(esqldat.DATAFALT[rakres]) > 0 THEN DO:
                     IF DECIMAL(esqldat.DATAFALT[rakres + 1]) > 0 THEN DO: 
                        CREATE ekgp3resurstemp.
                        ASSIGN 
                        ekgp3resurstemp.EKGSUBID  = tempsubid
                        ekgp3resurstemp.EBRKAT = tempebr
                        ekgp3resurstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
                        ekgp3resurstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
                        ekgp3resurstemp.RESURSNR = INTEGER(esqldat.DATAFALT[rakres]) 
                        ekgp3resurstemp.ANTAL = DECIMAL(esqldat.DATAFALT[rakres + 1]).
                     END.   
                  END.
                  rakres = rakres + 2.
               END.
               ekgp3temp.FREKVENS = FALSE.                           
            END.     
            
         END.
         /* borttaget 20130909 efter disskussion med Peter
         /*tillkommande bereartimmar, tidigare Justering beredrtimmar . flyttat till efter för att det inte skall bli dubbelt*/      
         IF DECIMAL(esqldat.DATAFALT[12]) > 0 THEN DO: 
            CREATE ekgp3resurstemp.
            ASSIGN 
            ekgp3resurstemp.EKGSUBID  = tempsubid
            ekgp3resurstemp.EBRKAT = tempebr
            ekgp3resurstemp.P3ARBKOD = SUBSTRING(esqldat.DATAFALT[1],1,3)      
            ekgp3resurstemp.P3LOPNR = INTEGER(SUBSTRING(esqldat.DATAFALT[1],4,2))
            ekgp3resurstemp.RESURSNR = 101 
            ekgp3resurstemp.ANTAL = DECIMAL(esqldat.DATAFALT[12]).
            /*peter, skall det vara så här??*/
            IF ekgp3temp.BERTIM = 0 THEN ekgp3temp.BERTIM = ekgp3resurstemp.ANTAL.
         END.*/ 
      END.                                  
   END.     
   /*FOR EACH ekgp3resurstemp BY ekgp3resurstemp.P3ARBKOD:
      DISPLAY "3RESURS" ekgp3resurstemp.P3ARBKOD ekgp3resurstemp.P3LOPNR ekgp3resurstemp.RESURSNR ekgp3resurstemp.ANTAL.
   END.*/
   MESSAGE "p3".    
   
END PROCEDURE.


PROCEDURE p4p5_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      CREATE ekgp4frekvenstemp.
      ASSIGN    
      ekgp4frekvenstemp.EKGSUBID  = tempsubid
      ekgp4frekvenstemp.EBRKAT = tempebr
      ekgp4frekvenstemp.P4ARBKOD = esqldat.DATAFALT[2]      
      ekgp4frekvenstemp.P4LOPNR = 0
      ekgp4frekvenstemp.P5ARBKOD = esqldat.DATAFALT[3]
      ekgp4frekvenstemp.P5LOPNR = 0
      ekgp4frekvenstemp.ANTAL = DECIMAL(esqldat.DATAFALT[4]).                               
   END.     
   /*FOR EACH ekgp4frekvenstemp :
      FIND FIRST ekgp4temp  WHERE ekgp4temp.P4ARBKOD =  ekgp4frekvenstemp.P4ARBKOD  AND ekgp4temp.P4LOPNR = ekgp4frekvenstemp.P4LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp4temp THEN DO:
         DISPLAY "ejfrek4" ekgp4frekvenstemp.P4ARBKOD ekgp4frekvenstemp.P4lopnr.
      END.
   END.   
   FOR EACH ekgp4frekvenstemp BY ekgp4frekvenstemp.P4ARBKOD:
      DISPLAY "4FREK" ekgp4frekvenstemp.P4ARBKOD ekgp4frekvenstemp.P5ARBKOD ekgp4frekvenstemp.ANTAL.
   END.*/    
   MESSAGE "p4p5".
   
END PROCEDURE.


PROCEDURE p4_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      CREATE ekgp4temp.
      ASSIGN    
      ekgp4temp.EKGSUBID  = tempsubid
      ekgp4temp.EBRKAT = tempebr
      ekgp4temp.P4ARBKOD = esqldat.DATAFALT[1]      
      ekgp4temp.P4LOPNR = 0
      ekgp4temp.BENAMNING = esqldat.DATAFALT[3]
      ekgp4temp.ENHET = esqldat.DATAFALT[4]
      ekgp4temp.ANMARKNING = esqldat.DATAFALT[9]
      ekgp4temp.FREKVENS = LOGICAL(esqldat.DATAFALT[2])
      ekgp4temp.MASKTIM = DECIMAL(esqldat.DATAFALT[7])
      ekgp4temp.EAMASK = DECIMAL(esqldat.DATAFALT[10])
      ekgp4temp.EAUTRUST = DECIMAL(esqldat.DATAFALT[11])
      ekgp4temp.EAOVRIGT = DECIMAL(esqldat.DATAFALT[12])
      ekgp4temp.EA = DECIMAL(esqldat.DATAFALT[8])
      ekgp4temp.MONTTIM = DECIMAL(esqldat.DATAFALT[6])
      ekgp4temp.BERTIM = DECIMAL(esqldat.DATAFALT[5]).
      IF ekgp4temp.BERTIM = ? THEN ekgp4temp.BERTIM = 0.
      IF ekgp4temp.MONTTIM = ? THEN ekgp4temp.MONTTIM = 0.
      IF ekgp4temp.MASKTIM = ? THEN ekgp4temp.MASKTIM = 0.
      IF ekgp4temp.BENAMNING = ? THEN ekgp4temp.BENAMNING = "".
      IF ekgp4temp.FREKVENS = FALSE THEN DO:
         /*bara om INTE frekvens får monttimmar och bertimmar läggas till
           bara om INTE frekvens -lägg upp resursen*/
         IF ekgp4temp.MONTTIM > 0 THEN DO: 
            CREATE ekgp4resurstemp.
            ASSIGN 
            ekgp4resurstemp.EKGSUBID  = tempsubid
            ekgp4resurstemp.EBRKAT = tempebr
            ekgp4resurstemp.P4ARBKOD = esqldat.DATAFALT[1]      
            ekgp4resurstemp.P4LOPNR = 0
            ekgp4resurstemp.RESURSNR = 1 
            ekgp4resurstemp.ANTAL = ekgp4temp.MONTTIM.
         END.
         IF ekgp4temp.BERTIM > 0 THEN DO: 
            CREATE ekgp4resurstemp.
            ASSIGN 
            ekgp4resurstemp.EKGSUBID  = tempsubid
            ekgp4resurstemp.EBRKAT = tempebr
            ekgp4resurstemp.P4ARBKOD = esqldat.DATAFALT[1]      
            ekgp4resurstemp.P4LOPNR = 0
            ekgp4resurstemp.RESURSNR = 17 
            ekgp4resurstemp.ANTAL = ekgp4temp.BERTIM.
         END.   
      
         rakres = 13.
         DO WHILE rakres < 25:
            IF INTEGER(esqldat.DATAFALT[rakres]) > 0 THEN DO: 
               IF DECIMAL(esqldat.DATAFALT[rakres + 1]) > 0 THEN DO:
                  CREATE ekgp4resurstemp.
                  ASSIGN 
                  ekgp4resurstemp.EKGSUBID  = tempsubid
                  ekgp4resurstemp.EBRKAT = tempebr
                  ekgp4resurstemp.P4ARBKOD = esqldat.DATAFALT[1]      
                  ekgp4resurstemp.P4LOPNR = 0
                  ekgp4resurstemp.RESURSNR = INTEGER(esqldat.DATAFALT[rakres]) 
                  ekgp4resurstemp.ANTAL = DECIMAL(esqldat.DATAFALT[rakres + 1]).
               END.   
            END.
            rakres = rakres + 2.
         END.
      END.
      /*ELSE DO:
         /*flyttat till efter skap av frekvens*/
         /*Om frekvens ibockat, men ingen frekvens upplagd skall monttimmar och bertimmar läggas till  ex 43110004 43110005 45100121   */
         FIND FIRST ekgp4frekvenstemp  WHERE ekgp4frekvenstemp.P4ARBKOD = esqldat.DATAFALT[1] NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekgp4frekvenstemp  THEN DO:         
            IF ekgp4temp.MONTTIM > 0 THEN DO: 
               CREATE ekgp4resurstemp.
               ASSIGN 
               ekgp4resurstemp.EKGSUBID  = tempsubid
               ekgp4resurstemp.EBRKAT = tempebr
               ekgp4resurstemp.P4ARBKOD = esqldat.DATAFALT[1]      
               ekgp4resurstemp.P4LOPNR = 0
               ekgp4resurstemp.RESURSNR = 1 
               ekgp4resurstemp.ANTAL = ekgp4temp.MONTTIM.
            END.
            IF ekgp4temp.BERTIM > 0 THEN DO: 
               CREATE ekgp4resurstemp.
               ASSIGN 
               ekgp4resurstemp.EKGSUBID  = tempsubid
               ekgp4resurstemp.EBRKAT = tempebr
               ekgp4resurstemp.P4ARBKOD = esqldat.DATAFALT[1]      
               ekgp4resurstemp.P4LOPNR = 0
               ekgp4resurstemp.RESURSNR = 17 
               ekgp4resurstemp.ANTAL = ekgp4temp.BERTIM.
            END.
         END.  
      END.*/                                
   END.     
   /*FOR EACH ekgp4resurstemp BY ekgp4resurstemp.P4ARBKOD:
      DISPLAY "4RESURS" ekgp4resurstemp.P4ARBKOD ekgp4resurstemp.RESURSNR ekgp4resurstemp.ANTAL.
   END.*/    
   MESSAGE "p4".
   
END PROCEDURE.


PROCEDURE p5_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      CREATE ekgp5temp.
      ASSIGN    
      ekgp5temp.EKGSUBID  = tempsubid
      ekgp5temp.EBRKAT = tempebr
      ekgp5temp.P5ARBKOD = esqldat.DATAFALT[1]      
      ekgp5temp.P5LOPNR = 0
      ekgp5temp.BENAMNING = esqldat.DATAFALT[2]
      ekgp5temp.ENHET = esqldat.DATAFALT[3]
      ekgp5temp.ANMARKNING = esqldat.DATAFALT[7]
      ekgp5temp.MASKTIM = DECIMAL(esqldat.DATAFALT[6])
      ekgp5temp.EAMASK = DECIMAL(esqldat.DATAFALT[8])
      ekgp5temp.EAUTRUST = DECIMAL(esqldat.DATAFALT[9])
      ekgp5temp.EAOVRIGT = DECIMAL(esqldat.DATAFALT[10])
      ekgp5temp.MONTTIM = DECIMAL(esqldat.DATAFALT[5])
      ekgp5temp.BERTIM = DECIMAL(esqldat.DATAFALT[4]).
      IF ekgp5temp.BENAMNING = ? THEN ekgp5temp.BENAMNING = "".
      IF ekgp5temp.BERTIM = ? THEN ekgp5temp.BERTIM = 0.
      IF ekgp5temp.MONTTIM = ? THEN ekgp5temp.MONTTIM = 0.
      IF ekgp5temp.MASKTIM = ? THEN ekgp5temp.MASKTIM = 0.      
      IF ekgp5temp.MONTTIM > 0 THEN DO: 
         CREATE ekgp5resurstemp.
         ASSIGN 
         ekgp5resurstemp.EKGSUBID  = tempsubid
         ekgp5resurstemp.EBRKAT = tempebr
         ekgp5resurstemp.P5ARBKOD = esqldat.DATAFALT[1]      
         ekgp5resurstemp.P5LOPNR = 0
         ekgp5resurstemp.RESURSNR = 1 
         ekgp5resurstemp.ANTAL = ekgp5temp.MONTTIM.
      END.
      IF ekgp5temp.BERTIM > 0 THEN DO: 
         CREATE ekgp5resurstemp.
         ASSIGN 
         ekgp5resurstemp.EKGSUBID  = tempsubid
         ekgp5resurstemp.EBRKAT = tempebr
         ekgp5resurstemp.P5ARBKOD = esqldat.DATAFALT[1]      
         ekgp5resurstemp.P5LOPNR = 0
         ekgp5resurstemp.RESURSNR = 17 
         ekgp5resurstemp.ANTAL = ekgp5temp.BERTIM.
      END.
      rakres = 11.
      DO WHILE rakres < 23:
         IF INTEGER(esqldat.DATAFALT[rakres]) > 0 THEN DO: 
            IF DECIMAL(esqldat.DATAFALT[rakres + 1]) > 0 THEN DO:
               CREATE ekgp5resurstemp.
               ASSIGN 
               ekgp5resurstemp.EKGSUBID  = tempsubid
               ekgp5resurstemp.EBRKAT = tempebr
               ekgp5resurstemp.P5ARBKOD = esqldat.DATAFALT[1]      
               ekgp5resurstemp.P5LOPNR = 0
               ekgp5resurstemp.RESURSNR = INTEGER(esqldat.DATAFALT[rakres]) 
               ekgp5resurstemp.ANTAL = DECIMAL(esqldat.DATAFALT[rakres + 1]).
            END.   
         END.
         rakres = rakres + 2.
      END.                          
   END.     
   /*FOR EACH ekgp5resurstemp:
      DISPLAY ekgp5resurstemp.P5ARBKOD ekgp5resurstemp.RESURSNR ekgp5resurstemp.ANTAL.
   END.*/    
   MESSAGE "p5".
   
END PROCEDURE.

PROCEDURE mtrl_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      CREATE ekgmtrltemp.
      ASSIGN      
      ekgmtrltemp.EKGSUBID  = tempsubid
      ekgmtrltemp.EBRKAT = tempebr
      ekgmtrltemp.ARTNR = esqldat.DATAFALT[1]
      ekgmtrltemp.BENAMNING = esqldat.DATAFALT[2]
      ekgmtrltemp.ENHET = esqldat.DATAFALT[3]
      ekgmtrltemp.PRIS = DECIMAL(esqldat.DATAFALT[4])
      ekgmtrltemp.KALKYLPRIS = DECIMAL(esqldat.DATAFALT[5])
      ekgmtrltemp.BASPRIS = DECIMAL(esqldat.DATAFALT[6])
      /*ekgmtrltemp.BASPRIS = DECIMAL(esqldat.DATAFALT[5])
      ekgmtrltemp.KALKYLPRIS = DECIMAL(esqldat.DATAFALT[6])*/
      ekgmtrltemp.LASTA = LOGICAL(esqldat.DATAFALT[7])
      ekgmtrltemp.LEVKOD = ""
      ekgmtrltemp.AKTIV = TRUE.
      /*ekgmtrltemp.AKTIV = LOGICAL(esqldat.DATAFALT[8]).*/
   END.    
   /*skapa första posten som ej kommer med manuellt*/
   IF tempsubid = 1 AND tempebr = "KLG1" THEN DO:
      CREATE ekgmtrltemp.
      ASSIGN      
      ekgmtrltemp.EKGSUBID  = tempsubid
      ekgmtrltemp.EBRKAT = tempebr
      ekgmtrltemp.ARTNR = "0001-11"
      ekgmtrltemp.BENAMNING = "Stagsats 0001/11"
      ekgmtrltemp.ENHET ="sats"
      ekgmtrltemp.PRIS = 207.24
      ekgmtrltemp.BASPRIS = 223.82
      ekgmtrltemp.KALKYLPRIS = 223.82
      ekgmtrltemp.LASTA = FALSE
      ekgmtrltemp.LEVKOD = ""
      ekgmtrltemp.AKTIV = TRUE.
   END.   
   
     
   /*FOR EACH ekgmtrltemp WHERE NO-LOCK:
      DISPLAY ekgmtrltemp.ARTNR ekgmtrltemp.PRIS.
   END.*/
   MESSAGE "mtrl".      
END PROCEDURE.

PROCEDURE niva_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   CREATE ekgnivatemp.  
   ASSIGN 
   ekgnivatemp.EKGSUBID = tempsubid
   ekgnivatemp.EBRKAT = tempebr 
   ekgnivatemp.NIVA = 1.
   CREATE ekgnivatemp.  
   ASSIGN
   ekgnivatemp.EKGSUBID = tempsubid
   ekgnivatemp.EBRKAT = tempebr 
   ekgnivatemp.NIVA = 2.
   CREATE ekgnivatemp.
   ASSIGN  
   ekgnivatemp.EKGSUBID = tempsubid
   ekgnivatemp.EBRKAT = tempebr 
   ekgnivatemp.NIVA = 3.
   CREATE ekgnivatemp.
   ASSIGN  
   ekgnivatemp.EKGSUBID = tempsubid
   ekgnivatemp.EBRKAT = tempebr 
   ekgnivatemp.NIVA = 4.
   CREATE ekgnivatemp.
   ASSIGN  
   ekgnivatemp.EKGSUBID = tempsubid
   ekgnivatemp.EBRKAT = tempebr
   ekgnivatemp.NIVA = 5.  
   MESSAGE "niva".
END PROCEDURE.

PROCEDURE resursniva_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER tempsubid2 AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr2 AS CHARACTER NO-UNDO.
   
   rakres = 1.
   DO WHILE rakres < 40:
      FIND FIRST ekgresurstemp WHERE ekgresurstemp.RESURSNR = rakres NO-LOCK NO-ERROR.
      IF AVAILABLE ekgresurstemp THEN DO:
         IF rakres NE 17 THEN DO:
            CREATE ekgresursnivatemp.  
            ASSIGN 
            ekgresursnivatemp.EKGSUBID = tempsubid
            ekgresursnivatemp.EBRKAT = tempebr
            ekgresursnivatemp.RESURSNR = rakres 
            ekgresursnivatemp.NIVA = 5
            ekgresursnivatemp.FORVALDEJFREK = FALSE 
            ekgresursnivatemp.FORVALDFREK = FALSE.
            IF rakres = 1  THEN ekgresursnivatemp.FORVALDEJFREK = TRUE.
            CREATE ekgresursnivatemp.  
            ASSIGN 
            ekgresursnivatemp.EKGSUBID = tempsubid
            ekgresursnivatemp.EBRKAT = tempebr
            ekgresursnivatemp.RESURSNR = rakres 
            ekgresursnivatemp.NIVA = 4
            ekgresursnivatemp.FORVALDEJFREK = FALSE 
            ekgresursnivatemp.FORVALDFREK = FALSE.
            IF rakres = 1  THEN ekgresursnivatemp.FORVALDEJFREK = TRUE.
         END.   
         CREATE ekgresursnivatemp.  
         ASSIGN 
         ekgresursnivatemp.EKGSUBID = tempsubid
         ekgresursnivatemp.EBRKAT = tempebr
         ekgresursnivatemp.RESURSNR = rakres 
         ekgresursnivatemp.NIVA = 3
         ekgresursnivatemp.FORVALDEJFREK = FALSE 
         ekgresursnivatemp.FORVALDFREK = FALSE.
         IF rakres = 1 OR rakres = 17 THEN ekgresursnivatemp.FORVALDEJFREK = TRUE.
         CREATE ekgresursnivatemp.  
         ASSIGN 
         ekgresursnivatemp.EKGSUBID = tempsubid
         ekgresursnivatemp.EBRKAT = tempebr
         ekgresursnivatemp.RESURSNR = rakres 
         ekgresursnivatemp.NIVA = 2
         ekgresursnivatemp.FORVALDEJFREK = FALSE 
         ekgresursnivatemp.FORVALDFREK = FALSE.
         IF rakres = 1 OR rakres = 17 THEN ekgresursnivatemp.FORVALDEJFREK = TRUE.
      END.   
      rakres = rakres + 1.
   END.    
   
   /*rakres = 60.
   DO WHILE rakres < 89:
      CREATE ekgresursnivatemp.  
      ASSIGN 
      ekgresursnivatemp.EKGSUBID = tempsubid
      ekgresursnivatemp.EBRKAT = tempebr
      ekgresursnivatemp.RESURSNR = rakres 
      ekgresursnivatemp.NIVA = 2
      ekgresursnivatemp.FORVALDEJFREK = FALSE 
      ekgresursnivatemp.FORVALDFREK = FALSE.
      rakres = rakres + 1.
   END.*/
   CREATE ekgresursnivatemp.  
   ASSIGN 
   ekgresursnivatemp.EKGSUBID = tempsubid
   ekgresursnivatemp.EBRKAT = tempebr
   ekgresursnivatemp.RESURSNR = 101 
   ekgresursnivatemp.NIVA = 3
   ekgresursnivatemp.FORVALDEJFREK = FALSE 
   ekgresursnivatemp.FORVALDFREK = TRUE .
   
   CREATE ekgresursnivatemp.  
   ASSIGN 
   ekgresursnivatemp.EKGSUBID = tempsubid
   ekgresursnivatemp.EBRKAT = tempebr
   ekgresursnivatemp.RESURSNR = 101 
   ekgresursnivatemp.NIVA = 2
   ekgresursnivatemp.FORVALDEJFREK = FALSE 
   ekgresursnivatemp.FORVALDFREK = TRUE .
   IF tempebr = klgklass THEN DO:
      FOR EACH ekgresursnivatemp WHERE ekgresursnivatemp.EKGSUBID  = tempsubid and ekgresursnivatemp.EBRKAT = tempebr:
         IF ekgresursnivatemp.NIVA = 1 OR ekgresursnivatemp.NIVA = 2 OR ekgresursnivatemp.NIVA = 3 THEN DO:
            /*NIVÅ 4 OCH 5 FINNS EJ I KLG2*/
            CREATE ekgresursnivabuff.
            BUFFER-COPY ekgresursnivatemp TO ekgresursnivabuff.
            ASSIGN 
            ekgresursnivabuff.EKGSUBID  = tempsubid2
            ekgresursnivabuff.EBRKAT  = tempebr2.
         END.   
      END.
   END.
   /*FOR EACH  ekgresursnivatemp:
      DISPLAY ekgresursnivatemp.RESURSNR ekgresursnivatemp.NIVA ekgresursnivatemp.FORVALDEJFREK ekgresursnivatemp.FORVALDFREK.
   END.*/   
   MESSAGE "resursniva".
END PROCEDURE.

PROCEDURE regel_UI:
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   
   CREATE ekgregeltemp.
   ASSIGN 
   ekgregeltemp.EKGSUBID = tempsubid
   ekgregeltemp.EBRKAT = tempebr
   ekgregeltemp.REGELID  = 1
   ekgregeltemp.TIMSUMKOD = "Arb.kost."
   ekgregeltemp.EASORTKLG1 = "ea"   
   ekgregeltemp.UTOVERFREK = FALSE 
   ekgregeltemp.EAFAKTOR = FALSE 
   ekgregeltemp.ANMARKNING = "Montör EA".
   CREATE ekgregeltemp.
   ASSIGN 
   ekgregeltemp.EKGSUBID = tempsubid
   ekgregeltemp.EBRKAT = tempebr
   ekgregeltemp.REGELID  = 2
   ekgregeltemp.TIMSUMKOD = "Mask.kost."
   ekgregeltemp.EASORTKLG1 = "eamask"   
   ekgregeltemp.UTOVERFREK = FALSE 
   ekgregeltemp.EAFAKTOR = FALSE 
   ekgregeltemp.ANMARKNING = "Maskin EA".
   CREATE ekgregeltemp.
   ASSIGN 
   ekgregeltemp.EKGSUBID = tempsubid
   ekgregeltemp.EBRKAT = tempebr
   ekgregeltemp.REGELID  = 3
   ekgregeltemp.TIMSUMKOD = "Övr.kost."
   ekgregeltemp.EASORTKLG1 = "eaövrigt"   
   ekgregeltemp.UTOVERFREK = FALSE 
   ekgregeltemp.EAFAKTOR = FALSE 
   ekgregeltemp.ANMARKNING = "Övrigt EA".
   /* används ej KLG1 
   CREATE ekgregeltemp.
   ASSIGN 
   ekgregeltemp.EKGSUBID = tempsubid
   ekgregeltemp.EBRKAT = tempebr
   ekgregeltemp.REGELID  = 4
   ekgregeltemp.TIMSUMKOD = "Övr.kost."
   /*ekgregeltemp.TIMSUMKOD = "Utr.kost."*/
   ekgregeltemp.EASORTKLG1 = "eaövrigt"
   /*ekgregeltemp.EASORTKLG1 = "eautrust"*/
   ekgregeltemp.UTOVERFREK = FALSE 
   ekgregeltemp.EAFAKTOR = FALSE 
   ekgregeltemp.ANMARKNING = "Utrust EA".*/
   CREATE ekgregeltemp.
   ASSIGN 
   ekgregeltemp.EKGSUBID = tempsubid
   ekgregeltemp.EBRKAT = tempebr
   ekgregeltemp.REGELID  = 5
   ekgregeltemp.TIMSUMKOD = ""
   ekgregeltemp.EASORTKLG1 = "eaövrigt"   
   ekgregeltemp.UTOVERFREK = FALSE 
   ekgregeltemp.EAFAKTOR = true 
   ekgregeltemp.ANMARKNING = "Eafaktor".
   CREATE ekgregeltemp.
   ASSIGN 
   ekgregeltemp.EKGSUBID = tempsubid
   ekgregeltemp.EBRKAT = tempebr
   ekgregeltemp.REGELID  = 6
   ekgregeltemp.TIMSUMKOD = "Arb.kost."
   ekgregeltemp.EASORTKLG1 = ""
   ekgregeltemp.EASORTKLG2 = ""
   ekgregeltemp.UTOVERFREK = FALSE 
   ekgregeltemp.EAFAKTOR = FALSE 
   ekgregeltemp.ANMARKNING = "Beredare".
   CREATE ekgregeltemp.
   ASSIGN 
   ekgregeltemp.EKGSUBID = tempsubid
   ekgregeltemp.EBRKAT = tempebr
   ekgregeltemp.REGELID  = 7
   ekgregeltemp.TIMSUMKOD = "Arb.kost."
   ekgregeltemp.EASORTKLG1 = ""
   ekgregeltemp.EASORTKLG2 = ""
   ekgregeltemp.UTOVERFREK = TRUE  
   ekgregeltemp.EAFAKTOR = FALSE 
   ekgregeltemp.ANMARKNING = "Tillk.bererdare".
   CREATE ekgregeltemp.
   ASSIGN 
   ekgregeltemp.EKGSUBID = tempsubid
   ekgregeltemp.EBRKAT = tempebr
   ekgregeltemp.REGELID  = 8
   ekgregeltemp.TIMSUMKOD = "Övr.kost."
   ekgregeltemp.EASORTKLG1 = ""
   ekgregeltemp.EASORTKLG2 = ""
   ekgregeltemp.UTOVERFREK = TRUE  
   ekgregeltemp.EAFAKTOR = FALSE 
   ekgregeltemp.ANMARKNING = "Övrigt".
   MESSAGE "regel".
END PROCEDURE.   

PROCEDURE paslag_UI:
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   CREATE ekgpaslagtemp.
   ASSIGN     
   ekgpaslagtemp.EKGSUBID  = tempsubid
   ekgpaslagtemp.EBRKAT = tempebr
   ekgpaslagtemp.PASLAGNR =  1 
   ekgpaslagtemp.BENAMNING =  "Påslag för materielhanteringomkostnader" 
   ekgpaslagtemp.PASLAG = 0.08.
   CREATE ekgpaslagtemp.
   ASSIGN     
   ekgpaslagtemp.EKGSUBID  = tempsubid
   ekgpaslagtemp.EBRKAT = tempebr
   ekgpaslagtemp.PASLAGNR =  2 
   ekgpaslagtemp.BENAMNING =  "Maskinomkostnadstillägg" 
   ekgpaslagtemp.PASLAG = 0.0.
   CREATE ekgpaslagtemp.
   ASSIGN     
   ekgpaslagtemp.EKGSUBID  = tempsubid
   ekgpaslagtemp.EBRKAT = tempebr
   ekgpaslagtemp.PASLAGNR =  3 
   ekgpaslagtemp.BENAMNING =  "Omkostnadspåslag montör" 
   ekgpaslagtemp.PASLAG = 1.52.
   CREATE ekgpaslagtemp.
   ASSIGN     
   ekgpaslagtemp.EKGSUBID  = tempsubid
   ekgpaslagtemp.EBRKAT = tempebr
   ekgpaslagtemp.PASLAGNR =  4 
   ekgpaslagtemp.BENAMNING =  "Omkostnadspåslag beredare" 
   ekgpaslagtemp.PASLAG = 1.52.
   CREATE ekgpaslagtemp.
   ASSIGN     
   ekgpaslagtemp.EKGSUBID  = tempsubid
   ekgpaslagtemp.EBRKAT = tempebr
   ekgpaslagtemp.PASLAGNR =  5 
   ekgpaslagtemp.BENAMNING =  "Faktor för uppräkning av övriga kostnader" 
   ekgpaslagtemp.PASLAG = 0.0224.
   
   CREATE ekgresurspaslagtemp.
   ASSIGN     
   ekgresurspaslagtemp.EKGSUBID  = tempsubid
   ekgresurspaslagtemp.EBRKAT = tempebr
   ekgresurspaslagtemp.RESURSNR = 1
   ekgresurspaslagtemp.PASLAGNR =  3.
   CREATE ekgresurspaslagtemp.
   ASSIGN     
   ekgresurspaslagtemp.EKGSUBID  = tempsubid
   ekgresurspaslagtemp.EBRKAT = tempebr
   ekgresurspaslagtemp.RESURSNR = 17
   ekgresurspaslagtemp.PASLAGNR =  4.
   CREATE ekgresurspaslagtemp.
   ASSIGN     
   ekgresurspaslagtemp.EKGSUBID  = tempsubid
   ekgresurspaslagtemp.EBRKAT = tempebr
   ekgresurspaslagtemp.RESURSNR = 6
   ekgresurspaslagtemp.PASLAGNR =  2.
   CREATE ekgresurspaslagtemp.
   ASSIGN     
   ekgresurspaslagtemp.EKGSUBID  = tempsubid
   ekgresurspaslagtemp.EBRKAT = tempebr
   ekgresurspaslagtemp.RESURSNR = 9
   ekgresurspaslagtemp.PASLAGNR =  2.
   
   CREATE ekgresurspaslagtemp.
   ASSIGN     
   ekgresurspaslagtemp.EKGSUBID  = tempsubid
   ekgresurspaslagtemp.EBRKAT = tempebr
   ekgresurspaslagtemp.RESURSNR = 18
   ekgresurspaslagtemp.PASLAGNR =  2. 
   CREATE ekgresurspaslagtemp.
   ASSIGN     
   ekgresurspaslagtemp.EKGSUBID  = tempsubid
   ekgresurspaslagtemp.EBRKAT = tempebr
   ekgresurspaslagtemp.RESURSNR = 101
   ekgresurspaslagtemp.PASLAGNR =  4.
   
   MESSAGE "paslag".
END PROCEDURE.

PROCEDURE resurs_UI:
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER tempsubid2 AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr2 AS CHARACTER NO-UNDO.
   FOR EACH esqldat WHERE esqldat.TABNAMN = sqltab.TABNAMN:
      CREATE ekgresurstemp.
      ASSIGN
      ekgresurstemp.RESURSNR  = INTEGER(esqldat.DATAFALT[1]).
      ekgresurstemp.BENAMNING  = esqldat.DATAFALT[2].
      ekgresurstemp.ENHET = "tim".
      /*ekgresurstemp.REGELID = 1.*/
      ekgresurstemp.AKTIV = TRUE.
      ekgresurstemp.ANMARKNING = esqldat.DATAFALT[5].
      IF ekgresurstemp.RESURSNR = 21 THEN ekgresurstemp.ENHET = "dag".
      CREATE ekgresurspristemp.
      ASSIGN
      ekgresurspristemp.EKGSUBID  = tempsubid
      ekgresurspristemp.EBRKAT  = tempebr
      ekgresurspristemp.RESURSNR  = INTEGER(esqldat.DATAFALT[1])
      ekgresurspristemp.PRIS  = DECIMAL(esqldat.DATAFALT[3])
      ekgresurspristemp.EA = DECIMAL(esqldat.DATAFALT[4])
      ekgresurspristemp.REGELID = 3.
      
      IF ekgresurspristemp.RESURSNR = 1 THEN ekgresurspristemp.REGELID = 1.
      IF ekgresurspristemp.RESURSNR = 3 THEN ekgresurspristemp.REGELID = 5.
      IF ekgresurspristemp.RESURSNR = 6 THEN ekgresurspristemp.REGELID = 2.
      IF ekgresurspristemp.RESURSNR = 9 THEN ekgresurspristemp.REGELID = 2.
      IF ekgresurspristemp.RESURSNR = 18 THEN ekgresurspristemp.REGELID = 2.
      IF ekgresurspristemp.RESURSNR = 17 THEN ekgresurspristemp.REGELID = 6.            
      
      
      /* montör = ea faktor 1 beredare ej ea*/
      IF ekgresurspristemp.RESURSNR = 1 THEN ekgresurspristemp.EA = 1.
      IF ekgresurspristemp.RESURSNR = 17 THEN ekgresurspristemp.EA = 0.
            
                     
   END.   
   
   
   CREATE ekgresurstemp.
   ASSIGN
   ekgresurstemp.RESURSNR  = 101.
   ekgresurstemp.BENAMNING  = "Tillkommande beredning".
   ekgresurstemp.ENHET = "tim".
   /*ekgresurstemp.REGELID = 7.*/
   ekgresurstemp.AKTIV = TRUE.
   ekgresurstemp.ANMARKNING = "".
   FIND FIRST ekgresursprisbuff WHERE ekgresursprisbuff.EKGSUBID  = tempsubid AND ekgresursprisbuff.EBRKAT  = tempebr AND ekgresursprisbuff.RESURSNR  = 17  NO-LOCK NO-ERROR.
   IF AVAILABLE ekgresursprisbuff THEN DO:
      CREATE ekgresurspristemp.
      BUFFER-COPY ekgresursprisbuff TO ekgresurspristemp.
      ASSIGN ekgresurspristemp.resursnr = 101
      ekgresurspristemp.REGELID = 7.
   END.
   CREATE ekgresurstemp.
   ASSIGN
   ekgresurstemp.RESURSNR  = 37.
   ekgresurstemp.BENAMNING  = "Lastbil med kran".
   ekgresurstemp.ENHET = "tim".
   /*ekgresurstemp.REGELID = 4.*/
   ekgresurstemp.AKTIV = TRUE.
   ekgresurstemp.ANMARKNING = "".
   CREATE ekgresurspristemp.
   ASSIGN
   ekgresurspristemp.EKGSUBID  = tempsubid
   ekgresurspristemp.EBRKAT  = tempebr
   ekgresurspristemp.RESURSNR  = 37
   ekgresurspristemp.PRIS  = 810.6507
   ekgresurspristemp.EA = 2.207654412
   ekgresurspristemp.REGELID = 3.
   CREATE ekgresurstemp.
   ASSIGN
   ekgresurstemp.RESURSNR  = 38.
   ekgresurstemp.BENAMNING  = "Termografering".
   ekgresurstemp.ENHET = "tim".
   /*ekgresurstemp.REGELID = 4.*/
   ekgresurstemp.AKTIV = TRUE.
   ekgresurstemp.ANMARKNING = "".
   CREATE ekgresurspristemp.
   ASSIGN
   ekgresurspristemp.EKGSUBID  = tempsubid
   ekgresurspristemp.EBRKAT  = tempebr
   ekgresurspristemp.RESURSNR  = 38
   ekgresurspristemp.PRIS  = 504.4317   
   ekgresurspristemp.EA = 1.37372467320261
   ekgresurspristemp.REGELID = 3.
   CREATE ekgresurstemp.
   ASSIGN
   ekgresurstemp.RESURSNR  = 39.
   ekgresurstemp.BENAMNING  = "Lyftkran, stor".
   ekgresurstemp.ENHET = "tim".
   /*ekgresurstemp.REGELID = 4.*/
   ekgresurstemp.AKTIV = TRUE.
   ekgresurstemp.ANMARKNING = "".
   CREATE ekgresurspristemp.
   ASSIGN
   ekgresurspristemp.EKGSUBID  = tempsubid
   ekgresurspristemp.EBRKAT  = tempebr
   ekgresurspristemp.RESURSNR  = 39
   ekgresurspristemp.PRIS  = 5023.3068
   ekgresurspristemp.EA = 13.6800294117647
   ekgresurspristemp.REGELID = 3.
   IF tempebr = klgklass THEN DO:
      FOR EACH ekgresurspristemp WHERE ekgresurspristemp.EKGSUBID  = tempsubid and ekgresurspristemp.EBRKAT = tempebr:
         CREATE ekgresursprisbuff.
         BUFFER-COPY ekgresurspristemp TO ekgresursprisbuff.
         ASSIGN 
         ekgresursprisbuff.EKGSUBID  = tempsubid2
         ekgresursprisbuff.EBRKAT  = tempebr2.
         
         ekgresursprisbuff.REGELID = 3.
      
         IF ekgresursprisbuff.RESURSNR = 1 THEN ekgresursprisbuff.REGELID = 1.
         IF ekgresursprisbuff.RESURSNR = 3 THEN ekgresursprisbuff.REGELID = 5.
         IF ekgresursprisbuff.RESURSNR = 6 THEN ekgresursprisbuff.REGELID = 2.
         IF ekgresursprisbuff.RESURSNR = 9 THEN ekgresursprisbuff.REGELID = 2.
         IF ekgresursprisbuff.RESURSNR = 18 THEN ekgresursprisbuff.REGELID = 2.         
         IF ekgresursprisbuff.RESURSNR = 17 THEN ekgresursprisbuff.REGELID = 6.
         IF ekgresursprisbuff.RESURSNR = 101 THEN ekgresursprisbuff.REGELID = 7.
         
         IF ekgresursprisbuff.RESURSNR = 4 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 5 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 8 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 10 THEN ekgresursprisbuff.REGELID = 4.         
         IF ekgresursprisbuff.RESURSNR = 12 THEN ekgresursprisbuff.REGELID = 4.         
         IF ekgresursprisbuff.RESURSNR = 19 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 20 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 21 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 24 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 37 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 38 THEN ekgresursprisbuff.REGELID = 4.
         IF ekgresursprisbuff.RESURSNR = 39 THEN ekgresursprisbuff.REGELID = 4.         
                  
          
      END.
   END.      
   
   /*FOR EACH ekgresurstemp:
      DISPLAY ekgresurstemp.RESURSNR ekgresurstemp.BENAMNING ekgresurstemp.ENHET.
   END.*/
  /* FOR EACH ekgresurspristemp:
      DISPLAY "EKGresurspristemp" ekgresurspristemp.EKGSUBID ekgresurspristemp.EBRKAT ekgresurspristemp.RESURSNR ekgresurspristemp.PRIS FORMAT ">>>>99.99<" ekgresurspristemp.EA FORMAT ">>>9.999999".
   END.*/
   MESSAGE "resurs".
     
END PROCEDURE.




PROCEDURE close_UI :
   IF odbcvar = TRUE THEN DO:
      IF VALID-HANDLE(odbch) THEN RUN avslut_UI IN odbch.
      DELETE PROCEDURE odbch NO-ERROR.
      RELEASE OBJECT hCurrdb NO-ERROR.
      RETURN.
   END.
   IF VALID-HANDLE(hAccess) THEN DO:
      hAccess:CloseCurrentDatabase().
      RELEASE OBJECT hfield NO-ERROR.
      hfield = ?.
      RELEASE OBJECT htable NO-ERROR.
      htable = ?.
      RELEASE OBJECT htable2 NO-ERROR.
      htable2 = ?.
      RELEASE OBJECT hCurrdb NO-ERROR.
      hCurrdb = ?.
      hAccess:QUIT(2).
      RELEASE OBJECT hAccess NO-ERROR.
      hAccess = ?.
   END.
   
END PROCEDURE.


PROCEDURE isSiffra_UI :
   DEFINE INPUT PARAMETER varde AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER artecken AS LOGICAL NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
   
   ascivarde = ASC(varde).
   /*siffror*/
   IF ascivarde >= 48 AND ascivarde <= 57 THEN artecken = TRUE.
   
   /*/*stora bokstäver*/
   ELSE IF ascivarde >= 65 AND ascivarde <= 90 THEN artecken = TRUE.
   
   /*små bokstäver*/
   ELSE IF ascivarde >= 97 AND ascivarde <= 122 THEN artecken = TRUE.
   
   /*Å Ä Ö å ä ö*/
   ELSE IF ascivarde = 197 OR ascivarde = 196 OR ascivarde = 214 OR ascivarde = 229 OR 
      ascivarde = 228 OR ascivarde = 246 THEN artecken = TRUE.
   ELSE artecken = FALSE.*/
END PROCEDURE.



