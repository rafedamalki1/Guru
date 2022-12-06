/*XGRANST.P Import av störningar GRANINGE ACCESS*/       
DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE dlcvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE wtidvar AS CHARACTER NO-UNDO.



DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE nrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE nat1 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat2 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat3 AS INTEGER NO-UNDO.
DEFINE VARIABLE nat4 AS INTEGER NO-UNDO.
DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE stornr AS INTEGER NO-UNDO.
DEFINE VARIABLE driftvar LIKE STORNINGSTAB.STORTYPID NO-UNDO.
DEFINE VARIABLE AA AS INTEGER NO-UNDO.
DEFINE VARIABLE BB AS INTEGER NO-UNDO.
DEFINE VARIABLE CC AS INTEGER NO-UNDO.
DEFINE VARIABLE DD AS INTEGER NO-UNDO.
DEFINE VARIABLE specvar AS LOGICAL NO-UNDO.

DEFINE VARIABLE startdatum AS DATE NO-UNDO.
DEFINE VARIABLE starttid LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE totavbrott AS DECIMAL NO-UNDO.
DEFINE VARIABLE totbortfall AS DECIMAL NO-UNDO.
DEFINE VARIABLE lspantal LIKE STORNINGSTAB.ANTALLSP NO-UNDO.
DEFINE VARIABLE hspantal LIKE STORNINGSTAB.ANTALLSP NO-UNDO.

DEFINE VARIABLE datum70 AS DATE NO-UNDO.
DEFINE VARIABLE datum30 AS DATE NO-UNDO.
DEFINE VARIABLE tid70 LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE tid30 LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE timmar100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE timmar60 AS DECIMAL NO-UNDO.
DEFINE VARIABLE datum1 AS DATE NO-UNDO.
DEFINE VARIABLE datum2 AS DATE NO-UNDO.
DEFINE VARIABLE tid1 LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE tid2 LIKE TIDREGITAB.START NO-UNDO.
DEFINE VARIABLE bortavbrott AS DECIMAL NO-UNDO.
DEFINE VARIABLE mestbort AS INTEGER NO-UNDO.
DEFINE VARIABLE bortfallrec AS RECID NO-UNDO.
DEFINE VARIABLE divmestbort AS DECIMAL NO-UNDO.

DEFINE TEMP-TABLE tidin   
   FIELD F1                 AS DATE /*rapportdatum*/
   FIELD F2                 AS CHARACTER /*klockan*/
   FIELD F3                 AS CHARACTER /*mottagit av*/
   FIELD F4                 AS CHARACTER /*produktionsenhet*/
   FIELD F5                 AS CHARACTER /*anmält av*/
   FIELD F6                 AS CHARACTER /*telenr*/
   FIELD F7                 AS CHARACTER /*adress*/
   FIELD F8                 AS CHARACTER /*postnr*/
   FIELD F9                 AS CHARACTER /*ort*/
   FIELD F10                AS CHARACTER /*fel uppgivet*/
   FIELD F11                AS CHARACTER /*adress för fel*/
   FIELD F12                AS CHARACTER /*postnr för fel*/
   FIELD F13                AS CHARACTER /*ort för fel*/
   FIELD F14                AS LOGICAL /*planerat avbrott*/
   FIELD F15                AS LOGICAL /*driftstörning*/
   FIELD F16                AS DATE /*start datum*/
   FIELD F17                AS CHARACTER /*start tid*/
   FIELD F18                AS DATE /*slutdatum*/
   FIELD F19                AS CHARACTER /*slut tid*/
   FIELD F20                AS CHARACTER /*Montör*/
   FIELD F21                AS CHARACTER /*arbetsledare*/
   FIELD F22                AS CHARACTER /*kontroll klockan*/
   FIELD F23                AS CHARACTER /*berörda anläggningsdelar*/
   FIELD F24                AS INTEGER /*antal HSP*/
   FIELD F25                AS INTEGER /*antal LSP*/
   FIELD F26                AS INTEGER /*bort kopplad belastning*/
   FIELD F27                AS CHARACTER /*anmärkning*/
   FIELD F28                AS CHARACTER /*spänningsnivå*/
   FIELD F29                AS CHARACTER /*Avbrottstid*/
   FIELD F30                AS CHARACTER /*M-station*/
   FIELD F31                AS CHARACTER /*Hsp-linje*/
   FIELD F32                AS CHARACTER /*N-station*/
   FIELD F33                AS CHARACTER /*Skåp-/apparat*/
   FIELD F34                AS CHARACTER /*linje/grupp*/
   FIELD F35                AS CHARACTER /*kabelskåp nr:*/
   FIELD F36                AS LOGICAL /*ledning*/
   FIELD F37                AS LOGICAL /*apparat fel*/
   FIELD F38                AS LOGICAL /*Transformator*/
   FIELD F39                AS LOGICAL /*Stolpe*/
   FIELD F40                AS LOGICAL /*Mätarskåp*/
   FIELD F41                AS LOGICAL /*servis*/
   FIELD F42                AS LOGICAL /*Övrigt fel*/
   FIELD F43                AS LOGICAL /*reläskydd*/
   FIELD F44                AS LOGICAL /*övrigt skydd*/
   FIELD F45                AS CHARACTER /*Säkringstyp*/
   FIELD F46                AS CHARACTER /*Säkringstyp*/
   FIELD F47                AS CHARACTER /*Säkringstyp*/
   FIELD F48                AS CHARACTER /*Säkringstyp*/
   FIELD F49                AS CHARACTER /*Säkringstyp*/
   FIELD F50                AS CHARACTER /*Säkringstyp*/
   FIELD F51                AS LOGICAL /*jordslutning*/
   FIELD F52                AS LOGICAL /*kortslutning*/
   FIELD F53                AS LOGICAL /*överlast*/
   FIELD F54                AS LOGICAL /*Åverkan*/
   FIELD F55                AS LOGICAL /*felkoppling*/
   FIELD F56                AS LOGICAL /*is/snölast*/
   FIELD F57                AS LOGICAL /*trafik*/
   FIELD F58                AS LOGICAL /*åska*/
   FIELD F59                AS LOGICAL /*blåst*/
   FIELD F60                AS LOGICAL /*natur*/
   FIELD F61                AS LOGICAL /*materiel*/
   FIELD F62                AS LOGICAL /*okänd*/
   FIELD F63                AS LOGICAL /*jordkabel*/
   FIELD F64                AS LOGICAL /*hängkabel*/
   FIELD F65                AS LOGICAL /*BLX*/
   FIELD F66                AS LOGICAL /*friledning*/
   FIELD F67                AS LOGICAL /*kundanläggning*/
   FIELD F68                AS LOGICAL /*prov rep kundanläggning*/
   FIELD F69                AS LOGICAL /*belysning*/
   FIELD F70                AS CHARACTER /*anmärkning teknik*/
   FIELD F71                AS CHARACTER /*anmälan nr:*/
   FIELD KLOCK1             AS DECIMAL
   FIELD KLOCK2             AS DECIMAL
   FIELD KLOCK3             AS DECIMAL
   FIELD FEL                AS LOGICAL
   INDEX DAT FEL F16 KLOCK2 F18 KLOCK3
   INDEX FEL FEL.
   
   
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
   
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE TEMP-TABLE temp_text
   FIELD B1 AS INTEGER /*företag*/
   FIELD B2 AS CHARACTER /*distrikt*/
   FIELD PROGNAMN AS CHARACTER FORMAT "X(100)". 

DEFINE BUFFER distbuff FOR STORDISTRIKT.
DEFINE BUFFER tidbuff FOR tidin.

DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.

FUNCTION klockan100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL) :
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 
END FUNCTION.

FUNCTION klockan60 RETURNS DECIMAL
  (INPUT ber100 AS DECIMAL) :
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60.
END FUNCTION.

{muswait.i}         
{AMERICANEUROPEAN.I}
   filnamn = "\\pc112\delad\elpool\elpnj\graninge\avbr2.skv".
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.     
   RUN PROVAG.P.
   ASSIGN
   dlcvar = dlcvar + "QUOTER.EXE"
   wtidvar = "\\pc112\delad\elpool\elpnj\graninge\storning.q".   
   
   OS-COMMAND VALUE(dlcvar)
   VALUE(filnamn) > VALUE(wtidvar).   
   INPUT FROM VALUE(wtidvar) NO-ECHO.    
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      DO TRANSACTION: 
         SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 80 WITH FRAME DDD WIDTH 80.   
         CREATE intid.   
         ASSIGN intid.TIN = words.   
      END.
   END.
   INPUT CLOSE.               
   OUTPUT TO VALUE(wtidvar).
   FOR EACH intid:          
      PUT UNFORMATTED intid.TIN SKIP.     
   END.
   OUTPUT CLOSE.
   INPUT FROM VALUE(wtidvar) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.         
/*    OS-DELETE VALUE(wtidvar).                                                                                                             */
/*    OUTPUT TO "\\pc112\delad\elpool\elpNJ\GRANINGE\PELLE.TXT"                                                                                */
/*    CONVERT TARGET "iso8859-1" SOURCE "iso8859-1"                                                                                         */
/*    NO-ECHO.                                                                                                                              */
/*    FOR EACH tidin:                                                                                                                       */
/*       IF tidin.F17 = "" THEN DO:                                                                                                         */
/*          tidin.FEL = TRUE.                                                                                                               */
/*       END.                                                                                                                               */
/*       IF tidin.F18 = ? THEN DO:                                                                                                          */
/*          IF tidin.F16 = ? THEN DO:                                                                                                       */
/*             tidin.FEL = TRUE.                                                                                                            */
/*          END.                                                                                                                            */
/*       END.                                                                                                                               */
/*       IF tidin.F19 = "" THEN DO:                                                                                                         */
/*          tidin.FEL = TRUE.                                                                                                               */
/*       END.                                                                                                                               */
/*       IF tidin.F28 = "" THEN DO:                                                                                                         */
/*          tidin.FEL = TRUE.                                                                                                               */
/*       END.                                                                                                                               */
/*       IF tidin.FEL = TRUE THEN DO:                                                                                                       */
/*          PUT UNFORMATTED tidin.F1 ";" tidin.F4 ";" tidin.F16 ";" tidin.F17 ";" tidin.F18 ";" tidin.F19 ";" tidin.F28 ";" tidin.F71 SKIP. */
/*       END.                                                                                                                               */
/*       ELSE DO:                                                                                                                           */
/*          IF tidin.F2 = "" THEN DO:                                                                                                       */
/*             tidin.F2 = tidin.F17.                                                                                                        */
/*          END.                                                                                                                            */
/*          ASSIGN                                                                                                                          */
/*          tidin.F2 = REPLACE(tidin.F2,":",".")                                                                                            */
/*          tidin.KLOCK1 = DECIMAL(STRING(tidin.F2))                                                                                        */
/*          tidin.F17 = REPLACE(tidin.F17,":",".")                                                                                          */
/*          tidin.KLOCK2 = DECIMAL(STRING(tidin.F17))                                                                                       */
/*          tidin.F19 = REPLACE(tidin.F19,":",".")                                                                                          */
/*          tidin.KLOCK3 = DECIMAL(STRING(tidin.F19)).                                                                                      */
/*          IF tidin.F18 = ? THEN DO:                                                                                                       */
/*             tidin.F18 = tidin.F16.                                                                                                       */
/*          END.                                                                                                                            */
/*       END.                                                                                                                               */
/*    END.                                                                                                                                  */
/*    OUTPUT CLOSE.                                                                                                                         */
/*                                                                                                                                          */
/*    FOR EACH tidin WHERE tidin.fel = FALSE USE-INDEX DAT:                                                                                 */
/*       FIND LAST STORNINGSTAB                                                                                                             */
/*       USE-INDEX STORNUMMERID NO-LOCK NO-ERROR.                                                                                           */
/*       IF AVAILABLE STORNINGSTAB THEN DO:                                                                                                 */
/*          stornr = STORNINGSTAB.STORNUMMERID + 1.                                                                                         */
/*       END.                                                                                                                               */
/*       ELSE DO:                                                                                                                           */
/*          stornr = 1.                                                                                                                     */
/*       END.                                                                                                                               */
/*       IF tidin.F14 = TRUE THEN DO:                                                                                                       */
/*          driftvar = 2.                                                                                                                   */
/*       END.                                                                                                                               */
/*       ELSE DO:                                                                                                                           */
/*          driftvar = 1.                                                                                                                   */
/*       END.                                                                                                                               */
/*       FIND FIRST tidbuff WHERE tidbuff.F16 = tidin.F16 AND tidbuff.F17 = tidin.F17 AND                                                   */
/*       RECID(tidbuff) NE RECID(tidin) NO-LOCK NO-ERROR.                                                                                   */
/*       IF AVAILABLE tidbuff THEN DO:                                                                                                      */
/*          specvar = TRUE.                                                                                                                 */
/*          FOR EACH tidbuff WHERE tidbuff.F16 = tidin.F16 AND tidbuff.F17 = tidin.F17 AND                                                  */
/*          RECID(tidbuff) NE RECID(tidin):                                                                                                 */
/*             IF tidbuff.F14 = TRUE THEN DO:                                                                                               */
/*                driftvar = 2.                                                                                                             */
/*             END.                                                                                                                         */
/*             ELSE DO:                                                                                                                     */
/*                driftvar = 1.                                                                                                             */
/*             END.                                                                                                                         */
/*             CREATE BORTKOPPLAD.                                                                                                          */
/*             ASSIGN                                                                                                                       */
/*             BORTKOPPLAD.STORNUMMERID = stornr                                                                                            */
/*             BORTKOPPLAD.ATERDATUM = tidbuff.F18                                                                                          */
/*             BORTKOPPLAD.ATERKLOCKAN = tidbuff.KLOCK3                                                                                     */
/*             BORTKOPPLAD.ANTALHSP = tidbuff.F24                                                                                           */
/*             BORTKOPPLAD.ANTALLSP = tidbuff.F25                                                                                           */
/*             BORTKOPPLAD.AKTIVITET = tidbuff.F27.                                                                                         */
/*             DELETE tidbuff.                                                                                                              */
/*          END.                                                                                                                            */
/*          CREATE BORTKOPPLAD.                                                                                                             */
/*          ASSIGN                                                                                                                          */
/*          BORTKOPPLAD.STORNUMMERID = stornr                                                                                               */
/*          BORTKOPPLAD.ATERDATUM = tidin.F18                                                                                               */
/*          BORTKOPPLAD.ATERKLOCKAN = tidin.KLOCK3                                                                                          */
/*          BORTKOPPLAD.ANTALHSP = tidin.F24                                                                                                */
/*          BORTKOPPLAD.ANTALLSP = tidin.F25                                                                                                */
/*          BORTKOPPLAD.AKTIVITET = tidin.F27.                                                                                              */
/*          RUN skapa_UI.                                                                                                                   */
/*       END.                                                                                                                               */
/*       ELSE DO:                                                                                                                           */
/*          specvar = FALSE.                                                                                                                */
/*          RUN skapa_UI.                                                                                                                   */
/*       END.                                                                                                                               */
/*    END.                                                                                                                                  */
{musarrow.i}
{EUROPEANAMERICAN.I}
PROCEDURE skapa_UI:   
   CREATE STORNINGSTAB.   
   STORNINGSTAB.STORTYPID = driftvar.   
   IF tidin.F4 = "Sollefteå" THEN DO:
      IF YEAR(tidin.F16) = 2000 THEN DO:
         ASSIGN
         STORNINGSTAB.DISTRIKTID = 9.
      END.
      ELSE DO:
         ASSIGN
         STORNINGSTAB.DISTRIKTID = 2.
      END.
   END.
   ELSE DO:
      IF YEAR(tidin.F16) = 2000 THEN DO:
         ASSIGN
         STORNINGSTAB.DISTRIKTID = 8.
      END.
      ELSE DO:
         ASSIGN
         STORNINGSTAB.DISTRIKTID = 1.
      END.
   END.
   ASSIGN
   STORNINGSTAB.STORNUMMERID = stornr
   STORNINGSTAB.VSTORNUMMER = stornr
   STORNINGSTAB.INDATUM = TODAY
   STORNINGSTAB.INKLOCKAN = TIME
   STORNINGSTAB.FDATUM = tidin.F1
   STORNINGSTAB.FKLOCKAN = tidin.KLOCK1
   STORNINGSTAB.REGDATUM = tidin.F1
   STORNINGSTAB.REGKLOCKAN = tidin.KLOCK1
   STORNINGSTAB.ANVANDARE = tidin.F3
   STORNINGSTAB.VEMFEL = tidin.F5
   STORNINGSTAB.TELVEMFEL = tidin.F6
   STORNINGSTAB.ADRESSVEMFEL = tidin.F7
   STORNINGSTAB.VEMPOSTNUMMER = tidin.F8
   STORNINGSTAB.VEMORT = tidin.F9
   STORNINGSTAB.MERJOBB = FALSE.
   IF tidin.F10 NE "" THEN DO:
      STORNINGSTAB.LOSEN = tidin.F10.
   END.   
   IF tidin.F11 NE "" THEN DO:
      STORNINGSTAB.VEMINFO = STORNINGSTAB.VEMINFO + "Adress för fel:" + tidin.F11 + CHR(10).
   END.
   IF tidin.F12 NE "" THEN DO:
      STORNINGSTAB.VEMINFO = STORNINGSTAB.VEMINFO + "Postnr för fel:" + tidin.F12 + CHR(10).
   END.
   IF tidin.F13 NE "" THEN DO:
      STORNINGSTAB.VEMINFO = STORNINGSTAB.VEMINFO + "Ort för fel:" + tidin.F13 + CHR(10).
   END.
   ASSIGN
   STORNINGSTAB.ANTALHSP = tidin.F24
   STORNINGSTAB.ANTALLSP = tidin.F25.   
   BB = tidin.F26.
   AA = LENGTH(STRING(BB)).
   IF AA <= 3 THEN DO:
      CC = 0.
      DD = BB.
   END.
   ELSE DO:
      CC = INTEGER(SUBSTRING(STRING(BB),1,(AA - 3))).
      DD = INTEGER(SUBSTRING(STRING(BB),(AA - 2), 3)).
   END.   
   ASSIGN
   STORNINGSTAB.BORTMW = CC
   STORNINGSTAB.BORTKW = DD
   STORNINGSTAB.KOMMENTAR = tidin.F27.
   IF tidin.F28 = "0,4 kV" THEN DO:
      IF driftvar = 1 THEN DO:
         ASSIGN
         STORNINGSTAB.FELSPANID = 14
         STORNINGSTAB.FRANSPANID = 24.
      END.
      ELSE DO:
         STORNINGSTAB.FRANSPANID = 24.
      END.
      STORNINGSTAB.STDRIFTID = 2.
   END.
   ELSE IF tidin.F28 = "12 kV" THEN DO:
      IF driftvar = 1 THEN DO:
         ASSIGN
         STORNINGSTAB.FELSPANID = 11
         STORNINGSTAB.FRANSPANID = 22.
      END.
      ELSE DO:
         STORNINGSTAB.FRANSPANID = 22.
      END.
      STORNINGSTAB.STDRIFTID = 2.
   END.
   ELSE IF tidin.F28 = "24 kV" THEN DO:
      IF driftvar = 1 THEN DO:
         ASSIGN
         STORNINGSTAB.FELSPANID = 10
         STORNINGSTAB.FRANSPANID = 21.
      END.
      ELSE DO:
         STORNINGSTAB.FRANSPANID = 21.
      END.
      STORNINGSTAB.STDRIFTID = 2.
   END.
   ELSE IF tidin.F28 = "40 kV" THEN DO:
      IF driftvar = 1 THEN DO:
         ASSIGN
         STORNINGSTAB.FELSPANID = 7
         STORNINGSTAB.FRANSPANID = 19.
      END.
      ELSE DO:
         STORNINGSTAB.FRANSPANID = 19.
      END.
      STORNINGSTAB.STDRIFTID = 2.
   END.
   ELSE IF tidin.F28 = "130 kV" THEN DO:
      IF driftvar = 1 THEN DO:
         ASSIGN
         STORNINGSTAB.FELSPANID = 5
         STORNINGSTAB.FRANSPANID = 1.
      END.
      ELSE DO:
         STORNINGSTAB.FRANSPANID = 1.
      END.
      STORNINGSTAB.STDRIFTID = 2.
   END.
   ELSE IF tidin.F28 = "Överliggande nät" THEN DO:
      IF driftvar = 1 THEN DO:
         ASSIGN
         STORNINGSTAB.FELSPANID = 5
         STORNINGSTAB.FRANSPANID = 1.
      END.
      ELSE DO:
         STORNINGSTAB.FRANSPANID = 1.
      END.
      STORNINGSTAB.STDRIFTID = 1.
   END.

   IF driftvar = 1 THEN DO:
      ASSIGN
      STORNINGSTAB.SEKTIONERID = "5" /*okänd*/
      STORNINGSTAB.UTLOSID = 3 /*okänd*/
      STORNINGSTAB.RELINID = 7 /*okänd*/.
   END.
   ASSIGN
   STORNINGSTAB.NATTYPID = "1" /*radialnät*/
   STORNINGSTAB.BRYTOID = 6. /*okänd*/   
   
   IF driftvar = 1 THEN DO:
      IF tidin.F51 = TRUE THEN STORNINGSTAB.FELOID = 22.
      ELSE IF tidin.F52 = TRUE THEN STORNINGSTAB.FELOID = 22.
      ELSE IF tidin.F53 = TRUE THEN STORNINGSTAB.FELOID = 20.
      ELSE IF tidin.F54 = TRUE THEN STORNINGSTAB.FELOID = 22.
      ELSE IF tidin.F55 = TRUE THEN STORNINGSTAB.FELOID = 17.
      ELSE IF tidin.F56 = TRUE THEN STORNINGSTAB.FELOID = 3.
      ELSE IF tidin.F57 = TRUE THEN STORNINGSTAB.FELOID = 11.
      ELSE IF tidin.F58 = TRUE THEN STORNINGSTAB.FELOID = 1.
      ELSE IF tidin.F59 = TRUE THEN STORNINGSTAB.FELOID = 2.
      ELSE IF tidin.F60 = TRUE THEN STORNINGSTAB.FELOID = 22.
      ELSE IF tidin.F61 = TRUE THEN STORNINGSTAB.FELOID = 13.
      ELSE IF tidin.F62 = TRUE THEN STORNINGSTAB.FELOID = 22.
      ELSE STORNINGSTAB.FELOID = 22.
   END.

   IF tidin.F63 = TRUE THEN STORNINGSTAB.ADELID = 19.
   ELSE IF tidin.F64 = TRUE THEN STORNINGSTAB.ADELID = 15.
   ELSE IF tidin.F65 = TRUE THEN STORNINGSTAB.ADELID = 14.
   ELSE IF tidin.F66 = TRUE THEN STORNINGSTAB.ADELID = 13.
   ELSE STORNINGSTAB.ADELID = 12.
   IF specvar = FALSE THEN DO:
      ASSIGN
      startdatum = tidin.F16
      datum30 = tidin.F18
      starttid = tidin.KLOCK2
      tid30 = tidin.KLOCK3
      hspantal = tidin.F24
      lspantal = tidin.F25.
      RUN berakna_UI.
      ASSIGN
      STORNINGSTAB.HDATUM = tidin.F16
      STORNINGSTAB.HKLOCKAN = tidin.KLOCK2
      STORNINGSTAB.DATUM100% = tidin.F18
      STORNINGSTAB.KLOCKAN100% = tidin.KLOCK3
      STORNINGSTAB.AVBROTTSTID = totavbrott
      STORNINGSTAB.BORTFALL = totbortfall.
   END.
   ELSE DO:
      ASSIGN
      startdatum = tidin.F16
      starttid = tidin.KLOCK2
      hspantal = tidin.F24
      lspantal = tidin.F25.
      RUN berakna2_UI.
      ASSIGN
      STORNINGSTAB.HDATUM = tidin.F16
      STORNINGSTAB.HKLOCKAN = tidin.KLOCK2
      STORNINGSTAB.ANTALHSP = hspantal
      STORNINGSTAB.ANTALLSP = lspantal
      STORNINGSTAB.AVBROTTSTID = totavbrott.
      RUN bortfall_UI.
      STORNINGSTAB.BORTFALL = totbortfall.
   END.
END PROCEDURE.


PROCEDURE berakna_UI :
   ASSIGN
   totavbrott = 0
   totbortfall = 0.   
   RUN TIDBER.P (INPUT startdatum, INPUT datum30, INPUT starttid,
   INPUT tid30, OUTPUT timmar100, OUTPUT timmar60).   
   ASSIGN
   totbortfall = totbortfall + ((CC * 1000) + DD) * timmar100
   totavbrott = totavbrott + (timmar100 * (lspantal + hspantal)).      
END PROCEDURE.

PROCEDURE berakna2_UI :
   ASSIGN
   totavbrott = 0.
   FIND FIRST BORTKOPPLAD WHERE BORTKOPPLAD.STORNUMMERID = STORNINGSTAB.STORNUMMERID 
   USE-INDEX AVBROTT EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN
   datum1 = BORTKOPPLAD.ATERDATUM
   tid1 = BORTKOPPLAD.ATERKLOCKAN.
   IF BORTKOPPLAD.ANTALHSP > hspantal THEN DO:
      hspantal = BORTKOPPLAD.ANTALHSP.
   END.
   IF BORTKOPPLAD.ANTALLSP > lspantal THEN DO:
      lspantal = BORTKOPPLAD.ANTALLSP.
   END.
   RUN TIDBER.P (INPUT startdatum, INPUT datum1, INPUT starttid,
   INPUT tid1, OUTPUT timmar100, OUTPUT timmar60).   
   ASSIGN
   bortavbrott = klockan60((timmar100 * (BORTKOPPLAD.ANTALLSP + BORTKOPPLAD.ANTALHSP)))
   BORTKOPPLAD.AVBROTTSTID = bortavbrott.      
   
   totavbrott = totavbrott + (timmar100 * (BORTKOPPLAD.ANTALLSP + BORTKOPPLAD.ANTALHSP)).
   REPEAT:
      FIND NEXT BORTKOPPLAD WHERE BORTKOPPLAD.STORNUMMERID = STORNINGSTAB.STORNUMMERID 
      USE-INDEX AVBROTT EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE BORTKOPPLAD THEN DO:
         ASSIGN
         datum2 = BORTKOPPLAD.ATERDATUM
         tid2 = BORTKOPPLAD.ATERKLOCKAN.
         IF BORTKOPPLAD.ANTALHSP > hspantal THEN DO:
            hspantal = BORTKOPPLAD.ANTALHSP.
         END.
         IF BORTKOPPLAD.ANTALLSP > lspantal THEN DO:
            lspantal = BORTKOPPLAD.ANTALLSP.
         END.
         RUN TIDBER.P (INPUT datum1, INPUT datum2, INPUT tid1,
         INPUT tid2, OUTPUT timmar100, OUTPUT timmar60).
         ASSIGN
         bortavbrott = klockan60((timmar100 * (BORTKOPPLAD.ANTALLSP + BORTKOPPLAD.ANTALHSP)))
         BORTKOPPLAD.AVBROTTSTID = bortavbrott         
         totavbrott = totavbrott + (timmar100 * (BORTKOPPLAD.ANTALLSP + BORTKOPPLAD.ANTALHSP))
         datum1 = datum2
         tid1 = tid2.
      END.
      ELSE LEAVE.
   END.            
END PROCEDURE.

PROCEDURE bortfall_UI :
   ASSIGN
   totbortfall = 0
   mestbort = 0.
   FOR EACH BORTKOPPLAD WHERE BORTKOPPLAD.STORNUMMERID = STORNINGSTAB.STORNUMMERID 
   NO-LOCK:
      IF BORTKOPPLAD.ANTALHSP + BORTKOPPLAD.ANTALLSP >= mestbort THEN DO:
         ASSIGN
         mestbort = BORTKOPPLAD.ANTALHSP + BORTKOPPLAD.ANTALLSP
         bortfallrec = RECID(BORTKOPPLAD).
      END.
   END.
   FIND BORTKOPPLAD WHERE RECID(BORTKOPPLAD) = bortfallrec NO-LOCK NO-ERROR.
   totbortfall = totbortfall + (((STORNINGSTAB.BORTMW * 1000) + STORNINGSTAB.BORTKW) * (klockan100(BORTKOPPLAD.AVBROTTSTID) / (BORTKOPPLAD.ANTALLSP + BORTKOPPLAD.ANTALHSP))).
   FOR EACH BORTKOPPLAD WHERE BORTKOPPLAD.STORNUMMERID = STORNINGSTAB.STORNUMMERID AND
   RECID(BORTKOPPLAD) NE bortfallrec NO-LOCK:
      divmestbort = (BORTKOPPLAD.ANTALLSP + BORTKOPPLAD.ANTALHSP) / mestbort.
      totbortfall = totbortfall + ((((STORNINGSTAB.BORTMW * 1000) + STORNINGSTAB.BORTKW) * divmestbort) * (klockan100(BORTKOPPLAD.AVBROTTSTID) / (BORTKOPPLAD.ANTALLSP + BORTKOPPLAD.ANTALHSP))).
   END.        
END PROCEDURE.
