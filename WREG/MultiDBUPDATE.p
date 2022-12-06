/*MultiDBUPDATE.p

*/
/*Anders Olsson Elpool i Umeå AB  25 okt 2022 10:24:18 
hjälp program för STARTADMDB.W
*/
DEFINE VARIABLE FILL-IN-programkor AS CHARACTER NO-UNDO.
DEFINE VARIABLE FILL-IN-programDB AS CHARACTER NO-UNDO.
DEFINE VARIABLE TOG_ON AS LOGICAL NO-UNDO.
DEFINE VARIABLE FILL-IN-programDBUTIL AS CHARACTER NO-UNDO.
DEFINE VARIABLE computername  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE complength    AS INTEGER     NO-UNDO INITIAL 128.
DEFINE VARIABLE retvalue      AS INTEGER     NO-UNDO.
DEFINE VARIABLE provag AS CHARACTER NO-UNDO.
DEFINE VARIABLE berkopptabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE tabellerbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE stareabuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE areabuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE indexbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttkor NO-UNDO
FIELD KOR AS CHARACTER.

{VALDBDEF.I}
DEFINE VARIABLE progkopia AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnvarhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.

{VALDBALL.I}
{Computer_LanIP.I}  
FUNCTION namndb RETURNS CHARACTER :   
   IF PDBNAME(1) = ? THEN RETURN ?.
   ELSE IF R-INDEX(PDBNAME(1),"\") = 0 THEN RETURN PDBNAME(1).
   ELSE RETURN SUBSTRING(PDBNAME(1),R-INDEX(PDBNAME(1),"\") + 1).
     
END FUNCTION.


PROCEDURE startup_UI :
   DEFINE INPUT  PARAMETER FILL-IN-programkorin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER FILL-IN-programDBin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER TOG_ONin AS LOGICAL NO-UNDO.
   
   FILL-IN-programkor = FILL-IN-programkorin.
   FILL-IN-programDB = FILL-IN-programDBin.
   TOG_ON = TOG_ONin.
   
   
END PROCEDURE.

PROCEDURE server_UI :
   {Computer_LanIP.I}
   MESSAGE "Du kör för ip : " Computer_LanIP
   VIEW-AS ALERT-BOX.
   FIND FIRST valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE valdbtemp THEN DO:
      MESSAGE "Söker på maskin namn!"
      VIEW-AS ALERT-BOX.
      computername = FILL(" ", complength).
      RUN GetComputerNameA (INPUT-OUTPUT computername, OUTPUT complength, OUTPUT retvalue).
      computername = RIGHT-TRIM(computername).
      Computer_LanIP = computername.
      FIND FIRST valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valdbtemp THEN DO:
         MESSAGE "Hittar inga databaser!"
         VIEW-AS ALERT-BOX.
         RETURN.
      END.   
   END.
 END PROCEDURE.
 PROCEDURE acction_UI :
   
   /* Kör något subprogram från Program*/
   IF FILL-IN-programkor NE "" THEN DO:
      FOR EACH valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK:
         IF valdbtemp.FORETAG = "SEKG" OR valdbtemp.FORETAG = "DARPLU" THEN MESSAGE "SEKG OCH DARPLU KÖRS INTE"
                                            VIEW-AS ALERT-BOX.
         ELSE IF  {TAEJMEDDB.I} THEN.
         ELSE DO:
            MESSAGE valdbtemp.DBCON VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO-CANCEL TITLE "Köra?" UPDATE svarmp AS LOGICAL.
           
           
            IF svarmp THEN DO:
               {AppSprinSet.I}
               RUN val_UI.
               IF CONNECTED(LDBNAME(1)) THEN DO:       
                  /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
                  RUN ALIASSATT.P.     
                  RUN VALUE(FILL-IN-programkor).
                  DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
               END.
               ELSE DO:
                  MESSAGE valdbtemp.DBCON " Gick inte att ansluta!"
                  VIEW-AS ALERT-BOX.
               END.
            END.   
            ELSE IF svarmp = FALSE THEN.
            ELSE RETURN.      
         END.  
      END.
   END.

   IF FILL-IN-programDB NE "" THEN DO:
       
      IF FILL-IN-programDB MATCHES "*-C tablemove*" THEN DO:
          RUN sttmove_UI.
          RETURN.
      END.
      
      FOR EACH valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK:
         IF  {TAEJMEDDB.I} THEN.
         ELSE DO:
            {AppSprinSet.I}
            IF FILL-IN-programDB MATCHES "*GURUADD.ST*" THEN DO:
             
               
               FILL-IN-programDBUTIL = SUBSTRING(FILL-IN-programDB,1,INDEX(FILL-IN-programDB,"ADD") + 4) + " " + valdbtemp.DBPLATS + valdbtemp.DBNAMN + " " 
               + valdbtemp.DBPLATS + SUBSTRING(FILL-IN-programDB,INDEX(FILL-IN-programDB,"GURUADD")).
                
            END.
            ELSE DO:   
               FILL-IN-programDBUTIL = SUBSTRING(FILL-IN-programDB,1,INDEX(FILL-IN-programDB," ")) + " " + valdbtemp.DBPLATS + valdbtemp.DBNAMN + "  " 
               + SUBSTRING(FILL-IN-programDB,INDEX(FILL-IN-programDB," ")).
            END.   
            MESSAGE FILL-IN-programDBUTIL     VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO-CANCEL TITLE "Köra?" UPDATE svar AS LOGICAL.          
            IF svar THEN DO:
               
               OS-COMMAND VALUE(FILL-IN-programDBUTIL).
               IF FILL-IN-programDB MATCHES "*GURUADD.ST*" THEN DO:
                  progkopia = valdbtemp.DBPLATS + valdbtemp.DBNAMN + ".ST". 
                  prognamnvarhj = valdbtemp.DBPLATS + "GURUADD.ST".
                  OS-APPEND VALUE(prognamnvarhj) VALUE(progkopia).
               END.   
            END.
            ELSE IF svar = FALSE THEN.
            ELSE RETURN.
          
         END.  
      END.
   END.   
   MESSAGE "Klart för IP: " Computer_LanIP
   VIEW-AS ALERT-BOX.
END.   
PROCEDURE val_UI :
   DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
   /* TOG_ON = TRUE MULTIDB*/
   IF TOG_ON = TRUE THEN koppla = valdbtemp.DBCON + " -ld rt9" + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
   ELSE koppla = valdbtemp.DBPLATS + valdbtemp.DBNAMN + " -1 -ld rt9" + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
   
   CONNECT VALUE(koppla) NO-ERROR.       
END PROCEDURE.


PROCEDURE GetComputerNameA EXTERNAL "kernel32":
   DEFINE INPUT-OUTPUT PARAMETER lpszName AS CHAR.
   DEFINE OUTPUT PARAMETER lpdwcBuffer AS LONG.
   DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE buffer_UI :
  RUN delbuff_UI.
  CREATE BUFFER tabellerbuffh FOR TABLE "_file" .
  CREATE BUFFER stareabuffh FOR TABLE "_storageobject" .
  CREATE BUFFER areabuffh FOR TABLE "_area" .
  CREATE BUFFER indexbuffh FOR TABLE "_Index" . 
END PROCEDURE.
PROCEDURE delbuff_UI :
   EMPTY TEMP-TABLE ttkor NO-ERROR. 
   DELETE OBJECT tabellerbuffh NO-ERROR.
   DELETE OBJECT stareabuffh NO-ERROR.
   DELETE OBJECT areabuffh NO-ERROR.
   DELETE OBJECT indexbuffh NO-ERROR.
   tabellerbuffh = ?.
   stareabuffh = ?.
   areabuffh = ?.
   indexbuffh = ?.
END PROCEDURE.
PROCEDURE sttmove_UI :
   
   FOR EACH valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK:
      
      MESSAGE valdbtemp.DBCON
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Köra?" UPDATE svar AS LOGICAL.          
      IF svar THEN DO:
         {AppSprinSet.I}
         RUN val_UI.   
         IF CONNECTED(LDBNAME(1)) THEN DO:       
            /* IF prognamn NE "" THEN SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).*/
            RUN ALIASSATT.P.     
            
            RUN tmove_UI (INPUT valdbtemp.DBPLATS + valdbtemp.DBNAMN). 
            
            DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
            RUN ttkoruppdate_UI.
            
         END.
         ELSE DO:
            MESSAGE valdbtemp.DBCON " Gick inte att ansluta!"
            VIEW-AS ALERT-BOX.
         END.
      END. 
      ELSE IF svar = FALSE THEN.
      ELSE RETURN.  
   END.  
END PROCEDURE.
PROCEDURE ttkoruppdate_UI :
   FOR EACH ttkor WHERE NO-LOCK:
      OS-COMMAND SILENT VALUE(ttkor.KOR).
   END.
END PROCEDURE.

PROCEDURE IdxMoveBer_UI :
   DEFINE INPUT PARAMETER startkom AS CHARACTER NO-UNDO.
   MESSAGE startkom "BER"
   VIEW-AS ALERT-BOX.
   EMPTY TEMP-TABLE ttkor NO-ERROR.
   RUN Cttkor_UI (INPUT startkom + " " + "BERID.OMR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERID2.OMRADE mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERKALK.BKID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERKALK.KALKNUM mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERKALK.OMR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERKALKOPPLA.AONR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERKALKOPPLA.BERKALK mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERKALKOPPLA.KALKNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERLINKAB.DATUM mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERLINKAB.INKOP mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERLINKAB.OMR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERORD.ORD mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERVAL.LISTA mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERVAL.OMR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BILDERIBEREDNING.BERNRNUM mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDFORLKAB.FORL mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDFORLKAB.KABELLINJE mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDFORLKAB.SCHAKT mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDFORLSAM.FORL mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDFORLSAM.KOD mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDFORLSAM.SCHAKT mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDHANDELSE.ID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKABELLINJE.KABEL mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKABELLINJE.KABELLINJE mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKABELLINJE.ORDNING mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKABELLINJE.PUNKT mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKALK.ARBKOD mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKALK.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKALK.HKID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKALK.KALKNUM mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKALK.SKAPAD mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKKOPP.KATAR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDKKOPP.KKID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDPROTKOPP.ORDNING mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDPROTKOPPBER.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDPUNKT.NUM mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDPUNKT.ORDNING mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDPUNKT.PI mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDPUNKT.UPPLAG mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKT.SCHAKT mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKTFOR.FORSATT mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKTFOR.PUNKT mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKTFOR.SCHACT mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKTKALKSPEC.BERFI mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKTKALKSPEC.BERPI mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKTKALKSPEC.BERSI mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKTPROT.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHAKTPROTHAND.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDSCHSTOPP.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "HDVOLYMBER.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKADRESS.AONRAONR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKADRESS.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKADRESS.BESTDATUM mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKADRESS.INKBESTID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKBER.AONR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKBER.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKMTRL.AONR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKMTRL.BERNR mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKMTRL.INKBESTID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKMTRL.INKID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "INKMTRL.LEVENRID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "KONSTVAL.KOPIA mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "KONSTVAL.ORD mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "KONSTVAL.ORD2 mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "MARKSTATIONIBEREDNING.AONRNUM mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "UTBYTESLISTA.BUID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "UTBYTESLISTA.UID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "UTBYTESLNAMN.UID mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERMTRL.DATUM mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERMTRL.INKOP mtrlindex").
   RUN Cttkor_UI (INPUT startkom + " " + "BERMTRL.OMR mtrlindex").
   
   
   
   
   
   
   
   RUN ttkoruppdate_UI.
   EMPTY TEMP-TABLE ttkor NO-ERROR. 
END PROCEDURE.
PROCEDURE IdxMoveKalk_UI :
   DEFINE INPUT PARAMETER startkom AS CHARACTER NO-UNDO.
   EMPTY TEMP-TABLE ttkor NO-ERROR. 
   RUN Cttkor_UI (INPUT startkom + " " + "FREKVENSKATALOG.ARBKOD kalkylindex").
   RUN Cttkor_UI (INPUT startkom + " " + "FREKVENSKATALOG.KLOGSUBID kalkylindex").
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKEGNAPRISER.KALKNR kalkylindex").
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKFAKTORER.KALKNR kalkylindex").
   
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKHUV.KALKNR kalkylindex").
   RUN Cttkor_UI (INPUT startkom + " " + "KALKHUV.KLOGID kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKHUV.TYPKALK kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKMALLHUVUD.MALL kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKMALLKODER.ARBKOD kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKMALLKODER.MALL kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKMTR.KALK kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNATT.AONR kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUM.ARBKOD kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUM.KALKNR kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUM.MATRIS kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUM.NUM kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUMANVEGEN.ANVEGEN kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUMANVEGENSUB.ANV kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUMANVEGENSUB.NUM kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUMSUB.KALKNR kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKNUMSUB.NUM kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKVISNING.KVID kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLARBKODER.ARBKOD kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLARBKODER.KLOGSUBID kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLARBKODER.MARK kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLARBKODER.TYPKALK kalkylindex"). 
   
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLKATALOG.KATALOGTYP kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLKATALOG.KLOGID kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLKATALOG.VISARTAL kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLKATALOGANV.ANV kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLKATALOGANV.KLOGID kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLKATALOGSUB.KLOGSUBID kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLKATALOGSUB.KLOGID kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLLOPPOSTER.ARBKOD kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLLOPPOSTER.KLOGSUBID kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLLOPPOSTER.LOPNR kalkylindex"). 
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLLOPPOSTER.LOPNR kalkylindex"). 
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLLOPPOSTER.LOPNR kalkylindex").
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLPRISER.KPID kalkylindex").
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLPRISER.SOKBENAMNING kalkylindex").
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLTIDLAGE.KALKNR kalkylindex").
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLTIDLAGE.PERSONALKOD kalkylindex").
   
   RUN Cttkor_UI (INPUT startkom + " " + "KALKYLTIDLAGE.PERSONALKOD kalkylindex").
   RUN ttkoruppdate_UI.
   EMPTY TEMP-TABLE ttkor NO-ERROR. 
END PROCEDURE.
PROCEDURE Cttkor_UI :
   DEFINE INPUT PARAMETER startkom AS CHARACTER NO-UNDO.
   CREATE ttkor.
   ttkor.KOR = startkom.
   
END PROCEDURE.
PROCEDURE tmove_UI :
  DEFINE INPUT  PARAMETER dplatsnamn AS CHARACTER NO-UNDO.
  RUN buffer_UI.   
  kommandoquery = "FOR EACH _file where _file._file-number > 0 AND _file._file-number < 32000  NO-LOCK by _file-name".
  RUN CreateCustomQuery(INPUT tabellerbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      stareabuffh:FIND-FIRST("WHERE _object-type = 1 and _object-number = " + STRING(tabellerbuffh:BUFFER-FIELD("_file-number"):BUFFER-VALUE),NO-LOCK) NO-ERROR.
      IF stareabuffh:AVAILABLE THEN DO:
         areabuffh:FIND-FIRST("WHERE _area-number = " + STRING(stareabuffh:BUFFER-FIELD("_area-number"):BUFFER-VALUE),NO-LOCK) NO-ERROR.
         IF areabuffh:AVAILABLE THEN DO:
            IF areabuffh:BUFFER-FIELD("_area-number"):BUFFER-VALUE = 6 THEN DO:
               CREATE ttkor.
               ttkor.KOR = SUBSTRING(FILL-IN-programDB,1,INDEX(FILL-IN-programDB,"-C tablemove") - 1) + " " + dplatsnamn + " " 
               + " -C tablemove "
               + tabellerbuffh:BUFFER-FIELD("_file-name"):BUFFER-VALUE + " " +
               SUBSTRING(FILL-IN-programDB,INDEX(FILL-IN-programDB,"-C tablemove") + 13).
            END.
         END.
      END.
      qH:GET-NEXT().     
   END.
   RUN CloseCustomQuery(INPUT qH).
  
  
  
  /*
  FOR EACH _file WHERE _file._file-number > 0 AND _file._file-number < 32000  NO-LOCK by _file-name:
     FIND _storageobject WHERE _storageobject._object-type = 1 and _storageobject._object-number = _file._file-num NO-LOCK NO-ERROR.
     FIND _area WHERE _area._area-number = _storageobject._area-number NO-LOCK NO-ERROR. 
     DISPLAY _file._file-name _area._area-name _storageobject._area-number  _file._ianum.
       
  END.
  */
  
  
END PROCEDURE.
PROCEDURE idxmove_UI :
  RUN buffer_UI.
  /*
  FOR EACH _FILE WHERE _FILE._file-number > 0 AND _FILE._file-number < 32000  NO-LOCK by _file-name:
    for each  _Index  WHERE _Index._File-recid = RECID(_File) NO-LOCK : 
      FIND _storageobject WHERE _object-type = 2 and _object-number = _Index._Idx-num no-error.
      FIND _area WHERE _area._area-number = _storageobject._area-number NO-LOCK NO-ERROR. 
      DISPLAY _FILE._file-name _Index._Index-name _area._area-nam
          _Index._Idx-num  
          _storageobject._area-number  
          _Index._ianum.
      
      end.   
          
   END.
  */
END PROCEDURE.
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh .
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
PROCEDURE CloseCustomQuery:
   DEFINE INPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CustomQueryh:QUERY-CLOSE()  NO-ERROR.
   CustomQueryh = ?.
END PROCEDURE.

