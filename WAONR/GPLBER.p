
/*------------------------------------------------------------------------
    File        : GPLBER.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Apr 22 11:01:15 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/





&Scoped-define PUBLIC
{GPLDYNTABLEH.I}

{SparaDynDSstar.I}

CREATE WIDGET-POOL "DynTableGPL" NO-ERROR.
DEFINE INPUT PARAMETER inglobanv AS CHARACTER NO-UNDO.

DEFINE VARIABLE gpllog AS LOGICAL NO-UNDO.

PROCEDURE GPLCreate_UI:
   CREATE TEMP-TABLE gplhuvudtth IN WIDGET-POOL "DynTableGPL".
   gplhuvudtth:CREATE-LIKE("GPLHUVUD").
   gplhuvudtth:ADD-NEW-FIELD("TTRECID","RECID").
   /*gplhuvudtth:ADD-NEW-INDEX("PLID").
   gplhuvudtth:ADD-INDEX-FIELD("PLID","PLID").
   gplhuvudtth:ADD-NEW-INDEX("AONR").
   gplhuvudtth:ADD-INDEX-FIELD("AONRAONR","AODELNR").*/

   gplhuvudtth:TEMP-TABLE-PREPARE("gplhuvudtt").
   gplhuvudbuffh = gplhuvudtth:DEFAULT-BUFFER-HANDLE.
   
   CREATE TEMP-TABLE gplaktivitettth IN WIDGET-POOL "DynTableGPL".
   gplaktivitettth:CREATE-LIKE("GPLAKTIVITET").
   gplaktivitettth:ADD-NEW-FIELD("TTRECID","RECID").
   gplaktivitettth:ADD-NEW-FIELD("FORNAMN","CHARACTER").
   gplaktivitettth:ADD-NEW-FIELD("EFTERNAMN","CHARACTER").
   gplaktivitettth:ADD-NEW-FIELD("PERSONNUMMER","CHARACTER").
   gplaktivitettth:ADD-NEW-FIELD("RPERSONNUMMER","CHARACTER").
   gplaktivitettth:ADD-NEW-FIELD("RNAMN","CHARACTER").
   gplaktivitettth:ADD-NEW-FIELD("LOGGDATUM","DATE").
   /*
   gplaktivitettth:ADD-NEW-INDEX("PLAID").
   gplaktivitettth:ADD-INDEX-FIELD("PLID","PLAID").
   gplaktivitettth:ADD-NEW-INDEX("STARTTID").
   gplaktivitettth:ADD-INDEX-FIELD("STARTTID","SLUTTID").*/
   
   gplaktivitettth:TEMP-TABLE-PREPARE("gplaktivitett").
   gplaktivitetbuffh = gplaktivitettth:DEFAULT-BUFFER-HANDLE.   
   
END PROCEDURE.

PROCEDURE laddaGPLDS_UI:
   DEFINE INPUT  PARAMETER vad AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER DATASET-HANDLE GPLDS BIND.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE q2h AS HANDLE NO-UNDO.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE persBuffer     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE qString AS CHARACTER NO-UNDO.
   
   GPLDS:EMPTY-DATASET() NO-ERROR.
         
   CREATE BUFFER persBuffer FOR TABLE "PERSONALTAB" IN WIDGET-POOL "DynTableGPL". 
   
   RUN FINNSTABELL.P (INPUT "GPLHUVUD", OUTPUT gpllog).
   IF gpllog = TRUE THEN.
   ELSE RETURN. 
   
   IF NOT VALID-HANDLE(gplhuvudtth) THEN RUN GPLCreate_UI.
   RUN GetDatasetDeftt_UI ("GPLDS").  
   DatasetDeftt.antaltab = 2.
   DatasetDeftt.pcBuffers[1] = STRING(gplhuvudbuffh).
   DatasetDeftt.pcBuffers[2] = STRING(gplaktivitetbuffh). 
   DatasetDeftt.pcRelFields[1] = "PLID,PLID".
   DatasetDeftt.pcSources[1] = "GPLHUVUD".
   DatasetDeftt.pcSources[2] = "GPLAKTIVITET".
   DatasetDeftt.pcSourceKeys[1] = "PLID".
   DatasetDeftt.pcSourceKeys[2] = "PLID,PLAID".
   DatasetDeftt.pcKeyValue[1] = vad.
   /*
   RUN DefAndLoadDsEj_UI IN dyndamicDSh 
   */
   RUN DefAndLoadDsRe_UI IN dyndamicDSh
   ({DataSetInput.I} OUTPUT DATASET-HANDLE GPLDS BIND).
   kommandoquery = "FOR EACH " + gplaktivitetbuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT gplaktivitetbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      gplaktivitetbuffh:BUFFER-FIELD("LOGGDATUM"):BUFFER-VALUE = DATE(gplaktivitetbuffh:BUFFER-FIELD("LOGGTID"):BUFFER-VALUE).
      qString = "WHERE PERSONALKOD = '" + gplaktivitetbuffh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE + "'".
      persBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
      IF persBuffer:AVAILABLE THEN DO:
         gplaktivitetbuffh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE = persBuffer:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE.
         gplaktivitetbuffh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE = persBuffer:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
         gplaktivitetbuffh:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE = persBuffer:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + persBuffer:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
      END.
      IF gplaktivitetbuffh:BUFFER-FIELD("RATTELSE"):BUFFER-VALUE = TRUE THEN DO:
         qString = "WHERE PERSONALKOD = '" + gplaktivitetbuffh:BUFFER-FIELD("RATTELSEUTFORARE"):BUFFER-VALUE + "'".
         persBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
         gplaktivitetbuffh:BUFFER-FIELD("RPERSONNUMMER"):BUFFER-VALUE =  persBuffer:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE.  
         gplaktivitetbuffh:BUFFER-FIELD("RNAMN"):BUFFER-VALUE = persBuffer:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + persBuffer:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
      END.
      qH:GET-NEXT().
   END.
   Guru.GlobalaVariabler:GDPRtyp = "GPL".
   {GDPRLOGGCLIENT.I}
   persBuffer:BUFFER-RELEASE ( )   NO-ERROR.
   DELETE OBJECT persBuffer. 
   persBuffer = ?.
END PROCEDURE. 

PROCEDURE laddaOmGPLDS_UI:
   DEFINE INPUT  PARAMETER vad AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER DATASET-HANDLE GPLDS BIND.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE q2h AS HANDLE NO-UNDO.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE persBuffer     AS HANDLE  NO-UNDO.
   DEFINE VARIABLE qString AS CHARACTER NO-UNDO.
   GPLDS:EMPTY-DATASET() NO-ERROR.
  
   CREATE BUFFER persBuffer FOR TABLE "PERSONALTAB" IN WIDGET-POOL "DynTableGPL". 
   
   RUN FINNSTABELL.P (INPUT "GPLHUVUD", OUTPUT gpllog).
   IF gpllog = TRUE THEN.
   ELSE RETURN. 
    
   IF NOT VALID-HANDLE(gplhuvudtth) THEN RUN GPLCreate_UI.
   RUN GetDatasetDeftt_UI ("GPLDS").  
   DatasetDeftt.antaltab = 2.
   DatasetDeftt.pcBuffers[1] = STRING(gplhuvudbuffh).
   DatasetDeftt.pcBuffers[2] = STRING(gplaktivitetbuffh). 
   DatasetDeftt.pcRelFields[1] = "PLID,PLID".
   DatasetDeftt.pcSources[1] = "GPLHUVUD".
   DatasetDeftt.pcSources[2] = "GPLAKTIVITET".
   DatasetDeftt.pcSourceKeys[1] = "PLID".
   DatasetDeftt.pcSourceKeys[2] = "PLID,PLAID".
   DatasetDeftt.pcKeyValue[1] = vad.
   RUN ReloadDs_UI IN dyndamicDSh 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE GPLDS BIND).
   kommandoquery = "FOR EACH " + gplaktivitetbuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT gplaktivitetbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      gplaktivitetbuffh:BUFFER-FIELD("LOGGDATUM"):BUFFER-VALUE = DATE(gplaktivitetbuffh:BUFFER-FIELD("LOGGTID"):BUFFER-VALUE).
      qString = "WHERE PERSONALKOD = '" + gplaktivitetbuffh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE + "'".
      persBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
      IF persBuffer:AVAILABLE THEN DO:
         gplaktivitetbuffh:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE = persBuffer:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE.
         gplaktivitetbuffh:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE = persBuffer:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
         gplaktivitetbuffh:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE = persBuffer:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + persBuffer:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
      END.
      IF gplaktivitetbuffh:BUFFER-FIELD("RATTELSE"):BUFFER-VALUE = TRUE THEN DO:
         qString = "WHERE PERSONALKOD = '" + gplaktivitetbuffh:BUFFER-FIELD("RATTELSEUTFORARE"):BUFFER-VALUE + "'".
         persBuffer:FIND-FIRST(qString,NO-LOCK) NO-ERROR.
         gplaktivitetbuffh:BUFFER-FIELD("RPERSONNUMMER"):BUFFER-VALUE =  persBuffer:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE.  
         gplaktivitetbuffh:BUFFER-FIELD("RNAMN"):BUFFER-VALUE = persBuffer:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + persBuffer:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
      END.
      qH:GET-NEXT().
   END.
   Guru.GlobalaVariabler:GDPRtyp = "GPL".
   {GDPRLOGGCLIENT.I}
   persBuffer:BUFFER-RELEASE ( )   NO-ERROR.
   DELETE OBJECT persBuffer. 
   persBuffer = ?.
END PROCEDURE.
    
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "DynTableGPL".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.

PROCEDURE finnsgpl_UI:
    DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
    
    DEFINE OUTPUT PARAMETER fanns AS LOGICAL NO-UNDO INITIAL FALSE.   
    DEFINE OUTPUT PARAMETER plid AS INTEGER NO-UNDO INITIAL 0. 
    
    DEFINE VARIABLE huvBuffer     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE qString AS CHARACTER NO-UNDO.

    CREATE BUFFER huvBuffer FOR TABLE "GPLHUVUD" IN WIDGET-POOL "DynTableGPL". 

    qString = "WHERE AONRAONR = '" + STRING(aonrvar) + "' AND AONRDELNR = " + STRING(delnrvar).
    
    DO TRANSACTION:
        huvBuffer:FIND-FIRST(qString,EXCLUSIVE-LOCK) NO-ERROR.
        IF huvBuffer:AVAILABLE THEN DO:
           fanns = TRUE.
           plid = huvBuffer:BUFFER-FIELD("PLID"):BUFFER-VALUE NO-ERROR.   
        END.       
    END.
    huvBuffer:BUFFER-RELEASE ( )   NO-ERROR.
    DELETE OBJECT huvBuffer.
    
    huvBuffer = ?.
END PROCEDURE.

PROCEDURE nygpl_UI:
    DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
     
    DEFINE OUTPUT PARAMETER plid AS INTEGER NO-UNDO INITIAL 0. 
    
    DEFINE VARIABLE huvBuffer     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE qString AS CHARACTER NO-UNDO.

    CREATE BUFFER huvBuffer FOR TABLE "GPLHUVUD" IN WIDGET-POOL "DynTableGPL". 

    DO TRANSACTION:
        huvBuffer:FIND-LAST("USE-INDEX PLID",EXCLUSIVE-LOCK) NO-ERROR.
        IF huvBuffer:AVAILABLE THEN DO:
           plid = huvBuffer:BUFFER-FIELD("PLID"):BUFFER-VALUE NO-ERROR.   
        END.
        plid = plid + 1.
        huvBuffer:BUFFER-CREATE ().
        huvBuffer:BUFFER-FIELD("PLID"):BUFFER-VALUE = plid.
        huvBuffer:BUFFER-FIELD("AONRAONR"):BUFFER-VALUE = aonrvar.
        huvBuffer:BUFFER-FIELD("AONRDELNR"):BUFFER-VALUE = delnrvar.
        huvBuffer:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE.
    END.
    huvBuffer:BUFFER-RELEASE ( )   NO-ERROR.
    DELETE OBJECT huvBuffer.
    
    huvBuffer = ?.
END PROCEDURE.

PROCEDURE skapagplRattelse_UI:
   DEFINE INPUT  PARAMETER  gplrattelsetth AS HANDLE NO-UNDO.
   DEFINE OUTPUT  PARAMETER  felout AS LOGICAL INITIAL FALSE NO-UNDO.
   
   DEFINE VARIABLE huvBuffer     AS HANDLE  NO-UNDO. 
   
   DEFINE VARIABLE nyaktBuffer     AS HANDLE  NO-UNDO.
  
   DEFINE VARIABLE plaid AS INTEGER NO-UNDO.
   DEFINE VARIABLE todayDT AS DATETIME-TZ NO-UNDO.
   
   CREATE BUFFER huvBuffer FOR TABLE "GPLHUVUD" IN WIDGET-POOL "DynTableGPL".
   CREATE BUFFER nyaktBuffer FOR TABLE "GPLAKTIVITET" IN WIDGET-POOL "DynTableGPL".

   todayDT = DATETIME-TZ(TODAY).
          
   gplrattelsetth:FIND-FIRST().
   IF gplrattelsetth:AVAILABLE THEN DO:
      nyaktBuffer:FIND-LAST("WHERE PLID = " + huvBuffer:BUFFER-FIELD("PLID"):BUFFER-VALUE  + " AND RPLAID = '" + gplrattelsetth:BUFFER-FIELD("RPLAID"):BUFFER-VALUE + "' USE-INDEX PLAID",EXCLUSIVE-LOCK) NO-ERROR. 
      IF nyaktBuffer:AVAILABLE THEN DO:
            nyaktBuffer:BUFFER-RELEASE ( )   NO-ERROR.
            felout = TRUE.
            RETURN.
      END.
      
      huvBuffer:FIND-FIRST("WHERE PLID = " + STRING(gplrattelsetth:BUFFER-FIELD("PLID"):BUFFER-VALUE), NO-LOCK) NO-ERROR.
      IF huvBuffer:AVAILABLE AND huvBuffer:BUFFER-FIELD("AKTIV"):BUFFER-VALUE EQ TRUE THEN DO TRANSACTION:  
         nyaktBuffer:FIND-LAST("WHERE PLID = " + huvBuffer:BUFFER-FIELD("PLID"):BUFFER-VALUE  + " USE-INDEX PLAID",EXCLUSIVE-LOCK) NO-ERROR.   
         IF nyaktBuffer:AVAILABLE THEN DO:
            plaid = nyaktBuffer:BUFFER-FIELD("PLAID"):BUFFER-VALUE NO-ERROR.
         END.
         plaid = plaid + 1.                        
         nyaktBuffer:BUFFER-CREATE().
         IF gplrattelsetth:BUFFER-FIELD("RATTELSETYP"):BUFFER-VALUE = "Makulerar" THEN DO:
            nyaktBuffer:BUFFER-COPY(gplrattelsetth).
            nyaktBuffer:BUFFER-FIELD("PLAID"):BUFFER-VALUE = plaid.
            nyaktBuffer:BUFFER-FIELD("LOGGTID"):BUFFER-VALUE = DATETIME(TODAY, MTIME ).
         END.
         IF gplrattelsetth:BUFFER-FIELD("RATTELSETYP"):BUFFER-VALUE = "Ersätt" THEN DO:
            
         END.
         /*Markera rättad post*/
         nyaktBuffer:BUFFER-RELEASE ( )   NO-ERROR.
         nyaktBuffer:FIND-FIRST("WHERE PLID = " + gplrattelsetth:BUFFER-FIELD("PLID"):BUFFER-VALUE  + " AND PLAID = " + gplrattelsetth:BUFFER-FIELD("RPLAID"):BUFFER-VALUE + " USE-INDEX PLAID",EXCLUSIVE-LOCK).
         nyaktBuffer:BUFFER-FIELD("RATTELSETYP"):BUFFER-VALUE = "Makulerad".
      END.
      ELSE DO:
         /*EJ AKTIV*/
      END.
   END.
   ELSE DO:
      /*Ingen rättelsepost*/   
   END.
   nyaktBuffer:BUFFER-RELEASE ( )   NO-ERROR.
   DELETE OBJECT nyaktBuffer.   
   nyaktBuffer = ?.
   
   huvBuffer:BUFFER-RELEASE ( )   NO-ERROR.
   DELETE OBJECT huvBuffer.   
   huvBuffer = ?.    
END PROCEDURE.

PROCEDURE aktiveragpl_UI:
    DEFINE INPUT  PARAMETER plid AS INTEGER NO-UNDO.
     
    DEFINE INPUT PARAMETER lage AS LOGICAL. 
    
    DEFINE VARIABLE huvBuffer     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE qString AS CHARACTER NO-UNDO.

    CREATE BUFFER huvBuffer FOR TABLE "GPLHUVUD" IN WIDGET-POOL "DynTableGPL". 

    DO TRANSACTION:
        huvBuffer:FIND-FIRST("WHERE PLID = " + STRING(plid) + " USE-INDEX PLID",EXCLUSIVE-LOCK) NO-ERROR.
        IF huvBuffer:AVAILABLE THEN DO:
           huvBuffer:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = lage.
        END.
    END.
    huvBuffer:BUFFER-RELEASE ( )   NO-ERROR.
    DELETE OBJECT huvBuffer.
    
    huvBuffer = ?.
END PROCEDURE.

PROCEDURE uppdateraByggId_UI:
    DEFINE INPUT  PARAMETER plid AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER nyttNr AS CHARACTER. 
    DEFINE VARIABLE huvBuffer     AS HANDLE  NO-UNDO.
    
    CREATE BUFFER huvBuffer FOR TABLE "GPLHUVUD" IN WIDGET-POOL "DynTableGPL". 

    DO TRANSACTION:
        huvBuffer:FIND-FIRST("WHERE PLID = " + STRING(plid) + " USE-INDEX PLID",EXCLUSIVE-LOCK) NO-ERROR.
        IF huvBuffer:AVAILABLE THEN DO:
           huvBuffer:BUFFER-FIELD("IDNR"):BUFFER-VALUE = nyttNr.
        END.
    END.
    huvBuffer:BUFFER-RELEASE ( )   NO-ERROR.
    DELETE OBJECT huvBuffer.
    
    huvBuffer = ?.
END PROCEDURE.


/*
PROCEDURE aktiveragpl_UI:
    DEFINE INPUT  PARAMETER plid AS INTEGER NO-UNDO.
     
    DEFINE OUTPUT PARAMETER aktiv AS LOGICAL. 
    
    DEFINE VARIABLE huvBuffer     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE qString AS CHARACTER NO-UNDO.

    CREATE BUFFER huvBuffer FOR TABLE "GPLHUVUD" IN WIDGET-POOL "DynTableGPL". 

    DO TRANSACTION:
        huvBuffer:FIND-FIRST("WHERE PLID = " + STRING(plid) + " USE-INDEX PLID",EXCLUSIVE-LOCK) NO-ERROR.
        IF huvBuffer:AVAILABLE THEN DO:
           IF huvBuffer:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = TRUE THEN DO:
              aktiv = FALSE.
              huvBuffer:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = FALSE.
           END.
           ELSE DO:
              aktiv = TRUE.
              huvBuffer:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = TRUE.
           END.
        END.
    END.
    huvBuffer:BUFFER-RELEASE ( )   NO-ERROR.
    DELETE OBJECT huvBuffer.
    
    huvBuffer = ?.
END PROCEDURE.
*/

PROCEDURE avslutagpl_UI:
   
   DELETE WIDGET-POOL "DynTableGPL" NO-ERROR.
   DELETE OBJECT gplhuvudtth NO-ERROR.
   gplhuvudtth = ?.
   DELETE OBJECT gplaktivitettth NO-ERROR.
   gplaktivitettth = ?.
   DELETE OBJECT gplaktivitettth NO-ERROR.
   gplaktivitettth = ?.
   DELETE OBJECT gplaktivitetbuffh NO-ERROR.
   gplaktivitetbuffh = ?.
   IF VALID-HANDLE(dyndamicDSh) THEN DO:
      RUN RelDatset_UI IN dyndamicDSh.
      DELETE PROCEDURE dyndamicDSh NO-ERROR.
      dyndamicDSh = ?.
   END.   
  
END PROCEDURE.
