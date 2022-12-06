
/*------------------------------------------------------------------------
    File        : DYNSETSOKENRLEV.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Jun 09 12:46:20 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/
PROCEDURE StartSokEnrLev_UI :
   DEFINE INPUT  PARAMETER huvproghIN AS HANDLE NO-UNDO.
   huvprogh = huvproghIN.
   RUN setmenuenint_UI.
   /*RUN StartSokEnrLev_UI IN brwproc[15] (INPUT THIS-PROCEDURE). */
END PROCEDURE.
PROCEDURE setmenuenint_UI :
   
   IF dynbrwh:POPUP-MENU = ? THEN DO:
      CREATE MENU tempinth.
      tempinth:POPUP-ONLY = TRUE.
      dynbrwh:POPUP-MENU = tempinth.
   END.             
   ELSE DO:
      CREATE MENU-ITEM tempinth.
      tempinth:SUBTYPE = "RULE".
      tempinth:PARENT = dynbrwh:POPUP-MENU.
   END.
   CREATE MENU-ITEM tempinth.
   tempinth:LABEL = "Visa information via vald Leverantör".
   tempinth:PARENT = dynbrwh:POPUP-MENU.
   ON CHOOSE OF tempinth PERSISTENT RUN infoLE_UI IN THIS-PROCEDURE.
   
   CREATE MENU-ITEM tempinthAH.
   tempinthAH:LABEL = "Visa information via Ahlsell".
   tempinthAH:PARENT = dynbrwh:POPUP-MENU.
   ON CHOOSE OF tempinthAH PERSISTENT RUN infoAH_UI IN THIS-PROCEDURE.
   
   CREATE MENU-ITEM tempinthES.
   tempinthES:LABEL = "Visa information via Elektroskandia".
   tempinthES:PARENT = dynbrwh:POPUP-MENU.
   ON CHOOSE OF tempinthES PERSISTENT RUN infoES_UI IN THIS-PROCEDURE.
   
   
   CREATE MENU-ITEM tempinthON.
   tempinthON:LABEL = "Visa information via Onninen".
   tempinthON:PARENT = dynbrwh:POPUP-MENU.
   ON CHOOSE OF tempinthON PERSISTENT RUN infoON_UI IN THIS-PROCEDURE.
   
   CREATE MENU-ITEM tempinthSE.
   tempinthSE:LABEL = "Visa information via Selga".
   tempinthSE:PARENT = dynbrwh:POPUP-MENU.
   ON CHOOSE OF tempinthSE PERSISTENT RUN infoSE_UI IN THIS-PROCEDURE.
   
END PROCEDURE.


PROCEDURE infoLE_UI :
   DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
   RUN cellvalue_UI (INPUT "enr", OUTPUT enrvar).
   IF enrvar = "" OR enrvar = ? THEN RETURN.
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRW." THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME).
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRWINIT" THEN  RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME,INPUT THIS-PROCEDURE).
END PROCEDURE.
PROCEDURE infoAH_UI :
   DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
   Guru.GlobalaVariabler:Internetlev = "Ahlsell".
   RUN cellvalue_UI (INPUT "enr", OUTPUT enrvar).
   IF enrvar = "" OR enrvar = ? THEN RETURN.
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRW." THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME).
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRWINIT" THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME,INPUT THIS-PROCEDURE).
END PROCEDURE.
PROCEDURE infoES_UI :
   DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
   Guru.GlobalaVariabler:Internetlev = "Elektroskandia".
   RUN cellvalue_UI (INPUT "enr", OUTPUT enrvar).
   IF enrvar = "" OR enrvar = ? THEN RETURN.
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRW." THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME).
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRWINIT" THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME,INPUT THIS-PROCEDURE).
END PROCEDURE.
PROCEDURE infoON_UI :
   DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
   Guru.GlobalaVariabler:Internetlev = "Onninen".
   RUN cellvalue_UI (INPUT "enr", OUTPUT enrvar).
   IF enrvar = "" OR enrvar = ? THEN RETURN.
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRW." THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME).
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRWINIT" THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME,INPUT THIS-PROCEDURE).
END PROCEDURE.
PROCEDURE infoSE_UI :
   DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
   Guru.GlobalaVariabler:Internetlev = "Selga".
   RUN cellvalue_UI (INPUT "enr", OUTPUT enrvar).
   IF enrvar = "" OR enrvar = ? THEN RETURN.
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRW." THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME).
   IF THIS-PROCEDURE:NAME BEGINS "DYNBRWINIT" THEN RUN infoes_UI IN huvprogh (INPUT enrvar,INPUT dynbrwh:NAME,INPUT THIS-PROCEDURE).
END PROCEDURE.
