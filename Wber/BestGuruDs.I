
/*------------------------------------------------------------------------
    File        : BestGuruDs.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Oct 17 12:37:15 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

{BestGuruTT.I}
DEFINE TEMP-TABLE felmeddftptemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.  
DEFINE INPUT  PARAMETER elementhidden AS CHARACTER NO-UNDO.  
DEFINE INPUT PARAMETER globforetag  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER cFilein  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR GuruOrder.
DEFINE INPUT PARAMETER TABLE FOR GuruBestallare.
DEFINE INPUT PARAMETER TABLE FOR GuruLevAdress.
DEFINE INPUT PARAMETER TABLE FOR GuruUpplag.
DEFINE INPUT PARAMETER TABLE FOR GuruKonstruktion.
DEFINE INPUT PARAMETER TABLE FOR GuruKonstruktionOne. 
DEFINE INPUT PARAMETER TABLE FOR GuruMtrl.
DEFINE OUTPUT PARAMETER TABLE FOR felmeddftptemp.
EMPTY TEMP-TABLE felmeddftptemp NO-ERROR. 
DEFINE VARIABLE filnamnorg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
FIND FIRST GuruOrder WHERE NO-LOCK NO-ERROR.

filnamnorg = cFilein.
FOR EACH GuruOrder WHERE NO-LOCK:
    FIND FIRST GuruUpplag WHERE GuruUpplag.ID = GuruOrder.ID NO-LOCK NO-ERROR.
    IF NOT AVAILABLE GuruUpplag THEN DO:
       CREATE GuruUpplag.
       ASSIGN
       GuruUpplag.ID              = GuruOrder.ID
       GuruUpplag.UpplagNr        = 0. 
    END.
END.

FOR EACH GuruUpplag WHERE NO-LOCK:
   FIND FIRST GuruKonstruktion WHERE GuruKonstruktion.UpplagNr = GuruUpplag.UpplagNr NO-LOCK NO-ERROR.
   IF NOT AVAILABLE GuruKonstruktion THEN DO:
      CREATE GuruKonstruktion.
      ASSIGN 
      GuruKonstruktion.ID = 1.
      GuruKonstruktion.KonstruktionsNr   = 0.
      GuruKonstruktion.UpplagNr        = GuruUpplag.UpplagNr.
      CREATE GuruKonstruktionOne. 
      BUFFER-COPY GuruKonstruktion TO GuruKonstruktionOne.
   END.
END.
 
FOR EACH GuruUpplag WHERE GuruUpplag.UpplagNr = 0 NO-LOCK:
   GuruUpplag.UpplagNr = -1.
END.
FOR EACH GuruMtrl WHERE GuruMtrl.UpplagNr = 0 NO-LOCK:
   GuruMtrl.UpplagNr = -1.
END.
FOR EACH GuruMtrl WHERE GuruMtrl.KonstruktionsNr = 0 NO-LOCK:
   GuruMtrl.KonstruktionsNr = -1.
END.
FOR EACH GuruKonstruktion WHERE GuruKonstruktion.UpplagNr = 0 NO-LOCK:
   GuruKonstruktion.UpplagNr = -1.
END.
FOR EACH GuruKonstruktion WHERE GuruKonstruktion.KonstruktionsNr = 0 NO-LOCK:
   GuruKonstruktion.KonstruktionsNr = -1.
END.
FOR EACH GuruKonstruktionOne WHERE GuruKonstruktionOne.UpplagNr = 0 NO-LOCK:
   GuruKonstruktionOne.UpplagNr = -1.
END.
FOR EACH GuruKonstruktionOne WHERE GuruKonstruktionOne.KonstruktionsNr = 0 NO-LOCK:
   GuruKonstruktionOne.KonstruktionsNr = -1.
END.
/*behövs inte*/
PROCEDURE elementhidden_UI :
   
   IF elementhidden = "ELEMENT" OR elementhidden = "HIDDEN" THEN DO: 
      GuruKonstruktionbuffh:BUFFER-FIELD("KId"):XML-NODE-TYPE = elementhidden.
      GuruKonstruktionbuffh:BUFFER-FIELD("ByggNr"):XML-NODE-TYPE = elementhidden.
   END.   
END PROCEDURE.
PROCEDURE writexmlomini_UI :            
   DEFINE INPUT  PARAMETER hPDS AS HANDLE NO-UNDO.
   DEFINE INPUT  PARAMETER cTargetType AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cFile AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cEncoding AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER cSchemaLocation AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lFormatted AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER lWriteSchema AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER lMinSchema AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER lWriteBeforeImage AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER omit-initial AS LOGICAL NO-UNDO.
   DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
   
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      lReturn = hPDS:WRITE-XML(cTargetType, cFile, lFormatted,cEncoding,cSchemaLocation,lWriteSchema,lMinSchema,lWriteBeforeImage, omit-initial).      
     
   END. 
    /*VATTENSRM*/ 
   ELSE DO:
      lReturn = hPDS:WRITE-XML(cTargetType, cFile, lFormatted,cEncoding,cSchemaLocation,lWriteSchema,lMinSchema,lWriteBeforeImage, omit-initial).      
      
   END.  
   FIND FIRST felmeddftptemp WHERE felmeddftptemp.FELMEDD = "Filen är utläst för vidare behandling."NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddftptemp THEN.
   ELSE DO:
      CREATE felmeddftptemp.
      felmeddftptemp.FELMEDD = "Filen är utläst för vidare behandling.".
   END.     
/*
 ASSIGN 
cTargetType    =  "FILE"
cFile           = "d:\elpool\delad\pro9s\ExportIFS\GuruOrdermtrlxsd"
lFormatted      = TRUE
cEncoding       = "iso8859-1"
lWriteSchema = FALSE 
lMinSchema = false. 
lReturn = hPDS:WRITE-XMLSCHEMA(cTargetType, cFile, lFormatted,  cEncoding, lMinSchema).
  */ 
 END PROCEDURE.   