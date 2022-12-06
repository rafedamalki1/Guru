
/*------------------------------------------------------------------------
    File        : SELGALAGERSTATUS.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Oct 25 14:00:06 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
USING Progress.Lang.Object.

USING OpenEdge.Core.WidgetHandle.
USING OpenEdge.Core.String.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.RequestBuilder.
USING OpenEdge.Net.HTTP.IHttpClient.
/*XML request DATASET*/
DEFINE TEMP-TABLE balanceRequest NO-UNDO
   FIELD item AS CHARACTER 
      XML-NODE-TYPE "ATTRIBUTE" 
   FIELD cuno AS CHARACTER 
      XML-NODE-TYPE "ATTRIBUTE" 
   FIELD pocd AS CHARACTER 
      XML-NODE-TYPE "ATTRIBUTE" 
   FIELD balanceRequest_Text AS CHARACTER 
      XML-NODE-TYPE "TEXT" .

DEFINE DATASET balanceRequests  
   FOR balanceRequest.
   
/*XML response DATASET*/
DEFINE TEMP-TABLE balanceResponse NO-UNDO
   FIELD cuno AS CHARACTER 
      XML-NODE-TYPE "ATTRIBUTE" 
   FIELD item AS CHARACTER 
      XML-NODE-TYPE "ATTRIBUTE" 
   FIELD pocd AS CHARACTER 
      XML-NODE-TYPE "ATTRIBUTE" .

DEFINE TEMP-TABLE balances NO-UNDO
   FIELD name AS CHARACTER 
      XML-NODE-TYPE "ATTRIBUTE" 
   FIELD qty AS INTEGER 
      XML-NODE-TYPE "ATTRIBUTE" 
   FIELD balances_Text AS CHARACTER 
      XML-NODE-TYPE "TEXT" 
   FIELD balanceResponse_id AS RECID 
      XML-NODE-TYPE "HIDDEN" .

DEFINE DATASET balanceResponses  
   FOR balanceResponse, balances
   PARENT-ID-RELATION RELATION1 FOR balanceResponse, balances
      PARENT-ID-FIELD balanceResponse_id.

DEFINE VARIABLE oClient AS IHttpClient NO-UNDO.
DEFINE VARIABLE oRequest AS IHttpRequest NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
DEFINE VARIABLE oEntity AS Object NO-UNDO.

DEFINE VARIABLE balanceRequestsLong AS LONGCHAR NO-UNDO.
DEFINE VARIABLE balanceResponsLong AS LONGCHAR NO-UNDO.

DEFINE VARIABLE respDoc AS HANDLE NO-UNDO.

DEFINE VARIABLE reqDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE reqDocWid AS WidgetHandle NO-UNDO.


{ESLAGERSTATUSTT.I}
DEFINE INPUT  PARAMETER pnrkod AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eslagersatatustt.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.  

/* Procedure invocation of GetAvailableStock operation. */
FOR EACH eslagersatatustt WHERE NO-LOCK:
   CREATE balanceRequest.
   ASSIGN 
   balanceRequest.item = eslagersatatustt.ENR
   balanceRequest.cuno = ""
   balanceRequest.pocd = pnrkod.
END.

       
DATASET balanceRequests:WRITE-XML("LONGCHAR", balanceRequestsLong).

CREATE X-DOCUMENT reqDoc.

reqDoc:LOAD("LONGCHAR", balanceRequestsLong, FALSE).
reqDocWid = NEW WidgetHandle(reqDoc).

oClient =  ClientBuilder:Build():Client.
/*
DATASET balanceRequests:WRITE-XML("FILE","C:\SELGA.XML").
*/
oRequest = RequestBuilder:Put('http://stockbalance.rexel.se:17000/Rexel/rest/warehouse/multi', reqDocWid):AcceptXML():REQUEST NO-ERROR.
IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
   MESSAGE "Ingen kontakt med Rexel eller så saknar ni behörighet!" SKIP 
   SKIP
      "Vill du se felmeddelandena ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
   UPDATE view-errs AS LOGICAL .       
   IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
      MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
      ERROR-STATUS:GET-MESSAGE(ivar)
      VIEW-AS ALERT-BOX.
   END.     
   RETURN.     
END.
IF oRequest = ? THEN DO:
   MESSAGE "Ingen kontakt med Rexel eller så saknar ni behörighet!" 
   VIEW-AS ALERT-BOX.
   RETURN.
END.   
oResponse = oClient:Execute(oRequest) NO-ERROR.
IF oResponse:StatusCode NE 200 THEN DO:
   MESSAGE "Ingen kontakt med Rexel" 
   VIEW-AS ALERT-BOX.
   RETURN.    
END.

/*
oResponse = ClientBuilder:Build():Client:Execute(oRequest).
*/
oEntity = oResponse:Entity.

respDoc = CAST(oEntity, WidgetHandle):Value.
respDoc:SAVE('LONGCHAR', balanceResponsLong).
 
DATASET balanceResponses:READ-XML("LONGCHAR", balanceResponsLong, "EMPTY", ?, FALSE).
/*
DATASET balanceResponses:WRITE-XML("FILE","C:\SELGA.XML").
*/
FOR EACH balanceResponse,
EACH balances WHERE balances.balanceResponse_id = recid(balanceResponse): 
   FOR EACH  eslagersatatustt WHERE eslagersatatustt.ENR = balanceResponse.Item :
      IF eslagersatatustt.ANTALILAGER = 0 THEN DO:
         ASSIGN 
         eslagersatatustt.SVAR =  balances.name 
         eslagersatatustt.ANTALILAGER = balances.qty.
      END.   
      
      /*
      IF eslagersatatustt.SVAR = "" THEN DO:
         ASSIGN 
         eslagersatatustt.SVAR =  balances.name 
         eslagersatatustt.ANTALILAGER = balances.qty.
      END.
      */   
   END.
END.   
