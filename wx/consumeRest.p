/*consumeRest.p*/
USING Progress.Lang.Object.
USING OpenEdge.Core.WidgetHandle.
USING OpenEdge.Core.String.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.RequestBuilder.

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


DEFINE VARIABLE oRequest AS IHttpRequest NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
DEFINE VARIABLE oEntity AS Object NO-UNDO.

DEFINE VARIABLE balanceRequestsLong AS LONGCHAR NO-UNDO.
DEFINE VARIABLE balanceResponsLong AS LONGCHAR NO-UNDO.

DEFINE VARIABLE respDoc AS HANDLE NO-UNDO.

DEFINE VARIABLE reqDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE reqDocWid AS WidgetHandle NO-UNDO.


/*Skapa requestposter*/
CREATE balanceRequest.
ASSIGN 
       balanceRequest.item = "1830122"
       balanceRequest.cuno = ""
       balanceRequest.pocd = "".
       
CREATE balanceRequest.
ASSIGN 
       balanceRequest.item = "1414002"
       balanceRequest.cuno = ""
       balanceRequest.pocd = "".
       /*
       balanceRequest.pocd = "43363".
       */
DATASET balanceRequests:WRITE-XML("LONGCHAR", balanceRequestsLong).
/*
DATASET balanceRequests:WRITE-XML("FILE", "c:\testar\request.xml", TRUE).
*/
CREATE X-DOCUMENT reqDoc.

reqDoc:LOAD("LONGCHAR", balanceRequestsLong, FALSE).
reqDocWid = NEW WidgetHandle(reqDoc).

oRequest = RequestBuilder:Put('http://stockbalance.rexel.se:17000/Rexel/rest/warehouse/multi', reqDocWid):AcceptXML()
:Request.

oResponse = ClientBuilder:Build():Client:Execute(oRequest).
oEntity = oResponse:Entity.

respDoc = CAST(oEntity, WidgetHandle):Value.
respDoc:SAVE('LONGCHAR', balanceResponsLong).
 
DATASET balanceResponses:READ-XML("LONGCHAR", balanceResponsLong, "EMPTY", ?, FALSE).


DATASET balanceResponses:WRITE-XML("FILE", "c:\protemp11\respons.xml", TRUE).   

FOR EACH balanceResponse,
EACH balances: 
   MESSAGE balances.name  balances.qty balanceResponse.Item 
   VIEW-AS ALERT-BOX. 
   
END.   

/* 
MESSAGE
oResponse:StatusCode SKIP
oResponse:StatusReason SKIP
VIEW-AS ALERT-BOX.
    
FOR EACH balances:
   MESSAGE 
   balances.balanceResponse_id
   balances.balances_Text
   balances.name 
balances.qty
VIEW-AS ALERT-BOX.

END.
 */
 