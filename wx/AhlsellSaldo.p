
/*------------------------------------------------------------------------
    File        :.
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : RUN C:\delad\pro116\GuruAnders\wx\AhlsellSaldo.p (INPUT-OUTPUT TABLE eslagersatatustt)
    Created     : Tue Nov 16 13:20:32 CET 2021
    Notes       :
  
  {ESLAGERSTATUSTT.I}
  CREATE eslagersatatustt.
  eslagersatatustt.ANTAL = 3. 
  eslagersatatustt.UTANEENR = "100078".
  eslagersatatustt.ENR = "100078".
  RUN C:\delad\pro116\GuruAnders\wx\AhlsellSaldo.p (INPUT-OUTPUT TABLE eslagersatatustt)
  ----------------------------------------------------------------------*/

USING OpenEdge.Core.*.
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.HTTP.Lib.ClientLibraryBuilder.
USING OpenEdge.Net.URI.


{ESLAGERSTATUSTT.I}

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eslagersatatustt.
DEFINE TEMP-TABLE Body NO-UNDO
   FIELD Body AS CHARACTER.
DEFINE TEMP-TABLE GetQuantityResponse NO-UNDO
   FIELD GetQuantityResponse AS CHARACTER.   
DEFINE TEMP-TABLE GetQuantityResult NO-UNDO
   FIELD ArticleNo AS CHARACTER 
   FIELD InStockQuantity AS CHARACTER 
   FIELD EnoughQuantity AS CHARACTER.
DEFINE VARIABLE lRetOK AS LOGICAL   NO-UNDO.
DEFINE VARIABLE oRequest     AS IHttpRequest         NO-UNDO.
DEFINE VARIABLE oResponse    AS IHttpResponse        NO-UNDO.
DEFINE VARIABLE oURI         AS URI                  NO-UNDO.
DEFINE VARIABLE oRequestBody AS OpenEdge.Core.String NO-UNDO.
DEFINE VARIABLE hXMLHandle   AS HANDLE               NO-UNDO.
DEFINE VARIABLE lcXML        AS LONGCHAR             NO-UNDO.
DEFINE VARIABLE chelp AS CHARACTER NO-UNDO FORMAT "x(64)".
DEFINE VARIABLE chelp_ED AS CHARACTER VIEW-AS EDITOR SIZE 70 BY 15.
DEFINE VARIABLE chelpi AS INTEGER NO-UNDO.
DEFINE VARIABLE chelpislut AS INTEGER NO-UNDO.
FOR EACH eslagersatatustt WHERE NO-LOCK:

   oRequestBody =  NEW OpenEdge.Core.String(
                   '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
                    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                    xmlns:xsd="http://www.w3.org/2001/XMLSchema">
                    <soap:Body>
                    <GetQuantity xmlns="http://tempuri.org/">
                    <articleNo>' + eslagersatatustt.UTANEENR + '</articleNo>
                    </GetQuantity>
                    </soap:Body>
                    </soap:Envelope>'  ).

   oURI = URI:Parse("http://perimeter2.ahlsell.com/inbound/soap/stockrequest").
   oRequest = RequestBuilder:Post(oUri, oRequestBody)
                         :ContentType('text/xml;charset=UTF-8')
                         :AcceptAll()
                         :AddHeader('SOAPAction', 'http://tempuri.org/Add';)
                         :Request.

   oResponse = ClientBuilder:Build()
                         :Client:Execute(oRequest).

   IF oResponse:StatusCode <> 200 THEN DO:
      MESSAGE "http error: " oResponse:StatusCode VIEW-AS ALERT-BOX. 
      RETURN ERROR "Request Error: " + STRING(oResponse:StatusCode).
   END.
   ELSE DO:
      hXMLHandle = CAST(oResponse:Entity,WidgetHandle):Value.
      hXMLHandle:SAVE('LONGCHAR',lcXML).
      /*
      lRetOK = TEMP-TABLE Body:GetQuantityResponse:READ-XML("LONGCHAR", lcXML, "EMPTY", ?, FALSE).
      FOR EACH Body,   
         EACH GetQuantityResponse,
         EACH GetQuantityResult WHERE:
         IF AVAILABLE GetQuantityResult THEN DO: 
            IF GetQuantityResult.EnoughQuantity = "J" THEN DO:
               eslagersatatustt.ANTALILAGER = DECIMAL(GetQuantityResult.InStockQuantity).
               IF eslagersatatustt.ANTALILAGER >= eslagersatatustt.ANTAL THEN eslagersatatustt.SVAR = "OK".
               ELSE eslagersatatustt.SVAR = "För få i lager".
            END.
            ELSE DO:
               eslagersatatustt.SVAR = "Lagerförs inte!".
            END.
         END.
         ELSE DO:
            eslagersatatustt.SVAR = "Lagerförs inte!".
         END.      
      END.
      */
      
      chelp_ED = STRING(lcXML). 
      chelp = SUBSTRING(STRING(lcXML),15 + INDEX(STRING(lcXML),"EnoughQuantity"),1).
      MESSAGE  STRING(lcXML)
      VIEW-AS ALERT-BOX.
      
      IF chelp = "J" THEN DO:
         chelpi = INDEX(STRING(lcXML),"InStockQuantity").
         chelpislut = INDEX(STRING(lcXML),"</InStockQuantity",chelpi) - 1. 
         chelp = SUBSTRING(STRING(lcXML),16 + chelpi,chelpislut - (15 + chelpi)).
         eslagersatatustt.ANTALILAGER = DECIMAL(chelp).
         IF eslagersatatustt.ANTALILAGER >= eslagersatatustt.ANTAL THEN eslagersatatustt.SVAR = "OK".
         ELSE eslagersatatustt.SVAR = "För få i lager".
     
         /*
         EnoughQuantity 
         InStockQuantity
         */
      END.
      ELSE DO:
         eslagersatatustt.SVAR = "Finns inte i lager!".
      END.
     
   END.  
END.
/*
FOR EACH eslagersatatustt WHERE NO-LOCK:
   DISPLAY 
   eslagersatatustt.enr  eslagersatatustt.ANTAL eslagersatatustt.ANTALILAGER eslagersatatustt.SVAR.
END.
<?xml version="1.0" encoding="UTF-8" ?>
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
<soap:Body>
   <GetQuantityResponse xmlns="http://tempuri.org/">
       <GetQuantityResult>
            <articleNo>100078</articleNo>
            <InStockQuantity>8302</InStockQuantity>
             <EnoughQuantity>J</EnoughQuantity>
        </GetQuantityResult>
    </GetQuantityResponse>
 </soap:Body></soap:Envelope>

UPDATE chelp_ED WITH FRAME CCC.
*/
