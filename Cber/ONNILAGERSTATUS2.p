
/*------------------------------------------------------------------------
    File        : ONNILAGERSTATUS2.p
    Purpose     : 

    Syntax      :LAGERSALDO

    Description : 

    Author(s)   : 
    Created     : Wed Oct 25 14:00:06 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
USING Progress.Lang.Object.
USING Progress.Lang.Object.
USING OpenEdge.Core.WidgetHandle.
USING OpenEdge.Core.String.

USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.IHttpClientLibrary.
USING OpenEdge.Net.HTTP.lib.ClientLibraryBuilder.
USING OpenEdge.Net.HTTP.RequestBuilder.
USING OpenEdge.Net.HTTP.ResponseBuilder. 

DEFINE VARIABLE oLib        AS OpenEdge.Net.HTTP.IHttpClientLibrary NO-UNDO.
DEFINE VARIABLE oHttpClient AS OpenEdge.Net.HTTP.IHttpClient        NO-UNDO.
DEFINE VARIABLE oRequest    AS IHttpRequest                         NO-UNDO.
DEFINE VARIABLE oResponse   AS IHttpResponse                        NO-UNDO.

DEFINE TEMP-TABLE Onninen NO-UNDO
   FIELD ArticleNo AS CHARACTER 
   FIELD InStockQuantity AS CHARACTER .
{ESLAGERSTATUSTT.I}
DEFINE INPUT  PARAMETER pnrkod AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eslagersatatustt.
DEFINE VARIABLE oEntity AS Object NO-UNDO.
DEFINE VARIABLE lRetOK AS LOGICAL   NO-UNDO.

DEFINE VARIABLE onniArtikelLong AS LONGCHAR NO-UNDO.
DEFINE VARIABLE respDoc AS HANDLE NO-UNDO.

    oLib        = ClientLibraryBuilder:Build():sslVerifyHost(NO):LIBRARY NO-ERROR.
    oHttpClient = ClientBuilder:Build():UsingLibrary(oLib):Client NO-ERROR.
CREATE X-DOCUMENT respDoc.
DEFINE VARIABLE onniart AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
   FOR EACH eslagersatatustt WHERE NO-LOCK:
     
      onniart = eslagersatatustt.ENR.
      IF SUBSTRING(onniart,1,1) = "E" THEN.
      ELSE onniart = "E" + onniart.
      /*
       oRequest =  RequestBuilder:Get('157.144.13.135 /prd/seatp?ArticleNo=' + onniart):REQUEST.
      */
      oRequest =  RequestBuilder:Get('http://eai.onninen.com/prd/seatp?ArticleNo=' + onniart):REQUEST NO-ERROR.
      IF oRequest = ? THEN DO:
         MESSAGE "Ingen kontakt med Onninen eller så saknar ni behörighet!" 
         VIEW-AS ALERT-BOX.
         RETURN.
      END.  
    
      oResponse   = ResponseBuilder:Build():Response.
      oHttpClient:Execute(oRequest, oResponse).
      oEntity = oResponse:Entity.
      IF oResponse:StatusCode EQ 200 THEN DO:
         
         respDoc = CAST(oEntity, WidgetHandle):Value.
         respDoc:SAVE("LONGCHAR", onniArtikelLong).
         
         lRetOK = TEMP-TABLE Onninen:READ-XML("LONGCHAR", onniArtikelLong, "EMPTY", ?, FALSE).
         FIND FIRST Onninen WHERE NO-LOCK NO-ERROR.
         IF AVAILABLE Onninen THEN DO:       
            eslagersatatustt.ANTALILAGER = DECIMAL(Onninen.InStockQuantity).
            IF eslagersatatustt.ANTALILAGER >= eslagersatatustt.ANTAL THEN eslagersatatustt.SVAR = "OK".
            ELSE eslagersatatustt.SVAR = "För få i lager".
         END.
         ELSE DO:
            eslagersatatustt.SVAR = "Lagerförs inte!".
         END.   
      END.
      ELSE DO:
          {musarrow.i}
          MESSAGE "Det gick inte att ansluta till Onninens lager!" VIEW-AS ALERT-BOX.    
      END.
   END.