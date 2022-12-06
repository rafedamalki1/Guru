
/*------------------------------------------------------------------------
    File        : ONNILAGERWEB.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Feb 23 16:39:51 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE oClient AS IHttpClient NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
DEFINE VARIABLE oRequest AS IHttpRequest NO-UNDO.
DEFINE VARIABLE oEntity AS Object NO-UNDO.
DEFINE VARIABLE lRetOK AS LOGICAL   NO-UNDO.

DEFINE VARIABLE onniArtikelLong AS LONGCHAR NO-UNDO.
DEFINE VARIABLE respDoc AS HANDLE NO-UNDO.

oClient =  ClientBuilder:Build():Client.

CREATE X-DOCUMENT respDoc.

