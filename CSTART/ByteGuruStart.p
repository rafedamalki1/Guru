
/*------------------------------------------------------------------------
    File        : ByteGuruStart.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Aug 24 13:46:04 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER gftg AS CHARACTER NO-UNDO.

{HKEYSTART.I}
DEFINE VARIABLE guruwtidir AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcguruwtidir AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcguruwtidirstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.

DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.

DEFINE VARIABLE vAns AS CHARACTER NO-UNDO.
IF gftg = "SNAT" THEN.
ELSE RETURN. 
{HKEYCURRENTUSER.I}
ASSIGN 
companyname = "SOFTWARE\Elpool i Umeå AB\"

appnamn = "GuruOnWeb11\ProwcappLocator".
LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
LOAD companyname BASE-KEY hkeyvar NO-ERROR. 
USE companyname NO-ERROR. 


PUT-KEY-VALUE SECTION appnamn KEY "URL" VALUE {WWWGURU11.I} NO-ERROR.


GET-KEY-VALUE SECTION appnamn KEY "URL" VALUE vAns.
UNLOAD companyname NO-ERROR.



MESSAGE appnamn vAns
VIEW-AS ALERT-BOX.

/* "http://www2.guruonweb.se/guruonweb11/" 

"http://www.guruonweb.se/elpoolweb/GuruOnWeb11/"
*/
/*
     EKGOnWeb11
     valdbtemp.WWWSTART =  "http://www2.guruonweb.se/guruonweb11/"
     valdbtemp.WWWSTART10 = "http://www2.guruonweb.se/guruonweb10/" /*eltel*/
     valdbtemp.WWWSTART =  "http://srv01449/guruonweb11/"           /*ONE*/
     valdbtemp.WWWSTART = "http://goliat/guruonweb11/"              /*KALM*/
     valdbtemp.WWWSTART10 = "http://goliat/guruonweb10/"            /*KALM*/
     valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(26)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(4)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(147)) + "/guruonweb11/"        /*MITT*/
     valdbtemp.WWWSTART = "http://172.26.4.75/guruonweb11/"          /*SUND*/
     valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + "/guruonweb11/"     /*VATTEN*/
      */
      /*
DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hkeyvar AS CHARACTER NO-UNDO.

DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.
DEFINE VARIABLE urlvar AS CHARACTER NO-UNDO.
ASSIGN 
companyname = "SOFTWARE\Elpool i Umeå AB\"
hkeyvar = globalhkeyvar.
IF PROVERSION BEGINS "10" THEN appnamn = "GuruOnWeb11\ProwcappLocator".
IF PROVERSION BEGINS "11" THEN appnamn = "GuruOnWeb11\ProwcappLocator".


LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
LOAD companyname BASE-KEY hkeyvar NO-ERROR. 
USE companyname NO-ERROR. 
GET-KEY-VALUE SECTION appnamn KEY "URL" VALUE urlvar.
IF PROVERSION BEGINS "10" THEN DO:
   IF urlvar NE  valdbtemp.WWWSTART10 THEN  PUT-KEY-VALUE SECTION appnamn KEY "URL" VALUE  valdbtemp.WWWSTART10 NO-ERROR.
END.
IF PROVERSION BEGINS "11" THEN DO:
   IF urlvar NE  valdbtemp.WWWSTART THEN  PUT-KEY-VALUE SECTION appnamn KEY "URL" VALUE  valdbtemp.WWWSTART NO-ERROR.
END.


UNLOAD companyname + appnamn + "\"  NO-ERROR.
UNLOAD companyname  NO-ERROR.      
      */
