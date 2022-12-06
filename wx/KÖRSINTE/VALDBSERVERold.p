
/*------------------------------------------------------------------------
    File        : VALDBSERVERold.p
    Purpose     : VALDBSERVERold.p


    Syntax      :

    Description : FLYTTAR PROGRAM FRÅN WEBCLIENTPLATS TILL WTID PÅ SERVERS  

    Author(s)   : 
    Created     : Mon Aug 24 13:46:04 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/


DEFINE VARIABLE guruwtidir AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcguruwtidir AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcguruwtidirstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.

DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hkeyvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAns AS CHARACTER NO-UNDO.
{HKEYADMPER.I}  
{HKEYCURRENTUSER.I}
ASSIGN

companyname = "SOFTWARE\Elpool i Umeå AB\"
appnamn = "GuruOnWeb11".

LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
   companyname = "SOFTWARE\Wow6432Node\Elpool i Umeå AB\".
   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
END.   
LOAD companyname BASE-KEY hkeyvar NO-ERROR. 
USE companyname NO-ERROR. 
GET-KEY-VALUE SECTION appnamn KEY "ApplicationDirectory" VALUE vAns.
UNLOAD companyname NO-ERROR.
wcguruwtidir = vAns + "\WTID".


 ASSIGN 
 companyname = "SOFTWARE\PSC\PROGRESS\".
 IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN appnamn = PROVERSION.
 ELSE appnamn = "11.2".
 LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.


 IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
    appnamn = "11.4".
    LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
       appnamn = "11.5".
       LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
    END.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
       appnamn = "11.6".
       LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
    END.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
       appnamn = "11.7".
       LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
    END.
 END.
   
 IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
    MESSAGE "Funkar inte! wcguruwtidir" wcguruwtidir skip
    "companyname" companyname skip
    "appnamn" appnamn
    VIEW-AS ALERT-BOX.
    RETURN.
 
END. 

UNLOAD companyname + appnamn + "\".
/*letAr efter PROGRESS INSTALLATIONEN*/  
hkeyvar = "HKEY_LOCAL_MACHINE".
LOAD companyname BASE-KEY hkeyvar NO-ERROR.
USE companyname. 
 
GET-KEY-VALUE SECTION appnamn + "\Startup" KEY "DLC" VALUE guruwtidir.
UNLOAD companyname.
  
guruwtidir = REPLACE(guruwtidir,"DLC","GURU\WTID").
guruwtidir = 'robocopy "' + wcguruwtidir + '" "' + guruwtidir + '" *.* /mir'.
/*
guruwtidir = wcguruwtidir + 'Gurucopy "' + wcguruwtidir + '" "' + guruwtidir + '" *.* /mir'.


wcguruwtidirstart = REPLACE(wcguruwtidir,'Elpool i Umeå AB','"Elpool i Umeå AB"'). 
guruwtidir = '"' + wcguruwtidirstart + '\Gurucopy " "' + wcguruwtidir + '" "' + guruwtidir + '" *.* /mir'.
*/
MESSAGE guruwtidir
VIEW-AS ALERT-BOX
QUESTION BUTTONS OK-CANCEL UPDATE valk1 AS LOGICAL.       
CASE valk1:
   WHEN TRUE THEN DO:
   END.
   WHEN FALSE THEN DO:
      RETURN NO-APPLY.  
   END.   
END CASE.

OS-COMMAND SILENT VALUE(guruwtidir).
MESSAGE "Klart!"
VIEW-AS ALERT-BOX.