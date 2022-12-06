
/*------------------------------------------------------------------------
    File        : dstestapp.P
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Jun 14 12:52:26 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

{SparaDynDSstar.I}

DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.

DEFINE VARIABLE usersparrtth AS HANDLE NO-UNDO.
DEFINE VARIABLE usersparrbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE sparrvad  AS CHARACTER NO-UNDO.
DEFINE VARIABLE SparrDS AS HANDLE NO-UNDO.
DEFINE VARIABLE Kalknumanvegenbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE Kalknumanvegensubbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE Kalknumanvegentth AS HANDLE NO-UNDO.
DEFINE VARIABLE Kalknumanvegensubtth AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER inglobanv AS CHARACTER NO-UNDO.
CREATE WIDGET-POOL "DynTable" NO-ERROR.
/*
WEBUSERDATE.APPID = "IPCHK"  
WEBUSERDATE.IDUSER = Computer_LanIP
WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 1)
WEBUSERDATE.SOKCHAR[2] = datoruser.

WEBUSERDATE.APPID = "USERCHK"  
WEBUSERDATE.IDUSER = GuruAnvandare
WEBUSERDATE.SOKCHAR[1] = STRING(INTEGER(WEBUSERDATE.SOKCHAR[1]) + 1).
WEBUSERDATE.SOKCHAR[2] = datoruser.
WEBUSERDATE.SOKCHAR[3] = Computer_LanIP.
*/

PROCEDURE SparrAnv_UI :
   DEFINE OUTPUT PARAMETER DATASET-HANDLE SparrDS BIND.
   DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
   /*sparrvad = "WEBUSERDATE.APPID = " + "IPCHK" + " OR WEBUSERDATE.APPID = '" + "USERCHK" + "'".
   sparrvad = "(" + "WEBUSERDATE.APPID = " + "IPCHK" + " OR WEBUSERDATE.APPID = " + "USERCHK" + ")" + " AND INTEGER(WEBUSERDATE.SOKCHAR[1] > STRING(4) ".*/
   sparrvad =  "WEBUSERDATE.APPID = 'IPCHK' OR WEBUSERDATE.APPID = 'USERCHK'"  .
   
   FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.   
   RUN SparrCreate_UI.
   RUN GetDatasetDeftt_UI ("SparrDS").  
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(usersparrbuffh).
   DatasetDeftt.pcSources[1] = "WEBUSERDATE".
   DatasetDeftt.pcSourceKeys[1] = "APPID,IDUSER".
   DatasetDeftt.pcKeyValue[1] = sparrvad.
   RUN DefAndLoadDs_UI IN dyndamicDSh 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE SparrDS BIND).
   SPARAXML = "C:\CTestA" + STRING(TIME) + ".xml". 
   SparrDS:WRITE-XML("FILE", SPARAXML). 
   RETURN.
END PROCEDURE.
   
PROCEDURE SparrCreate_UI :
   
   CREATE TEMP-TABLE usersparrtth IN WIDGET-POOL "DynTable".
   usersparrtth:CREATE-LIKE("WEBUSERDATE").
   usersparrtth:ADD-NEW-FIELD("TTRECID","RECID").   
   /*usersparrtth:ADD-NEW-FIELD("DATADB","CHARACTER").
   usersparrtth:ADD-NEW-INDEX("DATADB").
   usersparrtth:ADD-INDEX-FIELD("DATADB","DATADB").
   usersparrtth:ADD-INDEX-FIELD("DATADB","FORETAG").
   usersparrtth:ADD-INDEX-FIELD("DATADB","ANVANDARE").*/
   usersparrtth:TEMP-TABLE-PREPARE("Sparrusiptt").
   usersparrbuffh = usersparrtth:DEFAULT-BUFFER-HANDLE.
      
END PROCEDURE.
PROCEDURE AvsSparrUser_UI :
   DELETE OBJECT usersparrtth NO-ERROR.
   usersparrtth = ?.
   DELETE OBJECT usersparrbuffh NO-ERROR.
   usersparrbuffh = ?.
   DELETE PROCEDURE dyndamicDSh NO-ERROR.
   dyndamicDSh = ?.
   DELETE WIDGET-POOL "DynTable" NO-ERROR. 
END PROCEDURE.

PROCEDURE AnvEgenCreate_UI :
   
   CREATE TEMP-TABLE Kalknumanvegentth IN WIDGET-POOL "DynTable".
   Kalknumanvegentth:CREATE-LIKE("KALKNUMANVEGEN").
   Kalknumanvegentth:ADD-NEW-FIELD("TTRECID","RECID").
   Kalknumanvegentth:ADD-NEW-FIELD("VIARBKOD","CHARACTER",0,"","SPARAD").
   Kalknumanvegentth:ADD-NEW-INDEX("MATRIS").
   Kalknumanvegentth:ADD-INDEX-FIELD("MATRIS","MATRIS").
   Kalknumanvegentth:ADD-INDEX-FIELD("MATRIS","ANVANDARE").
   
   Kalknumanvegentth:ADD-NEW-INDEX("KLOGSUBID").
   Kalknumanvegentth:ADD-INDEX-FIELD("KLOGSUBID","KLOGSUBID").
   Kalknumanvegentth:ADD-NEW-INDEX("ARBKOD").
   Kalknumanvegentth:ADD-INDEX-FIELD("ARBKOD","ARBKOD").
   Kalknumanvegentth:ADD-INDEX-FIELD("ARBKOD","LOPNR").
   Kalknumanvegentth:ADD-INDEX-FIELD("ARBKOD","NUM").
   Kalknumanvegentth:ADD-NEW-INDEX("NUM").
   Kalknumanvegentth:ADD-INDEX-FIELD("NUM","NUM").

   Kalknumanvegentth:TEMP-TABLE-PREPARE("Kalknumegen").
   Kalknumanvegenbuffh = Kalknumanvegentth:DEFAULT-BUFFER-HANDLE.
   CREATE TEMP-TABLE Kalknumanvegensubtth IN WIDGET-POOL "DynTable".
   Kalknumanvegensubtth:CREATE-LIKE("KALKNUMANVEGENSUB").
   Kalknumanvegensubtth:ADD-NEW-FIELD("TTRECID","RECID").
   Kalknumanvegensubtth:ADD-NEW-INDEX("BENAMNING").
   Kalknumanvegensubtth:ADD-INDEX-FIELD("BENAMNING","BENAMNING").
   
   Kalknumanvegensubtth:TEMP-TABLE-PREPARE("Kalknumegensub").
   Kalknumanvegensubbuffh = Kalknumanvegensubtth:DEFAULT-BUFFER-HANDLE.   
END PROCEDURE.


PROCEDURE laddaKalkylAnvEgenDS_UI :
   DEFINE INPUT  PARAMETER vad AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER DATASET-HANDLE KalkylAnvEgenDS BIND.
   
   RUN AnvEgenCreate_UI.
   RUN GetDatasetDeftt_UI ("KalkylAnvEgenDS").  
   DatasetDeftt.antaltab = 2.
   DatasetDeftt.pcBuffers[1] = STRING(Kalknumanvegenbuffh).
   DatasetDeftt.pcBuffers[2] = STRING(Kalknumanvegensubbuffh). 
   DatasetDeftt.pcRelFields[1] = "ANVANDARE,ANVANDARE,NUM,NUM".
   DatasetDeftt.pcSources[1] = "KALKNUMANVEGEN".
   DatasetDeftt.pcSources[2] = "KALKNUMANVEGENSUB".
   DatasetDeftt.pcSourceKeys[1] = "ANVANDARE,NUM".
   DatasetDeftt.pcSourceKeys[2] = "ANVANDARE,NUM,NUMSUBID".
   DatasetDeftt.pcKeyValue[1] = "KALKNUMANVEGEN.ANVANDARE = '" + vad + "'".
   RUN DefAndLoadDs_UI IN dyndamicDSh 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE KalkylAnvEgenDS BIND).
  
  
END PROCEDURE.




PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "DynTable".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
   
PROCEDURE CloseCustomQuery:
   DEFINE INPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CustomQueryh:QUERY-CLOSE()  NO-ERROR.
   CustomQueryh = ?.
END PROCEDURE.
