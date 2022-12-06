
/*------------------------------------------------------------------------
    File        : dstest.p
    Purpose     : 

    Syntax      : run C:\DELAD\pro9\guru\wx\dstest.p.

    Description : 

    Author(s)   : 
    Created     : Thu Nov 03 11:07:13 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/
 DEFINE VARIABLE usersparrbuffh AS HANDLE NO-UNDO.
 DEFINE VARIABLE AppServerHandles AS HANDLE NO-UNDO.
 DEFINE VARIABLE AppServerHandle AS HANDLE NO-UNDO.
 DEFINE VARIABLE SparrDS AS HANDLE NO-UNDO.
 DEFINE VARIABLE KalkylAnvEgenDS AS HANDLE NO-UNDO.
 DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
 DEFINE VARIABLE Kalknumanvegenbuffh AS HANDLE NO-UNDO.
 DEFINE VARIABLE Kalknumanvegensubbuffh AS HANDLE NO-UNDO.
/* ***************************  Definitions  ************************** */
 /*
 RUN AnvEgen.
 */
 RUN AnvSparr.
 
 PROCEDURE AnvSparr :
      RUN C:\DELAD\pro9\guru\wx\dstestapp.P PERSISTENT SET AppServerHandles (INPUT CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)).         
      
      RUN SparrAnv_UI IN AppServerHandles (OUTPUT DATASET-HANDLE SparrDS BIND).
      IF VALID-HANDLE(SparrDS) THEN DO:
        MESSAGE "UT NU"
        VIEW-AS ALERT-BOX.
         SPARAXML = "C:\CTestC" + STRING(TIME) + ".xml". 
         SparrDS:WRITE-XML("FILE", SPARAXML).
          MESSAGE SparrDS:NUM-BUFFERS SparrDS:GET-BUFFER-HANDLE(1) SparrDS:GET-BUFFER-HANDLE(2) SparrDS:GET-BUFFER-HANDLE("Sparrusiptt")
         VIEW-AS ALERT-BOX.
         usersparrbuffh = SparrDS:GET-BUFFER-HANDLE(1).         
         /*
         THIS-OBJECT:AnvSparrTracking(FALSE).
         */         
      END.      
   
 END PROCEDURE.
 
 
PROCEDURE AnvEgen:
      RUN C:\DELAD\pro9\guru\wx\dstestapp.P PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).
      RUN laddaKalkylAnvEgenDS_UI IN AppServerHandle (INPUT CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79),OUTPUT DATASET-HANDLE KalkylAnvEgenDS BIND).
      
      IF VALID-HANDLE(KalkylAnvEgenDS) THEN DO:
         Kalknumanvegenbuffh = KalkylAnvEgenDS:GET-BUFFER-HANDLE(1).
         Kalknumanvegensubbuffh = KalkylAnvEgenDS:GET-BUFFER-HANDLE(2).
         MESSAGE Kalknumanvegenbuffh Kalknumanvegensubbuffh
         VIEW-AS ALERT-BOX.
          SPARAXML = "C:\CTestC" + STRING(TIME) + ".xml". 
         KalkylAnvEgenDS:WRITE-XML("FILE", SPARAXML).
         
      END.
      
END PROCEDURE.
