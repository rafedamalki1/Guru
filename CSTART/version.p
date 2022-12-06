
    /* ts-version.p - BEGIN */
   
    DEF VAR mc AS CHAR FORMAT "x(20)". 
    DEF VAR vc AS CHAR FORMAT "x(20)". 
    DEF VAR vi AS INT.
    vc = FILL( ' ', 256 ).
    RUN getFullVersion( OUTPUT vc, INPUT 256, OUTPUT vi ).
    mc = "Full version".
    display  mc vc  with frame cc down.
    DOWN 1 WITH FRAME cc. 
    RUN getBuildNumber ( OUTPUT vi ).
    mc = "Build Number".
    display mc   vi with frame cc down.
    DOWN 1 WITH FRAME cc.
    RUN getVersion ( OUTPUT vc, INPUT 256, OUTPUT vi ).
    mc = "version".
    display mc  vc  with frame cc down.
    DOWN 1 WITH FRAME cc.
    RUN getMajorNumber ( OUTPUT vi ).
    mc = "major".
    display mc    vi with frame cc down.
    DOWN 1 WITH FRAME cc.
    RUN getMinorNumber ( OUTPUT vi ).
    mc = "minor".
    display mc    vi with frame cc down.
    DOWN 1 WITH FRAME cc.
    RUN getMaintenanceLevel ( OUTPUT vc, INPUT 256, OUTPUT vi ).
    mc = "level".
    display mc   vc  with frame cc down.
    DOWN 1 WITH FRAME cc.
    RUN getServicePackNumber ( OUTPUT vi ).
    mc = "sp".
    display mc   vi  with frame cc down.
    DOWN 1 WITH FRAME cc.
    RUN getTemporaryFixNumber ( OUTPUT vi ).
    mc = "tfixn".
    display mc  vi  with frame cc down.
    DOWN 1 WITH FRAME cc.
    /* ts-version.p - END */
PROCEDURE getFullVersion EXTERNAL "versioninfo.dll" :
   DEFINE OUTPUT PARAM pcver AS CHARACTER. 
   DEFINE INPUT PARAM pilen AS LONG.
   DEFINE RETURN PARAM iret AS LONG.
END.
PROCEDURE getBuildNumber EXTERNAL "versioninfo.dll" :
   DEFINE RETURN PARAM iret AS LONG.
END.
PROCEDURE getVersion EXTERNAL "versioninfo.dll" :
   DEFINE OUTPUT PARAM pcver AS CHARACTER.
   DEFINE INPUT PARAM pilen AS LONG. 
   DEFINE RETURN PARAM iret AS LONG.
END.

PROCEDURE getMajorNumber EXTERNAL "versioninfo.dll" :
   DEFINE RETURN PARAM iret AS LONG.
END.

PROCEDURE getMinorNumber EXTERNAL "versioninfo.dll" :
   DEFINE RETURN PARAM iret AS LONG.
END.

PROCEDURE getMaintenanceLevel EXTERNAL "versioninfo.dll" :
   DEFINE OUTPUT PARAM pcver AS CHARACTER.
   DEFINE INPUT PARAM pilen AS LONG.
   DEFINE RETURN PARAM iret AS LONG.
END.

PROCEDURE getServicePackNumber EXTERNAL "versioninfo.dll" :
   DEFINE RETURN PARAM iret AS LONG.
END.

PROCEDURE getTemporaryFixNumber EXTERNAL "versioninfo.dll" : 
   DEFINE RETURN PARAM iret AS LONG.
END.



    
    


