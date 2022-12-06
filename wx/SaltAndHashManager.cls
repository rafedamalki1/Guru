 
 /*------------------------------------------------------------------------
    File        : SaltAndHashManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Mon Feb 13 12:51:19 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Guru.SaltAndHashManager:
   METHOD PUBLIC VOID SaltSatt(anv AS CHARACTER,saltpswd  AS CHARACTER) :
       {SaltAndHash.I}
      
       DO TRANSACTION:
          FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = anv EXCLUSIVE-LOCK NO-ERROR.
          saltpswd = ENCODE(saltpswd).
          rKey = GENERATE-PBE-KEY(saltpswd , rSalt).
          IF AVAILABLE ANVANDARE THEN ANVANDARE.AV-LOSEN = STRING(BASE64-ENCODE(rKey)).
       END.
      
   END METHOD.
   METHOD PUBLIC LOGICAL SaltUpp(anv AS CHARACTER,saltpswd  AS CHARACTER) :
      {SaltAndHash.I}
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = anv NO-LOCK NO-ERROR. 
      IF AVAILABLE ANVANDARE THEN DO:
         saltpswd = ENCODE(saltpswd).
         rKey = GENERATE-PBE-KEY(saltpswd , rSalt).
         IF ANVANDARE.AV-LOSEN = STRING(BASE64-ENCODE(rKey)) THEN RETURN TRUE.
      END.
      RETURN FALSE.
   END METHOD.
   METHOD PUBLIC CHARACTER SaltRetur(saltpswd  AS CHARACTER) :
      {SaltAndHash.I}
      {SaltReturn.i}
   END METHOD.
   CONSTRUCTOR PUBLIC SaltAndHashManager () :
      
     
   END CONSTRUCTOR.
   DESTRUCTOR PUBLIC SaltAndHashManager():
   
   END DESTRUCTOR.
    

END CLASS.