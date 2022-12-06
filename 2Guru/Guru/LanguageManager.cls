 
 /*------------------------------------------------------------------------
    File        : LanguageManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Wed May 02 09:56:25 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Guru.LanguageManager: 
   {SPRAKTEMP.i}
   DEFINE PUBLIC VARIABLE LanguageId AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE SprakTT AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE SprakStrangTT AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE AppServerHandle AS HANDLE NO-UNDO.

   DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.


   CONSTRUCTOR PUBLIC LanguageManager(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:AppServerHandle = r:DatabaseManager:Global:AppServerHandle.
      THIS-OBJECT:LoadLanguage(Guru.Konstanter:globsprak).
   END CONSTRUCTOR.

   METHOD PUBLIC  System.Collections.ArrayList GetLanguages():
      DEFINE VARIABLE Returner AS System.Collections.ArrayList NO-UNDO.
      Returner = NEW System.Collections.ArrayList().
      RUN hamtasprak IN AppServerHandle (OUTPUT TABLE spraktemp).
      FOR EACH spraktemp NO-LOCK:
         Returner:Add(NEW Guru.Language(spraktemp.ID, spraktemp.BENAMNING)).
      END.
      RETURN Returner.
   END METHOD.

  {TranslateConstants.I}
      
   METHOD PUBLIC CHARACTER GetStringAsMessage(INPUT string_id AS INTEGER):
      FIND FIRST sprakstrangtemp WHERE sprakstrangtemp.SPRAKID = LanguageId AND sprakstrangtemp.SOKID = string_id AND 
      sprakstrangtemp.SOKCHAR = "" NO-ERROR.
      IF NOT AVAILABLE (sprakstrangtemp) THEN DO:
         RUN hamtasprakstrang IN AppServerHandle (INPUT LanguageId, INPUT string_id, OUTPUT TABLE sprakstrangtemp APPEND).
         FIND FIRST sprakstrangtemp WHERE sprakstrangtemp.SPRAKID = LanguageId AND sprakstrangtemp.SOKID = string_id AND 
         sprakstrangtemp.SOKCHAR = "" NO-ERROR.
         IF NOT AVAILABLE(sprakstrangtemp) THEN DO:
            System.Windows.Forms.MessageBox:Show("An error has occured while trying to load data " + STRING(string_id) + " from language " + STRING(LanguageId) + ". Contact Elpool at +46 090-184540 or via our website www.elpool.se.").
            Guru.Konstanter:globsprak         = 0.
            LanguageId = Guru.Konstanter:globsprak.
            RETURN "".
         END.
      END.
      RETURN sprakstrangtemp.BENAMNING + " (" + STRING(sprakstrangtemp.SPRAKID) + "." + STRING(sprakstrangtemp.SOKID) + ")".
   END METHOD.
   
END CLASS.