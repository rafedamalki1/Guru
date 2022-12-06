
/*------------------------------------------------------------------------
    File        : TranslateConstants.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Thu Mar 21 09:26:24 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
METHOD PUBLIC VOID LoadLanguage(INPUT lang_id AS INTEGER):
  THIS-OBJECT:LanguageId = lang_id.
  
  /*THIS-OBJECT:TranslateConstants().*/
  
END METHOD.
METHOD PUBLIC VOID TranslateConstants():
   {TranslateConstantsSet.I}
   
END METHOD.
/*GURUKONSTANTER*/
METHOD PUBLIC CHARACTER GetStringCH(INPUT stringc_id AS CHARACTER):
   FIND FIRST sprakstrangtemp WHERE sprakstrangtemp.SPRAKID = LanguageId AND sprakstrangtemp.SOKCHAR = stringc_id NO-ERROR.
   IF NOT AVAILABLE (sprakstrangtemp) THEN DO:
      RUN hamtasprakstrangc IN AppServerHandle (INPUT LanguageId, INPUT stringc_id, OUTPUT TABLE sprakstrangtemp APPEND).
      FIND FIRST sprakstrangtemp WHERE sprakstrangtemp.SPRAKID = LanguageId AND sprakstrangtemp.SOKCHAR = stringc_id NO-ERROR.
      IF NOT AVAILABLE(sprakstrangtemp) THEN  DO:
         System.Windows.Forms.MessageBox:Show("An error has occured while trying to load data " + STRING(stringc_id) + " from language " + STRING(LanguageId) + ". Contact Elpool at +46 090-184540 or via our website www.elpool.se.").
         Guru.Konstanter:globsprak         = 0.
         LanguageId = Guru.Konstanter:globsprak.
      END.
      
   END.
   RETURN sprakstrangtemp.BENAMNING.
END METHOD.
/*MEDDELANDESTRÄNGAR*/
METHOD PUBLIC CHARACTER GetString(INPUT string_id AS INTEGER):
   FIND FIRST sprakstrangtemp WHERE sprakstrangtemp.SPRAKID = LanguageId AND sprakstrangtemp.SOKID = string_id NO-ERROR.
   IF NOT AVAILABLE (sprakstrangtemp) THEN DO:
      RUN hamtasprakstrang IN AppServerHandle (INPUT LanguageId, INPUT string_id, OUTPUT TABLE sprakstrangtemp APPEND ).
      FIND FIRST sprakstrangtemp WHERE sprakstrangtemp.SPRAKID = LanguageId AND sprakstrangtemp.SOKID = string_id NO-ERROR.
      IF NOT AVAILABLE(sprakstrangtemp) THEN DO:
         System.Windows.Forms.MessageBox:Show("An error has occured while trying to load data " + STRING(string_id) + " from language " + STRING(LanguageId) + ". Contact Elpool at +46 090-184540 or via our website www.elpool.se.").
         Guru.Konstanter:globsprak         = 0.
         LanguageId = Guru.Konstanter:globsprak.
         RETURN "".
      END.
   END.
   RETURN sprakstrangtemp.BENAMNING.
END METHOD.
