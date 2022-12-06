 
 /*------------------------------------------------------------------------
    File        : Language
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Wed May 02 10:03:00 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Guru.Language INHERITS System.Object: 

   CONSTRUCTOR PUBLIC Language(INPUT id AS INTEGER, INPUT n AS CHARACTER):
      THIS-OBJECT:LanguageName = n.
      THIS-OBJECT:LanguageId = id.
   END CONSTRUCTOR.

   DEFINE PUBLIC VARIABLE LanguageName AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE LanguageId AS INTEGER NO-UNDO. 

END CLASS.