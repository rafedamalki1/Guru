
/*------------------------------------------------------------------------
    File        : AnvsparrHelpStart.p
    Purpose     : 

    Syntax      :run Modules\Register\AnvsparrHelpStart.p ("Usersparr_UI").

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE Usersparr_UI :
   /*DEFINE INPUT PARAMETER TABLE FOR rubrikTT.*/
   DEFINE VARIABLE AnvSparr   AS Modules.Register.AnvSparr NO-UNDO.
   DEFINE VARIABLE startanvspRoot AS Guru.Root NO-UNDO.
    
   /*får windows,dbmanager mm*/
   startanvspRoot = NEW Guru.Root().
   /*kopplar till rätt db.cls via dbmanager*/
   startanvspRoot:StartAnvSparrDb().
   /*kör connect i db.cls*/
   /*startanvspRoot:StartAnvSparr(THIS-PROCEDURE).*/
   
   /*kör MP.cls som kör INiTT för att hämta tt*/
  
   AnvSparr = NEW Modules.Register.AnvSparr(INPUT startanvspRoot).
   WAIT-FOR AnvSparr:ShowDialog().
   /*EMPTY TEMP-TABLE rubrikTT NO-ERROR.*/ 
   DELETE OBJECT startanvspRoot NO-ERROR.
   DELETE OBJECT AnvSparr NO-ERROR.
   AnvSparr = ?.
   startanvspRoot = ?.
END PROCEDURE.

