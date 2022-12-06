

/*------------------------------------------------------------------------
    File        : AllaDatabaserStart.p
    Purpose     : 
                
                    run Register\AllaDatabaserStart.p.
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

RUN AllaDatabaserU_UI.

PROCEDURE AllaDatabaser_UI :
   DEFINE VARIABLE startalladbroot AS Guru.Root NO-UNDO.
   DEFINE VARIABLE AllaDB   AS Register.AllaDatabaser NO-UNDO.
    
   /*får windows,dbmanager mm*/
   startalladbroot = NEW Guru.Root().
   AllaDB = NEW Register.AllaDatabaser(INPUT startalladbroot).
   WAIT-FOR AllaDB:ShowDialog().
   /*EMPTY TEMP-TABLE rubrikTT NO-ERROR.*/ 
   DELETE OBJECT startalladbroot NO-ERROR.
   DELETE OBJECT AllaDB NO-ERROR.
   AllaDB = ?.
   startalladbroot = ?.  

   
   
END PROCEDURE.

PROCEDURE AllaDatabaserU_UI :
   
   DEFINE VARIABLE AllaDB   AS Register.AllaDatabaser NO-UNDO.
    
   /*får windows,dbmanager mm*/
   
   AllaDB = NEW Register.AllaDatabaser().
   WAIT-FOR AllaDB:ShowDialog().
   /*EMPTY TEMP-TABLE rubrikTT NO-ERROR.*/ 
   
   DELETE OBJECT AllaDB NO-ERROR.
   AllaDB = ?.
  

   
   
END PROCEDURE.
