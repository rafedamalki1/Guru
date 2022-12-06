
/*------------------------------------------------------------------------
    File        : BACKUPSTATUS.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Aug 31 15:02:10 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER dbnamn AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER bckstatus AS CHARACTER NO-UNDO.
FUNCTION namndb RETURNS CHARACTER :   
   IF PDBNAME(1) = ? THEN RETURN ?.
   ELSE IF R-INDEX(PDBNAME(1),"\") = 0 THEN RETURN PDBNAME(1).
   ELSE RETURN SUBSTRING(PDBNAME(1),R-INDEX(PDBNAME(1),"\") + 1).
     
END FUNCTION.
FIND FIRST _DbStatus NO-LOCK.
IF AVAILABLE _DbStatus THEN DO:
   bckstatus = _DbStatus._dbStatus-fbDate.
END.   
dbnamn = namndb().
