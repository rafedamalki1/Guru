
/*------------------------------------------------------------------------
    File        : locktest.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Aug 31 13:10:28 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttLock
  FIELD LockId LIKE _Lock._Lock-Id
  FIELD LockUsr LIKE _Lock._Lock-Usr
  FIELD LockName LIKE _Lock._Lock-Name
  FIELD LockTable LIKE _Lock._Lock-Table
  FIELD LockFlags LIKE _Lock._Lock-flags
  INDEX LockIdx IS PRIMARY UNIQUE LockId.

FOR EACH _Lock NO-LOCK:
  IF _Lock._Lock-Usr = ? THEN NEXT .
  CREATE ttLock.
  ASSIGN
      LockId    = _Lock._Lock-Id
      LockUsr   = _Lock._Lock-Usr
      LockName  = _Lock._Lock-Name
      LockTable = _Lock._Lock-Table
      lockFlags = _Lock._Lock-flags.
END.

FOR EACH ttlock:
  FIND _Trans NO-LOCK WHERE  _Trans._Trans-Usrnum = ttLock.LockUsr NO-ERROR.
  FIND _File NO-LOCK WHERE _File-Number = ttLock.LockTable.

  MESSAGE
   "Transaction Id:~t" (IF AVAILABLE _Trans THEN _Trans._Trans-Id ELSE ?) "~n"
   "User Number:~t" ttLock.LockUsr "~n"
   "User Name~t" ttLock.LockName "~n"
   "Table Number:~t" ttLock.LockTable "~n"
   "Table Name:~t" _File-Name "~n"
   "Flags:~t" ttLock.LockFlags
   VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.