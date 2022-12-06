   /*SETUSE.I*/
   uservar = USERID.
   IF uservar NE {setuser.I} THEN DO:
      SETUSERID({setuser.I},{setpwd.I}).
   END.
   FIND FIRST _FILE WHERE _FILE._File-Name = orginaltab NO-LOCK NO-ERROR.
   IF NOT AVAILABLE _FILE THEN DO:
      {UNSETUSE.I}
      RETURN.
   END.
  
  