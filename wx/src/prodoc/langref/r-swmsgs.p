DEFINE VARIABLE newlang AS CHARACTER FORMAT "x(16)"
                        LABEL "Language" NO-UNDO.
DEFINE VARIABLE msgfile AS CHARACTER.

SET newlang HELP "Enter the new language for messages.".

IF newlang = "English"
THEN ASSIGN msgfile = "promsgs".
ELSE ASSIGN msgfile = "prolang/promsgs." + LC(SUBSTRING(newlang, 1, 3)).

IF SEARCH(msgfile) <> ?
THEN DO:
   PROMSGS = msgfile.
   MESSAGE "Messages will now be taken from" PROMSGS.
END.
ELSE DO:
   MESSAGE "Cannot find" msgfile.
   UNDO, RETRY. 
END.
