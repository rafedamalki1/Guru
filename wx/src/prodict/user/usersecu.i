/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/* usersecu.i - security instructions portion of frame */

"Examples:"                                         AT 2  VIEW-AS TEXT
"*" 	       	     	      	       	     	    AT 4  VIEW-AS TEXT
  "- All users (login Ids) are allowed access."     AT 23 VIEW-AS TEXT SKIP
"<user>,<user>,etc."                                AT 4  VIEW-AS TEXT
  "- Only these users have access."                 AT 23 VIEW-AS TEXT SKIP
"!<user>,!<user>,*"                                 AT 4  VIEW-AS TEXT
  "- All except these users have access."           AT 23 VIEW-AS TEXT SKIP
"acct*":t18     	     	      	       	    AT 4  VIEW-AS TEXT
  "- Only users that begin with ~"acct~" allowed."  AT 23 VIEW-AS TEXT 
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
     SKIP({&VM_WIDG})  /*Don't need and can't afford blank line in TTY*/
&ENDIF
"Do not use spaces in the string (they will be taken literally)." 
      	       	     	      	       	     	    AT 2  VIEW-AS TEXT


