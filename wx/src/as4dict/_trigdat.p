/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _trigdat.p

Description:
   Produce a report on all the schema triggers that indicate where the 
   specified procedure is found (full path) and what the CRC status is.

Input Parameters:
   p_DbId    - Id of the _Db record for this database.

Author: Laura Stern

Date Created: 11/18/92 

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs to file accesses. 
Modified on 02/13/95 by D. McMann to work with Progress/400 data dictionary.
----------------------------------------------------------------------------*/


DEFINE INPUT PARAMETER p_DbId AS RECID NO-UNDO.

DEFINE SHARED STREAM rpt.

Define var crc_val as integer NO-UNDO.
Define var name	   as char    NO-UNDO.
Define var event   as char    NO-UNDO.
Define var proc    as char    NO-UNDO.
Define var crc     as char    NO-UNDO.
Define var any_t   as logical NO-UNDO.  /* any table triggers listed */.
Define var any_f   as logical NO-UNDO.  /* any field triggers listed  */
Define var flags   as char    NO-UNDO.

FORM
   name  FORMAT "x(18)"  COLUMN-LABEL "Table/Field Name"
   event FORMAT "x(6)"   COLUMN-LABEL "Event"
   crc   FORMAT "x(5)"   COLUMN-LABEL "Check!CRC"
   flags FORMAT "x(5)"   COLUMN-LABEL "Flags"
   proc  FORMAT "x(38)"  COLUMN-LABEL "Procedure"
   WITH FRAME shotrig USE-TEXT STREAM-IO DOWN.

/* Go through all tables */
for each as4dict.p__File NO-LOCK:
 
   any_t = no.
   for each as4dict.p__Trgfl where as4dict.p__Trgfl._File-number =
                                   as4dict.p__File._File-number NO-LOCK: 
                                   
      assign
      	 name = (if NOT any_t then p__File._File-name else "")
      	 any_t = yes
      	 event = p__Trgfl._Event
      	 proc = SEARCH(p__Trgfl._Proc-Name)
      	 rcode-info:filename = proc
      	 crc_val = rcode-info:crc-value
      	 crc = (if p__Trgfl._Trig-CRC = ? then "no" else "yes")
      	 flags = (if p__Trgfl._Override = "y" then "*" else "") + 
      	       	 (if crc = "yes" AND crc_val = ? then "nr" else "") +
      	       	 (if crc = "yes" AND crc_val <> ? AND
      	       	     crc_val <> p__Trgfl._Trig-CRC
      	       	     then "m" else "")
      	 .

      /* In case procedure is in a library, just display name even though
      	 we couldn't find the file.
      */
      if proc = ? then proc = p__Trgfl._Proc-Name.

      display STREAM rpt
      	 name event crc flags proc with frame shotrig.
      down STREAM rpt with frame shotrig.
   end.

   /* Field info will be interspersed throughout the report, each
      under the table it belongs to.
   */
   for each as4dict.p__Field WHERE as4dict.p__Field._File-number =
                           as4dict.p__File._File-number NO-LOCK:

      any_f = no.
      for each as4dict.p__Trgfd where as4dict.p__Trgfd._File-number =
               as4dict.p__Field._File-number
               AND as4dict.p__Trgfd._Fld-number = as4dict.p__Field._fld-number
                          NO-LOCK:
      	 /* If the table name hasn't been listed yet, display it so user
      	    knows what tables these fields belong to. 
      	 */
      	 if NOT any_t then
      	 do:
      	    display STREAM rpt
      	       p__File._File-name @ name with frame shotrig.
      	    down STREAM rpt with frame shotrig.
      	    any_t = yes.
      	 end.
      	 
	 assign
      	    /* field name will be indented slightly from file name */
      	    name = (if NOT any_f then "  " + as4dict.p__Field._Field-Name else "")
      	    any_f = yes
	    event = as4dict.p__Trgfd._Event
	    proc = SEARCH(as4dict.p__Trgfd._Proc-Name)
	    rcode-info:filename = proc
	    crc_val = rcode-info:crc-value
	    crc = (if as4dict.p__Trgfd._Trig-CRC = ? then "no" else "yes")
      	    flags = (if as4dict.p__Trgfd._Override = "Y" then "*" else "") + 
      	       	    (if crc = "yes" AND crc_val = ? then "nr" else "") +
      	       	    (if crc = "yes" AND crc_val <> ? AND
      	       	     	crc_val <> as4dict.p__Trgfd._Trig-CRC
      	       	     	then "m" else "")
      	 .
   
      /* In case procedure is in a library, just display name even though
      	 we couldn't find the file.
      */
      if proc = ? then proc = as4dict.p__Trgfd._Proc-Name.

	 display STREAM rpt
	    name event crc flags proc with frame shotrig.
	 down STREAM rpt with frame shotrig.
      end.
   end.

   /* Put an extra line between tables */   
   down STREAM rpt with frame shotrig.
end.


