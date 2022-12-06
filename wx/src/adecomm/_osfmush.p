/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/*
**  Program: adecomm/_osfmush.p
**       By: wlb
** Descript: mush to parts of a file spec together
** Last change: jep 12/14/95 - WIN95-LFN
**              Added WIN32 to OPSYS test as part of temporary support.
*/

DEFINE INPUT  PARAMETER p_spec1   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER p_spec2   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER p_outspec AS CHARACTER NO-UNDO.

/* if both parms are NOT set then just put results in outspec */
IF p_spec1 = "" OR p_spec2 = "" THEN
p_outspec = p_spec1 + p_spec2.

ELSE  /* add a backslash between files for DOS */
IF CAN-DO("WIN32,MSDOS,OS2", OPSYS) AND NOT
  (p_spec1 MATCHES "*:" OR
  p_spec1 MATCHES "*~~~\" OR p_spec2 matches "~~~\*" OR
  p_spec1 MATCHES "*/" OR p_spec2 matches "/*") THEN
p_outspec = p_spec1 + "~\" + p_spec2.

ELSE  /* add a slash between files for DOS */
IF OPSYS = "UNIX" AND NOT(p_spec1 MATCHES "*/" OR p_spec2 matches "/*") THEN
p_outspec = p_spec1 + "/" + p_spec2.

/* if then are both dirs, take out square brackets and mush together */
ELSE  /* on VMS make [adsf] + [.frgd] = [adsf.frgd] */
IF OPSYS = "VMS":u AND p_spec1 matches "*]":u AND p_spec2 matches "[*":u THEN
p_outspec = SUBSTRING(p_spec1,1,LENGTH(p_spec1,"CHARACTER":u) - 1,
                      "CHARACTER":u) 
          + SUBSTRING(p_spec2,2,-1,"CHARACTER":u).

ELSE  /* add slash in file name for BTOS */
IF OPSYS = "BTOS" AND NOT p_spec1 matches "*>" THEN
p_outspec = p_spec1 + "/":u + p_spec2.

/*
**  All of the above IFs checked for special cases where we could NOT
**  just concatinate the parms together.  Once we get to here, we know
**  that we CAN just concatinate them together.
*/
ELSE
p_outspec = p_spec1 + p_spec2.

/* Trim off a trailing \ or / as long as it does not follow a drive indicator */
IF NOT CAN-DO("WIN32,MSDOS,OS2":u, OPSYS) OR
  (NOT p_outspec MATCHES "*:/":u AND NOT p_outspec MATCHES "*:~~~\":u) THEN
  p_outspec = RIGHT-TRIM(p_outspec,"/~\":u).
