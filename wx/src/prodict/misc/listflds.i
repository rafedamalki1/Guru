/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* listflds.i - _rptflds.p include file */
/*
{&msg} - first line message
{&fld} - field
{&siz} - field width
*/
c = SUBSTRING(IF {&fld} = ? THEN "" ELSE {&fld},{&siz} + 1).
PUT UNFORMATTED {&msg} ": " SUBSTRING({&fld},1,{&siz}) SKIP.
DO WHILE c <> "":
  PUT UNFORMATTED FILL(" ",LENGTH({&msg}) + 2) SUBSTRING(c,1,{&siz}) SKIP.
  c = TRIM(SUBSTRING(c,{&siz} + 1)).
END.
