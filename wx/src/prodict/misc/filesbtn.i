/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* filesbtn.i--defines a Files... button, with a specified name.*/
/**
** PARAMETERS:
** &NAME    button name
** &NOACC   if present leave off accelerator on Files button
**/
&IF "{&NAME}" = "" &THEN
   &SCOPED-DEFINE BTN-NAME btn_File /*Default button name is btn_File*/
&ELSE
   &SCOPED-DEFINE BTN-NAME {&NAME}
&ENDIF

&IF "{&NOACC}" = "" &THEN
   DEFINE BUTTON {&BTN-NAME} LABEL "&Files..." 
&ELSE
   DEFINE BUTTON {&BTN-NAME} LABEL "Files..." 
&ENDIF
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
   SIZE 11 by 1
&ENDIF
   .
