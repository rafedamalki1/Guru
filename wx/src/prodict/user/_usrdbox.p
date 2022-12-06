/*************************************************************/
/* Copyright (c) 1984-1999 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/* _usrdbox.p - dialog box routine - gets a true/false value from user */

/*
Input text comes in 'question' parameter, and may be any length.  This
program splits it up into 45-character chunks, breaking at spaces.
Embedded '!' marks get translated into line-feeds (like in
column-labels).
*/


/* The yes-val and not-val parms are supported for backward compatibility 
   of API but are no longer supported.  The buttons will always be
   "Yes" and "No".

History:
   Added assignment of answer to false 98-11-19-021 D. McMann 12/17/98
*/   
   
DEFINE INPUT-OUTPUT PARAMETER answer   AS LOGICAL   NO-UNDO.
DEFINE INPUT        PARAMETER yes-val  AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER not-val  AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER question AS CHARACTER NO-UNDO.

/* s-alert uses a carat instead of ! (why?) */
ASSIGN question = REPLACE(question, "!", "^").

RUN adecomm/_s-alert.p 
   (INPUT-OUTPUT answer, INPUT "QUESTION", 
    INPUT "YES-NO",      INPUT question).


