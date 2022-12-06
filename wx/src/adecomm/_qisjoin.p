/**************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                            */
/* All rights reserved.  No part of this program or document  */
/* may be  reproduced in  any form  or by  any means without  */
/* permission  in  writing  from  Progress Software Corp.     */
/**************************************************************/
/*
 * _qisjoin.p
 *
 *    Detirmines if two fields are "OF" joinable
 *
 *  Input Parameters
 *
 *    rightField - The "right" field to be joined
 *
 *   leftField   - THe "left" side of the join
 *
 *  Output Parameters
 *
 *   joinable    - Logical telling if the feilds are OF joinable
 */

{ adecomm/browqury.i }

define input  parameter rightField as character no-undo.
define input  parameter leftField  as character no-undo.
define output parameter joinable   as logical   no-undo.

define variable cDbRight as character no-undo.
define variable cDbLeft  as character no-undo.
define variable isOk     as logical   no-undo.

if (num-entries (rightField, ".") = 1) then
    assign
        cDbRight  = cCurrentDb + "." + rightField
        cDbLeft   = cCurrentDb + "." + leftField
    .
else
    assign
        cDbRight  = rightField
        cDbLeft   = leftField
    .

run "adecomm/_j-test.p" (cDbLeft, cDbRight, output isOk).
if not isOK then run "adecomm/_j-test.p" (cDbRight, cDbLeft, output isOK).

joinable = isOk.

