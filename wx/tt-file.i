/*------------------------------------------------------------------------

  File: tt-file.i

  Description: include file for using tt-file.p

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Jeff Pilant

  Revision History:
  Version Date         Description
  1.0     06-MAR-2002  Created

------------------------------------------------------------------------*/
&IF DEFINED(TT-FILE-DEF) <> 1 &THEN
  &global-define TT-FILE-DEF    tt-file.i

  &global-define tt-fmt-hm      ":9999"
  &global-define tt-fmt-hm-a    ":9999 AM"
  &global-define tt-fmt-hms     "::999999"
  &global-define tt-fmt-hms-a   "::999999 AM"
  &global-define tt-fmt-mdy     "99/99/9999"
  &global-define tt-fmt-dmy     "99-99-99"
  &global-define tt-fmt-md-y    "99.99.99"
  &global-define tt-fmt-ymd     "999999"
  &global-define tt-fmt-cymd    "99999999"

  define variable tt-text   as character initial "TXT":U no-undo.
  define variable tt-excel  as character initial "XLS":U no-undo.
  define variable tt-csv    as character initial "CSV":U no-undo.
  define variable tt-browse as character initial "BRS":U no-undo.
  define variable tt-sylk   as character initial "SLK":U no-undo.
  define variable tt-df     as character initial "DF":U  no-undo.
&ENDIF
