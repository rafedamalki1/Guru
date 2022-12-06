/* row-list.i - 4/15/96 */
&IF "{1}" eq "{&FIRST-EXTERNAL-TABLE}" &THEN       
  IF key-name eq ? THEN tbl-list = "{1}":U.    
&ELSE
  IF tbl-list <> "":U THEN tbl-list = tbl-list + ",":U.
  tbl-list = tbl-list + "{1}":U.
&ENDIF
