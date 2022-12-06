/*------------------------------------------------------------------------

  File: tt-file.p

  Description: Convert a temp table into a file

  Input Parameters:
      temp table handle
      filename
      file type

  Output Parameters:
      <none>

  Author: Jeff Pilant

  Usage:
      This program is designed to assist in the generation of output 
      for other programs.  Its basic function is to take a temp table 
      and create a file with the contents of the temp table in a 
      convenient form.  Several outputs are supported: comma-separated 
      values (*.CSV), Excel spreadsheet (*.XLS), plain text (*.TXT), 
      a quick browse, a Sylk spreadsheet (*.SLK), and a Dictionary 
      Definition file (*.DF).

      The program is called as follows:
	      RUN tt-file.p(temp-table tt-name:handle, filename, switch)

      | Note: if the definition of the temp table does not add the 
      | rcode-information tag, then tt-file.p cannot access the label 
      | or format of the temp table.  It will instead use the field's 
      | name and the data-type default format.

      The filename, if not supplied, will be prompted for.  [Except if 
      a browse is selected.]  This will be used to store the results 
      of the program.

      The switch is one of the character values defined in tt-file.i.  
      These are: tt-text, tt-excel, tt-csv, tt-browse, tt-sylk and 
      tt-df.  If left blank, the type will be derived from the 
      filename (even if prompted).  If the filename does not have a 
      valid type, it will be assumed a text file.

      | Note: Excel spreadsheets require access to the desktop, so 
      | they cannot be run in batch mode.

      The Sylk spreadsheet format was added to allow generation of 
      spreadsheets in batch programs.  While the format is outdated, 
      it is still accepted by Excel.  The Sylk format allows some 
      special formatting options in this routine for date and time 
      values.  Time can be formatted as "HH:MM", "HH:MM AM", "HH:MM:SS",
      "HH:MM:SS AM".  Dates can be formatted as "MM/DD/YY", "DD-MMM-YY",
      "Month DD, YYYY", "YYMMDD", "YYYYMMDD".  To assist you, the 
      following preprocessor constants are defined: tt-fmt-hm, 
      tt-fmt-hm-a, tt-fmt-hms, tt-fmt-hms-a, tt-fmt-mdy, tt-fmt-dmy, 
      tt-fmt-md-y, tt-fmt-ymd, and tt-fmt-cymd.  You can use them like 
      this:
          define variable date-var as date format {&tt-fmt-dmy}.
          define variable time-var as integer format {&tt-fmt-hms}.
      If you are showing a number, and you use the format chars "9" or 
      "Z", the format results in forcing that many places.  For example:
          define variable zip-code as integer format "99999".
          define variable four-dig as integer format "Z,ZZ9".

      | Note: There are lots of notes about the Sylk format in the code 
      | for those that are interested.

  Revision History:
  Version Date         Description
  1.0     06-Mar-2002  Created (TXT, XLS)
          06-Mar-2002  Added (CSV), prompt for filename
          ??-???-2002  Added (BRS)
          11-Apr-2002  Added AutoFit to Excel output
          23-May-2002  Fixed f-col()
          08-May-2002  Changed default column width for excel to a function
                       Added Title Row code to Excel output
          13-Sep-2002  Added code to fix Title Row to always be row 1
          10-Oct-2002  Added ability to do csv, txt, or xls files if
                       type not given
          16-Jul-2003  Added Sylk file type
  1.1     08-Sep-2003  Fixup Sylk date format for dates < 1/1/1900
                       Added df file type [From code found at:
                         http://www.v9stuff.com/dynexport.htm
                         thanks Tony Lavinio and Peter van Dam

------------------------------------------------------------------------*/
define input parameter htt as handle    no-undo.
define input parameter fn  as character no-undo.
define input parameter ft  as character no-undo.

/* Excel Constants */
&scoped-define xlExcel9795         43
&scoped-define xlWBATWorksheet  -4167
&scoped-define xlWorkbookNormal -4143
&scoped-define xlRight          -4152
/* Sylk Constants */
&scoped-define sylk-fmt-number  "N"
&scoped-define sylk-fmt-string  "S"
&scoped-define sylk-fmt-date    "D"
&scoped-define sylk-fmt-time    "T"
&scoped-define sylk-fmt-logical "L"
&scoped-define sylk-xls-hm      "HH:MM"
&scoped-define sylk-xls-hm-a    "HH:MM AM"
&scoped-define sylk-xls-hms     "HH:MM:SS"
&scoped-define sylk-xls-hms-a   "HH:MM:SS AM"
&scoped-define sylk-xls-mdy     "MM/DD/YY"
&scoped-define sylk-xls-dmy     "DD-MMM-YY"
&scoped-define sylk-xls-md-y    "Month DD, YYYY"
&scoped-define sylk-xls-ymd     "YYMMDD"
&scoped-define sylk-xls-cymd    "YYYYMMDD"
&scoped-define sylk-xls-title   "TITLE"
&scoped-define sylk-xls-body    "BODY"
&scoped-define sylk-fil-picture "P"
&scoped-define sylk-fil-font    "M"
&scoped-define sylk-fil-c-wid   "W"
&scoped-define sylk-fil-c-fmt   "C"
&scoped-define sylk-fil-c-title "T"

/* Include files */
{tt-file.i}

/* General variables */
define variable nf     as integer   no-undo.
define variable nx     as integer   no-undo.
define variable wf     as integer   no-undo.
define variable nc     as integer   no-undo.
define variable btt    as handle    no-undo.
define variable hwf    as handle    no-undo.
define variable hquery as handle    no-undo.
define variable hfield as handle    no-undo.
define variable lf     as character no-undo.
define variable cr     as character no-undo.
define variable qt     as character no-undo.
define variable qn     as character no-undo.
define variable ofn    as character no-undo.
define variable gfn    as logical   no-undo.
define variable cols   as character no-undo.
define variable cnt    as integer   no-undo.

/* Excel specific variables */
define variable chExcelApplication as com-handle no-undo.
define variable chWorkbook         as com-handle no-undo.
define variable chWorksheet        as com-handle no-undo.
define variable chRange            as com-handle no-undo.

/* Browse & df variables */
define variable bwin as handle no-undo.
define sub-menu m_File menu-item m_Exit label "E&xit".
define menu     m_bar  menubar sub-menu m_File label "&File".
define button   B-EXIT label "E&xit" size 8 by .92.
define button   B-CLIP label "&Clip" size 8 by .92.
define variable cw     as integer   no-undo.
define variable mw     as integer   no-undo initial 88.
define variable hq     as handle    no-undo.
define variable hb     as handle    no-undo.
define variable str    as character no-undo.
define variable tag    as character no-undo.
define variable fil-sp as character no-undo initial "".

/* Sylk variables */
define variable fmtstr as character no-undo.
define variable fmtkey as character no-undo.
define variable fmttmp as character no-undo.
define variable fmtcol as integer   no-undo.
define variable fmtrow as integer   no-undo.
define variable fmtsiz as integer   no-undo.
define variable fmtwid as character no-undo.

/* Structure for holding Sylk parts */
define temp-table tt-sylk
  field typ as character
  field seq as integer
  field val as character
  field fmt as character
  index pu-one is primary unique typ seq.

/* Structure & query for the browse */
define temp-table q-help
  field a as integer.
define query Q-BRS for q-help scrolling.
define browse Q-BRS
  query Q-BRS no-lock display
    with no-row-markers separators size 88 /* mw */ by 13 expandable.
define frame    F-MASTER
     B-EXIT at row 1 col 2
     B-CLIP at row 1 col 11
     Q-BRS  at row 2 col 2
    with 1 down no-box keep-tab-order overlay
         side-labels no-underline three-d
         at col 1 row 1 size 90 /* mw + 2 */ by 15.

/*----------------------------------------------------------------------*/

/* Get Excel column reference from column number */
function f-col returns character
  ( input pos as integer ) :
  define variable ifirst  as integer   no-undo.
  define variable isecond as integer   no-undo.
  define variable cols as character no-undo.
  ifirst  = integer(truncate((pos - 1) / 26, 0)).
  isecond = pos - (26 * ifirst).
  cols    = chr(64 + isecond).
  if ifirst > 0 then
    cols = chr(64 + ifirst) + cols.
  return cols.
end.

/* Get width needed for column using label and format */
function f-wid returns integer
  ( input hnd as handle ) :
  return integer(max(length(hnd:label), hnd:width-chars)).
end.

/* Default column width to set Excel column */
function f-pad returns integer
  ( input hnd as handle ) :
  return integer(min(1.25 * max(length(hnd:label), hnd:width-chars), 255)).
end.

/* Convert string to valid filename */
function valid-fn returns character
  ( input ifn as character ) :
  define variable ofn as character no-undo.
  ofn = ifn.
  ofn = replace(ofn, "\":U,  "_":U).
  ofn = replace(ofn, "/":U,  "_":U).
  ofn = replace(ofn, ":":U,  "_":U).
  ofn = replace(ofn, "*":U,  "_":U).
  ofn = replace(ofn, "?":U,  "_":U).
  ofn = replace(ofn, """":U, "_":U).
  ofn = replace(ofn, "<":U,  "_":U).
  ofn = replace(ofn, ">":U,  "_":U).
  ofn = replace(ofn, "|":U,  "_":U).
  if ofn = "" then ofn = "[blank]".
  return trim(ofn).
end function.

/* Get the reference used for a part */
function sylk-val returns character
  ( input p-typ as character,
    input p-fmt as character ) :
  find first tt-sylk where tt-sylk.typ = p-typ
                       and tt-sylk.fmt = p-fmt
                           no-lock no-error.
  if available(tt-sylk) then
    return p-typ + string(tt-sylk.seq).
  else
    return p-typ + "0".
end function.

/* Add a part to the Sylk structure */
procedure sylk-fmt :
  define input parameter p-typ as character no-undo.
  define input parameter p-val as character no-undo.
  define input parameter p-fmt as character no-undo.
  define variable nxt as integer no-undo.

  find last tt-sylk where tt-sylk.typ = p-typ no-lock no-error.
  if available(tt-sylk) then
    nxt = tt-sylk.seq + 1.
  else
    nxt = 0.
  create tt-sylk.
  assign tt-sylk.typ = p-typ
         tt-sylk.seq = nxt
         tt-sylk.val = p-val
         tt-sylk.fmt = p-fmt.
end procedure.

/*----------------------------------------------------------------------*/

/* If output type is blank, allow choice via filename */
if ft = tt-text  or ft = "" then fil-sp = fil-sp + ",*.txt":U.
if ft = tt-csv   or ft = "" then fil-sp = fil-sp + ",*.csv":U.
if ft = tt-excel or ft = "" then fil-sp = fil-sp + ",*.xls":U.
if ft = tt-sylk  or ft = "" then fil-sp = fil-sp + ",*.slk":U.
if ft = tt-df    or ft = "" then fil-sp = fil-sp + ",*.df":U.
fil-sp = trim(fil-sp, ",":U).
ofn = fn.
if ofn = "" and not ft = tt-browse then
do:
  system-dialog get-file ofn
    filters "Standard (":U + fil-sp + ")":U lc(fil-sp),
            "Any File (*.*)":U                "*.*":U
    ask-overwrite
    create-test-file
    default-extension lc(".":U + entry(1,fil-sp))
    save-as
    update gfn.
  if not gfn then return.
end.
/* Process blank type choice */
if ft = "" then
do:
  fil-sp = substring(ofn, length(ofn) - 2).
  case fil-sp:
    when "txt":U then ft = tt-text.
    when "xls":U then ft = tt-excel.
    when "csv":U then ft = tt-csv.
    when "xls":U then ft = tt-excel.
    when "slk":U then ft = tt-sylk.
    when "df":U  then ft = tt-df.
  end case.
end.
/* If an unknown extension is used, exit */
if ft = "" then
  return.

qn = ofn.
if not ft = tt-browse then
do:
  qn = entry(num-entries(qn, "\":U), qn, "\":U).
  qn = entry(num-entries(qn, "/":U), qn, "/":U).
end.
lf = chr(10).
cr = chr(13).
qt = chr(34).
btt = htt:default-buffer-handle.
nf  = btt:num-fields.

/*----------------------------------------------------------------------*/

case ft:
  when tt-df     then /*******************************************************/
  do:
    output to value(ofn).
    /* table */
    put unformatted 'ADD TABLE "':u   htt:name '"' skip
                    '  AREA "Schema Area"':u skip
                    '  DUMP-NAME "':u substring(htt:name, 1, 8) '"' skip(2).
    /* field */
    do wf = 1 to nf:
      assign hwf = btt:buffer-field(wf).
      put unformatted 'ADD FIELD "':u hwf:name '" OF "':u htt:name '" AS ':u hwf:data-type skip
                      '  FORMAT "':u hwf:format '"'             skip
                      '  INITIAL "':u trim(hwf:initial) '"'     skip
                      '  POSITION ':u trim(string(wf + 1))      skip.
      if hwf:data-type = "CHARACTER" then
        put unformatted '  SQL-WIDTH ' hwf:width-chars * 2 skip.
      else
        put unformatted '  SQL-WIDTH 4' skip.
      put unformatted '  LABEL "':u hwf:label '"'               skip
                      '  COLUMN-LABEL "':u hwf:column-label '"' skip.
      if hwf:help <> ? then put unformatted '  HELP "':u hwf:help '"' skip.
      put unformatted '  ORDER ':u + trim(string(wf * 10)) skip(2).
    end.
    /* index */
    do wf = 1 to 200:
      if btt:index-information(wf) = ? then leave.
      assign str = btt:index-information(wf).
      if entry(1, str) = "default" then leave.
      put unformatted 'ADD INDEX "':u entry(1, str) '" ON "':u htt:name '"' skip
                      '  AREA "Schema Area"':u skip.
      if entry(2, str) = "1" then put unformatted '  UNIQUE':u  skip.
      if entry(3, str) = "1" then put unformatted '  PRIMARY':u skip.
      if entry(4, str) = "1" then put unformatted '  WORD':u    skip.
      do nx = 5 to num-entries(str) by 2:
        tag = "0".
        tag = entry(nx + 1, str) no-error.
        put unformatted '  INDEX-FIELD "':u entry(nx, str) '" ' (if tag = "0" then 'ASCENDING ':u else 'DESCENDING ':u) skip.
      end.
      put unformatted skip(1).
    end.
    /* Got this from: http://www.v9stuff.com/dynexport.htm , thanks Tony Lavinio and Peter van Dam */
    assign nx = seek(output).
    put unformatted "." skip
                    "PSC":u skip
                    "cpstream=":u session:cpstream skip
                    "." skip
                    string(nx, "9999999999") skip.
    output close.
  end.
  when tt-text   then /*******************************************************/
  do:
    output to value(ofn).
    do wf = 1 to nf:
      hwf = btt:buffer-field(wf).
      if wf > 1 then put unformatted "  ".
      nx = hwf:extent.
      if nx = 0 then
      do:
        put unformatted
          string(replace(hwf:label, "_":U, " ":U), fill("X":U, f-wid(hwf))).
      end.
      else
      do nx = 1 to hwf:extent:
        if nx > 1 then put unformatted "  ":U.
        put unformatted
          string(replace(hwf:label, "_":U, " ":U) + "[":U + string(nx) + "]":U,
                 fill("X":U, 3 + f-wid(hwf))).
      end.
    end.
    put unformatted lf.
    create query hquery.
    hquery:set-buffers(btt:handle).
    hquery:query-prepare("FOR EACH " + htt:name).
    hquery:query-open.
    repeat:
      hquery:get-next().
      if hquery:query-off-end then leave.
      do wf = 1 to btt:num-fields:
        if wf > 1 then put unformatted "  ".
        hwf = btt:buffer-field(wf).
        nx = hwf:extent.
        if nx = 0 then
        do:
          str = hwf:string-value.
          if hwf:buffer-value = ? then str = "?".
          put unformatted string(trim(str), fill("X":U, f-wid(hwf))).
        end.
        else
        do nx = 1 to hwf:extent:
          if nx > 1 then put unformatted "  ":U.
          str = hwf:string-value(nx).
          if hwf:buffer-value(nx) = ? then str = "?".
          put unformatted string(trim(str), fill("X":U, 3 + f-wid(hwf))).
        end.
      end.
      put unformatted lf.
    end.
    hquery:query-close().
    delete object hquery.
    output close.
  end.
  when tt-excel  then /*******************************************************/
  do:
    create "Excel.Application" chExcelApplication.
    assign chExcelApplication:Visible = true.
    chExcelApplication:Workbooks:Add({&xlWBATWorksheet}).
    assign chWorkbook  = chExcelApplication:WorkBooks:Item(1)
           chWorkSheet = chExcelApplication:Sheets:Item(1).
    assign chWorkSheet:Name = valid-fn(qn).
    nc = 0.
    do wf = 1 to nf:
      hwf = btt:buffer-field(wf).
      nx  = hwf:extent.
      if nx = 0 then
      do:
        nc = nc + 1.
        cols = f-col(nc).
        chWorksheet:Range(cols + ":":U + cols):ColumnWidth = f-pad(hwf).
        chWorksheet:Range(cols + "1":U):Value = replace(hwf:label, "_":U, " ":U).
        case hwf:data-type:
          when "integer" or
          when "date"    or
          when "decimal" then
            chWorksheet:Range(cols + "1":U):HorizontalAlignment = {&xlRight}.
        end case.
      end.
      else
      do nx = 1 to hwf:extent:
        nc = nc + 1.
        cols = f-col(nc).
        chWorksheet:Range(cols + ":":U + cols):ColumnWidth = f-pad(hwf).
        chWorksheet:Range(cols + "1":U):Value = replace(hwf:label, "_":U, " ":U)
                                              + "[":U + string(nx) + "]":U.
        case hwf:data-type:
          when "integer" or
          when "date"    or
          when "decimal" then
            chWorksheet:Range(cols + "1":U):HorizontalAlignment = {&xlRight}.
        end case.
      end.
    end.
    create query hquery.
    hquery:set-buffers(btt:handle).
    hquery:query-prepare("FOR EACH " + htt:name).
    hquery:query-open.
    cnt = 1.
    repeat:
      cnt = cnt + 1.
      hquery:get-next().
      if hquery:query-off-end then leave.
      nc = 0.
      do wf = 1 to btt:num-fields:
        hwf = btt:buffer-field(wf).
        nx = hwf:extent.
        if nx = 0 then
        do:
          nc = nc + 1.
          cols = f-col(nc) + string(cnt).
          str = hwf:string-value.
          if hwf:buffer-value = ? then str = "?".
          chWorksheet:Range(cols):Value = right-trim(str).
        end.
        else
        do nx = 1 to hwf:extent:
          nc = nc + 1.
          cols = f-col(nc) + string(cnt).
          str = hwf:string-value(nx).
          if hwf:buffer-value(nx) = ? then str = "?".
          chWorksheet:Range(cols):Value = right-trim(str).
        end.
      end.
    end.
    hquery:query-close().
    delete object hquery.
    /* Adjust column sizes */
    nc = 0.
    do wf = 1 to btt:num-fields:
      hwf = btt:buffer-field(wf).
      nx = hwf:extent.
      if nx = 0 then
      do:
        nc = nc + 1.
        cols = f-col(nc).
        chRange = chWorksheet:Range(cols + ":":U + cols):Columns().
        chRange:AutoFit().
        release object chRange no-error.
      end.
      else
      do nx = 1 to hwf:extent:
        nc = nc + 1.
        cols = f-col(nc).
        chRange = chWorksheet:Range(cols + ":":U + cols):Columns().
        chRange:AutoFit().
        release object chRange no-error.
      end.
    end.

    /* Set first row as title row */
    chWorksheet:Range("A1:A1"):Select.
    chWorkbook:Windows(1):SplitColumn = 0.
    chWorkbook:Windows(1):SplitRow    = 1.
    chWorkbook:Windows(1):FreezePanes = True.

    /* Save data */
    if chExcelapplication:Version begins "8":U 
    then chWorkBook:SaveAs(ofn, {&xlExcel9795},,,,,,).
    else chWorkBook:SaveAs(ofn, {&xlWorkbookNormal},,,,,,).
    
    chWorkBook:Close().
    release object chWorkSheet  no-error.
    release object chWorkBook   no-error.
    chExcelApplication:Quit().
    release object chExcelApplication no-error.
  end.
  when tt-csv    then /*******************************************************/
  do:
    output to value(ofn).
    do wf = 1 to nf:
      if wf > 1 then put unformatted ",".
      hwf = btt:buffer-field(wf).
      nx = hwf:extent.
      if nx = 0 then
      do:
        put unformatted qt trim(replace(hwf:label, "_":U, " ":U)) qt.
      end.
      else
      do nx = 1 to hwf:extent:
        if nx > 1 then put unformatted ",":U.
        put unformatted qt trim(replace(hwf:label, "_":U, " ":U))
          "[":U string(nx) "]":U qt.
      end.
    end.
    put unformatted lf.
    create query hquery.
    hquery:set-buffers(btt:handle).
    hquery:query-prepare("FOR EACH " + htt:name).
    hquery:query-open.
    repeat:
      hquery:get-next().
      if hquery:query-off-end then leave.
      do wf = 1 to btt:num-fields:
        if wf > 1 then put unformatted ",".
        hwf = btt:buffer-field(wf).
        nx = hwf:extent.
        if nx = 0 then
        do:
          if hwf:data-type = "character" then put unformatted qt.
          put unformatted trim(hwf:string-value).
          if hwf:buffer-value = ? then put unformatted "?".
          if hwf:data-type = "character" then put unformatted qt.
        end.
        else
        do nx = 1 to hwf:extent:
          if nx > 1 then put unformatted ",":U.
          if hwf:data-type = "character" then put unformatted qt.
          put unformatted trim(hwf:string-value(nx)).
          if hwf:buffer-value(nx) = ? then put unformatted "?".
          if hwf:data-type = "character" then put unformatted qt.
        end.
      end.
      put unformatted lf.
    end.
    hquery:query-close().
    delete object hquery.
    output close.
  end.
  when tt-browse then /*******************************************************/
  do:
    btt = htt:default-buffer-handle.
    create query hquery.
    hquery:set-buffers(btt:handle).
    hquery:query-prepare("FOR EACH " + htt:name).
    Q-BRS:query = hquery.
    Q-BRS:width-chars = 1.
    cw = 2.
    do wf = 1 to btt:num-fields:
      hwf = btt:buffer-field(wf).
      nx  = hwf:extent.
      if nx = 0 then nx = 1.
      cw  = cw + f-wid(hwf) * nx.
      Q-BRS:width-chars = cw.
      Q-BRS:add-like-column(hwf).
    end.
    if cw > mw then cw = mw.
    if cw < 20 then cw = 20. /* oom for buttons */
    frame F-MASTER:width-chars          = cw + 1.
    frame F-MASTER:virtual-width-chars  = cw + 1.
    Q-BRS:width-chars = cw.
    Q-BRS:column-scrolling = false.
    hquery:query-open.

    create window bwin assign
           hidden             = yes
           title              = "tt-browse"
           height             = 15
           width              = cw + 2
           max-height         = 15
           max-width          = cw + 2
           virtual-height     = 15
           virtual-width      = cw + 2
           resize             = no
           scroll-bars        = no
           status-area        = no
           bgcolor            = ?
           fgcolor            = ?
           keep-frame-z-order = yes
           three-d            = yes
           message-area       = no
           sensitive          = yes.
    assign bwin:menubar = menu m_bar:handle.
    bwin:hidden = no.
    bwin:top-only = yes.

    on entry of bwin
    do:
      on esc bell.
      on return tab.
    end.
    on leave of bwin
    do:
      on esc end-error. 
      on return return.   
    end.
    on endkey, end-error, window-close of bwin anywhere do:
      apply "close":U to bwin.
      return no-apply.
    end.
    on choose of B-CLIP /* Clip */
    do:
      output to "clipboard".
      put unformatted replace(Q-BRS:tooltip, lf, cr + lf) cr lf.
      output close.
      apply "entry" to Q-BRS.
    end.
    on choose of B-EXIT /* Exit */
    do:
      apply "close":U to bwin.
    end.
    on choose of menu-item m_Exit /* Exit */
    do:
      apply "close":U to bwin.
    end.

    on value-changed of Q-BRS
    do:
      Q-BRS:fetch-selected-row(1) no-error.
      hq = Q-BRS:query.
      if not hq:get-current(no-lock) then
        return no-apply.
      hb = hq:get-buffer-handle(1).
      str = "".
      do wf = 1 to hb:num-fields:
        if wf > 1 then str = str + lf.
        hwf = btt:buffer-field(wf).
        nx = hwf:extent.
        if nx = 0 then
        do:
          str = str + replace(hwf:label, "_":U, " ":U) + ": ":U + hwf:string-value.
          if hwf:buffer-value = ? then str = str + "?".
        end.
        else
        do nx = 1 to hwf:extent:
          if nx = 1 then str = str + replace(hwf:label, "_":U, " ":U) + ": ":U.
                    else str = str + ", ":U.
          str = str + trim(hwf:string-value(nx)).
          if hwf:buffer-value(nx) = ? then str = str + "?".
        end.
      end.
      Q-BRS:tooltip = str.
    end.

    enable B-CLIP B-EXIT Q-BRS with frame F-MASTER in window bwin.
    view frame F-MASTER in window bwin.
    view bwin.
    Q-BRS:select-row(1).
    apply "entry" to Q-BRS.
    apply "value-changed" to Q-BRS.

    wait-for close of bwin.
    hquery:query-close().
    delete object hquery.
    on esc end-error. 
    on return return.
    delete widget bwin.

  end.

  when tt-sylk   then /*******************************************************/
  do:
    /* Prefill temp table with formats and fonts */
    run sylk-fmt({&sylk-fil-picture}, "PGeneral",         "").
    run sylk-fmt({&sylk-fil-picture}, "Pdd\-mmm\-yy",     {&sylk-xls-dmy}).
    run sylk-fmt({&sylk-fil-picture}, "Pmm/dd/yy",        {&sylk-xls-mdy}).
    run sylk-fmt({&sylk-fil-picture}, "Pmmmm\ d\,\ yyyy", {&sylk-xls-md-y}).
    run sylk-fmt({&sylk-fil-picture}, "Pyymmdd",          {&sylk-xls-ymd}).
    run sylk-fmt({&sylk-fil-picture}, "Pyyyymmdd",        {&sylk-xls-cymd}).
    run sylk-fmt({&sylk-fil-picture}, "Ph:mm",            {&sylk-xls-hm}).
    run sylk-fmt({&sylk-fil-picture}, "Ph:mm\ AM/PM",     {&sylk-xls-hm-a}).
    run sylk-fmt({&sylk-fil-picture}, "Ph:mm:ss",         {&sylk-xls-hms}).
    run sylk-fmt({&sylk-fil-picture}, "Ph:mm:ss\ AM/PM",  {&sylk-xls-hms-a}).
    run sylk-fmt({&sylk-fil-font},    "ECourier;M200;SB", {&sylk-xls-title}).
    run sylk-fmt({&sylk-fil-font},    "ECourier;M200",    {&sylk-xls-body}).

    fmtcol = 0.
    fmtkey = "".
    do wf = 1 to nf:
      hwf = btt:buffer-field(wf).
      fmtstr = hwf:format.
      /* Determine type of data in column */
      case hwf:data-type:
        when "INTEGER"   then fmttmp = {&sylk-fmt-number}. /* Number  */
        when "DECIMAL"   then fmttmp = {&sylk-fmt-number}. /* Number  */
        when "CHARACTER" then fmttmp = {&sylk-fmt-string}. /* String  */
        when "DATE"      then fmttmp = {&sylk-fmt-date}.   /* Date    */
        otherwise             fmttmp = " ".                /* Unknown */
      end case.
      /* Determine format to use... */
      /* ...for a number */
      if     (   hwf:data-type = "INTEGER"
              or hwf:data-type = "DECIMAL")
         and not hwf:format begins ":" then
      do:
        fmtstr = replace(fmtstr, "9", "0"). /* Forced character   */
        fmtstr = replace(fmtstr, "Z", "0"). /* Forced character   */
        fmtstr = replace(fmtstr, ">", "#"). /* Optional character */
        fmtstr = "P" + fmtstr.
        /* Add format if not already there */
        if sylk-val({&sylk-fil-picture}, fmtstr) = "P0" then run sylk-fmt({&sylk-fil-picture}, fmtstr, fmtstr).
      end.
      /* ...for a time */
      if hwf:data-type = "INTEGER" then
      do:
        /* check for time formats */
        case hwf:format:
          when {&tt-fmt-hm}    then do: fmtstr = {&sylk-xls-hm}.    fmttmp = {&sylk-fmt-time}. end.
          when {&tt-fmt-hm-a}  then do: fmtstr = {&sylk-xls-hm-a}.  fmttmp = {&sylk-fmt-time}. end.
          when {&tt-fmt-hms}   then do: fmtstr = {&sylk-xls-hms}.   fmttmp = {&sylk-fmt-time}. end.
          when {&tt-fmt-hms-a} then do: fmtstr = {&sylk-xls-hms-a}. fmttmp = {&sylk-fmt-time}. end.
        end case.
      end.
      /* ...for a date */
      if hwf:data-type = "DATE" then
      do:
        if fmtstr = {&tt-fmt-mdy}  then fmtstr = {&sylk-xls-mdy}.
        if fmtstr = {&tt-fmt-dmy}  then fmtstr = {&sylk-xls-dmy}.
        if fmtstr = {&tt-fmt-md-y} then fmtstr = {&sylk-xls-md-y}.
        if fmtstr = {&tt-fmt-ymd}  then fmtstr = {&sylk-xls-ymd}.
        if fmtstr = {&tt-fmt-cymd} then fmtstr = {&sylk-xls-cymd}.
      end.
      /* ...for a logical */
      if hwf:data-type = "LOGICAL" then
      do:
        fmtstr = fill("X",
                      max(length(entry(1, hwf:format, "/")),
                          length(entry(2, hwf:format, "/")))).
        fmttmp = {&sylk-fmt-logical}.
      end.
      fmtkey = fmtkey + fmttmp.
      /* Get data for column formatting and titles */
      nx = hwf:extent.
      /* Two versions: scalar and array */
      if nx = 0 then
      do:
        fmtcol = fmtcol + 1.
        fmtwid = string(fmtcol).
        /* Get column width as max(label width, data width) */
        fmtsiz = length(hwf:label).
        if fmttmp = {&sylk-fmt-logical} then fmtsiz = max(fmtsiz, length(fmtstr)).
        if fmttmp = {&sylk-fmt-time} then fmtsiz = max(fmtsiz, length(fmtstr)).
                                     else fmtsiz = max(fmtsiz, length(hwf:string-value)).
        if fmttmp = {&sylk-fmt-date} and fmtstr = {&sylk-xls-md-y} then
          fmtsiz = max(length(hwf:label), 18).
        /* Store width info */
        run sylk-fmt({&sylk-fil-c-wid},
                     "W" + fmtwid + " " + fmtwid + " " + string(fmtsiz),
                     fmtwid).
        /* Store title info (alignment and text) */
        run sylk-fmt({&sylk-fil-c-title},
                     "F;FG0" + (if fmttmp={&sylk-fmt-string} then "L" else "R") + ";SD"
                   + sylk-val({&sylk-fil-font}, {&sylk-xls-title}) + ";Y1;X" + fmtwid + lf
                   + "C;K" + qt + trim(replace(hwf:label, "_":U, " ":U)) + qt,
                     fmtwid).
        /* Store format info */
        run sylk-fmt({&sylk-fil-c-fmt},
                     sylk-val({&sylk-fil-picture}, fmtstr) + ";FG0G;C" + fmtwid,
                     fmtwid).
      end.
      else
      do:
        /* Get column width as max(label width, data width) */
        fmtsiz = length(hwf:label + "[" + string(nx) + "]").
        if fmttmp = {&sylk-fmt-logical} then fmtsiz = max(fmtsiz, length(fmtstr)).
        if fmttmp = {&sylk-fmt-time} then fmtsiz = max(fmtsiz, length(fmtstr)).
                                     else fmtsiz = max(fmtsiz, length(hwf:string-value[nx])).
        if fmttmp = {&sylk-fmt-date} and fmtstr = {&sylk-xls-md-y} then
          fmtsiz = max(fmtsiz, 18).
        do nx = 1 to hwf:extent:
          fmtcol = fmtcol + 1.
          fmtwid = string(fmtcol).
          /* Store width info */
          run sylk-fmt({&sylk-fil-c-wid},
                       "W" + fmtwid + " " + fmtwid + " " + string(fmtsiz)
                       , fmtwid).
          /* Store title info (alignment and text) */
          run sylk-fmt({&sylk-fil-c-title},
                       "F;FG0" + (if fmttmp={&sylk-fmt-string} then "L" else "R") + ";SD"
                     + sylk-val({&sylk-fil-font}, {&sylk-xls-title}) + ";Y1;X" + fmtwid + lf
                     + "C;K" + qt + trim(replace(hwf:label, "_":U, " ":U)) + "[":U + string(nx) + "]":U + qt,
                       fmtwid).
          /* Store format info */
          run sylk-fmt({&sylk-fil-c-fmt},
                       sylk-val({&sylk-fil-picture}, fmtstr) + ";FG0G;C" + fmtwid,
                       fmtwid).
        end.
      end.
    end.

    /* Write out file */
    output to value(ofn).
    /* BNF: "ID" ";P" <name> ";N" ";E"
     * ;P <name>   authoring-program
     * ;N          File uses ;N style cell protection (not ;P style).
     * ;E          NE records are redundant as formulas support external ref's directly.
     */
    put unformatted "ID;PTT-FILE;N;E" lf.
    /* BNF: "P;P" <xl-pic>
     * "P;P" <xl-pic>   Excel style picture format.  Count from zero.
     */
    for each tt-sylk where tt-sylk.typ = {&sylk-fil-picture}
                           no-lock
                        by tt-sylk.seq:
      put unformatted "P;" tt-sylk.val lf.
    end.
    /* BNF: "P;E" <xl-font> ";M200" [ ";S" ["B"] ["I"] ]
     * "P;E" <xl-font>    Excel font.  Count from zero.
     * ";M200"            ???
     * ";S" ["B"] ["I"]   Style [Bold] [Italic]
     */
    for each tt-sylk where tt-sylk.typ = {&sylk-fil-font}
                           no-lock
                        by tt-sylk.seq:
      put unformatted "P;" tt-sylk.val lf.
    end.
    /* BNF: "F;" <xl-def-fmt> ";D" <fmt> <dig> <ali> <wid> ";S" <xl-def-fnt> ";M240"
     * "P;" <xl-def-fmt>   Default format number.
     * ";D" <fmt> <dig> <ali> <wid>   Default formatting, digits, alignment, and width
     *                                "G0G8" = General Format, 0 digits after the decimal,
     *                                         General (textleft, numbersright) alignment,
     *                                         8 chars wide
     * ";S" <xl-def-fnt>              Default font to use by font number
     * ";M240"                        ???
     */
    put unformatted "F;" sylk-val({&sylk-fil-picture}, "") ";DG0G8;S" sylk-val({&sylk-fil-font}, {&sylk-xls-body}) ";M240" LF.
    /* BNF: "B" ";Y" <num-row> ";X" <num-col> ";D0 0 1 3"
     * "B"              Boundry conditions
     * ";Y" <num-row>   Number of rows? 1 seems to work OK
     * ";X" <num-col>   Number of columns
     * ";D0 0 1 3"      ???
     */
    put unformatted "B;Y1;X" fmtcol ";D0 0 1 3" lf.
    /* BNF: "O" ";L" ";D;V0;K47;G100 0.001"
     * "O"                      Options
     * ";L"                     Use A1 mode references (R1C1 always used in SYLK file expressions).
     * ";D;V0;K47;G100 0.001"   ???
     */
    put unformatted "O;L;D;V0;K47;G100 0.001" lf.
    /* BNF: "F;W" <begin> <end> <wid>
     * "F;W" <begin> <end> <wid>   Column width define columns <begin> to <end> as <wid> wide
     */
    for each tt-sylk where tt-sylk.typ = {&sylk-fil-c-wid}
                           no-lock
                        by tt-sylk.seq:
      put unformatted "F;" tt-sylk.val lf.
    end.
    /* BNF: "F;P" <xl-fmt> ";F" <fmt> <dig> <ali> ";C" <col-num>
     * "F;P" <xl-fmt>           Use format by number
     * ";F" <fmt> <dig> <ali>   formatting, digits, and alignment
     * ";C" <col-num>           Apply to column (A = 1)
     */
    for each tt-sylk where tt-sylk.typ = {&sylk-fil-c-fmt}
                           no-lock
                        by tt-sylk.seq:
      put unformatted "F;" tt-sylk.val lf.
    end.
    /* BNF: "F;F" <fmt> <dig> <ali> ";SDM" <xl-font> ";Y" <row-num> ";X" <col-num>
     *      "C;K" <q-value>
     * "F;F" <fmt> <dig> <ali>   formatting, digits, and alignment
     *                           alignment of ("L" | "R")=(Left | Right)
     * ";SDM" <xl-font>          Font to use
     * ";Y" <row-num>            Cell location
     * ";X" <col-num>            Cell location
     * "C;K" <q-value>           Cell value (quoted for text)
     */
    for each tt-sylk where tt-sylk.typ = {&sylk-fil-c-title}
                           no-lock
                        by tt-sylk.seq:
      put unformatted tt-sylk.val lf.
    end.

    create query hquery.
    hquery:set-buffers(btt:handle).
    hquery:query-prepare("FOR EACH " + htt:name).
    hquery:query-open.
    fmtrow = 1.
    repeat:
      hquery:get-next().
      if hquery:query-off-end then leave.
      fmtcol = 0.
      fmtrow = fmtrow + 1.
      do wf = 1 to btt:num-fields:
        hwf = btt:buffer-field(wf).
        nx = hwf:extent.
        fmttmp = substring(fmtkey, wf, 1).
        if nx = 0 then
        do:
          fmtcol = fmtcol + 1.
          /* BNF: "C;Y" <row-num> ";X" <col-num> ";K" <q-value>
           * "C;Y" <row-num>   Cell location
           * ";X" <col-num>    Cell location
           * ";K" <q-value>    Cell value (quoted for text)
           */
          put unformatted "C;Y" string(fmtrow) ";X" string(fmtcol) ";K".
          if fmttmp = {&sylk-fmt-string} or fmttmp = {&sylk-fmt-logical} or hwf:buffer-value = ? then put unformatted qt.
          case fmttmp:
            when {&sylk-fmt-date}    then
            do:
              if date(hwf:string-value) >= 1/1/1900 then
                put unformatted integer(date(hwf:string-value) - 1/1/1900) + 2.
              else
                put unformatted qt hwf:string-value qt.
            end.
            when {&sylk-fmt-time}    then put unformatted integer(hwf:buffer-value) / 24 / 60 / 60.
            when {&sylk-fmt-logical} then put unformatted right-trim(hwf:string-value).
            otherwise                     put unformatted right-trim(hwf:buffer-value).
          end case.
          if hwf:buffer-value = ? then put unformatted "?".
          if fmttmp = {&sylk-fmt-string} or fmttmp = {&sylk-fmt-logical} or hwf:buffer-value = ? then put unformatted qt.
          put unformatted lf.
        end.
        else
        do nx = 1 to hwf:extent:
          fmtcol = fmtcol + 1.
          put unformatted "C;Y" string(fmtrow) ";X" string(fmtcol) ";K".
          if fmttmp = {&sylk-fmt-string} or fmttmp = {&sylk-fmt-logical} or hwf:buffer-value(nx) = ? then put unformatted qt.
          case fmttmp:
            when {&sylk-fmt-date}    then
            do:
              if date(hwf:string-value(nx)) >= 1/1/1900 then
                put unformatted integer(date(hwf:string-value(nx)) - 1/1/1900) + 2.
              else
                put unformatted qt hwf:string-value qt.
            end.
            when {&sylk-fmt-time}    then put unformatted integer(hwf:buffer-value(nx)) / 24 / 60 / 60.
            when {&sylk-fmt-logical} then put unformatted right-trim(hwf:string-value(nx)).
            otherwise                     put unformatted right-trim(hwf:buffer-value(nx)).
          end case.
          if hwf:buffer-value(nx) = ? then put unformatted "?".
          if fmttmp = {&sylk-fmt-string} or fmttmp = {&sylk-fmt-logical} or hwf:buffer-value(nx) = ? then put unformatted qt.
          put unformatted lf.
        end.
      end.
    end.
    hquery:query-close().
    delete object hquery.
    /* BNF: "E"
     * "E"   End of file
     */
    put unformatted "E" lf.
    output close.
  end.
  otherwise           /*******************************************************/
  do:
    output to value(ofn).
    put unformatted "Call to tt-file.p with unknown file type [":U ft "]":U lf.
    output close.
  end.
end case.
