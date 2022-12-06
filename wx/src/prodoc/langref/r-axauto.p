/* 
 * Demonstration of connecting to an Automation Object in Excel 
 * using the different connection options.
 */

DEF BUTTON bExit 
    LABEL "Exit" SIZE 16 BY 1.25 AUTO-GO.
DEF BUTTON bStart 
    LABEL "Option 1 - Start Excel" SIZE 32 BY 1.25 .
DEF BUTTON bConnect 
    LABEL "Option 2 - Connect to Active" SIZE 32 BY 1.25.
DEF BUTTON bConPerFile 
    LABEL "Option 3 - Connect per File" SIZE 32 BY 1.25.
DEF BUTTON bConnectMon 
    LABEL "Option 4 - Connect by Extension" SIZE 32 BY 1.25.
DEF VAR e AS CHAR VIEW-AS EDITOR SIZE 63 BY 1 LABEL "Result:" FONT 2.

DEFINE VARIABLE curDir AS CHARACTER.
FILE-INFO:FILE-NAME = ".".
curDir = FILE-INFO:FULL-PATHNAME.
DEFINE VAR wordAppl AS COM-HANDLE.


FORM e SKIP(0.5) bStart  SPACE bConnect  SPACE bConPerFile 
                         SPACE bConnectMon 
       SKIP(0.5) bExit WITH FRAME a VIEW-AS DIALOG-BOX THREE-D FONT 6.
FRAME a:TITLE = "Testing CREATE Automation Object Statement".

ENABLE ALL WITH FRAME a.
 
ON CHOOSE OF bStart IN FRAME a
DO:

/*
 * Option 1:
 * Connect using CREATE expression1 Com-Handle-Var.
 */

  DEFINE VARIABLE excelAppl AS COM-HANDLE.
  CREATE "Excel.Application" excelAppl. 
  excelAppl:Visible=true.
  excelAppl:Workbooks:Add.
  excelAppl:Range("A1"):Value = "testing CREATE".
  ASSIGN e:SCREEN-VALUE = String(excelAppl:Range("A1"):Value).
  release object excelAppl.
END.

ON CHOOSE OF bConnect IN FRAME a
DO:

/*
 * Option 2:
 * Connect using CREATE expression1 Com-Handle-Var CONNECT.
 */

  DEFINE VARIABLE excelAppl AS COM-HANDLE.
  CREATE "Excel.Application" excelAppl connect.
  excelAppl:Range("A2"):Value = "testing CONNECT".
  MESSAGE "Click me to continue!" VIEW-AS ALERT-BOX.
  ASSIGN e:SCREEN-VALUE = String(excelAppl:Range("A2"):Value).
  excelAppl:Workbooks:Item(1):SaveAs(curDir + "\zzz.xls").
  excelAppl:Quit().
  release object excelAppl.
END.

ON CHOOSE OF bConPerFile IN FRAME a
DO:

/*
 * Option 3:
 * Connect using CREATE expression1 Com-Handle-Var CONNECT TO expression2.
 */

  DEFINE VARIABLE excelSheet AS COM-HANDLE.
  DEFINE VARIABLE excelAppl AS COM-HANDLE.
  CREATE "Excel.Sheet" excelSheet connect to curDir + "\zzz.xls".
  excelAppl=excelSheet:Parent:Parent. 
  excelSheet:Visible=true.
  excelSheet:Range("A3"):Value = 
      "testing CONNECT TO by PersistFile Interface".
  ASSIGN e:SCREEN-VALUE = String(excelSheet:Range("A3"):Value).
  MESSAGE "Click me to continue!" VIEW-AS ALERT-BOX.
  excelAppl:Quit().
  release object excelAppl.
  release object excelSheet.
END.

ON CHOOSE OF bConnectMon IN FRAME a
DO:

/*
 * Option4:
 * Connect using CREATE "" Com-Handle-Var CONNECT TO expression2.
 */

  DEFINE VARIABLE excelAppl AS COM-HANDLE.
  CREATE "" excelAppl connect to curDir + "\zzz.xls". 
  excelAppl:Range("A4"):Value = "testing CONNECT TO by Extension".
  ASSIGN e:SCREEN-VALUE = String(excelAppl:Range("A4"):Value).
  MESSAGE "Click me to continue!" VIEW-AS ALERT-BOX.
  excelAppl:Parent:Parent:Quit().
  release object excelAppl.
END.

