
/*------------------------------------------------------------------------
    File        : MINNESTEST.p
    Purpose     : 

    Syntax      : Disconnect procedure: för app11elpool

    Description : 

    Author(s)   : 
    Created     : Wed Mar 09 09:44:35 CET 2016
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE hTemp   AS HANDLE               NO-UNDO.
DEFINE VARIABLE hObject AS HANDLE               NO-UNDO.
DEFINE VARIABLE vTemp   AS CHARACTER            NO-UNDO.
DEFINE VARIABLE oObject AS Progress.Lang.Object NO-UNDO.
DEFINE VARIABLE oTemp   AS Progress.Lang.Object NO-UNDO.

ASSIGN hObject = SESSION:FIRST-DATASET.
DO WHILE hObject <> ?:
    ASSIGN hTemp   = hObject
           hObject = hObject:NEXT-SIBLING.
    MESSAGE 'ProDataSet, Handle=' hTemp
                        ', Name=' hTemp:NAME
                     ', Dynamic=' hTemp:DYNAMIC VIEW-AS ALERT-BOX.
END.

ASSIGN hObject = SESSION:FIRST-DATA-SOURCE.
DO WHILE hObject <> ?:
    ASSIGN hTemp   = hObject
           hObject = hObject:NEXT-SIBLING
           vTemp   = (IF hTemp:QUERY = ? THEN ? ELSE hTemp:QUERY:PREPARE-STRING).
    MESSAGE 'DataSource, Handle=' hTemp
                        ', Name=' hTemp:NAME
                       ', Query=' vTemp VIEW-AS ALERT-BOX.
END.

ASSIGN hObject = SESSION:FIRST-BUFFER.
DO WHILE hObject <> ?:
    ASSIGN hTemp   = hObject
           hObject = hObject:NEXT-SIBLING.
    MESSAGE 'Buffer, Handle=' hTemp
                    ', Name=' hTemp:NAME
                   ', Table=' hTemp:TABLE
                 ', Dynamic=' hTemp:DYNAMIC
                 ', DataSet=' hTemp:DATASET VIEW-AS ALERT-BOX.
END.

ASSIGN hObject = SESSION:FIRST-PROCEDURE.
DO WHILE hObject <> ?:
    ASSIGN hTemp   = hObject
           hObject = hObject:NEXT-SIBLING.
    MESSAGE 'Procedure, Handle='hTemp
                      ', Name=' hTemp:NAME VIEW-AS ALERT-BOX.
END.

ASSIGN hObject = SESSION:FIRST-QUERY.
DO WHILE hObject <> ?:
    ASSIGN hTemp   = hObject
           hObject = hObject:NEXT-SIBLING.
    MESSAGE 'Query, Handle=' hTemp
                   ', Name=' hTemp:NAME
                ', Dynamic=' hTemp:DYNAMIC
                  ', Query=' hTemp:PREPARE-STRING VIEW-AS ALERT-BOX.
END.

ASSIGN oObject = SESSION:FIRST-OBJECT.
DO WHILE oObject <> ?:
    ASSIGN oTemp   = oObject
           oObject = oObject:NEXT-SIBLING.
    MESSAGE 'Object, Name=' oTemp:ToString() VIEW-AS ALERT-BOX.
END.
