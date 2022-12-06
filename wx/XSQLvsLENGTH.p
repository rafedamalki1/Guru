
/*------------------------------------------------------------------------
    File        : XSQLvsLENGTH.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Mar 01 09:05:21 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE TEMP-TABLE ttSQLWidth NO-UNDO
    FIELD tableName AS CHARACTER
    FIELD tableNum AS INTEGER
    FIELD fieldName AS CHARACTER
    FIELD sqlWidth AS INTEGER
    FIELD actualWidth AS INTEGER
    FIELD requireFix AS LOGICAL INIT FALSE

   INDEX tableNum
    tableNum
   INDEX tableName
     tableName.

FOR EACH _File NO-LOCK WHERE _File._File-name BEGINS "ODBC":
    FOR EACH _Field OF _File WHERE _Field._Data-type = "character":
        CREATE ttSQLWidth.
        ASSIGN tableName = _File._File-name
            tableNum = _File._File-num
            fieldName = _Field._Field-name
            sqlWidth = _Field._Width.
        RELEASE ttSQLWidth.
    END. /* FOR EACH _Field */
END. /* FOR EACH _File */

DEFINE VARIABLE bTab AS HANDLE      NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE      NO-UNDO.
DEFINE VARIABLE queryString AS CHARACTER   NO-UNDO.

FOR EACH ttSQLWidth NO-LOCK:
    CREATE BUFFER bTab FOR TABLE tableName.
    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER(bTab).

    queryString = "FOR EACH " + tableName + " WHERE LENGTH(" + fieldName + ") >= " + STRING(sqlWidth) + " BY LENGTH(" + fieldName + ") DESC".
    hQuery:QUERY-PREPARE(queryString).
    hQuery:QUERY-OPEN().
    
    IF hQuery:GET-NEXT() THEN DO:
        ASSIGN actualWidth = LENGTH(bTab:BUFFER-FIELD(fieldName):BUFFER-VALUE)
               requireFix = TRUE.
    END. /* IF .. THEN DO */

    hQuery:QUERY-CLOSE.
    DELETE OBJECT hQuery.

    bTab:BUFFER-RELEASE().
    DELETE OBJECT bTab.

END. /* FOR EACH ttSQLWidth */

OUTPUT TO "D:\SQL2FIX.TXT".

PUT 
    "Table#"  AT 1
    "TableName" AT 8 
    "FieldName" AT 29
    "sqlWidth" AT 55
    "actualWidth" AT 65
    "FIX" AT 78
    SKIP.

FOR EACH ttSQLWidth WHERE requireFix = TRUE:
PUT UNFORM 
        tableNum AT 1
        tableName AT 8
        fieldName AT 29
        sqlWidth TO 62
        actualWidth TO 75
        requireFix TO 80
        SKIP.
END. /* FOR EACH ttSQLWidth */

OUTPUT CLOSE.

