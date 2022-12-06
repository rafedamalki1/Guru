/*
    create_tab.i
    
    created by Saulius Sakarauskas
    
    
*/
                                  
FUNCTION gf_recount_tab RETURNS INTEGER (
    wMain   AS HANDLE,
    vStartX AS INTEGER
    ):

    DEF VAR vWidth  AS INTEGER NO-UNDO.
    DEF VAR vCnt    AS INTEGER NO-UNDO.
    DEF VAR vLastX  AS INTEGER NO-UNDO.
    DEF VAR vFrmX   AS INTEGER NO-UNDO.
    DEF VAR wGrp    AS HANDLE NO-UNDO.
    DEF VAR wHnd    AS HANDLE NO-UNDO.

    vCnt = 1.
    wGrp = wMain:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(wGrp):
        wHnd = wGrp:FIRST-CHILD.
        REPEAT WHILE VALID-HANDLE(wHnd):

            IF wHnd:TYPE EQ "button" 
                THEN vCnt = vCnt + 1.

            wHnd = wHnd:NEXT-SIBLING.
        END.
        wGrp = wGrp:NEXT-SIBLING.
    END.
    vWidth  = (wMain:WIDTH-PIX - vStartX * 2) / vCnt.
    vLastX  = vStartX.
    vFrmX   = 0.
    wGrp    = wMain:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(wGrp):
        wHnd = wGrp:FIRST-CHILD.
        REPEAT WHILE VALID-HANDLE(wHnd):

            IF wHnd:TYPE EQ "button" THEN DO:
                wHnd:X          = vLastX.
                wHnd:WIDTH-PIX  = vWidth.
                vLastX          = vLastX + vWidth.
            END.
            IF wHnd:TYPE EQ "frame" THEN DO:
                RUN gp_recount_tab_rectangle(wHnd,"up.left",0,vFrmX).
                vFrmX = vFrmX + vWidth.
                RUN gp_recount_tab_rectangle(wHnd,"up.right",vFrmX,wHnd:X + wHnd:WIDTH-PIX).
            END.

            wHnd = wHnd:NEXT-SIBLING.
        END.
        wGrp = wGrp:NEXT-SIBLING.
    END.

    RETURN vWidth.

END FUNCTION. /* gf_recount_tab */




PROCEDURE gp_recount_tab_rectangle:
    DEF INPUT PARAM wTab    AS HANDLE.
    DEF INPUT PARAM vId     AS CHAR.
    DEF INPUT PARAM vLeft   AS INTEGER.
    DEF INPUT PARAM vRight  AS INTEGER.

    DEF VAR wHnd    AS HANDLE NO-UNDO.
    DEF VAR wGrp    AS HANDLE NO-UNDO.

    wGrp = wTab:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(wGrp):
        wHnd = wGrp:FIRST-CHILD.
        REPEAT WHILE VALID-HANDLE(wHnd):

            IF wHnd:TYPE EQ "rectangle" AND wHnd:PRIVATE-DATA EQ vId THEN DO:
                wHnd:X = vLeft.
                wHnd:WIDTH-PIX = vRight - vLeft.
            END.

            wHnd = wHnd:NEXT-SIBLING.
        END.
        wGrp = wGrp:NEXT-SIBLING.
    END.

END PROCEDURE.




FUNCTION gf_create_tab RETURNS HANDLE (
    vLabel  AS CHAR,
    wMain   AS HANDLE,
    wTab    AS HANDLE,
    vProcName   AS CHAR,
    vProcHnd    AS HANDLE
    ):

    DEF VAR vWidth  AS INTEGER NO-UNDO INIT 80.
    DEF VAR vHeight AS INTEGER NO-UNDO INIT 24.
    DEF VAR vStartX AS INTEGER NO-UNDO INIT 10.
    DEF VAR vStartY AS INTEGER NO-UNDO INIT 10.

    DEF VAR wBut    AS HANDLE NO-UNDO.
    DEF VAR wGrp    AS HANDLE NO-UNDO.
    DEF VAR wHnd    AS HANDLE NO-UNDO.

    DEF VAR vLastX  AS INTEGER NO-UNDO.
    
    ASSIGN
        vLastX  = vStartX
        wGrp    = wMain:FIRST-CHILD
        .
    REPEAT WHILE VALID-HANDLE(wGrp):

        wHnd = wGrp:FIRST-CHILD.
        REPEAT WHILE VALID-HANDLE(wHnd):
            IF wHnd:TYPE EQ "button" THEN DO:
                
                IF wHnd:X + wHnd:WIDTH-PIX > vLastX 
                    THEN vLastX = wHnd:X + wHnd:WIDTH-PIX.
                    
            END.

            wHnd = wHnd:NEXT-SIBLING.
        END.

        wGrp = wGrp:NEXT-SIBLING.
    END.
    
    IF vLastX + vWidth > wMain:WIDTH-PIX - vStartX * 2 THEN DO:
        vWidth = gf_recount_tab(wMain,vStartX).
        vLastX = wMain:WIDTH-PIX - vStartX - vWidth.
    END.

    CREATE BUTTON wBut
        ASSIGN
            LABEL       = vLabel
            NO-FOCUS    = TRUE
            WIDTH-PIX   = vWidth
            HEIGHT-PIX  = vHeight
            X           = vLastX
            Y           = vStartY
            FRAME       = wMain
        TRIGGERS:
            ON "CHOOSE" PERSISTENT RUN gp_choose_tab IN gMainProc 
                                        (wTab,vProcName,vProcHnd).
        END TRIGGERS
        .

    ASSIGN
        wTab:OVERLAY    = TRUE
        wTab:THREE-D    = TRUE
        wTab:BOX        = FALSE
        wTab:X          = vStartX
        wTab:Y          = vStartY + vHeight - 2
        wTab:WIDTH-PIX  = wMain:WIDTH-PIX - vStartX - vStartX
        wTab:HEIGHT-PIX = wMain:HEIGHT-PIX - vStartY - vStartY - vHeight
        wTab:VISIBLE    = FALSE
        wTab:SENSITIVE  = FALSE
        .

    CREATE RECTANGLE wHnd   /* left */
        ASSIGN
            FRAME       = wTab
            EDGE-PIX    = 0
            BGCOLOR     = 15
            X           = 0
            Y           = 0
            WIDTH-PIX   = 1
            HEIGHT-PIX  = wTab:HEIGHT-PIX
        .
    CREATE RECTANGLE wHnd   /* right */
        ASSIGN
            FRAME       = wTab
            EDGE-PIX    = 0
            BGCOLOR     = 0
            X           = wTab:WIDTH-PIX - 1
            Y           = 0
            WIDTH-PIX   = 1
            HEIGHT-PIX  = wTab:HEIGHT-PIX
        .
    CREATE RECTANGLE wHnd   /* bottom */
        ASSIGN
            FRAME       = wTab
            EDGE-PIX    = 0
            BGCOLOR     = 0
            X           = 0
            Y           = wTab:HEIGHT-PIX - 1
            WIDTH-PIX   = wTab:WIDTH-PIX
            HEIGHT-PIX  = 1
        .
    CREATE RECTANGLE wHnd   /* top-right */
        ASSIGN
            FRAME       = wTab
            PRIVATE-DATA = "up.right"
            EDGE-PIX    = 0
            BGCOLOR     = 15
            X           = wBut:X + wBut:WIDTH-PIX - wTab:X
            Y           = 0
            WIDTH-PIX   =  wTab:X + wTab:WIDTH-PIX - wBut:X - wBut:WIDTH-PIX
            HEIGHT-PIX  = 1
        .
    CREATE RECTANGLE wHnd   /* top-left */
        ASSIGN
            FRAME       = wTab
            PRIVATE-DATA = "up.left"
            EDGE-PIX    = 0
            BGCOLOR     = 15
            X           = 0
            Y           = 0
            WIDTH-PIX   = wBut:X - wTab:X + 1
            HEIGHT-PIX  = 1
        .
    
    RETURN wBut.

END FUNCTION. /* gf_create_tab */




PROCEDURE gp_choose_tab:
    DEF INPUT PARAM wTab        AS HANDLE               NO-UNDO.
    DEF INPUT PARAM vProcName   AS CHAR FORM "x(255)"   NO-UNDO.
    DEF INPUT PARAM wProcHnd    AS HANDLE               NO-UNDO.

    DEF VAR vWidth  AS INTEGER NO-UNDO INIT 80.
    DEF VAR vHeight AS INTEGER NO-UNDO INIT 24.
    DEF VAR vStartX AS INTEGER NO-UNDO INIT 10.
    DEF VAR vStartY AS INTEGER NO-UNDO INIT 10.

    DEF VAR wMain   AS HANDLE NO-UNDO.
    wMain = SELF:FRAME.
    DEF VAR wGrp    AS HANDLE NO-UNDO.
    DEF VAR wHnd    AS HANDLE NO-UNDO.

    DEF VAR vProcPre  AS CHAR FORM "x(255)" NO-UNDO.
    DEF VAR vProcPost AS CHAR FORM "x(255)" NO-UNDO.

    ASSIGN
	vProcPost = ENTRY(1,vProcName)
	NO-ERROR.
    IF ERROR-STATUS:ERROR
	THEN vProcPost = ?.
    ASSIGN
	vProcPre = ENTRY(2,vProcName)
	NO-ERROR.
    IF ERROR-STATUS:ERROR
	THEN vProcPre = ?.


    /* Call pre-processing procedure */
    IF chk_proc(vProcPre,wProcHnd)
	THEN RUN VALUE(vProcPre) IN wProcHnd (wTab).

    
    wGrp = wMain:FIRST-CHILD.
    REPEAT WHILE VALID-HANDLE(wGrp):

        wHnd = wGrp:FIRST-CHILD.
        REPEAT WHILE VALID-HANDLE(wHnd):
            IF wHnd:TYPE EQ "frame" THEN DO:
                wHnd:VISIBLE    = FALSE.
                wHnd:SENSITIVE  = FALSE.
            END.
            wHnd = wHnd:NEXT-SIBLING.
        END.

        wGrp = wGrp:NEXT-SIBLING.
    END.

    ASSIGN
        wTab:VISIBLE    = TRUE
        wTab:SENSITIVE  = TRUE
        .
    
    wTab:MOVE-TO-TOP().

    /* Optionally call the post-processing procedure */
    IF chk_proc(vProcPost,wProcHnd)
	THEN RUN VALUE(vProcPost) IN wProcHnd (wTab).

END PROCEDURE. /* gp_choose_tab */


