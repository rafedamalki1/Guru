


/* e-dllex1.p */

DEFINE VARIABLE result AS INTEGER.
MESSAGE "   It's a whole new world!"
	VIEW-AS
		ALERT-BOX MESSAGE
		BUTTONS OK
		TITLE "Progress DLL Access".

RUN MessageBoxA (0, "   It's a whole new world!",
	"Progress DLL Access - from the DLL!", 0, OUTPUT result).

PROCEDURE MessageBoxA EXTERNAL "user32.dll":
    DEFINE INPUT PARAMETER hwnd AS LONG.
    DEFINE INPUT PARAMETER mbtext AS CHARACTER.
    DEFINE INPUT PARAMETER mbtitle AS CHARACTER.
    DEFINE INPUT PARAMETER style AS LONG.
    DEFINE RETURN PARAMETER result AS LONG.
END.




