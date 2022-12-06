


/* e-dllex3.p */

/* DLL routine to create an elliptic region */

PROCEDURE CreateEllipticRgnIndirect EXTERNAL "gdi32.dll":
	DEFINE RETURN PARAMETER RegionHandle AS LONG.
	DEFINE INPUT PARAMETER RegionSpec AS MEMPTR.
END PROCEDURE.

/* DLL routine to get drawing object */

PROCEDURE GetStockObject EXTERNAL "gdi32.dll":
	DEFINE RETURN PARAMETER ObjectHandle AS LONG.
	DEFINE INPUT PARAMETER ObjectType AS LONG.
END PROCEDURE.

/* DLL routine to select region into device context */

PROCEDURE SelectObject EXTERNAL "gdi32.dll":
	DEFINE INPUT PARAMETER DeviceHandle AS LONG.
	DEFINE INPUT PARAMETER ObjectHandle AS LONG.
END PROCEDURE.

/* DLL routine to display region */

PROCEDURE PaintRgn EXTERNAL "gdi32.dll":
	DEFINE INPUT PARAMETER DeviceHandle AS LONG.
	DEFINE INPUT PARAMETER RegionHandle AS LONG.
END PROCEDURE.

/* DLL routine to get handle of window device context */

PROCEDURE GetDC EXTERNAL "user.exe":
	DEFINE RETURN PARAMETER DeviceHandle AS LONG.
	DEFINE INPUT PARAMETER WindowHandle AS LONG.
END PROCEDURE.

/* DLL routine to release device context handle */

PROCEDURE ReleaseDC EXTERNAL "user.exe":
	DEFINE INPUT PARAMETER DeviceHandle AS LONG.
END PROCEDURE.

/* DLL routine to delete elliptic region */

PROCEDURE DeleteObject EXTERNAL "gdi32.dll":
	DEFINE INPUT PARAMETER RegionHandle AS LONG.
END PROCEDURE.
/* Variable Definitions */

DEFINE VARIABLE ElipRegion AS MEMPTR. /* Elliptic region structure */
DEFINE VARIABLE hDevice AS INTEGER.   /* Device context handle   */
DEFINE VARIABLE hObject AS INTEGER.   /* Drawing object handle   */
DEFINE VARIABLE hRegion AS INTEGER.   /* Elliptic region handle  */

DEFINE VARIABLE erLeft AS INTEGER INITIAL 1.    /* Elliptic    */
DEFINE VARIABLE erTop AS INTEGER INITIAL 3.     /* Coordinates */
DEFINE VARIABLE erRight AS INTEGER INITIAL 5.   /*             */
DEFINE VARIABLE erBottom AS INTEGER INITIAL 7.  /*             */

/* Allocate and build elliptic region structure */
/* specifying rectangular coordinates of region */

SET-SIZE(ElipRegion) = 4  /* int left   */
                     + 4  /* int top    */
                     + 4  /* int right  */
                     + 4  /* int bottom */
                       .

PUT-LONG(ElipRegion, erLeft) = 50.
PUT-LONG(ElipRegion, erTop) = 50.
PUT-LONG(ElipRegion, erRight) = 200.
PUT-LONG(ElipRegion, erBottom) = 100.

/* Initialize current window with PAUSE */
/* and display perfunctory message      */

DISPLAY "Preparing drawing..." .
PAUSE.

/* Get device context, region, and drawing object handles */

RUN GetDC (OUTPUT hDevice, INPUT CURRENT-WINDOW:HWND).
RUN CreateEllipticRgnIndirect(OUTPUT hRegion, INPUT ElipRegion).
RUN GetStockObject(OUTPUT hObject, INPUT 4).
/* Select drawing object and region for device context, */
/* and paint region                                     */

RUN SelectObject(INPUT hDevice, INPUT hObject).
RUN SelectObject(INPUT hDevice, INPUT hRegion).
RUN PaintRgn(INPUT hDevice, INPUT hRegion).

/* Free resources */

SET-SIZE(ElipRegion) = 0.         /* Free region structure  */
RUN ReleaseDC (INPUT CURRENT-WINDOW:HWND, INPUT hDevice).
                                 /* Release device context */
RUN DeleteObject(INPUT hRegion).  /* Delete elliptic region */




