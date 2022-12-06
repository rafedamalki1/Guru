/*  
 * This sample demonstrates the use of Microsoft Office 95 Powerpoint as an 
 * ActiveX Automation Server.  It shows a couple of slides in edit mode and 
 * then runs a slide show.
 */

DEFINE VAR objApplication AS COM-HANDLE.
DEFINE VAR objPresentation AS COM-HANDLE.
DEFINE VAR filename AS CHARACTER.
DEFINE VAR filename1 AS CHARACTER.
DEFINE VAR curDir AS CHARACTER.
DEFINE VAR k AS INTEGER.
DEFINE VAR j AS INTEGER.
DEFINE VAR m AS INTEGER.
DEFINE VAR n AS INTEGER.

FILE-INFO:FILE-NAME = ".".
curDir = FILE-INFO:FULL-PATHNAME.
filename = curDir + "\test.ppt".
filename1 = curDir + "\joke.ppt".

/*
 * Create the Automation Object and open a file.
 */

create "PowerPoint.Application.7" objApplication.
objApplication:AppWindow:Visible = TRUE.
objApplication:AppWindow:WindowState = 2.
objPresentation = objApplication:Presentations:Open(filename).
filename = curDir + "\auto.bmp".

/*
 * Create new slides
 * Loop through the Slides collection and remove any existing slides
 * Insert new picture from auto.bmp
 */

j = objPresentation:Slides:Count.
DO k = 1 TO j:
 objPresentation:Slides:Item(k):Objects:Item(3):Cut().
 objPresentation:Slides:Item(k):Objects:AddPicture(filename,4800,4300,5000,5000).
 objPresentation:Slides:Item(k):SlideShowEffects:AdvanceEvent = 2.
 objPresentation:Slides:Item(k):SlideShowEffects:AdvanceTime = 6.
END.

/*
 * Present slide show from a file of saved slides.
 * Overwrites existing slides by using InsertFromFile()
 */

objPresentation:Slides:InsertFromFile(filename1,0).
objPresentation:SlideShow:StartingSlide = 1.
objPresentation:SlideShow:EndingSlide = 5.
objPresentation:SlideShow:AdvanceMode = 1.
objPresentation:SlideShow:RunContinuously = YES.
objPresentation:SlideShow:Run(1).
objApplication:AppWindow:WindowState = 1. /* Minimize window */

MESSAGE "Sit back and watch the show!" SKIP
    "Make sure the Powerpoint window has the focus." SKIP(1)
    "To end the show, press OK"
    VIEW-AS ALERT-BOX.

RELEASE OBJECT objPresentation.
objApplication:Quit().
RELEASE OBJECT objApplication. 
