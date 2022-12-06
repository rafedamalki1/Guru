/** SplashDemo.p
    Demonstration program for a splash window.
    
    Put all files in a directory that is in your Propath and 
    run this file from the Editor.
    
    The following code fragment runs the splash window
    for 5 seconds. If the user clicks on the bitmap
    image, the Window will close.
    
    You can run this code from the 'local-initialize'
    procedure of a SmartWindow, but be aware that 
    you will need to save the handle of the 
    current-window in that SmartWindow if you want to 
    refer to it while this splash window is running.
    
    Change the bitmap by pulling it up in the UIB and 
    choosing a new one.
**/

run wSplash.w PERSISTENT (INPUT "Välkommen till GURU",
                          INPUT "Av: Ander Olsson, Lena Olsson, Niklas Granholm,",
                          INPUT "Ove Wanhainen, Harriet Berggren,",
                          INPUT "Germund Gustavsson, Mikael Eriksson.",
                          INPUT "Copyright © Elpool i Umeå AB 1988 - 2006. Med ensamrätt.",
                          INPUT 5).
                         
