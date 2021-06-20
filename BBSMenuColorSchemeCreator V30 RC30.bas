Dim x As Integer ' x is used in my program as a variable for the background color
Dim y As Integer ' y is used as a variable for the foreground color
Dim i As Integer ' i is used as a variable for the length of the line of display
Dim b As String * 1
' above variables are used for the reading and writing of each character of info
' in each line of the ascii screens that are read into an array.

Dim lnumber As String ' used as a variable for the line number of the ascii screen
Dim file As String ' used as a variable for the filename entered in the program
Dim answer As String ' used as a variable for the answer to a question
Dim asciidisp As String ' ascii display variable used as the ascii display path
Dim ansidisp As String ' ansi display variable used as the ansi path and filename
Dim wc8disp As String 'Winserver 8 variable used as the wc8 BBS display path
Dim pcbdisp As String 'Pc Board display variable used as the pcb display path
Dim asciipromptmenu As String
Dim syndisp As String 'Syncronet display variabe used as the Syncronet display path
Dim asciimenu As String 'ascii menu variable used as the ascii menu path for input
Dim atasciimenu As String 'ATASCII menu character display
Dim atasciidisp As String 'ATASCII disp character display
Dim ansimenu As String 'ansi menu variable used as the ansi path for output
Dim wc8menu As String 'Winserver 8 variable used as the wc8 menu bbs path for output
Dim pcbmenu As String 'PC Board variable used as the PcB BBS menu display for output
Dim synmenu As String 'Syncronet variable used as the Syncronet Menu Display for output
Dim mysmenu As String 'Mystic variable
Dim mysdisp As String 'Mystic variable
Dim atascii_1 As String 'atascii variable
Dim atascii_2 As String
Dim filess As String
Dim fnames As String
'above variables are all string variables that are used for storing different information that in the
'Wildcat, Winserver, Ascii, and Ansi screens that are either read in , or written out to different filenames
'in my program. The *120, or *16 etc are just string lengths.

Dim file1 As String ' file1 is the ascii input file for the displays or menus that either other people or I create
Dim file2 As String ' file2 is the ansi file created by he program from the ascii file above
Dim file3 As String ' file3 is the Wildcator WInserver .BBS file that is created when the user selects wc8
Dim file4 As String ' file4 is the PCBoard .BBS file created when the user selects pcb as a display
Dim file5 As String ' file5 is reserved for Synchronet BBS when the user selects syncronet as a display
Dim file6 As String ' file6 is reserved for Mystic BBS in the user selects mystic as a display
Dim file7 As String ' file7 is reserved for the prompt files that follow any menu in any BBS
Dim file8 As String ' file8 is reserved for the ATASCII filenames
'ALL of the above are actual filenames without the drive letter and path for my program.


Dim horizontalines As String
Dim verticalines As String
Dim foregroundcoloransi As String
Dim backgroundcoloransi As String
Dim foregroundcolorbbs As String
Dim backgroundcolorbbs As String ' Background and foreground string settings
Dim foregroundcolorpcb As String
Dim backgroundcolorpcb As String
Dim foregroundcolorsynchro As String
Dim backgroundcolorsynchro As String
Dim foregroundcolormystic As String
Dim backgroundcolormystic As String
Dim foregroundcoloratascii As String
Dim backgroundcoloratascii As String
Dim code0b As String ' Code Setup for Black   Background
Dim code0f As String ' Code Setup for Black   Foreground
Dim code1b As String ' Code Setup for Blue    Background
Dim code1f As String ' Code Setup for Blue    Foreground
Dim code2b As String ' Code Setup for Green   Background
Dim code2f As String ' Code Setup for Green   Foreground
Dim code3b As String ' Code Setup for Cyan    Background
Dim code3f As String ' Code Setup for Cyan    Foreground
Dim code4b As String ' Code Setup for Red     Background
Dim code4f As String ' Code Setup for Red     Foreground
Dim code5b As String ' Code Setup for Magenta Background
Dim code5f As String ' Code Setup for Magenta Foreground
Dim code6b As String ' Code Setup for Brown   Background
Dim code6f As String ' Code Setup for Yellow  Foreground
Dim code7b As String ' Code Setup for White   Background
Dim code7f As String ' Code Setup for White   Foreground
Dim corner1 As String
Dim corner2 As String
Dim corner3 As String
Dim corner4 As String
Dim connector1 As String
Dim connector2 As String
Dim connector3 As String
Dim connector4 As String
Dim toplineconnector As String
Dim middlelineconnector As String
Dim bottomlineconnector As String

Dim corner1a As String
Dim corner2a As String
Dim corner3a As String
Dim corner4a As String
Dim connector1a As String
Dim connector2a As String
Dim connector3a As String
Dim connector4a As String
Dim toplineconnectora As String
Dim middlelineconnectora As String
Dim bottomlineconnectora As String

Dim menuchoice As String
Dim flag As String
Dim leftverticalline As String
Dim rightverticalline As String
Dim leftverticallinea As String
Dim rightverticallinea As String

Dim display As String
Dim ran As String
Dim randome As String
_FullScreen
Cls
Screen 12
Color 12, 1

Locate 10, 24
Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
Locate 11, 24
Print "%                 BBS Screen Creator Written By Russ Campbell                  %"
Locate 12, 24
Print "%                                                                              %"
Locate 13, 24
Print "%           ***       Alpha Version 30 Release Candidate RC30     ***          %"
Locate 14, 24
Print "%                                                                              %"
Locate 15, 24
Print "%                All parts of the program are: fully functional                %"
Locate 16, 24
Print "%                                                                              %"
Locate 17, 24
Print "%             Now with ATASCII charcter sets added (Fully Functional)          %"
Locate 18, 24
Print "%                                                                              %"
Locate 19, 24
Print "%                    Press any key to continue.............                    %"
Locate 20, 24
Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
Do While InKey$ = ""
Loop


Color 7, 0
setup:
Cls
Print
Print "BBS Menu Color Scheme Creator Version 30 Alpha Version RC30"
Print "Written by Russ Campbell Updated May 18 2021 (c) 2020-2021"
Print
Print "First of all design your menus in straight text no colors, etc"
Print "Do not put any background colors in either as the program will"
Print "Do that for you automatically. Use the following convention"
Print "that is laid out in the next display."
Print
Print "This program will then take your plain text menu and turn it into"
Print "a random colored display, methods will be taken to ensure that"
Print "the backgound and text colors do not print the same colors"
Print
Print "Take a look and see what you think, it will of course write"
Print "out your menus in .BBS and .ANS formats, kind of a neat feature."
Print
Print "Don't like what you get? Then run the program again and again"
Print "until you get what you like as a menu screen."
Print
Print "This is a complicated program, you don't have to use boxes"
Print "but for now that is the only shape that my program will"
Print "accept. I will print an example of a menu on the screen so"
Print "that you know exactly how to use my program with a text drawing."
Print ""
Print "Press any key to continue..."
Do While InKey$ = ""
Loop
Cls
Print
Print "The menus have changed , see below for instuctions"
Print "Especiallly the symbols for Top, Middle and Bottom"
Print "Lines as well as Vertical left and right lines    "
Print "This gives me more control over screen making.    "
Print
Print " ^{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{$"
Print " !               Main Message Menu                 <"
Print " *}}}}}}}}}}}}}}}}}}}<}}}}}}}}}}}}}}}}}}}}}}}}}}}}}="
Print " !                   <                             <"
Print " *}}}}}}}}}}}}}}}}}}}#}}}}}}}}}}}}}}}}}}}}}}}}}}}}}="
Print " ! [C] Check Your Mail   [S] Scan Messages         <"
Print " ! [E] Enter New Message [J] Join Conference       <"
Print " ! [F] FILE MENU         [U] Update Settings       <"
Print " ! [G] Goodbye           [H] Help Level            <"
Print " ! [Q] Quit to Main Menu [?] Command Help          <"
Print " ! [R] Read Messages                               <"
Print " ~>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>%"
Print
Print "Pay special attention to the symbols on each corner"
Print "They are all different, one for each corner, to work"
Print "properly with my program , they must be right. The "
Print "rest of the characters also must be right except for"
Print "whats in the menu. You can see the other characters"
Print "that I use lines and connectors, the program turns"
Print "into nice corners and borders, for double corners "
Print "and borders, well , you can figure that one out."
Print
Print "Press any key to continue..."
Do While InKey$ = ""
Loop


Cls
Print
rand:
Cls
Print
Print "Do you wish to have multi colors randomly chosen for everything"
Print "or user set colors for the entire area that makes up all of the "
Print "the menus. This can all be used in either displays or menus"
Print
Print "Choose [R]andom or [U]ser Set Patterns or"
Print "[F]ixed Display Screens [L]oad Data File or E[X]it Program : "
answer = UCase$(Input$(1))

ran = answer
If ran = "R" Then GoSub randomchoices: GoTo inputscreen
If ran = "F" Then GoSub fixedcolors: GoTo inputscreen
If ran = "L" Then GoSub load: GoTo inputscreen
If ran = "U" Then GoTo inputscreen1
If ran = "X" Then GoTo finish
GoTo rand
inputscreen1:
Cls
Print "Do you wish to use pre-selected colors or use your own sets "
Print "Of color selections: color schemes can be customised."

Print "This section is for entering your own color schemes , it is  "
Print "for selecting either Preselected or your own colors."
Print
Print "If you want random colors, choose [Y]es as your answer."
Print
Print "However if you want to enter your own colors , then enter"
Print " N as your choice of color selection"
Print
Print "Do you wish to use random colors? [Y]es  or [N]o  [Defaults to [N]o   "
answer = UCase$(Input$(1))

If answer = "N" Then randome = "N"
If answer = "" Then randome = "N": GoTo prompts
If answer = "N" Then GoTo prompts
If answer = "Y" Then GoSub randomchoices: GoTo inputscreen
GoTo inputscreen1
prompts:
Cls
Print
Print "This section will either load or create a new user data file with all used"
Print "variables saved in a filename that you entered. It will give you the option"
Print "of loading a saved file or creating a totally new filename. You do not have"
Print "to write over any existing files, and the program will be fool proof."
Print
Print "[L]oad or [C]reate User Data file or [X]xit Program"
answer = UCase$(Input$(1))

If answer = "L" Then GoTo filen
If answer = "C" Then GoTo combos
If answer = "X" Then GoTo finish
GoTo prompts
filen:
Print
If answer = "L" Then
    GoSub load
    GoTo inputscreen
End If
combos:
Cls
Print

Print "Please enter 6 different color combinations for the display , be creative : "
Print "the program will use the colors that you have selected as output for your,"
Print "display based on what you choose, you will be given the option to load "
Print "and save yout colors every time you run the program."
Print
Print "Press any key...."
Do While InKey$ = ""
Loop
GoSub back
GoSub create
GoTo inputscreen
back:
back1a:
GoSub background
Input "Outside Lines Background 1a [From 0 to  7] : ", g1a
Input "Outside Lines Foreground 1a [From 0 to 15] : ", t1a
Print
Color t1a, g1a

Print "[------------------]";: Color 7, 0: Color 7, 0
Print
Print "Is This readable [Y]es or [N]o : "
answer = Input$(1)
answer = UCase$(answer)
If answer = "N" Then GoTo back1a
If answer = "Y" Then GoTo back1b
GoTo back1a
back1b:
GoSub background
Input "Outside Lines Background 1b [From 0 to  7] : ", g1b
Input "Outside Lines Foreground 1b [From 0 to 15] : ", t1b
Print
Color t1b, g1b

Print "[------------------]";: Color 7, 0: Color 7, 0
Print
Print "Is This readable [Y]es or [N]o : "
answer = Input$(1)
answer = UCase$(answer)
If answer = "N" Then GoTo back1b
If answer = "Y" Then GoTo back1c
GoTo back1b

back1c:
GoSub background
Input "Outside Lines Background 1c [From 0 to  7] : ", g1c
Input "Outside Lines Foreground 1c [From 0 to 15] : ", t1c
Print
Color t1c, g1c

Print "[------------------]";: Color 7, 0: Color 7, 0
Print
Print "Is This readable [Y]es or [N]o : "
answer = Input$(1)
answer = UCase$(answer)
If answer = "N" Then GoTo back1c
If answer = "Y" Then GoTo back1d
GoTo back1c

back1d:
GoSub background
Input "Outside Lines Background 1d [From 0 to  7] : ", g1d
Input "Outside Lines Foreground 1d [From 0 to 15] : ", t1d
Print
Color t1d, g1d

Print "[------------------]";: Color 7, 0: Color 7, 0
Print
Print "Is This readable [Y]es or [N]o : "
answer = Input$(1)
answer = UCase$(answer)
If answer = "N" Then GoTo back1d
If answer = "Y" Then GoTo back2
GoTo back1d

back2:
GoSub background
Input "Brackets  Background [From 0 to  7] : ", g2
Input "Brackets  Foreground [From 0 to 15] : ", t2
Print
Print
Color t2, g2
Print "[";: Color 7, 0: Color 7, 0
Print "A";
Color t2, g2
Print "]";: Color 7, 0: Color 7, 0
Print
Print "Is This readable [Y]es or [N]o : "
answer = UCase$(Input$(1))
If answer = "N" Then GoTo back2
If answer = "Y" Then GoTo back3a
GoTo back2

back3a:
GoSub background
Input "Inside Lines Background  [From 0 to  7] : ", g
Input "Inside Lines Foreground  [From 0 to 15] : ", t
Print
Color t, g

Print "[------------------]";: Color 7, 0: Color 7, 0
Print
Print "Is This readable [Y]es or [N]o : "
answer = UCase$(Input$(1))
If answer = "N" Then GoTo back3a
If answer = "Y" Then GoTo fnd

GoTo back3a
fnd:
Return

inputscreen:


Cls
Print "Demo files that are available are: "
Print
Print "In the Menu selector"
Print
Print "    inet5     inet6     inet9"
Print "    msg1      msg5      msg6     msg9"
Print "    main1     main5     main6    main9"
Print "    file1     file5     file6    file9"
Print "    sysop8    sysop9" '                  display demo files that are available
Print
Print "In the Disp selector"
Print
Print "    prelog goodbye hello1 title"
Print
Print "More are coming soon. Custom files are up to you to upload and then use"
Print
Print "Filename : [Do not put in the extender ]"
Input "           [Enter defaults to main5    ] : ", file ' ask user for filename
If file = "" Then file = "main5"
If file = "a" Or file = "A" Then GoTo inputscreen
If file = "b" Or file = "B" Then GoTo inputscreen
If file = "c" Or file = "C" Then GoTo inputscreen
If file = "d" Or file = "D" Then GoTo inputscreen
If file = "e" Or file = "E" Then GoTo inputscreen
If file = "f" Or file = "F" Then GoTo inputscreen
If file = "inet5" Then menuchoice = "M"
If file = "inet6" Then menuchoice = "M"
If file = "inet9" Then menuchoice = "M"
If file = "msg1" Then menuchoice = "M"
If file = "msg5" Then menuchoice = "M"
If file = "msg6" Then menuchoice = "M"
If file = "msg9" Then menuchoice = "M"
If file = "main1" Then menuchoice = "M"
If file = "main5" Then menuchoice = "M"
If file = "main6" Then menuchoice = "M"
If file = "main9" Then menuchoice = "M"
If file = "file1" Then menuchoice = "M"
If file = "file5" Then menuchoice = "M"
If file = "file6" Then menuchoice = "M"
If file = "file9" Then menuchoice = "M"
If file = "sysop8" Then menuchoice = "M"
If file = "sysop9" Then menuchoice = "M"
If file = "prelog" Then menuchoice = "D"
If file = "title" Then menuchoice = "D"
If file = "goodbye" Then menuchoice = "D"
If file = "hello1" Then menuchoice = "D"
displays:

Cls
Print "Output display [A] Any Ansi BBS    [B] Wildcat 5-8 BBS  [C] PCboard BBS   "
Print "Output Display [D] Synchronet BBS  [E] Mystic BBS  [F] ATASCII BBS Files  " 'which BBS display the user wants
answer = Input$(1)
answer = UCase$(answer)
If answer = "A" Then display = "ansi": GoTo menu
If answer = "B" Then display = "wc8": GoTo menu ' select display coding by what the user types in
If answer = "C" Then display = "pcb": GoTo menu
If answer = "D" Then display = "synchro": GoTo menu
If answer = "E" Then display = "mystic": GoTo menu
If answer = "F" Then display = "atascii": GoTo menu
GoTo displays


startit:
Print display
selector:
If pick = 1 Then
    horizontalines = "double": verticalines = "double"
    corner1 = Chr$(201)
    corner2 = Chr$(187) ' corner character for new corners of new box output
    corner3 = Chr$(188)
    corner4 = Chr$(200)
    connector1 = Chr$(203)
    connector2 = Chr$(185) 'various connectors for lines in boxes
    connector3 = Chr$(202)

    connector4 = Chr$(204)
    toplineconnector = Chr$(205) ' Horizontal line character for new box output
    middlelineconnector = Chr$(205)
    bottomlineconnector = Chr$(205)
    rightverticalline = Chr$(186)
    leftverticalline = Chr$(186)
End If
If pick = 2 Then
    horizontalines = "double": verticalines = "single"
    corner1 = Chr$(213)
    corner2 = Chr$(184) ' corner character for new corners of new box output
    corner3 = Chr$(190)
    corner4 = Chr$(212)
    connector1 = Chr$(209)
    connector2 = Chr$(181) 'various connectors for lines in boxes
    connector3 = Chr$(207)
    connector4 = Chr$(198)
    toplineconnector = Chr$(205) ' Horizontal line character for new box output
    middlelineconnector = Chr$(205)
    bottomlineconnector = Chr$(205)
    rightverticalline = Chr$(179)
    leftverticalline = Chr$(179)
End If
If pick = 3 Then
    horizontalines = "single": verticalines = "double"
    corner1 = Chr$(214)
    corner2 = Chr$(183) ' corner character for new corners of new box output
    corner3 = Chr$(189)
    corner4 = Chr$(211)
    connector1 = Chr$(210)
    connector2 = Chr$(182) 'various connectors for lines in boxes
    connector3 = Chr$(208)
    connector4 = Chr$(199)
    toplineconnector = Chr$(196) ' Horizontal line character for new box output
    middlelineconnector = Chr$(196) ' Horizontal line character for new box output
    bottomlineconnector = Chr$(196) ' Horizontal line character for new box output
    rightverticalline = Chr$(186)
    leftverticalline = Chr$(186)

End If
If pick = 4 Then
    horizontalines = "single": verticalines = "single"
    corner1 = Chr$(218)
    corner2 = Chr$(191) ' corner character for new corners of new box output
    corner3 = Chr$(217)
    corner4 = Chr$(192)
    connector1 = Chr$(194)
    connector2 = Chr$(180) 'various connectors for lines in boxes
    connector3 = Chr$(193)
    connector4 = Chr$(195)
    toplineconnector = Chr$(196) ' Horizontal line character for new box output
    middlelineconnector = Chr$(196) ' Horizontal line character for new box output
    bottomlineconnector = Chr$(196) ' Horizontal line character for new box output
    rightverticalline = Chr$(179)
    leftverticalline = Chr$(179)
End If
Return
atasciiselector:
If pick = 1 Then
    horizontalines = "double": verticalines = "double"
    corner1a = Chr$(17)
    corner2a = Chr$(5) ' corner character for new corners of new box output
    corner3a = Chr$(3)
    corner4a = Chr$(26)
    connector1a = Chr$(24)
    connector2a = Chr$(4) 'various connectors for lines in boxes
    connector3a = Chr$(23)
    connector4a = Chr$(1)
    toplineconnectora = Chr$(18) ' Horizontal line character for new box output
    middlelineconnectora = Chr$(18)
    bottomlineconnectora = Chr$(18)
    rightverticallinea = Chr$(124)
    leftverticallinea = Chr$(124)
End If
If pick = 2 Then
    horizontalines = "double": verticalines = "single"
    corner1a = Chr$(17)
    corner2a = Chr$(5) ' corner character for new corners of new box output
    corner3a = Chr$(3)
    corner4a = Chr$(26)
    connector1a = Chr$(24)
    connector2a = Chr$(4) 'various connectors for lines in boxes
    connector3a = Chr$(23)
    connector4a = Chr$(1)
    toplineconnectora = Chr$(18) ' Horizontal line character for new box output
    middlelineconnectora = Chr$(18)
    bottomlineconnectora = Chr$(18)
    rightverticallinea = Chr$(124)
    leftverticallinea = Chr$(124)
End If
If pick = 3 Then
    horizontalines = "single": verticalines = "double"
    corner1a = Chr$(17)
    corner2a = Chr$(5) ' corner character for new corners of new box output
    corner3a = Chr$(3)
    corner4a = Chr$(26)
    connector1a = Chr$(24)
    connector2a = Chr$(4) 'various connectors for lines in boxes
    connector3a = Chr$(23)
    connector4a = Chr$(1)
    toplineconnectora = Chr$(18) ' Horizontal line character for new box output
    middlelineconnectora = Chr$(18)
    bottomlineconnectora = Chr$(18)
    rightverticallinea = Chr$(124)
    leftverticallinea = Chr$(124)

End If
If pick = 4 Then
    corner1a = Chr$(17)
    corner2a = Chr$(5) ' corner character for new corners of new box output
    corner3a = Chr$(3)
    corner4a = Chr$(26)
    connector1a = Chr$(24)
    connector2a = Chr$(4) 'various connectors for lines in boxes
    connector3a = Chr$(23)
    connector4a = Chr$(1)
    toplineconnectora = Chr$(18) ' Horizontal line character for new box output
    middlelineconnectora = Chr$(18)
    bottomlineconnectora = Chr$(18)
    rightverticallinea = Chr$(124)
    leftverticallinea = Chr$(124)
End If
Return

menu:


'----------------------------------------------------------------------------------------------------------------------------



fileit:
If file = "" And menuchoice = "D" Then ' default file for display files
    file = "goodbye"
End If
If file = "" And menuchoice = "M" Then ' default file for menu files
    file = "main5"
End If




'-------------------------------------------------------------------------------------------------------------
' Start of actual program , continues until ended
'Start of menu selection decisions by the program

file1 = file + ".txt"
file2 = file + ".ans" ' adds file extension to each type of file
file3 = file + ".bbs"
file4 = file + ".bbs"
file5 = file + ".txt"
file6 = file + ".txt"
file7 = file + "a.txt"
file8 = file + ".txt"
If display = "atascii" Then
    asciidisp = "c:\display\bbsdispascii40\" + file1
    type1$ = asciidisp
Else
    asciidisp = "c:\display\bbsdispascii\" + file1
    type1$ = asciidisp
End If
ansidisp = "c:\display\bbsdispansi\" + file2
type2$ = ansidisp
wc8disp = "c:\display\bbsdispwildcat\" + file3
type2$ = wc8disp
pcbdisp = "c:\display\bbsdisppcb\" + file4 ' all path, drive and filename variables
type2$ = pcbdisp
syndisp = "c:\display\bbsdispsyncro\" + file5
type2$ = syndisp
mysdisp = "c:\display\bbsdispmystic\" + file6
type2$ = mysdisp
If display = "atascii" Then
    asciimenu = "c:\display\bbsmenuascii40\" + file1
Else
    asciimenu = "c:\display\bbsmenuascii\" + file1
    type1$ = asciimenu
End If
ansimenu = "c:\display\bbsmenuansi\" + file2 ' for various screen ouputs
type3$ = ansimenu
wc8menu = "c:\display\bbsmenuwildcat\" + file3
type3$ = wc8menu
pcbmenu = "c:\display\bbsmenupcb\" + file4
type3$ = pcbmenu
synmenu = "c:\display\bbsmenusyncro\" + file5
type3$ = synmenu
mysmenu = "C:\display\bbsmenumystic\" + file6
type3$ = mysmenu
asciipromptmenu = "c:\display\bbsmenuascii\" + file7
type3$ = atascii_2
atasciimenu = "c:\display\bbsmenuatascii\" + file8
type3$ = atascii_1
atasciidisp = "c:\display\bbsdispatascii\" + file8
If menuchoice = "D" Then
    contlne = 0
    GoSub pick0
    If display = "atascii" Then
        GoSub atasciiselector
    Else
        GoSub selector
    End If
    On Error GoTo errorhandle
    Open asciidisp For Input As #1

    If display = "ansi" Then
        Open ansidisp For Output As #2
    End If

    If display = "wc8" Then
        Open wc8disp For Output As #2
    End If

    If display = "pcb" Then
        Open pcbdisp For Output As #2
    End If
    If display = "synchro" Then
        Open syndisp For Output As #2
    End If
    If display = "mystic" Then
        Open mysdisp For Output As #2
    End If
    If display = "atascii" Then
        Open atasciidisp For Output As #2
    End If

End If

If menuchoice = "M" Then

    contlne = 0
    GoSub pick0
    If display = "atascii" Then
        GoSub atasciiselector
    Else
        GoSub selector
    End If
    'On Error GoTo errorhandle
    Open asciimenu For Input As #1
    'On Error GoTo errorhandle
    Open asciipromptmenu For Input As #3

    If display = "ansi" Then
        Open ansimenu For Output As #2
    End If

    If display = "wc8" Then
        Open wc8menu For Output As #2
    End If

    If display = "pcb" Then
        Open pcbmenu For Output As #2
    End If
    If display = "synchro" Then
        Open synmenu For Output As #2
    End If
    If display = "mystic" Then
        Open mysmenu For Output As #2
    End If
    If display = "atascii" Then
        Open atasciimenu For Output As #2
    End If
End If ' end of menu selection decisions by the program.
If display = "ansi" Then
    ' Clear screen
    Cls
    ' Set background to black
    Color 0, 0
End If
If display = "wc8" Then
    ' clear screen
    Cls
    Print #2, "@00@" + "@CLS@"
    Print #2, ""
    ' set background to black
    Color 7, 0
End If
If display = "pcb" Then
    ' clear screen
    Cls
    ' set background to black
    Color 0, 0
End If
If display = "synchro" Then
    ' clear screen
    Cls
    ' set background to black
    Color 7, 0
End If
If display = "mystic" Then
    ' clear screen
    Cls
    ' set background to black
    Color 7, 0
End If
If display = "atascii" Then
    Cls
End If
Print "The Display that is Being Output to Disk is : "; display
GoSub checkit
Do Until EOF(1)
    Line Input #1, lnumber

    For i = 1 To Len(lnumber)

        b = Mid$(lnumber, i, 1) ' Calculates the ascii value of every character in the line
        If display = "ansi" Then
            GoSub specialcharacters3
            GoSub specialcharacters2
            If flag = "Y1" Then
                If b1 = 1 Then
                    GoSub colorchange
                    Color t1a, g1a
                    x = g1a
                    y = t1a
                    GoSub displ
                    If colorchanges = 1 Then
                        Print #2, backgroundcoloransi + foregroundcoloransi + b;
                        Print b;
                    ElseIf colorchanges = 0 Then
                        Print #2, b;
                        Print b;
                    End If

                Else If b1 = 2 Then
                        GoSub colorchange
                        Color t1b, g1b
                        x = t1b
                        y = g1b
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcoloransi + foregroundcoloransi + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 3 Then
                        GoSub colorchange
                        Color t1c, g1c
                        x = g1c
                        y = t1c
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcoloransi + foregroundcoloransi + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 4 Then
                        GoSub colorchange
                        Color t1d, g1d
                        x = g1d
                        y = t1d
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcoloransi + foregroundcoloransi + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If
                    End If
                End If
            End If

            If flag$ = "Y2" Then
                GoSub colorchange
                Color t2, g2
                x = g2
                y = t2
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcoloransi + foregroundcoloransi + b;
                    Print b;
                ElseIf colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
            If flag = "Y3" Then
                GoSub colorchange
                Color t, g
                x = g
                y = t
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcoloransi + foregroundcoloransi + b;
                    Print b;
                End If
            Else
                If colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
        End If

        If display = "wc8" Then
            GoSub specialcharacters3
            GoSub specialcharacters2
            If flag = "Y1" Then
                If b1 = 1 Then
                    GoSub colorchange
                    Color t1a, g1a
                    x = g1a
                    y = t1a
                    GoSub displ
                    If colorchanges = 1 Then
                        Print #2, backgroundcolorbbs + foregroundcolorbbs + b;
                        Print b;
                    ElseIf colorchanges = 0 Then
                        Print #2, b;
                        Print b;
                    End If

                Else If b1 = 2 Then
                        GoSub colorchange
                        Color t1b, g1b
                        x = t1b
                        y = g1b
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorbbs + foregroundcolorbbs + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 3 Then
                        GoSub colorchange
                        Color t1c, g1c
                        x = g1c
                        y = t1c
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorbbs + foregroundcolorbbs + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 4 Then
                        GoSub colorchange
                        Color t1d, g1d
                        x = g1d
                        y = t1d
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorbbs + foregroundcolorbbs + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If
                    End If
                End If
            End If

            If flag = "Y2" Then
                GoSub colorchange
                x = g2
                y = t2
                Color t2, g2
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcolorbbs + foregroundcolorbbs + b;
                    Print b;
                ElseIf colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
            If flag = "Y3" Then
                GoSub colorchange
                x = g
                y = t
                Color t, g
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcolorbbs + foregroundcolorbbs + b;
                    Print b;
                End If
            Else
                If colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
        End If
        If display = "pcb" Then
            GoSub specialcharacters3
            GoSub specialcharacters2
            If flag = "Y1" Then
                If b1 = 1 Then
                    GoSub colorchange
                    Color t1a, g1a
                    x = g1a
                    y = t1a
                    GoSub displ
                    If colorchanges = 1 Then
                        Print #2, backgroundcolorpcb + foregroundcolorpcb + b;
                        Print b;
                    ElseIf colorchanges = 0 Then
                        Print #2, b;
                        Print b;
                    End If

                Else If b1 = 2 Then
                        GoSub colorchange
                        Color t1b, g1b
                        x = t1b
                        y = g1b
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorpcb + foregroundcolorpcb + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 3 Then
                        GoSub colorchange
                        Color t1c, g1c
                        x = g1c
                        y = t1c
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorpcb + foregroundcolorpcb + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 4 Then
                        GoSub colorchange
                        Color t1d, g1d
                        x = g1d
                        y = t1d
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorpcb + foregroundcolorpcb + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If
                    End If
                End If
            End If

            If flag = "Y2" Then
                GoSub colorchange
                Color t2, g2
                x = g2
                y = t2
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcolorpcb + foregroundcolorpcb + b;
                    Print b;
                ElseIf colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
            If flag = "Y3" Then
                GoSub colorchange
                Color t, g
                x = g
                y = t
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcolorpcb + foregroundcolorpcb + b;
                    Print b;
                End If
            Else
                If colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
        End If
        If display = "synchro" Then
            GoSub specialcharacters3
            GoSub specialcharacters2
            If flag = "Y1" Then
                If b1 = 1 Then
                    GoSub colorchange
                    Color t1a, g1a
                    x = g1a
                    y = t1a
                    GoSub displ
                    If colorchanges = 1 Then
                        Print #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                        Print b;
                    ElseIf colorchanges = 0 Then
                        Print #2, b;
                        Print b;
                    End If

                Else If b1 = 2 Then
                        GoSub colorchange
                        Color t1b, g1b
                        x = t1b
                        y = g1b
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 3 Then
                        GoSub colorchange
                        Color t1c, g1c
                        x = g1c
                        y = t1c
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 4 Then
                        GoSub colorchange
                        Color t1d, g1d
                        x = g1d
                        y = t1d
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If
                    End If
                End If
            End If

            If flag = "Y2" Then
                GoSub colorchange
                Color t2, g2
                x = g2
                y = t2
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                    Print b;
                ElseIf colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
            If flag = "Y3" Then
                GoSub colorchange
                Color t, g
                x = g
                y = t
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcolorsynchro + foregroundcolorsynchro + b;
                    Print b;
                End If
            Else
                If colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
        End If
        If display = "mystic" Then
            GoSub specialcharacters3
            GoSub specialcharacters2
            If flag = "Y1" Then
                If b1 = 1 Then
                    GoSub colorchange
                    Color t1a, g1a
                    x = g1a
                    y = t1a
                    GoSub displ
                    If colorchanges = 1 Then
                        Print #2, backgroundcolormystic + foregroundcolormystic + b;
                        Print b;
                    ElseIf colorchanges = 0 Then
                        Print #2, b;
                        Print b;
                    End If

                Else If b1 = 2 Then
                        GoSub colorchange
                        Color t1b, g1b
                        x = t1b
                        y = g1b
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolormystic + foregroundcolormystic + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 3 Then
                        GoSub colorchange
                        Color t1c, g1c
                        x = g1c
                        y = t1c
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolormystic + foregroundcolormystic + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If

                    ElseIf b1 = 4 Then
                        GoSub colorchange
                        Color t1d, g1d
                        x = g1d
                        y = t1d
                        GoSub displ
                        If colorchanges = 1 Then
                            Print #2, backgroundcolormystic + foregroundcolormystic + b;
                            Print b;
                        ElseIf colorchanges = 0 Then
                            Print #2, b;
                            Print b;
                        End If
                    End If
                End If
            End If

            If flag = "Y2" Then
                GoSub colorchange
                Color t2, g2
                x = g2
                y = t2
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcolormystic + foregroundcolormystic + b;
                    Print b;
                ElseIf colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
            If flag = "Y3" Then
                GoSub colorchange
                Color t, g
                x = g
                y = t
                GoSub displ
                If colorchanges = 1 Then
                    Print #2, backgroundcolormystic + foregroundcolormystic + b;
                    Print b;
                End If
            Else
                If colorchanges = 0 Then
                    Print #2, b;
                    Print b;
                End If
            End If
        End If
        If display = "atascii" Then
            GoSub specialcharacters3
            GoSub specialcharacters2a
            If flag = "Y1" Then
                If b1 = 1 Then
                    GoSub atasciicolors
                    GoSub colorchange
                    Color t1a, g1a
                    x = g1a
                    y = t1a
                    GoSub displ
                    If colorchanges = 1 Then
                        zz = Asc(b) + 128
                        Print #2, Chr$(zz);
                        Print b;
                    ElseIf colorchanges = 0 Then
                        zz = Asc(b) + 128
                        Print #2, Chr$(zz);
                        Print b;
                    End If

                Else If b1 = 2 Then
                        GoSub atasciicolors
                        GoSub colorchange
                        Color t1b, g1b
                        x = t1b
                        y = g1b
                        GoSub displ
                        If colorchanges = 1 Then
                            zz = Asc(b) + 128
                            Print #2, Chr$(128);
                            Print b;
                        ElseIf colorchanges = 0 Then
                            zz = Asc(b) + 128
                            Print #2, Chr$(zz);
                            Print b;
                        End If

                    ElseIf b1 = 3 Then
                        GoSub atasciicolors
                        GoSub colorchange
                        Color t1c, g1c
                        x = g1c
                        y = t1c
                        GoSub displ
                        If colorchanges = 1 Then
                            zz = Asc(b) + 128
                            Print #2, Chr$(zz);
                            Print b;
                        ElseIf colorchanges = 0 Then
                            zz = Asc(b) + 128
                            Print #2, Chr$(zz);
                            Print b;
                        End If

                    ElseIf b1 = 4 Then
                        GoSub atasciicolors
                        GoSub colorchange
                        Color t1d, g1d
                        x = g1d
                        y = t1d
                        GoSub displ
                        If colorchanges = 1 Then
                            zz = Asc(b) + 128
                            Print #2, Chr$(zz);
                            Print b;
                        ElseIf colorchanges = 0 Then
                            zz = Asc(b) + 128
                            Print #2, Chr$(zz);
                            Print b;
                        End If
                    End If
                End If
            End If

            If flag = "Y2" Then
                GoSub atasciicolors
                GoSub colorchange
                Color t2, g2
                x = g2
                y = t2
                GoSub displ
                If colorchanges = 1 Then
                    zz = Asc(b) + 128
                    Print #2, Chr$(zz);
                    Print b;
                ElseIf colorchanges = 0 Then
                    zz = Asc(b) + 128
                    Print #2, Chr$(zz);
                    Print b;
                End If
            End If
            If flag = "Y3" Then
                GoSub atasciicolors
                GoSub colorchange
                Color t, g
                x = g
                y = t
                GoSub displ
                If colorchanges = 1 Then
                    zz = Asc(b) + 128
                    Print #2, Chr$(zz);
                    Print b;
                End If
            Else
                If colorchanges = 0 Then
                    zz = Asc(b) + 128
                    Print #2, Chr$(zz);
                    Print b;
                End If
            End If
        End If


    Next i
    b8a = 0
    b9a = 0
    b10a = 0
    b11a = 0
    If display = "atascii" Then
        Print #2, Chr$(155);
        Print
    Else
        Print #2, ""
        Print
    End If
Loop
fini:
b1a = 0
b2a = 0
b3a = 0
b4a = 0
b5a = 0
b6a = 0
b7a = 0
b8a = 0
b9a = 0
b10a = 0
b11a = 0
closeit:
Close #1
Color 7, 0
If menuchoice = "M" And display = "atascii" Then
    Print #2, Chr$(155);
    Print #2, "Your Choice is : "; Chr$(155);
    Print #2, Chr$(155);
ElseIf menuchoice = "M" Then
    Do Until EOF(3)
        Line Input #3, lnumber
        Print #2, lnumber
        Print lnumber
    Loop

End If
Close #2
Close #3
Color 7, 0
Print
Print " Create another screen ? [Defaults to [Y]es : "
answer = UCase$(Input$(1))
If answer = "N" Then
    GoTo finish
Else
    contlne = 0
    GoSub randomchoices: GoTo inputscreen
End If

finish:
Cls
Print "My program is now finished and I"
Print "am looking for suggestions on ways"
Print "To improve it. The program has been"
Print "tested with Winserver 8.0 but has"
Print "not been tested with any other BBS"
Print "programs at this moment."
Print
Print "By now you will have noticed custom"
Print "Color sets, if you have not tried "
Print "this out , then give it a try."
Print
Print "The option to load and save and create"
Print "data sets is now here , if you have not"
Print "tried this feature out then give it a try."
Print
Print "Program written by Russ Campbell"
Print "For more information on how I"
Print "wrote this program, contact me"
Print "on Facebook at many of the groups"
Print "I am in , or email me at"
Print "rcamp48@rogers.com"
Print
Print "Thank you for using BBS Menu Color Scheme Creator 30 RC30"
Print
End
pick0:
Randomize Timer
pick = Int(Rnd(1) * 4) + 1
Return
displ:
If display = "ansi" Then
    If x = 0 Then code0b = Chr$(27) + Chr$(91) + Chr$(52) + Chr$(48) + Chr$(109) ' Black     Background
    If y = 0 Then code0f = Chr$(27) + Chr$(91) + Chr$(51) + Chr$(48) + Chr$(109) ' Black     Foreground
    If x = 1 Then code1b = Chr$(27) + Chr$(91) + Chr$(52) + Chr$(52) + Chr$(109) ' Blue      Background
    If y = 1 Then code1f = Chr$(27) + Chr$(91) + Chr$(51) + Chr$(52) + Chr$(109) ' Blue      Foreground
    If x = 2 Then code2b = Chr$(27) + Chr$(91) + Chr$(52) + Chr$(50) + Chr$(109) ' Green     Background
    If y = 2 Then code2f = Chr$(27) + Chr$(91) + Chr$(51) + Chr$(50) + Chr$(109) ' Green     Foreground
    If x = 3 Then code3b = Chr$(27) + Chr$(91) + Chr$(52) + Chr$(54) + Chr$(109) ' Cyan      Background
    If y = 3 Then code3f = Chr$(27) + Chr$(91) + Chr$(51) + Chr$(54) + Chr$(109) ' Cyan      Foreground
    If x = 4 Then code4b = Chr$(27) + Chr$(91) + Chr$(52) + Chr$(49) + Chr$(109) ' Red       Background
    If y = 4 Then code4f = Chr$(27) + Chr$(91) + Chr$(51) + Chr$(49) + Chr$(109) ' Red       Foreground
    If x = 5 Then code5b = Chr$(27) + Chr$(91) + Chr$(52) + Chr$(53) + Chr$(109) ' Magenta   Background
    If y = 5 Then code5f = Chr$(27) + Chr$(91) + Chr$(51) + Chr$(53) + Chr$(109) ' Magenta   Foreground
    If x = 6 Then code6b = Chr$(27) + Chr$(91) + Chr$(52) + Chr$(51) + Chr$(109) ' Brown     Background
    If y = 6 Then code6f = Chr$(27) + Chr$(91) + Chr$(51) + Chr$(51) + Chr$(109) ' Brown     Foreground
    If x = 7 Then code7b = Chr$(27) + Chr$(91) + Chr$(52) + Chr$(55) + Chr$(109) ' White     Background
    If y = 7 Then code7f = Chr$(27) + Chr$(91) + Chr$(51) + Chr$(55) + Chr$(109) ' White     Foreground
    If x = 0 Then backgroundcoloransi = code0b
    If x = 1 Then backgroundcoloransi = code1b
    If x = 2 Then backgroundcoloransi = code2b
    If x = 3 Then backgroundcoloransi = code3b
    If x = 4 Then backgroundcoloransi = code4b
    If x = 5 Then backgroundcoloransi = code5b
    If x = 6 Then backgroundcoloransi = code6b
    If x = 7 Then backgroundcoloransi = code7b
    If y = 0 Then foregroundcoloransi = code0f
    If y = 1 Then foregroundcoloransi = code1f
    If y = 2 Then foregroundcoloransi = code2f
    If y = 3 Then foregroundcoloransi = code3f
    If y = 4 Then foregroundcoloransi = code4f
    If y = 5 Then foregroundcoloransi = code5f
    If y = 6 Then foregroundcoloransi = code6f
    If y = 7 Then foregroundcoloransi = code7f
End If
If display = "wc8" Then
    If x = 0 Then backgroundcolorbbs = "@0"
    If x = 1 Then backgroundcolorbbs = "@1"
    If x = 2 Then backgroundcolorbbs = "@2"
    If x = 3 Then backgroundcolorbbs = "@3"
    If x = 4 Then backgroundcolorbbs = "@4"
    If x = 5 Then backgroundcolorbbs = "@5"
    If x = 6 Then backgroundcolorbbs = "@6"
    If x = 7 Then backgroundcolorbbs = "@7"
    If x = 8 Then backgroundcolorbbs = "@8"
    If x = 9 Then backgroundcolorbbs = "@9"
    If x = 10 Then backgroundcolorbbs = "@A"
    If x = 11 Then backgroundcolorbbs = "@B"
    If x = 12 Then backgroundcolorbbs = "@C"
    If x = 13 Then backgroundcolorbbs = "@D"
    If x = 14 Then backgroundcolorbbs = "@E"
    If x = 15 Then backgroundcolorbbs = "@F"
    If y = 0 Then foregroundcolorbbs = "0@"
    If y = 1 Then foregroundcolorbbs = "1@"
    If y = 2 Then foregroundcolorbbs = "2@"
    If y = 3 Then foregroundcolorbbs = "3@"
    If y = 4 Then foregroundcolorbbs = "4@"
    If y = 5 Then foregroundcolorbbs = "5@"
    If y = 6 Then foregroundcolorbbs = "6@"
    If y = 7 Then foregroundcolorbbs = "7@"
    If y = 8 Then foregroundcolorbbs = "8@"
    If y = 9 Then foregroundcolorbbs = "9@"
    If y = 10 Then foregroundcolorbbs = "A@"
    If y = 11 Then foregroundcolorbbs = "B@"
    If y = 12 Then foregroundcolorbbs = "C@"
    If y = 13 Then foregroundcolorbbs = "D@"
    If y = 14 Then foregroundcolorbbs = "E@"
    If y = 15 Then foregroundcolorbbs = "F@"
End If
If display = "pcb" Then
    If x = 0 Then backgroundcolorpcb = "@X0"
    If x = 1 Then backgroundcolorpcb = "@X1"
    If x = 2 Then backgroundcolorpcb = "@X2"
    If x = 3 Then backgroundcolorpcb = "@X3"
    If x = 4 Then backgroundcolorpcb = "@X4"
    If x = 5 Then backgroundcolorpcb = "@X5"
    If x = 6 Then backgroundcolorpcb = "@X6"
    If x = 7 Then backgroundcolorpcb = "@X7"
    If x = 8 Then backgroundcolorpcb = "@X8"
    If x = 9 Then backgroundcolorpcb = "@X9"
    If x = 10 Then backgroundcolorpcb = "@XA"
    If x = 11 Then backgroundcolorpcb = "@XB"
    If x = 12 Then backgroundcolorpcb = "@XC"
    If x = 13 Then backgroundcolorpcb = "@XD"
    If x = 14 Then backgroundcolorpcb = "@XE"
    If x = 15 Then backgroundcolorpcb = "@XF"
    If y = 0 Then foregroundcolorpcb = "0@"
    If y = 1 Then foregroundcolorpcb = "1@"
    If y = 2 Then foregroundcolorpcb = "2@"
    If y = 3 Then foregroundcolorpcb = "3@"
    If y = 4 Then foregroundcolorpcb = "4@"
    If y = 5 Then foregroundcolorpcb = "5@"
    If y = 6 Then foregroundcolorpcb = "6@"
    If y = 7 Then foregroundcolorpcb = "7@"
    If y = 8 Then foregroundcolorpcb = "8@"
    If y = 9 Then foregroundcolorpcb = "9@"
    If y = 10 Then foregroundcolorpcb = "A@"
    If y = 11 Then foregroundcolorpcb = "B@"
    If y = 12 Then foregroundcolorpcb = "C@"
    If y = 13 Then foregroundcolorpcb = "D@"
    If y = 14 Then foregroundcolorpcb = "E@"
    If y = 15 Then foregroundcolorpcb = "F@"

End If
If display = "synchro" Then
    If x = 0 Then backgroundcolorsynchro = Chr$(1) + "0"
    If x = 1 Then backgroundcolorsynchro = Chr$(1) + "1"
    If x = 2 Then backgroundcolorsynchro = Chr$(1) + "2"
    If x = 3 Then backgroundcolorsynchro = Chr$(1) + "3"
    If x = 4 Then backgroundcolorsynchro = Chr$(1) + "4"
    If x = 5 Then backgroundcolorsynchro = Chr$(1) + "5"
    If x = 6 Then backgroundcolorsynchro = Chr$(1) + "6"
    If x = 7 Then backgroundcolorsynchro = Chr$(1) + "7"
    If y = 0 Then foregroundcolorsynchro = "k"
    If y = 1 Then foregroundcolorsynchro = "b"
    If y = 2 Then foregroundcolorsynchro = "g"
    If y = 3 Then foregroundcolorsynchro = "c"
    If y = 4 Then foregroundcolorsynchro = "r"
    If y = 5 Then foregroundcolorsynchro = "m"
    If y = 6 Then foregroundcolorsynchro = "y"
    If y = 7 Then foregroundcolorsynchro = "w"


End If
If display = "mystic" Then
    If x = 0 Then backgroundcolormystic = Chr$(254) + "16"
    If x = 1 Then backgroundcolormystic = Chr$(254) + "17"
    If x = 2 Then backgroundcolormystic = Chr$(254) + "18"
    If x = 3 Then backgroundcolormystic = Chr$(254) + "19"
    If x = 4 Then backgroundcolormystic = Chr$(254) + "20"
    If x = 5 Then backgroundcolormystic = Chr$(254) + "21"
    If x = 6 Then backgroundcolormystic = Chr$(254) + "22"
    If x = 7 Then backgroundcolormystic = Chr$(254) + "23"
    If x = 8 Then backgroundcolormystic = Chr$(254) + "24"
    If x = 9 Then backgroundcolormystic = Chr$(254) + "25"
    If x = 10 Then backgroundcolormystic = Chr$(254) + "26"
    If x = 11 Then backgroundcolormystic = Chr$(254) + "27"
    If x = 12 Then backgroundcolormystic = Chr$(254) + "28"
    If x = 13 Then backgroundcolormystic = Chr$(254) + "29"
    If x = 14 Then backgroundcolormystic = Chr$(254) + "30"
    If x = 15 Then backgroundcolormystic = Chr$(254) + "31"
    If y = 0 Then foregroundcolormystic = Chr$(254) + "00"
    If y = 1 Then foregroundcolormystic = Chr$(254) + "01"
    If y = 2 Then foregroundcolormystic = Chr$(254) + "02"
    If y = 3 Then foregroundcolormystic = Chr$(254) + "03"
    If y = 4 Then foregroundcolormystic = Chr$(254) + "04"
    If y = 5 Then foregroundcolormystic = Chr$(254) + "05"
    If y = 6 Then foregroundcolormystic = Chr$(254) + "06"
    If y = 7 Then foregroundcolormystic = Chr$(254) + "07"
    If y = 8 Then foregroundcolormystic = Chr$(254) + "08"
    If y = 9 Then foregroundcolormystic = Chr$(254) + "09"
    If y = 10 Then foregroundcolormystic = Chr$(254) + "10"
    If y = 11 Then foregroundcolormystic = Chr$(254) + "11"
    If y = 12 Then foregroundcolormystic = Chr$(254) + "12"
    If y = 13 Then foregroundcolormystic = Chr$(254) + "13"
    If y = 14 Then foregroundcolormystic = Chr$(254) + "14"
    If y = 15 Then foregroundcolormystic = Chr$(254) + "15"
End If
If display = "atascii" Then
    If x = 0 Then backgroundcoloratascii = Chr$(128)
    If x = 1 Then backgroundcoloratascii = Chr$(128)
    If x = 2 Then backgroundcoloratascii = Chr$(128)
    If x = 3 Then backgroundcoloratascii = Chr$(128)
    If x = 4 Then backgroundcoloratascii = Chr$(128)
    If x = 5 Then backgroundcoloratascii = Chr$(128)
    If x = 6 Then backgroundcoloratascii = Chr$(128)
    If x = 7 Then backgroundcoloratascii = Chr$(128)
    If x = 8 Then backgroundcoloratascii = Chr$(128)
    If x = 9 Then backgroundcoloratascii = Chr$(128)
    If x = 10 Then backgroundcoloratascii = Chr$(128)
    If x = 11 Then backgroundcoloratascii = Chr$(128)
    If x = 12 Then backgroundcoloratascii = Chr$(128)
    If x = 13 Then backgroundcoloratascii = Chr$(128)
    If x = 14 Then backgroundcoloratascii = Chr$(128)
    If x = 15 Then backgroundcoloratascii = Chr$(128)
    If y = 0 Then foregroundcoloratascii = "00"
    If y = 1 Then foregroundcoloratascii = "01"
    If y = 2 Then foregroundcoloratascii = "02"
    If y = 3 Then foregroundcoloratascii = "03"
    If y = 4 Then foregroundcoloratascii = "04"
    If y = 5 Then foregroundcoloratascii = "05"
    If y = 6 Then foregroundcoloratascii = "06"
    If y = 7 Then foregroundcoloratascii = "07"
    If y = 8 Then foregroundcoloratascii = "08"
    If y = 9 Then foregroundcoloratascii = "09"
    If y = 10 Then foregroundcoloratascii = "10"
    If y = 11 Then foregroundcoloratascii = "11"
    If y = 12 Then foregroundcoloratascii = "12"
    If y = 13 Then foregroundcoloratascii = "13"
    If y = 14 Then foregroundcoloratascii = "14"
    If y = 15 Then foregroundcoloratascii = "15"

End If
Return
specialcharacters2:
If b = Chr$(94) Then
    b = corner1
    b1a = b1a + 1
    If b1a = 1 Then
        b1 = 1
    ElseIf b1a = 1 And b6a = 1 Then
        b1 = 2
    ElseIf b1a = 2 And b6a = 2 Then
        b1 = 3
    ElseIf b1a = 3 And b6a = 3 Then
        b1 = 4

    End If


End If
If b = Chr$(36) Then
    b = corner2
    b2a = b2a + 1
    If b2a = 1 Then
        b1 = 1
    ElseIf b2a = 1 And b1a = 1 Then
        b1 = 2
    ElseIf b2a = 2 And b1a = 2 Then
        b1 = 3
    ElseIf b2a = 3 And b1a = 3 Then
        b1 = 4

    End If
End If
If b = Chr$(37) Then
    b = corner3
    b3a = b3a + 1
    If b3a = 1 Then
        b1 = 1
    ElseIf b3a = 1 And b2a = 1 Then
        b1 = 2
    ElseIf b3a = 2 And b2a = 2 Then
        b1 = 3
    ElseIf b3a = 3 And b2a = 3 Then
        b1 = 4
    End If
End If
If b = Chr$(126) Then
    b = corner4
    b4a = b4a + 1
    If b4a = 1 Then
        b1 = 1
    ElseIf b4a = 1 And b1a = 1 Then
        b1 = 2
    ElseIf b4a = 2 And b1a = 2 Then
        b1 = 3
    ElseIf b4a = 3 And b1a = 3 Then
        b1 = 4
    End If
End If

If b = Chr$(125) Then
    b = middlelineconnector
    b1 = 1

End If

If b = Chr$(62) Then
    b = bottomlineconnector
    b5a = b5a + 1
    If b5a = 1 Then
        b1 = 1
    ElseIf b5a = 1 And b3a = 1 Then
        b1 = 2
    ElseIf b5a = 2 And b3a = 2 Then
        b1 = 3
    ElseIf b5a = 3 And b3a = 3 Then
        b1 = 4

    End If
End If

If b = Chr$(123) Then
    b = toplineconnector
    b6a = b6a + 1
    If b6a = 1 Then
        b1 = 1
    ElseIf b6a = 1 And b1a = 1 Then
        b1 = 2
    ElseIf b6a = 2 And b1a = 2 Then
        b1 = 3
    ElseIf b6a = 3 And b1a = 3 Then
        b1 = 4
    End If



End If

If b = Chr$(38) Then
    b = connector1
    b8a = b8a + 1
    If b8a = 1 Then
        b1 = 1
    ElseIf b8a = 2 And b1a = 1 Then
        b1 = 1
    ElseIf b8a = 2 And b1a = 2 Then
        b1 = 2
    ElseIf b8a = 3 And b1a = 2 Then
        b1 = 3
    ElseIf b8a = 4 And b1a = 3 Then
        b1 = 4
    End If



End If
If b = Chr$(61) Then
    b = connector2
    b9a = b9a + 1
    If b9a = 1 Then
        b1 = 1
    ElseIf b9a = 2 And b4a = 1 Then
        b1 = 2
    ElseIf b9a = 3 And b4a = 2 Then
        b1 = 3
    ElseIf b9a = 4 And b4a = 3 Then
        b1 = 4
    End If

End If
If b = Chr$(35) Then
    b = connector3
    b10a = b10a + 1
    If b10a = 1 Then
        b1 = 1
    ElseIf b10a = 2 And b1a = 1 Then
        b10a = 1
    ElseIf b10a = 2 And b1a = 2 Then
        b10a = 2
    ElseIf b10a = 3 And b1a = 2 Then
        b1 = 3
    ElseIf b10a = 4 And b1a = 3 Then
        b1 = 4
    End If

End If
If b = Chr$(42) Then
    b = connector4
    b11a = b11a + 1
    If b11a = 1 Then
        b1 = 1
    ElseIf b11a = 2 And b3a = 1 Then
        b1 = 2
    ElseIf b11a = 3 And b3a = 2 Then
        b1 = 3
    ElseIf b11a = 4 And b3a = 3 Then
        b1 = 4
    End If

End If
If b = Chr$(33) Then
    b = leftverticalline
    b6a = b6a + 1
    If b6a = 1 Then
        b1 = 1
    ElseIf b6a = 1 And b1a = 1 Then
        b1 = 2
    ElseIf b6a = 2 And b1a = 2 Then
        b1 = 3
    ElseIf b6a = 3 And b1a = 3 Then
        b1 = 4
    End If
End If
If b = Chr$(60) Then
    b = rightverticalline
    b7a = b7a + 1
    If b7a = 1 Then
        b1 = 1
    ElseIf b7a = 1 And b4a = 1 Then
        b1 = 2
    ElseIf b7a = 2 And b4a = 2 Then
        b1 = 3
    ElseIf b7a = 3 And b4a = 3 Then
        b1 = 4
    End If
End If
Return
specialcharacters2a:
If b = Chr$(94) Then
    b = corner1a
    b1a = b1a + 1
    If b1a = 1 Then
        b1 = 1
    ElseIf b1a = 1 And b6a = 1 Then
        b1 = 2
    ElseIf b1a = 2 And b6a = 2 Then
        b1 = 3
    ElseIf b1a = 3 And b6a = 3 Then
        b1 = 4

    End If


End If
If b = Chr$(36) Then
    b = corner2a
    b2a = b2a + 1
    If b2a = 1 Then
        b1 = 1
    ElseIf b2a = 1 And b1a = 1 Then
        b1 = 2
    ElseIf b2a = 2 And b1a = 2 Then
        b1 = 3
    ElseIf b2a = 3 And b1a = 3 Then
        b1 = 4

    End If
End If
If b = Chr$(37) Then
    b = corner3a
    b3a = b3a + 1
    If b3a = 1 Then
        b1 = 1
    ElseIf b3a = 1 And b2a = 1 Then
        b1 = 2
    ElseIf b3a = 2 And b2a = 2 Then
        b1 = 3
    ElseIf b3a = 3 And b2a = 3 Then
        b1 = 4
    End If
End If
If b = Chr$(126) Then
    b = corner4a
    b4a = b4a + 1
    If b4a = 1 Then
        b1 = 1
    ElseIf b4a = 1 And b1a = 1 Then
        b1 = 2
    ElseIf b4a = 2 And b1a = 2 Then
        b1 = 3
    ElseIf b4a = 3 And b1a = 3 Then
        b1 = 4
    End If
End If

If b = Chr$(125) Then
    b = middlelineconnectora
    b1 = 1

End If

If b = Chr$(62) Then
    b = bottomlineconnectora
    b5a = b5a + 1
    If b5a = 1 Then
        b1 = 1
    ElseIf b5a = 1 And b3a = 1 Then
        b1 = 2
    ElseIf b5a = 2 And b3a = 2 Then
        b1 = 3
    ElseIf b5a = 3 And b3a = 3 Then
        b1 = 4

    End If
End If

If b = Chr$(123) Then
    b = toplineconnectora
    b6a = b6a + 1
    If b6a = 1 Then
        b1 = 1
    ElseIf b6a = 1 And b1a = 1 Then
        b1 = 2
    ElseIf b6a = 2 And b1a = 2 Then
        b1 = 3
    ElseIf b6a = 3 And b1a = 3 Then
        b1 = 4
    End If



End If

If b = Chr$(38) Then
    b = connector1a
    b8a = b8a + 1
    If b8a = 1 Then
        b1 = 1
    ElseIf b8a = 2 And b1a = 1 Then
        b1 = 1
    ElseIf b8a = 2 And b1a = 2 Then
        b1 = 2
    ElseIf b8a = 3 And b1a = 2 Then
        b1 = 3
    ElseIf b8a = 4 And b1a = 3 Then
        b1 = 4
    End If



End If
If b = Chr$(61) Then
    b = connector2a
    b9a = b9a + 1
    If b9a = 1 Then
        b1 = 1
    ElseIf b9a = 2 And b4a = 1 Then
        b1 = 2
    ElseIf b9a = 3 And b4a = 2 Then
        b1 = 3
    ElseIf b9a = 4 And b4a = 3 Then
        b1 = 4
    End If

End If
If b = Chr$(35) Then
    b = connector3a
    b10a = b10a + 1
    If b10a = 1 Then
        b1 = 1
    ElseIf b10a = 2 And b1a = 1 Then
        b10a = 1
    ElseIf b10a = 2 And b1a = 2 Then
        b10a = 2
    ElseIf b10a = 3 And b1a = 2 Then
        b1 = 3
    ElseIf b10a = 4 And b1a = 3 Then
        b1 = 4
    End If

End If
If b = Chr$(42) Then
    b = connector4a
    b11a = b11a + 1
    If b11a = 1 Then
        b1 = 1
    ElseIf b11a = 2 And b3a = 1 Then
        b1 = 2
    ElseIf b11a = 3 And b3a = 2 Then
        b1 = 3
    ElseIf b11a = 4 And b3a = 3 Then
        b1 = 4
    End If

End If
If b = Chr$(33) Then
    b = leftverticallinea
    b6a = b6a + 1
    If b6a = 1 Then
        b1 = 1
    ElseIf b6a = 1 And b1a = 1 Then
        b1 = 2
    ElseIf b6a = 2 And b1a = 2 Then
        b1 = 3
    ElseIf b6a = 3 And b1a = 3 Then
        b1 = 4
    End If
End If
If b = Chr$(60) Then
    b = rightverticallinea
    b7a = b7a + 1
    If b7a = 1 Then
        b1 = 1
    ElseIf b7a = 1 And b4a = 1 Then
        b1 = 2
    ElseIf b7a = 2 And b4a = 2 Then
        b1 = 3
    ElseIf b7a = 3 And b4a = 3 Then
        b1 = 4
    End If
End If

Return
specialcharacters3:
If b = Chr$(94) Or b = Chr$(60) Or b = Chr$(62) Or b = Chr$(123) Or b = Chr$(36) Or b = Chr$(43) Or b = Chr$(37) Or b = Chr$(126) Or b = Chr$(125) Or b = Chr$(38) Or b = Chr$(35) Or b = Chr$(42) Or b = Chr$(33) Or b = Chr$(61) Then
    flag = "Y1"
ElseIf b = Chr$(91) Or b = Chr$(93) Then
    flag = "Y2"
Else
    flag = "Y3"
End If
Return


' This is where I detect the amount of times that there are corners
' For each corner counted elsewhere in the program
' I do a different colorchange
' Thus opening up the possibility of differnt colors for each
' Box where the lines are
' Many boxes can be colored a different color
' Using this method
' Changes must also be made in the main program as well as in the
' Selection of colors for each set of corners, lines and connectors                                                                                                                               '

colorchange:
If flag = "Y1" Then
    g1a = g1a ' Color change for First Outer Box
    t1a = t1a
    g1b = g1b ' Color change for Second Outer Box
    t1b = t1b
    g1c = g1c ' Color change for Third Outer Box
    t1c = t1c
    g1d = g1d ' Color change for Fourth Outer Box
    t1d = t1d

    colorchanges = 1
ElseIf flag = "Y2" Then
    g2 = g2
    t2 = t2
    colorchanges = 1
ElseIf flag = "Y3" Then
    g = g
    t = t

    colorchanges = 1
Else
    g = g
    t = t
    colorchanges = 0
End If
Return


errorhandle:
Cls
Close #1
Print
Print "Oops, you did something wrong there (or I did) ... "
Print
Print "Most likely there was a filename that you entered"
Print "That did not exist or you have not created the"
Print "prompt file for the Menu item desired. Or you picked"
Print "a Menu filename and selected a display output instead "
Print "a Menu display, the difference is that Menu output "
Print "files have a prompt file, display files do not. "
Print
Print "Your File "; file1; " Has Not Been Found .... Please Try Again."
Print
Print "If the file is there and it still gives an error, then please"
Print "report this as a program error and contact me at rcamp48@rogers.com "
Print
Print "Press any key to try again......"
Do While InKey$ = ""
Loop
GoTo inputscreen
Return

background:
Cls
Print
Print "Please enter the color of your Background "
Print
Print "Black        is color code 0            Black is color code    8"
Print "Blue         is color code 1            Blue is  color code    9"
Print "Green        is color code 2            Green is color code   10"
Print "Cyan         is color code 3            Cyan is  color code   11"
Print "Red          is color code 4            Red  is  color code   12"
Print "Magenta      is color code 5            Magenta is color code 13"
Print "Brown        is color code 6            Yellow  is color code 14"
Print "White        is color code 7            White   is colr code  15"
Print
Print "Don't worry, the program will translate from decimal to hex for this."
Print "Make your Background selection from [0] to [7] for Syncronet Ansi."
Print "And [0] to [15] for everything else"
Print
Return
fixedcolors:
g1a = 2
t1a = 14
g1b = 1
t1b = 12
g1c = 5
t1c = 7
g1d = 6
t1d = 11
g2 = 7
t2 = 1
g = 6
t = 14
Return
atasciicolors:
g1a = 15
t1a = 0
g1b = 15
t1b = 0
g1c = 15
t1c = 0
g1d = 15
t1d = 0
g2 = 15
t2 = 0
g = 15
t = 0
Return

create:
Cls
Print "Save File: [Y]es or [N]o : "
answer = UCase$(Input$(1))
If answer$ = "N" Then GoTo inputscreen
Print
Print
Input "Enter Filename [No extender] [Press enter for default] : ", filess
Print
If filess = "" Then filess = "default"

filess = filess + ".dat"
fnames = "c:\display\data\" + filess
Print "Saving data............"
Print
On Error GoTo errorhandle
Open fnames For Output As #2
Print #2, g1a
Print #2, t1a
Print #2, g1b
Print #2, t1b
Print #2, g1c
Print #2, t1c
Print #2, g1d
Print #2, t1d
Print #2, g2
Print #2, t2
Print #2, g
Print #2, t
Close #2
Return
load:
Cls
Print "Load Data File..........."
Input "Enter Filename [No extender] [Press enter for default] :  ", filess
Print
If filess = "" Then filess = "default"
filess = filess + ".dat"
fnames = "c:\display\data\" + filess
Print
Print "Loading data..........."
Print
On Error GoTo errorhandle
Open fnames For Input As #1
Input #1, g1a
Input #1, t1a
Input #1, g1b
Input #1, t1b
Input #1, g1c
Input #1, t1c
Input #1, g1d
Input #1, t1d
Input #1, g2
Input #1, t2
Input #1, g
Input #1, t
Close #1
Return
checkit:
If display = "Ansi" Or display = "Synchro" Then
    ddd = 7
Else
    ddd = 15
End If
Return

randomchoices:
Randomize Timer
trulyrandom:

g1a = Int(Rnd(1) * 7) + 1
t1a = Int(Rnd(1) * 15) + 1
g1b = Int(Rnd(1) * 7) + 1
t1b = Int(Rnd(1) * 7) + 1
g1c = Int(Rnd(1) * 7) + 1
t1c = Int(Rnd(1) * 7) + 1
g1d = Int(Rnd(1) * 7) + 1
t1d = Int(Rnd(1) * 7) + 1
g2 = Int(Rnd(1) * 7) + 1
t2 = Int(Rnd(1) * 7) + 1
g = Int(Rnd(1) * 7) + 1
t = Int(Rnd(1) * 7) + 1



If g1a = t1a Then GoTo trulyrandom
If g1b = t1b Then GoTo trulyrandom
If g1c = t1c Then GoTo trulyrandom
If g1d = t1d Then GoTo trulyrandom
If g1a = g1b Then GoTo trulyrandom
If g1a = g1c Then GoTo trulyrandom
If g1a = g1d Then GoTo trulyrandom
If g1b = g1a Then GoTo trulyrandom
If g1b = g1c Then GoTo trulyrandom
If g1b = g1d Then GoTo trulyrandom
If g1c = g1a Then GoTo trulyrandom
If g1c = g1b Then GoTo trulyrandom
If g1c = g1d Then GoTo trulyrandom
If g1d = g1a Then GoTo trulyrandom
If g1d = g1b Then GoTo trulyrandom
If g1d = g1c Then GoTo trulyrandom
If g1a = g Then GoTo trulyrandom
If g1b = g Then GoTo trulyrandom
If g1c = g Then GoTo trulyrandom
If g1d = g Then GoTo trulyrandom
If g2 = t2 Then GoTo trulyrandom
If g = ga1 Or g = g2 Then GoTo trulyrandom
If g = g1b Or g = g2 Then GoTo trulyrandom
If g = g1c Or g = g2 Then GoTo trulyrandom
If g = g1d Or g = g2 Then GoTo trulyrandom
If g = t Then GoTo trulyrandom
Return
