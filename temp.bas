Dim source As String
Dim destination As String
Dim sourcepath As String * 160
Dim destinationpath As String * 100
Dim a$(12)
Dim b$(8)
Dim c$(36)
Dim d$(8)
Dim e$(36)
_FullScreen
Print
If Not _DirExists("c:\Atari") Then
    MkDir "c:\Atari"
End If
source = "c:\8bit\"
destination = "c:\Atari\"
a$(1) = "Applications\"
a$(2) = "Compilations\Applications\"
a$(3) = "Compilations\Demos\"
a$(4) = "Compilations\Educational\"
a$(5) = "Compilations\Games\"
a$(6) = "Demos\"
a$(7) = "Docs\"
a$(8) = "Educational\"
a$(9) = "Games\"
a$(10) = "Magazines\"
a$(11) = "Operating Systems\"
a$(12) = "Sources\"
b$(1) = "[ATR]\"
b$(2) = "[ATX]\"
b$(3) = "[BAS]\"
b$(4) = "[BIN]\"
b$(5) = "[CAS]\"
b$(6) = "[XEX]\"
b$(7) = "[XFD]\"
b$(8) = "[ASM]\"
c$(0) = "0\"
c$(1) = "1\"
c$(2) = "2\"
c$(3) = "3\"
c$(4) = "4\"
c$(5) = "5\"
c$(6) = "6\"
c$(7) = "7\"
c$(8) = "8\"
c$(9) = "9\"
c$(10) = "A\"
c$(11) = "B\"
c$(12) = "C\"
c$(13) = "D\"
c$(14) = "E\"
c$(15) = "F\"
c$(16) = "G\"
c$(17) = "H\"
c$(18) = "I\"
c$(19) = "J\"
c$(20) = "K\"
c$(21) = "L\"
c$(22) = "M\"
c$(23) = "N\"
c$(24) = "O\"
c$(25) = "P\"
c$(26) = "Q\"
c$(27) = "R\"
c$(28) = "S\"
c$(29) = "T\"
c$(30) = "U\"
c$(31) = "V\"
c$(32) = "W\"
c$(33) = "X\"
c$(34) = "Y\"
c$(35) = "Z\"
d$(1) = "*.ATR"
d$(2) = "*.ATX"
d$(3) = "*.BAS"
d$(4) = "*.BIN"
d$(5) = "*.CAS"
d$(6) = "*.XEX"
d$(7) = "*.XFD"
d$(8) = "*.ASM"
e$(0) = "0"
e$(1) = "1"
e$(2) = "2"
e$(3) = "3"
e$(4) = "4"
e$(5) = "5"
e$(6) = "6"
e$(7) = "7"
e$(8) = "8"
e$(9) = "9"
e$(10) = "A"
e$(11) = "B"
e$(12) = "C"
e$(13) = "D"
e$(14) = "E"
e$(15) = "F"
e$(16) = "G"
e$(17) = "H"
e$(18) = "I"
e$(19) = "J"
e$(20) = "K"
e$(21) = "L"
e$(22) = "M"
e$(23) = "N"
e$(24) = "O"
e$(25) = "P"
e$(26) = "Q"
e$(27) = "R"
e$(28) = "S"
e$(29) = "T"
e$(30) = "U"
e$(31) = "V"
e$(32) = "W"
e$(33) = "X"
e$(34) = "Y"
e$(35) = "Z"
If Not _DirExists(destination) Then
    MkDir destination


End If
Screen 12
Cls
Color 14, 12
Cls
For w = 1 To 12
    For x = 1 To 8
        For z = 1 To 8
            For y = 0 To 35
                Cls
                Locate 4, 2
                Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 5, 2
                Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 6, 2
                Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 7, 2
                Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 8, 2
                Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 9, 2
                Print "%%%%%                  The Atari 8 Bit Tosek File Sorter                 %%%%%"
                Locate 10, 2
                Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 11, 2
                Print "%%%%%                      Written by Russ Campbell                      %%%%%"
                Locate 12, 2
                Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                sourcepath = source + a$(w) + b$(x) + e$(y) + d$(z)
                destinationpath = destination + a$(w) + b$(x) + c$(y) + d$(z)
                Shell "move " + sourcepath + " " + destinationpath + "/Y >NULL"
                Locate 13, 2: Print "%%%%%                                                                    %%%%%"
                Locate 13, 2: Print "%%%%%                                                                    %%%%%"
                Locate 14, 2: Print "%%%%%                                                                    %%%%%"
                Locate 15, 2: Print "%%%%%    As long as we are sorting RETRO files then it is fitting        %%%%%"
                Locate 16, 2: Print "%%%%%    that we would use a RETRO style program to do this......        %%%%%"
                Locate 17, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 18, 2: Print "%%%%% Atari 8 Bit Files are all sorted alpabetically every time          %%%%%"
                Locate 19, 2: Print "%%%%% Atari ST is next for programming of Sorting Routines  .....        %%%%%"
                Locate 14, 2: Print "%%%%%         Program Copyright (c) 2021 All Rights Reeserved.           %%%%%"
                Locate 20, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 21, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 22, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 23, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 24, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
                Locate 13, 4: Print "%%%%%  Sorting Directory " + e$(y) + " of " + b$(x) + "\" + d$(z) + " Files  "
            Next y
        Next z
    Next x
Next w
finish:
End

