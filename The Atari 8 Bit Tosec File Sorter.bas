Dim source As String
Dim destination As String
Dim sourcepath As String
Dim destinationpath As String
Dim a$(44)
Dim b$(1)
Dim c$(36)
Dim d$(1)
Dim e$(36)
_FullScreen
Print
If Not _DirExists("g:\Amiga_Sorted") Then
    MkDir "g:\Amiga_Sorted"
End If
source = "h:\Amiga\"
destination = "g:\Amiga_Sorted\"
a$(1) = "Applications\"
a$(2) = "Collections\Amiga_Amateur_Radio_User_Group\"
a$(3) = "Collections\Amiga_Public_Domain_Connection\"
a$(4) = "Collections\Auge_4000\"
a$(5) = "Collections\Bavarian\"
a$(6) = "Collections\Berliner_Spielekiste\"
a$(7) = "Collections\Cactus\"
a$(8) = "Collections\Den_Norske_Hjemmedataklubben\"
a$(9) = "Collections\Fred_Fish\"
a$(10) = "Collections\Megadisk\"
a$(11) = "Collections\Memphis_Amiga_Group\"
a$(12) = "Collections\New_Zealand_Amiga_Users_Group\"
a$(13) = "Collections\Norlicht_Spiele\"
a$(14) = "Collections\Panorama\"
a$(15) = "Collections\Scope\"
a$(16) = "Collections\Software_of_the_Month_Club_(SOMC)"
a$(17) = "Collections\Tiafun\"
a$(18) = "Collections\Tampa_Bay_Amiga_Group\"
a$(19) = "Collections\The_Assassins\"
a$(20) = "Collections\The_Best_of_Public_Domain\"
a$(21) = "Collections\TOGA\"
a$(22) = "Collections\Topik\"
a$(23) = "Collections\Various\"
a$(24) = "Compilations\Applications\"
a$(25) = "Compilations\Games\"
a$(26) = "Compilations\Various\"
a$(27) = "Demos\Animations_and_Various\"
a$(28) = "Demos\Music\"
a$(29) = "Demos\Packs\"
a$(30) = "Demos\Slideshows\"
a$(31) = "Demos\Various\"
a$(32) = "Games\ADF\"
a$(33) = "Games\Emerald_Mine\"
a$(34) = "Games\Public_Domain\"
a$(35) = "Games\Save_Disks\"
a$(36) = "Games\Unofficial_addon_&_Patches\"
a$(37) = "Diskmags\"
a$(38) = "Docs\"
a$(39) = "Educational\"
a$(40) = "Firmware\"
a$(41) = "Games\"
a$(42) = "Kickstart-Disks\"
a$(43) = "Operating_Systems\"
a$(44) = "Packmags\"
b$(1) = "[ADF]\"
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
d$(1) = "*.ADF"
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
Else

End If
Screen 12
Cls
Color 7, 1
Cls

For w = 1 To 13
    Cls
    Locate 3, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 4, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 5, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 6, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 7, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 8, 2
    Print "%%%%%      The Amiga Tosek File Sorter  -  Written By Russ Campbell      %%%%%"
    Locate 9, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 10, 2
    Print "%%%%%            Release Date June 1 2021  -  Version 1.0a               %%%%%"
    Locate 11, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 12, 2: Print "%%%%%                                                                    %%%%%"
    Locate 13, 2: Print "%%%%%                                                                    %%%%%"
    Locate 14, 2: Print "%%%%%                                                                    %%%%%"
    Locate 15, 2: Print "%%%%%    As long as we are sorting RETRO files then it is fitting        %%%%%"
    Locate 16, 2: Print "%%%%%    that we would use a RETRO style program to do this......        %%%%%"
    Locate 17, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 18, 2: Print "%%%%% Commodore Amiga Files are all sorted aphabetically Each Time       %%%%%"
    Locate 19, 2: Print "%%%%% Commodor 64 and Commodore 128 are next in line for programming.    %%%%%"
    Locate 20, 2: Print "%%%%%         Program Copyright (c) 2021 All Rights Reserved.            %%%%%"
    Locate 21, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 22, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 23, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 24, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 25, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    For x = 1 To 11
        For y = 0 To 35
            For z = 1 To 12
                sourcepath = source + a$(w) + b$(x) + e$(y) + d$(z)
                destinationpath = destination + a$(w) + b$(x) + c$(y) + d$(z)

                Locate 13, 2: Print "%%%%%    Now Writing : " + destination + a$(w) + b$(x) + c$(y) + d$(z)

                Shell _Hide Chr$(34) + "xcopy " + sourcepath + " " + destinationpath + " /Y /s" + Chr$(34)

            Next z
        Next y
    Next x
Next w
finish:
End
