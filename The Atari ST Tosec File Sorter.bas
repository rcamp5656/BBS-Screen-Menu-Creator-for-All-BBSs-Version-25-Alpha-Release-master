Dim source As String
Dim destination As String
Dim sourcepath As String
Dim destinationpath As String
Dim a$(32)
Dim b$(6)
Dim c$(36)
Dim d$(6)
Dim e$(36)
_FullScreen
Print
If Not _DirExists("f:\AtariST_Sorted") Then
    MkDir "f:\AtariST_Sorted"
End If
source = "f:\ST\"
destination = "f:\AtariST_Sorted\"
a$(1) = "Applications\"
a$(2) = "Collections\4U\"
a$(3) = "Collections\ACN\"
a$(4) = "Collections\Atari_Inside\"
a$(5) = "Collections\Atari_Journal\"
a$(6) = "Collections\Atari_Magazine\"
a$(7) = "Collections\Bitz\"
a$(8) = "Collections\Budgie_UK\"
a$(9) = "Collections\Cobra\"
a$(10) = "Collections\Delta_Labs\"
a$(11) = "Collections\Diverse\"
a$(12) = "Collections\Falcon\"
a$(13) = "Collections\FaST_Club\"
a$(14) = "Collections\FloppyShop\"
a$(15) = "Collections\GFA_Club\"
a$(16) = "Collections\Heim_Special_Line"
a$(17) = "Collections\Kontrast\"
a$(18) = "Collections\Poolware\"
a$(19) = "Collections\ST_Computer\"
a$(20) = "Collections\ST_Vision\"
a$(21) = "Collections\XEST\"
a$(22) = "Compilations\Applications\"
a$(23) = "Compilations\Demos\"
a$(24) = "Compilations\Educational\"
a$(25) = "Compilations\Games\"
a$(26) = "Coverdisks\"
a$(27) = "Demos\"
a$(28) = "Diskmags\"
a$(29) = "Docs\"
a$(30) = "Educational\"
a$(31) = "Games\"
a$(32) = "Operating_Systems\"
b$(1) = "[ST]\"
b$(2) = "[STX]\"
b$(3) = "[MSA]\"
b$(4) = "[PRG]\"
b$(5) = "[TOS]\"
b$(6) = "[IMG]\"
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
d$(1) = "*.ST"
d$(2) = "*.STX"
d$(3) = "*.MSA"
d$(4) = "*.TOS"
d$(6) = "*.PRG"
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
Color 0, 2
Cls

For w = 1 To 32
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
    Print "%%%%%    The Atari ST Tosek File Sorter  -  Written By Russ Campbell     %%%%%"
    Locate 9, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 10, 2
    Print "%%%%%            Release Date May 26 2021  -  Version 1.0b               %%%%%"
    Locate 11, 2
    Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 12, 2: Print "%%%%%                                                                    %%%%%"
    Locate 13, 2: Print "%%%%%                                                                    %%%%%"
    Locate 14, 2: Print "%%%%%                                                                    %%%%%"
    Locate 15, 2: Print "%%%%%    As long as we are sorting RETRO files then it is fitting        %%%%%"
    Locate 16, 2: Print "%%%%%    that we would use a RETRO style program to do this......        %%%%%"
    Locate 17, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 18, 2: Print "%%%%%      Atari ST Files are all sorted alpabetically every time        %%%%%"
    Locate 19, 2: Print "%%%%% Includes compressed ST files but will include individual programs  %%%%%"
    Locate 20, 2: Print "%%%%%         Program Copyright (c) 2021 All Rights Reserved.            %%%%%"
    Locate 21, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 22, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 23, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 24, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    Locate 25, 2: Print "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    For x = 1 To 6
        For y = 0 To 35
            For z = 1 To 6
                sourcepath = source + a$(w) + b$(x) + e$(y) + d$(z)
                destinationpath = destination + a$(w) + b$(x) + c$(y) + d$(z)

                Locate 13, 2: Print "%%%%%    Now Writing : " + b$(x) + c$(y) + d$(z)

                Shell _Hide Chr$(34) + "xcopy " + sourcepath + " " + destinationpath + " /Y /s" + Chr$(34)

            Next z
        Next y
    Next x
Next w
finish:
End
