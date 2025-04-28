Dim Shared perm2(0 To 255, 0 To 255, 0 To 7) As Single
Dim Shared perm3(0 To 15, 0 To 15, 0 To 15, 0 To 7) As Single

'Calculate Fade Function Lookup Table
Dim Shared fade!(0 To 255)
For I = 0 To 255
    t! = I / 256
    fade!(I) = t! * t! * (3 - 2 * t!)
Next I

RePERM Timer
