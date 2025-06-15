Dim Shared perm1(0 To 255, 0 To 7) As Single
Dim Shared perm2(0 To 255, 0 To 255, 0 To 7) As Single
Dim Shared perm3(0 To 15, 0 To 15, 0 To 15, 0 To 7) As Single

'Calculate Fade Function Lookup Table
Dim Shared fade!(0 To 255)
For I = 0 To 255
    __t! = I / 256
    fade!(I) = __t! * __t! * (3 - 2 * __t!)
Next I

RePERM Timer
