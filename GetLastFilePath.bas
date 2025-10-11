Function GetLastFilePath$ (F$) Static
    Dim As _Unsigned Long P_A, P_B
    P_A = _InStrRev(F$, "/")
    P_B = _InStrRev(F$, "\")
    GetLastFilePath$ = Mid$(F$, (P_B - P_A) * (P_A > P_B) + P_B + 1)
End Function
