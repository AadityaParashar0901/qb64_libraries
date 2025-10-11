Function GetValue~&& (S$) Static
    Select Case Asc(S$, Len(S$))
        Case 72, 104: GetValue~&& = Val("&H" + Left$(S$, Len(S$) - 1))
        Case 66, 98: GetValue~&& = Val("&B" + Left$(S$, Len(S$) - 1))
        Case Else: GetValue~&& = Val(S$)
    End Select
End Function
