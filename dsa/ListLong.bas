Function ListLongNew$
    ListLongNew$ = Chr$(3) + MKL$(0)
End Function
Function ListLongFromArray$ (ARRAY() As Long, START_INDEX~&, END_INDEX~&)
    LIST$ = Chr$(3) + MKL$(END_INDEX~& - START_INDEX~& + 1) + String$((END_INDEX~& - START_INDEX~& + 1) * 4, 0)
    K~& = 0
    For I~& = START_INDEX~& To END_INDEX~&
        K~& = K~& + 1
        Mid$(LIST$, 4 * K~& + 2, 4) = MKL$(ARRAY(I~&))
    Next I~&
    ListLongFromArray$ = LIST$
    LIST$ = ""
End Function
Function ListLongPrint$ (LIST$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 3 Then Exit Function
    Dim As _Unsigned Long O, T_OFFSET, I, L
    O = 6: T_OFFSET = 2
    T$ = String$(CVL(Mid$(LIST$, 2, 4)) * 12 + 2, 0)
    Asc(T$) = 91 '[
    For I = 1 To CVL(Mid$(LIST$, 2, 4)) - 1
        N$ = _Trim$(Str$(CVL(Mid$(LIST$, 2 + I * 4, 4))))
        L = Len(N$)
        Mid$(T$, T_OFFSET, L + 1) = N$ + ","
        T_OFFSET = T_OFFSET + L + 1
    Next I
    N$ = _Trim$(Str$(CVL(Mid$(LIST$, 2 + I * 4, 4))))
    L = Len(N$)
    Mid$(T$, T_OFFSET, L + 1) = N$ + "]"
    ListLongPrint$ = Left$(T$, T_OFFSET + L)
End Function
Function ListLongLength~& (LIST$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 3 Then Exit Function
    ListLongLength~& = CVL(Mid$(LIST$, 2, 4))
End Function
Sub ListLongAdd (LIST$, ITEM&)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 3 Then Exit Sub
    LIST$ = Chr$(3) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6) + MKL$(ITEM&)
End Sub
Function ListLongGet& (LIST$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 3 Then Exit Function
    If CVL(Mid$(LIST$, 2, 4)) < POSITION Then Exit Function
    ListLongGet& = CVL(Mid$(LIST$, 2 + 4 * POSITION, 4))
End Function
Sub ListLongInsert (LIST$, ITEM&, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 3 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION Then Exit Sub
    LIST$ = Chr$(3) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6, POSITION * 4 - 2) + MKL$(ITEM&) + Mid$(LIST$, 2 + POSITION * 4)
End Sub
Sub ListLongDelete (LIST$, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 3 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION Then Exit Sub
    LIST$ = Chr$(3) + MKL$(CVL(Mid$(LIST$, 2, 4)) - 1) + Mid$(LIST$, 6, POSITION * 4 - 2) + Mid$(LIST$, 2 + POSITION * 4)
End Sub
Function ListLongSearch~& (LIST$, ITEM&)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 3 Then Exit Function
    I~& = InStr(6, LIST$, MKL$(ITEM&))
    If ((I~& - 2) And 3) = 0 Then ListLongSearch~& = _SHR(I~& - 2, 2) Else ListLongSearch~& = 0
End Function
Sub ListLongEdit (LIST$, ITEM&, POSITION As _Unsigned Long)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 3 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION Then Exit Sub
    LIST$ = Left$(LIST$, 1 + POSITION * 4) + MKL$(ITEM&) + Mid$(LIST$, 6 + POSITION * 4)
End Sub
