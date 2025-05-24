Function RecursivePrint$ (S$, N~&)
    If Len(S$) < 5 Then RecursivePrint = S$: Exit Function
    If N~& = 0 Then N~& = Pos(0) - 1
    Select Case Asc(S$)
        Case 1, 2, 5: RecursivePrint = RecursiveListStringPrint(S$, N~& + 1)
        Case 3: RecursivePrint = ListLongPrint(S$)
        Case 4: RecursivePrint = ListDoublePrint(S$)
        Case 6: RecursivePrint = "Tree"
        Case 7: RecursivePrint = "Hash Table"
        Case 8: RecursivePrint = RecursiveMapPrint(S$, N~& + 1)
        Case Else: RecursivePrint = Replace$(S$, Chr$(0), ".")
    End Select
End Function
Function RecursiveListStringPrint$ (LIST$, NEST~&)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 And Asc(LIST$) <> 2 And Asc(LIST$) <> 5 Then Exit Function
    Dim As _Unsigned Long I, O, T_OFFSET
    O = 6: T_OFFSET = 2
    T$ = "["
    For I = 1 To CVL(Mid$(LIST$, 2, 4)) - 1
        L = CVI(Mid$(LIST$, O, 2))
        If I > 1 Then T$ = T$ + Space$(NEST~&)
        T$ = T$ + RecursivePrint(Mid$(LIST$, O + 2, L), NEST~&) + "," + Chr$(10) ' + Chr$(10)
        O = O + L + 2
    Next I
    L = CVI(Mid$(LIST$, O, 2))
    T$ = T$ + Space$(NEST~&) + RecursivePrint(Mid$(LIST$, O + 2, L), NEST~&) + "]"
    RecursiveListStringPrint$ = T$
End Function
Function RecursiveMapPrint$ (__MAP$, NEST~&)
    If Len(__MAP$) < 5 Then Exit Function
    If Asc(__MAP$) <> 8 Then Exit Function
    Dim __LENGTH~&, __OFFSET~&, __LEN_KEY~&, __LEN_VALUE~&
    __LENGTH~& = CVL(Mid$(__MAP$, 2, 4))
    __OFFSET~& = 6
    __PRINT$ = ""
    For __I~& = 1 To __LENGTH~& 'check if exists
        __LEN_KEY~& = CVL(Mid$(__MAP$, __OFFSET~&, 4))
        __LEN_VALUE~& = CVL(Mid$(__MAP$, __OFFSET~& + 4, 4))
        __K$ = Mid$(__MAP$, __OFFSET~& + 8, __LEN_KEY~&)
        __V$ = RecursivePrint(Mid$(__MAP$, __OFFSET~& + 8 + __LEN_KEY~&, __LEN_VALUE~&), NEST~& + Len(__K$))
        __OFFSET~& = __OFFSET~& + 8 + __LEN_KEY~& + __LEN_VALUE~&
        If __I~& > 1 Then __PRINT$ = __PRINT$ + Space$(NEST~&)
        __PRINT$ = __PRINT$ + __K$ + ":" + __V$
        If __I~& < __LENGTH~& Then __PRINT$ = __PRINT$ + "," + Chr$(10)
    Next __I~&
    RecursiveMapPrint$ = "{" + __PRINT$ + "}"
    __PRINT$ = ""
End Function
