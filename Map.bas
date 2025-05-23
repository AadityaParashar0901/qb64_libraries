Function MapNew$
    MapNew$ = Chr$(8) + MKL$(0)
End Function
Sub MapSetKey (__MAP$, __KEY$, __VALUE$)
    If Len(__MAP$) < 5 Then Exit Sub
    If Asc(__MAP$, 1) <> 8 Then Exit Sub
    Dim __LENGTH~&, __OFFSET~&
    __LENGTH~& = CVL(Mid$(__MAP$, 2, 4))
    __OFFSET~& = 6
    Dim __LEN_KEY~&, __LEN_VALUE~&
    For __I~& = 1 To __LENGTH~& 'check if exists
        __LEN_KEY~& = CVL(Mid$(__MAP$, __OFFSET~&, 4))
        __LEN_VALUE~& = CVL(Mid$(__MAP$, __OFFSET~& + 4, 4))
        __K$ = Mid$(__MAP$, __OFFSET~& + 8, __LEN_KEY~&)
        If __KEY$ = __K$ Then __BOOL` = -1: Exit For
        __V$ = Mid$(__MAP$, __OFFSET~& + 8 + __LEN_KEY~&, __LEN_VALUE~&)
        __OFFSET~& = __OFFSET~& + 8 + __LEN_KEY~& + __LEN_VALUE~&
    Next __I~&
    If __BOOL` Then
        __MAP$ = Left$(__MAP$, __OFFSET~& - 1) + MKL$(Len(__KEY$)) + MKL$(Len(__VALUE$)) + __KEY$ + __VALUE$ + Mid$(__MAP$, __OFFSET~& + 8 + __LEN_KEY~& + __LEN_VALUE~&)
    Else
        __MAP$ = Chr$(8) + MKL$(__LENGTH~& + 1) + Mid$(__MAP$, 6, __OFFSET~& - 6) + MKL$(Len(__KEY$)) + MKL$(Len(__VALUE$)) + __KEY$ + __VALUE$
    End If
End Sub
Function MapPrint$ (__MAP$)
    If Len(__MAP$) < 5 Then Exit Function
    If Asc(__MAP$, 1) <> 8 Then Exit Function
    Dim __LENGTH~&, __OFFSET~&
    __LENGTH~& = CVL(Mid$(__MAP$, 2, 4))
    __OFFSET~& = 6
    Dim __LEN_KEY~&, __LEN_VALUE~&
    __PRINT$ = ""
    For __I~& = 1 To __LENGTH~& 'check if exists
        __LEN_KEY~& = CVL(Mid$(__MAP$, __OFFSET~&, 4))
        __LEN_VALUE~& = CVL(Mid$(__MAP$, __OFFSET~& + 4, 4))
        __K$ = Mid$(__MAP$, __OFFSET~& + 8, __LEN_KEY~&)
        If __KEY$ = __K$ Then Exit Function
        __V$ = Mid$(__MAP$, __OFFSET~& + 8 + __LEN_KEY~&, __LEN_VALUE~&)
        __OFFSET~& = __OFFSET~& + 8 + __LEN_KEY~& + __LEN_VALUE~&
        __PRINT$ = __PRINT$ + __K$ + ":" + __V$
        If __I~& < __LENGTH~& Then __PRINT$ = __PRINT$ + ","
    Next __I~&
    MapPrint$ = "{" + __PRINT$ + "}"
End Function
Function MapGetKey$ (__MAP$, __KEY$)
    If Len(__MAP$) < 5 Then Exit Function
    If Asc(__MAP$, 1) <> 8 Then Exit Function
    Dim __LENGTH~&, __OFFSET~&
    __LENGTH~& = CVL(Mid$(__MAP$, 2, 4))
    __OFFSET~& = 6
    Dim __LEN_KEY~&, __LEN_VALUE~&
    For __I~& = 1 To __LENGTH~& 'check if exists
        __LEN_KEY~& = CVL(Mid$(__MAP$, __OFFSET~&, 4))
        __LEN_VALUE~& = CVL(Mid$(__MAP$, __OFFSET~& + 4, 4))
        __K$ = Mid$(__MAP$, __OFFSET~& + 8, __LEN_KEY~&)
        __V$ = Mid$(__MAP$, __OFFSET~& + 8 + __LEN_KEY~&, __LEN_VALUE~&)
        If __KEY$ = __K$ Then MapGetKey$ = __V$: Exit Function
        __OFFSET~& = __OFFSET~& + 8 + __LEN_KEY~& + __LEN_VALUE~&
    Next __I~&
End Function
Sub MapDeleteKey (__MAP$, __KEY$)
    If Len(__MAP$) < 5 Then Exit Sub
    If Asc(__MAP$, 1) <> 8 Then Exit Sub
    Dim __LENGTH~&, __OFFSET~&
    __LENGTH~& = CVL(Mid$(__MAP$, 2, 4))
    __OFFSET~& = 6
    Dim __LEN_KEY~&, __LEN_VALUE~&
    For __I~& = 1 To __LENGTH~& 'check if exists
        __LEN_KEY~& = CVL(Mid$(__MAP$, __OFFSET~&, 4))
        __LEN_VALUE~& = CVL(Mid$(__MAP$, __OFFSET~& + 4, 4))
        __K$ = Mid$(__MAP$, __OFFSET~& + 8, __LEN_KEY~&)
        If __KEY$ = __K$ Then __BOOL` = -1: Exit For
        __V$ = Mid$(__MAP$, __OFFSET~& + 8 + __LEN_KEY~&, __LEN_VALUE~&)
        __OFFSET~& = __OFFSET~& + 8 + __LEN_KEY~& + __LEN_VALUE~&
    Next __I~&
    If __BOOL` Then __MAP$ = Chr$(8) + MKL$(__LENGTH~& - 1) + Mid$(__MAP$, 6, __OFFSET~& - 6) + Mid$(__MAP$, __OFFSET~& + 8 + __LEN_KEY~& + __LEN_VALUE~&)
End Sub
