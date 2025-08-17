Function IndexedListNew$
    IndexedListNew$ = Chr$(9) + MKL$(0)
End Function
Function IndexedListLength~& (__L$)
    If Len(__L$) < 5 Then Exit Function
    If Asc(__L$) <> 9 Then Exit Function
    IndexedListLength~& = CVL(Mid$(__L$, 2, 4))
End Function
Sub IndexedListAdd (__L$, __ITEM$)
    If Len(__L$) < 5 Then Exit Sub
    If Asc(__L$) <> 9 Then Exit Sub
    __L~& = CVL(Mid$(__L$, 2, 4))
    __Hashes$ = Mid$(__L$, 6, _SHL(__L~&, 3))
    __NewHash~& = IndexedListHash~&(__ITEM$)
    If __L~& = 1 Then
        If CVL(Left$(__Hashes$, 4)) > __NewHash~& Then __P~& = 0 Else __P~& = 1
    Else
        __P~& = _SHR(__L~&, 1)
        __S~& = _SHR(__L~&, 2)
        If __S~& = 0 Then __S~& = 1
        While 1 >= __P~& And __P~& < __L~&
            __CurrentHash~& = CVL(Mid$(__Hashes$, _SHL(__P~&, 3) + 1, 4))
            If __CurrentHash~& = __NewHash~& Then Exit While
            If __CurrentHash~& > __NewHash~& Then
                If __P~& > 0 Then If CVL(Mid$(__Hashes$, _SHL(__P~& - 1, 3) + 1, 4)) < __NewHash~& Then Exit While
                __P~& = __P~& - __S~&
            Else
                If __P~& < __L~& - 1 Then
                    If CVL(Mid$(__Hashes$, _SHL(__P~& + 1, 3) + 1, 4)) > __NewHash~& Then __P~& = __P~& + 1: Exit While
                End If
                __P~& = __P~& + __S~&
            End If
            If __S~& > 1 Then __S~& = _SHR(__S~&, 1)
        Wend
    End If
    __Hashes$ = Mid$(__Hashes$, 1, _SHL(__P~&, 3)) + MKL$(__NewHash~&) + MKL$(Len(__L$) - _SHL(__L~&, 3) - 4) + Mid$(__Hashes$, 1 + _SHL(__P~&, 3))
    __L$ = Chr$(9) + MKL$(__L~& + 1) + __Hashes$ + Mid$(__L$, 6 + _SHL(__L~&, 3)) + MKL$(Len(__ITEM$)) + __ITEM$
End Sub
Function IndexedListGet$ (__L$, __POSITION~&)
    If Len(__L$) < 5 Then Exit Function
    If Asc(__L$) <> 9 Then Exit Function
    __L~& = CVL(Mid$(__L$, 2, 4))
    __Hashes$ = Mid$(__L$, 6, _SHL(__L~&, 3))
    If __POSITION~& > 0 And __POSITION~& < __L~& Then
        __P~& = CVL(Mid$(__Hashes$, 5 + _SHL(__POSITION~& - 1, 3), 4))
        __ItemLength~& = CVL(Mid$(__L$, 5 + _SHL(__L~&, 3) + __P~&, 4))
        IndexedListGet$ = Mid$(__L$, 9 + _SHL(__L~&, 3) + __P~&, __ItemLength~&)
        Exit Function
    ElseIf __POSITION~& = __L~& Then
        __P~& = CVL(Right$(__Hashes$, 4))
        IndexedListGet$ = Mid$(__L$, 9 + _SHL(__L~&, 3) + __P~&, CVL(Mid$(__L$, 5 + _SHL(__L~&, 3) + __P~&, 4)))
        Exit Function
    End If
    IndexedListGet$ = ""
End Function
Function IndexedListLinearSearch~& (__L$, __ITEM$)
    If Len(__L$) < 5 Then Exit Function
    If Asc(__L$) <> 9 Then Exit Function
    __L~& = CVL(Mid$(__L$, 2, 4))
    __Hashes$ = Mid$(__L$, 6, _SHL(__L~&, 3))
    __NewHash~& = IndexedListHash~&(__ITEM$)
    For __I~& = 1 To __L~&
        If CVL(Mid$(__Hashes$, _SHL(__I~& - 1, 3) + 1, 4)) = __NewHash~& Then IndexedListLinearSearch~& = __I~&: Exit Function
    Next __I~&
    IndexedListLinearSearch~& = 0
End Function
Function IndexedListBinarySearch~& (__L$, __ITEM$)
    If Len(__L$) < 5 Then Exit Function
    If Asc(__L$) <> 9 Then Exit Function
    __L~& = CVL(Mid$(__L$, 2, 4))
    __Hashes$ = Mid$(__L$, 6, _SHL(__L~&, 3))
    __NewHash~& = IndexedListHash~&(__ITEM$)
    __Low~& = 0
    __High~& = __L~& - 1
    While __Low~& <= __High~&
        __P~& = __Low~& + _SHR(__High~& - __Low~&, 1)
        __CurrentHash~& = CVL(Mid$(__Hashes$, _SHL(__P~&, 3) + 1, 4))
        If __CurrentHash~& = __NewHash~& Then IndexedListBinarySearch~& = __P~& + 1: Exit Function
        If __CurrentHash~& > __NewHash~& Then __High~& = __P~& - 1 Else __Low~& = __P~& + 1
    Wend
    IndexedListBinarySearch~& = 0
End Function
Function IndexedListHash~& (__I$) 'CRC
    Dim As _Unsigned Long __P, __C, __I
    Dim As _Unsigned _Byte __J
    __P = &HEDB88320
    __C = -1
    For __I = 1 To Len(__I$)
        __C = __C Xor Asc(__I$, __I)
        For __J = 1 To 8
            If __C And 1 Then __C = _SHR(__C, 1) Xor __P Else __C = _SHR(__C, 1)
        Next __J
    Next __I
    IndexedListHash~& = Not __C
End Function
