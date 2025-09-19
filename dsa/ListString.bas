Function ListStringNew$
    ListStringNew$ = Chr$(1) + MKL$(0)
End Function
Function ListStringFromString$ (ARRAY$)
    LIST$ = Chr$(1) + MKL$(0)
    __LISTLENGTH~& = 0
    For I~& = 1 To Len(ARRAY$)
        BYTE~%% = Asc(ARRAY$, I~&)
        Select Case BYTE~%%
            Case 91: NEST~& = NEST~& + 1
                V$ = V$ + Chr$(BYTE~%%)
            Case 93: NEST~& = NEST~& - 1
                V$ = V$ + Chr$(BYTE~%%)
            Case 44: If NEST~& = 0 Then
                    __LISTLENGTH~& = __LISTLENGTH~& + 1
                    LIST$ = LIST$ + MKL$(Len(V$)) + V$
                    V$ = ""
                Else
                    V$ = V$ + Chr$(BYTE~%%)
                End If
            Case Else:
                V$ = V$ + Chr$(BYTE~%%)
        End Select
    Next I~&
    If Len(V$) Then
        LIST$ = LIST$ + MKL$(Len(V$)) + V$
        __LISTLENGTH~& = __LISTLENGTH~& + 1
        V$ = ""
    End If
    Mid$(LIST$, 2, 4) = MKL$(__LISTLENGTH~&)
    ListStringFromString$ = LIST$
    LIST$ = ""
End Function
Function ListStringFromArray$ (ARRAY() As String, START_INDEX~&, END_INDEX~&)
    K~& = 0
    For I~& = START_INDEX~& To END_INDEX~&
        K~& = K~& + 4 + Len(ARRAY(I~&))
    Next I~&
    LIST$ = Chr$(1) + MKL$(END_INDEX~& - START_INDEX~& + 1) + String$(K~&, 0)
    K~& = 6
    L~% = 0
    For I~& = START_INDEX~& To END_INDEX~&
        L~% = Len(ARRAY(I~&))
        Mid$(LIST$, K~&, 4 + L~%) = MKL$(L~%) + ARRAY(I~&)
        K~& = K~& + L~% + 4
    Next I~&
    ListStringFromArray$ = LIST$
    LIST$ = ""
End Function
Function ListStringPrint$ (LIST$)
    Dim As Long O, T_OFFSET, I, L
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    If CVL(Mid$(LIST$, 2, 4)) = 0 Then ListStringPrint$ = "[]": Exit Function
    O = 6: T_OFFSET = 2
    T$ = String$(Len(LIST$) - 4, 0)
    Asc(T$) = 91 '[
    For I = 1 To CVL(Mid$(LIST$, 2, 4)) - 1
        L = CVL(Mid$(LIST$, O, 4))
        Mid$(T$, T_OFFSET, L + 1) = Mid$(LIST$, O + 4, L) + ","
        T_OFFSET = T_OFFSET + L + 1
        O = O + L + 4
    Next I
    L = CVL(Mid$(LIST$, O, 4))
    Mid$(T$, T_OFFSET, L + 1) = Mid$(LIST$, O + 4, L) + "]"
    ListStringPrint$ = Left$(T$, T_OFFSET + L)
End Function
Function ListStringLength~& (LIST$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    ListStringLength~& = CVL(Mid$(LIST$, 2, 4))
End Function
Sub ListStringAdd (LIST$, ITEM$)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    LIST$ = Chr$(1) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6) + MKL$(Len(ITEM$)) + ITEM$
End Sub
Function ListStringGet$ (LIST$, POSITION As _Unsigned Long)
    Dim As Long O, I, L
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Function
    O = 6
    For I = 1 To POSITION - 1
        L = CVL(Mid$(LIST$, O, 4))
        O = O + L + 4
    Next I
    ListStringGet$ = Mid$(LIST$, O + 4, CVL(Mid$(LIST$, O, 4)))
End Function
Sub ListStringInsert (LIST$, ITEM$, POSITION As _Unsigned Long)
    Dim As Long O, I, L
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    O = 6
    For I = 1 To POSITION - 1
        L = CVL(Mid$(LIST$, O, 4))
        O = O + L + 4
    Next I
    LIST$ = Chr$(1) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6, O - 6) + MKL$(Len(ITEM$)) + ITEM$ + Mid$(LIST$, O)
End Sub
Sub ListStringDelete (LIST$, POSITION As _Unsigned Long)
    Dim As Long O, I, L
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    O = 6
    For I = 1 To POSITION - 1
        L = CVL(Mid$(LIST$, O, 4))
        O = O + L + 4
    Next I
    LIST$ = Chr$(1) + MKL$(CVL(Mid$(LIST$, 2, 4)) - 1) + Mid$(LIST$, 6, O - 6) + Mid$(LIST$, O + CVL(Mid$(LIST$, O, 4)) + 4)
End Sub
Function ListStringSearch~& (LIST$, ITEM$)
    Dim As Long O, I, L
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    O = 6
    For I = 1 To CVL(Mid$(LIST$, 2, 4))
        L = CVL(Mid$(LIST$, O, 4))
        If ITEM$ = Mid$(LIST$, O + 4, L) Then ListStringSearch~& = I: Exit Function
        O = O + L + 4
    Next I
    ListStringSearch~& = 0
End Function
Function ListStringSearchI~& (LIST$, ITEM$)
    Dim As Long O, I, L
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    O = 6
    For I = 1 To CVL(Mid$(LIST$, 2, 4))
        L = CVL(Mid$(LIST$, O, 4))
        If _StriCmp(ITEM$, Mid$(LIST$, O + 4, L)) = 0 Then ListStringSearchI~& = I: Exit Function
        O = O + L + 4
    Next I
    ListStringSearchI~& = 0
End Function
Sub ListStringEdit (LIST$, ITEM$, POSITION As _Unsigned Long)
    Dim As Long O, I, L
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    If CVL(Mid$(LIST$, 2, 4)) < POSITION - 1 Then Exit Sub
    O = 6
    For I = 1 To POSITION - 1
        L = CVL(Mid$(LIST$, O, 4))
        O = O + L + 4
    Next I
    LIST$ = Left$(LIST$, O - 1) + MKL$(Len(ITEM$)) + ITEM$ + Mid$(LIST$, O + CVL(Mid$(LIST$, O, 4)) + 4)
End Sub
Function ListStringAppend$ (LIST1$, LIST2$)
    If Len(LIST1$) < 5 Then Exit Function
    If Len(LIST2$) < 5 Then Exit Function
    If Asc(LIST1$) <> 1 Then Exit Function
    If Asc(LIST2$) <> 1 Then Exit Function
    ListStringAppend$ = Chr$(1) + MKL$(CVL(Mid$(LIST1$, 2, 4)) + CVL(Mid$(LIST2$, 2, 4))) + Mid$(LIST1$, 6) + Mid$(LIST2$, 6)
End Function
