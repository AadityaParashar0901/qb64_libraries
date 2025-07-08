$Console:Only
L$ = GetNestedFileList$(_StartDir$)
Print ListStringPrint(L$)
ListStringToClipboard L$
Function GetNestedFileList$ (DIR$)
    Static As _Unsigned Integer TEMP
    Dim As _Unsigned Integer CurrentFileHandle
    L$ = ListStringNew$
    GetNestedFileList$ = L$

    ListStringAdd L$, DIR$ ' here the directory name is added to the list

    GetNestedFileList$ = L$
    If _FileExists(DIR$) Then Exit Function
    TEMP = TEMP + 1
    CurrentFileHandle = TEMP
    __FILE$ = "tmp" + _Trim$(Str$(CurrentFileHandle)) + ".txt"
    Shell "dir " + Chr$(34) + DIR$ + Chr$(34) + " /b /o:n > " + __FILE$
    Open __FILE$ For Input As #CurrentFileHandle
    Do Until EOF(CurrentFileHandle)
        Line Input #CurrentFileHandle, F$
        If _DirExists(DIR$ + "\" + F$) Then 'directory
            L$ = ListStringAppend(L$, GetNestedFileList$(DIR$ + "\" + F$))
        Else 'file

            ' here you can add with filters, like for *.jpg:
            'If Right$(DIR$ + "\" + F$, 4) = ".jpg" Then
            ListStringAdd L$, DIR$ + "\" + F$

        End If
    Loop
    Close #CurrentFileHandle
    Kill __FILE$
    GetNestedFileList$ = L$
    L$ = ""
End Function
Function ListStringNew$
    ListStringNew$ = Chr$(1) + MKL$(0)
End Function
Function ListStringPrint$ (LIST$)
    If Len(LIST$) < 5 Then Exit Function
    If Asc(LIST$) <> 1 Then Exit Function
    Dim As _Unsigned Long O, I, L
    O = 6
    T$ = String$(Len(LIST$) - 4, 0)
    Asc(T$) = 91 '[
    For I = 1 To CVL(Mid$(LIST$, 2, 4)) - 1
        L = CVI(Mid$(LIST$, O, 2))
        Mid$(T$, O - 4, L + 2) = Mid$(LIST$, O + 2, L) + "," + Chr$(10)
        O = O + L + 2
    Next I
    L = CVI(Mid$(LIST$, O, 2))
    Mid$(T$, O - 4, L + 1) = Mid$(LIST$, O + 2, L) + "]"
    ListStringPrint$ = Left$(T$, O + L - 4)
End Function
Sub ListStringToClipboard (LIST$)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    Dim As _Unsigned Long O, I, L
    O = 6
    T$ = String$(Len(LIST$) - 4, 0)
    For I = 1 To CVL(Mid$(LIST$, 2, 4)) - 1
        L = CVI(Mid$(LIST$, O, 2))
        Mid$(T$, O - 5, L + 2) = Mid$(LIST$, O + 2, L) + "," + Chr$(10)
        O = O + L + 2
    Next I
    L = CVI(Mid$(LIST$, O, 2))
    Mid$(T$, O - 5, L) = Mid$(LIST$, O + 2, L)
    _Clipboard$ = Left$(T$, O + L - 5)
End Sub
Sub ListStringAdd (LIST$, ITEM$)
    If Len(LIST$) < 5 Then Exit Sub
    If Asc(LIST$) <> 1 Then Exit Sub
    LIST$ = Chr$(1) + MKL$(CVL(Mid$(LIST$, 2, 4)) + 1) + Mid$(LIST$, 6) + MKI$(Len(ITEM$)) + ITEM$
End Sub
Function ListStringAppend$ (LIST1$, LIST2$)
    If Len(LIST1$) < 5 Then Exit Function
    If Len(LIST2$) < 5 Then Exit Function
    If Asc(LIST1$) <> 1 Then Exit Function
    If Asc(LIST2$) <> 1 Then Exit Function
    ListStringAppend$ = Chr$(1) + MKL$(CVL(Mid$(LIST1$, 2, 4)) + CVL(Mid$(LIST2$, 2, 4))) + Mid$(LIST1$, 6) + Mid$(LIST2$, 6)
End Function
