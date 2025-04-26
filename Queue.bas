Function QueueNew$
    QueueNew$ = Chr$(5) + MKL$(0)
End Function
Sub QueueInsert (QUEUE$, ITEM$)
    If Len(QUEUE$) < 5 Then Exit Sub
    If Asc(QUEUE$) <> 5 Then Exit Sub
    QUEUE$ = Chr$(5) + MKL$(CVL(Mid$(QUEUE$, 2, 4)) + 1) + Mid$(QUEUE$, 6) + MKI$(Len(ITEM$)) + ITEM$
End Sub
Function QueuePeek$ (QUEUE$)
    If Len(QUEUE$) <= 5 Then Exit Function
    If Asc(QUEUE$) <> 5 Then Exit Function
    If CVL(Mid$(QUEUE$, 2, 4)) < 1 Then Exit Function
    QueuePeek$ = Mid$(QUEUE$, 8, CVI(Mid$(QUEUE$, 6, 2)))
End Function
Function QueueRemove$ (QUEUE$)
    If Len(QUEUE$) <= 5 Then Exit Function
    If Asc(QUEUE$) <> 5 Then Exit Function
    QueueRemove$ = Mid$(QUEUE$, 8, CVI(Mid$(QUEUE$, 6, 2)))
    QUEUE$ = Chr$(5) + MKL$(CVL(Mid$(QUEUE$, 2, 4)) - 1) + Mid$(QUEUE$, CVI(Mid$(QUEUE$, 6, 2)) + 8)
End Function
