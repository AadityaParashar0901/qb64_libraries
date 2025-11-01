Sub ReadFileToListString (File$, OutputList$)
    Dim As _Unsigned Long I, OldI
    __F = FreeFile
    Open File$ For Binary As #__F
    F$ = String$(LOF(__F), 0)
    Get #__F, , F$
    Close #__F
    OldI = 1
    I = InStr(1, F$, FILE_SEPERATOR)
    OutputList$ = ListStringNew
    While I
        ListStringAdd OutputList$, Mid$(F$, OldI, I - OldI)
        OldI = I + Len(FILE_SEPERATOR)
        I = InStr(I + 1, F$, FILE_SEPERATOR)
    Wend
    ListStringAdd OutputList$, Mid$(F$, OldI)
End Sub
