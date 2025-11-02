Dim Shared As String LogFile: LogFile = ""
Dim Shared INFILE$, OUTFILE$
Dim Shared As _Unsigned Long BLOCK_SIZE: BLOCK_SIZE = 1048576
Dim Shared As _Unsigned _Byte RAW_MODE: RAW_MODE = 0
Select Case Command$(1)
    Case "-c": COMMAND = 1
    Case "-d": COMMAND = 2
End Select
For I = 2 To _CommandCount
    Select Case Command$(I)
        Case "-b": BLOCK_SIZE = Val(Command$(I + 1)): I = I + 1
            BLOCK_SIZE = _SHL(1, BLOCK_SIZE)
            Print "Block Size = "; PrintSize$(BLOCK_SIZE)
        Case "-r": RAW_MODE = 1
            Print "Raw Mode"
        Case "-nr": RAW_MODE = 0
            Print "Deflate Mode"
        Case Else
            INFILE$ = Command$(I)
            If _FileExists(INFILE$) = 0 Then INFILE$ = _StartDir$ + "\" + INFILE$
            If _FileExists(INFILE$) = 0 Then Print Command$(I); " does not exists.": _Continue
            Open INFILE$ For Binary As #1
            Y% = CsrLin + 1
            Select Case COMMAND
                Case 1
                    OUTFILE$ = INFILE$ + FILE_EXT$
                    Print "Compressing "; INFILE$; " -> "; OUTFILE$
                    Open OUTFILE$ For Output As #2: Close #2
                    Open OUTFILE$ For Binary As #2
                    Clear_Log
                    ST! = Timer(0.001): Do: LT! = Timer(0.001)
                        I$ = String$(Min(LOF(1) - Seek(1) + 1, BLOCK_SIZE), 0)
                        Get #1, , I$: Compress I$, O$
                        I$ = MKL$(Len(O$)) + O$: O$ = ""
                        Put #2, , I$
                        P = Round(100 * (Seek(1) - 1) / LOF(1))
                        Locate Y%: Print "[" + String$(P, 45) + Space$(100 - P) + "]"; P; "% "
                        Print " Ratio | Speed | Remaining Time | Elapsed Time | Processed | Compressed Size | Estimated Compressed Size"
                        T$ = _Trim$(Str$(Round(100 * LOF(2) / (Seek(1) - 1)))) + "%"
                        T$ = T$ + Space$(8 - Len(T$)) + PrintSize$((Seek(1) - 1) / (Timer(0.001) - ST!)) + "/s"
                        T$ = T$ + Space$(22 - Len(T$)) + PrintTime$((Timer(0.001) - ST!) * (LOF(1) / (Seek(1) - 1) - 1))
                        T$ = T$ + Space$(38 - Len(T$)) + PrintTime$(Timer(0.001) - ST!)
                        T$ = T$ + Space$(50 - Len(T$)) + PrintSize$(Seek(1) - 1)
                        T$ = T$ + Space$(65 - Len(T$)) + PrintSize$(LOF(2))
                        T$ = T$ + Space$(86 - Len(T$)) + PrintSize$(LOF(1) * LOF(2) / (Seek(1) - 1))
                        Print T$ + Space$(98 - Len(T$))
                        If LOF(1) < Seek(1) Then Exit Do
                    Loop
                    Put_Log
                    Locate Y%: Print "Ratio: "; Round(100 * LOF(2) / LOF(1)); "% => "; PrintSize$(LOF(1)); " -> "; PrintSize$(LOF(2)); Space$(80)
                    Locate Y% + 1: Print "Time: "; PrintTime$(Timer(0.001) - ST!); Space$(98)
                    Print Space$(100)
                    Locate Y% + 2, 1
                    Close
                Case 2
                    OUTFILE$ = INFILE$ + ".out"
                    If RAW_MODE Then OUTFILE$ = OUTFILE$ + "_raw"
                    Print "Decompressing "; INFILE$; " -> "; OUTFILE$
                    Open OUTFILE$ For Output As #2: Close #2
                    Open OUTFILE$ For Binary As #2
                    Clear_Log
                    ST! = Timer(0.001): Do: LT! = Timer(0.001)
                        I$ = String$(Min(LOF(1) - Seek(1) + 1, BLOCK_SIZE), 0)
                        Get #1, , L&
                        If EOF(1) Then Exit Do
                        I$ = String$(L&, 0)
                        Get #1, , I$
                        Decompress I$, O$
                        Put #2, , O$
                        P = Round(100 * (Seek(1) - 1) / LOF(1))
                        Locate Y%, 1: Print "[" + String$(P, 45) + Space$(100 - P) + "]"; P; "% "
                        Print " Speed | Elapsed Time"
                        T$ = Space$(2) + PrintTime$(Timer(0.001) - LT!)
                        T$ = T$ + Space$(12 - Len(T$)) + PrintTime$(Timer(0.001) - ST!)
                        T$ = T$ + Space$(22 - Len(T$))
                        Print T$
                    Loop
                    Put_Log
                    Locate Y%, 1: Print "Time: "; PrintTime$(Timer(0.001) - ST!); Space$(80): Print
                    Locate Y% + 1, 1
                    Close
            End Select
    End Select
Next I
System
Function Round (__N As Double)
    Round = Int(100 * __N) / 100
End Function
Function Remain~& (A~&, B~&)
    Remain~& = A~& \ B~& + Sgn(A~& Mod B~&)
End Function
'$Include:'printsize.bm'
'$Include:'printtime.bm'
'$Include:'min.bm'
Sub Clear_Log
    If _FileExists(OUTFILE$ + ".log.txt") Then Kill OUTFILE$ + ".log.txt"
End Sub
Sub Write_Log (T$)
    LogFile = LogFile + T$ + Chr$(10)
    If Len(LogFile) >= 65536 Then
        Put_Log
    End If
End Sub
Sub Put_Log
    If Len(LogFile) = 0 Then Exit Sub
    Open OUTFILE$ + ".log.txt" For Append As #100
    Print #100, LogFile;
    Close #100
    LogFile = ""
End Sub
