If _CommandCount = 0 Then System
Dim As _Unsigned Long BS
Dim ST!, LT!, I$, O$, Y%, INFILE$, OUTFILE$, PATHPREFIX$, MODE%%, L&
If Command$(1) = "-c" Or Command$(1) = "--compress" Then MODE%% = 1
If Command$(1) = "-d" Or Command$(1) = "--decompress" Then MODE%% = 2
INFILE$ = Command$(2)
If _FileExists(INFILE$) = 0 Then PATHPREFIX$ = _StartDir$ + "\"
INFILE$ = PATHPREFIX$ + INFILE$
If _FileExists(INFILE$) = 0 Then Print "File "; Command$(2); " does not exists!": System
Y% = CsrLin + 1
Dim Shared RAW_MODE As _Byte
If _StriCmp(Command$(3), "r") = 0 Or _StriCmp(Command$(4), "r") = 0 Then RAW_MODE = -1 Else RAW_MODE = 0
Select Case MODE%%
    Case 1: Print "Compressing": Open INFILE$ For Binary As #1
        OUTFILE$ = INFILE$ + FILE_EXT$
        If RAW_MODE Then OUTFILE$ = OUTFILE$ + "_raw"
        Open OUTFILE$ For Output As #2
        Close #2
        Open OUTFILE$ For Binary As #2
        ST! = Timer(0.001)
        BS = 2 ^ Val(Command$(3))
        If BS = 1 Then BS = 2 ^ 20
        Do
            LT! = Timer(0.001)
            If LOF(1) - Seek(1) + 1 >= BS Then I$ = Space$(BS) Else I$ = Space$(LOF(1) - Seek(1) + 1)
            Get #1, , I$
            O$ = Compress$(I$)
            I$ = MKL$(Len(O$)) + O$
            Put #2, , I$
            If LOF(1) <= Seek(1) - 1 Then Exit Do
            Locate Y%, 1: Print Round(100 * (Seek(1) - 1) / LOF(1)); "%", Round(100 * LOF(2) / (Seek(1) - 1)); "%", Round(Timer(0.001) - LT!); "s", Round((Timer(0.001) - LT!) / BS * (LOF(1) - Seek(1) + 1)); "s", Round(Timer(0.001) - ST!); "s"
        Loop
        Print "Ratio:"; Round(100 * LOF(2) / LOF(1)); "%"
        Print "Time: "; Timer(0.001) - ST!; "s"
        Close
    Case 2: Print "Decompressing": Open INFILE$ For Binary As #1
        OUTFILE$ = INFILE$ + ".out"
        Open OUTFILE$ For Output As #2
        Close #2
        Open OUTFILE$ For Binary As #2
        ST! = Timer(0.001)
        Do
            LT! = Timer(0.001)
            Get #1, , L&
            If EOF(1) = -1 Then Exit Do
            I$ = Space$(L&)
            Get #1, , I$
            O$ = Decompress$(I$)
            Put #2, , O$
            Locate Y%, 1: Print Round(100 * (Seek(1) - 1) / LOF(1)); "%", Round(Timer(0.001) - LT!); "s", Round(Timer(0.001) - ST!); "s"
        Loop
        Print "Time: "; Timer(0.001) - ST!; "s"
        Close
End Select
System
Function Round (__N As Double)
    Round = Int(100 * __N) / 100
End Function
Function Remain~& (A~&, B~&)
    Remain~& = A~& \ B~& + Sgn(A~& Mod B~&)
End Function
