Function HashTableStringNew$ (HASH_TABLE_SIZE As _Unsigned Long)
    HTN$ = Chr$(7) + MKL$(HASH_TABLE_SIZE)
    HTNL$ = Chr$(1) + MKL$(HASH_TABLE_SIZE) + String$(7 * HASH_TABLE_SIZE, 0)
    LSN$ = MKI$(5) + ListStringNew$
    For I~& = 1 To HASH_TABLE_SIZE: Mid$(HTNL$, 7 * I~& - 1, 7) = LSN$: Next I~&
    HashTableStringNew$ = HTN$ + HTNL$
End Function
Sub HashTableStringInsert (HASHTABLE$, ITEM$)
    If Len(HASHTABLE$) < 5 Then Exit Sub
    If Asc(HASHTABLE$) <> 7 Then Exit Sub
    H~& = HashTableString_CalculateHash~&(ITEM$) Mod CVL(Mid$(HASHTABLE$, 2, 4))
    H$ = Mid$(HASHTABLE$, 6)
    L$ = ListStringGet$(H$, H~&)
    ListStringAdd L$, ITEM$
    ListStringEdit H$, L$, H~&
    HASHTABLE$ = Left$(HASHTABLE$, 5) + H$
End Sub
Function HashTableStringSearch~& (HASHTABLE$, ITEM$)
    If Len(HASHTABLE$) < 5 Then Exit Function
    If Asc(HASHTABLE$) <> 7 Then Exit Function
    H~& = HashTableString_CalculateHash~&(ITEM$) Mod CVL(Mid$(HASHTABLE$, 2, 4))
    HashTableStringSearch~& = ListStringSearch~&(ListStringGet$(Mid$(HASHTABLE$, 6), H~&), ITEM$)
End Function
Sub HashTableStringDelete (HASHTABLE$, ITEM$)
    If Len(HASHTABLE$) < 5 Then Exit Sub
    If Asc(HASHTABLE$) <> 7 Then Exit Sub
    H~& = HashTableString_CalculateHash~&(ITEM$) Mod CVL(Mid$(HASHTABLE$, 2, 4))
    H$ = Mid$(HASHTABLE$, 6)
    L$ = ListStringGet$(H$, H~&)
    S~& = ListStringSearch~&(L$, ITEM$)
    If S~& = 0 Then Exit Sub
    ListStringDelete L$, S~&
    ListStringEdit H$, L$, H~&
    HASHTABLE$ = Left$(HASHTABLE$, 5) + H$
End Sub
Function HashTableString_CalculateHash~& (ITEM$)
    T~% = 0
    For I~& = 1 To Len(ITEM$)
        T~% = T~% + Asc(ITEM$, I~&)
    Next I~&
    HashTableString_CalculateHash~& = 1 + T~%
End Function
Function HashTableLongNew$
    HTN$ = Chr$(9)
    HTNL$ = Chr$(1) + MKL$(256) + String$(7 * 256, 0)
    LLN$ = MKI$(5) + ListLongNew$
    For I~& = 1 To 256: Mid$(HTNL$, 7 * I~& - 1, 7) = LLN$: Next I~&
    HashTableLongNew$ = HTN$ + HTNL$
End Function
Sub HashTableLongInsert (HASHTABLE$, ITEM~&)
    If Len(HASHTABLE$) < 1286 Then Exit Sub
    If Asc(HASHTABLE$) <> 9 Then Exit Sub
    H~%% = _SHR(ITEM~&, 24) Xor _SHR(ITEM~&, 16) Xor _SHR(ITEM~&, 8) Xor ITEM~&
    H$ = Mid$(HASHTABLE$, 2)
    L$ = ListStringGet$(H$, 1 + H~%%)
    ListLongAdd L$, ITEM~&
    ListStringEdit H$, L$, 1 + H~%%
    HASHTABLE$ = Left$(HASHTABLE$, 1) + H$
End Sub
Function HashTableLongSearch~& (HASHTABLE$, ITEM~&)
    If Len(HASHTABLE$) < 1286 Then Exit Function
    If Asc(HASHTABLE$) <> 9 Then Exit Function
    H~%% = _SHR(ITEM~&, 24) Xor _SHR(ITEM~&, 16) Xor _SHR(ITEM~&, 8) Xor ITEM~&
    HashTableLongSearch~& = ListLongSearch~&(ListStringGet$(Mid$(HASHTABLE$, 2), 1 + H~%%), ITEM~&)
End Function
Sub HashTableLongDelete (HASHTABLE$, ITEM~&)
    If Len(HASHTABLE$) < 1286 Then Exit Sub
    If Asc(HASHTABLE$) <> 9 Then Exit Sub
    H~%% = _SHR(ITEM~&, 24) Xor _SHR(ITEM~&, 16) Xor _SHR(ITEM~&, 8) Xor ITEM~&
    H$ = Mid$(HASHTABLE$, 2)
    L$ = ListStringGet$(H$, 1 + H~%%)
    S~& = ListLongSearch~&(L$, ITEM~&)
    If S~& = 0 Then Exit Sub
    ListLongDelete L$, S~&
    ListStringEdit H$, L$, 1 + H~%%
    HASHTABLE$ = Left$(HASHTABLE$, 1) + H$
End Sub

