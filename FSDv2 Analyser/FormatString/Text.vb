Imports FSDv2

Partial Public Class Analyser

  Private Function Text(Txt As FormatString.Text, ByRef Results As Parameters) As Parameters
    Dim _Count = Txt.InnerTokens.Count
    If _Count = 0 Then Return Results
    Dim Index = 0
    While Index < _Count
      Dim Current = Txt(Index)
      Select Case Current.Kind
        Case TokenKind.ParseError         : Results = ParseError(DirectCast(Current, ParseError), Results)
                                            MoveToNext(Index)
        Case TokenKind.Esc_Brace_Closing,
             TokenKind.Esc_Brace_Opening  : MoveToNext(Index)
        Case Else                         : Results = WasUnexpected(Index, Current, Results)
      End Select
    End While
    Return Results
  End Function


End Class
