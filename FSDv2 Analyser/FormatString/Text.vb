Imports FSDv2

Partial Public Class Analyser

  Private Function Text(Txt As FormatString.Text, ByRef Q As Parameters) As Parameters
    If Txt.InnerTokens.Count = 0 Then Return Q
    Dim en = Txt.InnerTokens.GetEnumerator.GetEnumerator
    While en.MoveNext
      Select Case en.Current.Kind
        Case TokenKind.ParseError : Q = ParseError(DirectCast(en.Current, ParseError), Q)
        Case TokenKind.Esc_Brace_Closing,
             TokenKind.Esc_Brace_Opening
        Case Else
          Q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, en.Current.Span, "")

      End Select
    End While
    Return Q
  End Function


End Class
