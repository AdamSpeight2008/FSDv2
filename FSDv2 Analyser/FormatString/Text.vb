Imports FSDv2

Partial Public Class Analyser

  Private Function Text(Txt As FormatString.Text, ByRef Q As Parameters) As Parameters
    If Txt.InnerTokens.Count = 0 Then Return Q
    For i = 0 To Txt.InnerTokens.Count - 1
      Dim Current = Txt(i)
      Select Case Current.Kind
        Case TokenKind.ParseError : Q = ParseError(DirectCast(Current, ParseError), Q)
        Case TokenKind.Esc_Brace_Closing, TokenKind.Esc_Brace_Opening
        Case Else
          Q.Result.Issues += Issue.Unexpected.Token(Current.Span, Current)

      End Select
    Next
    Return Q
  End Function


End Class
