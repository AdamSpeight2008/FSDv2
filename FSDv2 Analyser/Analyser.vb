Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Public Class Analyser

  Public Function Analyse(FS As FSDv2.Token, ByRef q As Parameters) As Parameters
    Select Case FS.Kind
      Case TokenKind.ParseError
        Dim pe As ParseError = DirectCast(FS, ParseError)
        Select Case pe.Why
          Case FSDv2.ParseError.Reason.NullParse
          Case Else
            q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, pe.Span, $"Reason:= {pe.Why}")
        End Select
      Case TokenKind.FormatString
        For Each t As Token In FS.InnerTokens.GetEnumerator
          Select Case t.Kind
            Case TokenKind.ArgHole : q = ArgHole(DirectCast(t, FormatString.ArgHole), q)
            Case TokenKind.Text : q = Text(DirectCast(t, FormatString.Text), q)
            Case TokenKind.ParseError : q = ParseError(DirectCast(t, ParseError), q)
            Case TokenKind.Esc_Brace_Closing, TokenKind.Esc_Brace_Opening
            Case Else
              q.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, t.Span, "")
          End Select
        Next
    End Select
    Return q
  End Function

End Class
