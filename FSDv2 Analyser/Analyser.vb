Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Public Class Analyser

  Public Function Analyse(FS As FSDv2.Token, ByRef Results As Parameters) As Parameters

    Select Case FS.Kind

      Case TokenKind.ParseError
        Dim ThisParseError As ParseError = DirectCast(FS, ParseError)
        Select Case ThisParseError.Why
          Case FSDv2.ParseError.Reason.NullParse
          Case Else
            Results.Result.Issues += New Issue(Issue.Kinds.Unexpected_Token, ThisParseError.Span, $"Reason:= {ThisParseError.Why}")
        End Select

      Case TokenKind.FormatString
        Dim Index = 0, _Count = FS.InnerTokens.Count
        While Index < _Count
          Dim Token = FS(Index)
          Select Case Token.Kind
            Case TokenKind.Esc_Brace_Closing,
                 TokenKind.Esc_Brace_Opening  : MoveToNext(Index) 
            Case TokenKind.ArgHole            : Results = ArgHole(DirectCast(Token, FormatString.ArgHole), Results)
                                                MoveToNext(Index) 
            Case TokenKind.Text               : Results = Text(DirectCast(Token, FormatString.Text), Results)
                                                MoveToNext(Index) 
            Case TokenKind.ParseError         : Results = ParseError(DirectCast(Token, ParseError), Results)
                                                MoveToNext(Index) 
            Case Else                         : Results = WasUnexpected(Index, Token, Results )
          End Select
        End While
    End Select
    Return Results
  End Function

  <Runtime.CompilerServices.MethodImpl(Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>
  Private Shared Sub MoveToNext(Byref Index As Integer)
    Index += 1
  End Sub
End Class
