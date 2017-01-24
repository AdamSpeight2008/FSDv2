Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function Expecting_Colon( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
Expecting_Colon:
    If EndOfText(Index, [End]) Then Return Results
    Dim Current = Token(Index)
    If Current.Kind = TokenKind.Colon Then Return Results
    Results = WasUnexpected(Index, Current, Results)
    GoTo Expecting_Colon
  End Function

  Private Shared Function WasUnexpected(byref Index As Integer,Token As Token, Results As Parameters) As Parameters
    Results.Result.Issues += Issue.Unexpected.Token(Token.Span, Token)
    MoveToNext(Index)
    Return Results
  End Function

   <Runtime.CompilerServices.MethodImpl(Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>
  Private Shared Function EndOfText(idx As Int32, edx As Int32) As Boolean
    Return (idx >= edx)
  End Function

  Private Function Validate_ArgFormatBody( ByRef Index As Integer, [End] As Integer, Token As Format, Results As Parameters ) As Parameters
    While Not EndOfText(Index, [End])
      Dim Current = Token(Index)
      Select Case Current.Kind
        Case TokenKind.Esc_Brace_Closing,
             TokenKind.Esc_Brace_Opening,
             TokenKind.Text          : MoveToNext(Index) ' These are valid within an ArgFormat
        Case TokenKind.Brace_Opening : Results.Result.Issues += Issue.Invalid(Current.Span, "Opening Brace is not allowed with the ArgFormat.")
                                       MoveToNext(Index)
        Case Else                    : Results = WasUnexpected(Index, Current, Results)
      End Select
    End While
    Return Results
  End Function


  Private Function ArgFormat( TheArgFormat As FormatString.ArgHole.Format, Results As Parameters ) As Parameters
    Dim Index = 0, [End] = TheArgFormat.InnerTokens.Count
    Results = Expecting_Colon(Index, [End], TheArgFormat, Results)
    Results = Validate_ArgFormatBody(Index + 1, [End], TheArgFormat, Results)
    Return Results
  End Function

End Class
