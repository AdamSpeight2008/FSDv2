Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function ArgHole_Expecting_OpeningBrace( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
Expecting_Opening_Brace:
    If EndOfText(Index, [End]) Then Return Results

    Dim curr = Token(Index)
    Select Case curr.Kind
      Case TokenKind.Brace_Opening : MoveToNext(Index)
                                     Return ArgHole_Expecting_ArgIndex(Index, [End], Token, Results)
      Case Else                    : Results = WasUnexpected(Index, Token, Results)
                                     GoTo Expecting_Opening_Brace
    End Select
    Return Results
  End Function

  Private Function ArgHole_Expecting_ArgIndex( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters

Expecting_Arghole_Index:
    If Index >= [End] Then Return Results

    Dim current = Token(Index)
    Select Case current.Kind

      Case TokenKind.ArgHole_Index  : Results = ArgIndex(DirectCast(current, Index), Results)
                                      MoveToNext(Index)
                                      Return ArgHole_Expecting_ArgAlign(Index, [End], Token, Results)

      Case TokenKind.ArgHole_Align  : Results.Result.Issues += Issue.Arg.Index.Missing(Token(Index).Span?.Start?.ToZeroSpan)
                                      Results = ArgHole_Expecting_ArgAlign(Index, [End], Token, Results)
                                      MoveToNext(Index)
                                      Return ArgHole_Expecting_ArgFormat(Index, [End], Token, Results)

      Case TokenKind.ArgHole_Format : Results.Result.Issues += Issue.Arg.Index.Missing(Token(Index).Span?.Start?.ToZeroSpan)
                                      Return ArgHole_Expecting_ArgFormat(Index, [End], Token, Results)

      Case TokenKind.Brace_Closing  : Results.Result.Issues += Issue.Arg.Index.Missing(Token(Index).Span?.Start?.ToZeroSpan)
                                      Return ArgHole_Expecting_ClosingBrace(Index, [End], Token, Results)

      Case TokenKind.ParseError     
        Dim TheParseError = DirectCast(current, ParseError)
        Select Case TheParseError.Why

          Case FSDv2.ParseError.Reason.EoT  :  Results.Result.Issues += Issue.Arg.Index.Missing(TheParseError.Span?.Start?.ToZeroSpan) +
                                                                        Issue.Missing.ClosingBrace(TheParseError.Span?.Start?.ToZeroSpan)
                                               MoveToNext(Index)
                                               Return ArgHole_Completed(Index, [End], Token, Results)

          Case FSDv2.ParseError.Reason.Partial
            Select Case TheParseError(0).Kind
              Case TokenKind.Brace_Closing : Results.Result.Issues += Issue.Arg.Index.Missing(TheParseError(0).Span?.Start?.ToZeroSpan)
                                             MoveToNext(Index)
                                             Return ArgHole_Expecting_ClosingBrace(Index, [End], Token, Results)
              Case Else                    : Results = ParseError(TheParseError, Results)
            End Select

          Case FSDv2.ParseError.Reason.NullParse
            If TheParseError.InnerTokens.Count > 0 Then
              Dim pe0 = TryCast(TheParseError(0), ParseError)
              If (pe0 IsNot Nothing) AndAlso (pe0.Why = FSDv2.ParseError.Reason.UnexpectedCharacter) Then
                Results.Result.Issues += Issue.Unexpected.Characters(pe0.Span)
              End If
            End If
            Results.Result.Issues += Issue.Arg.Index.Missing(TheParseError.Span?.Start?.ToZeroSpan)
            MoveToNext(Index)
            Return ArgHole_Expecting_ArgAlign(Index, [End], Token, Results)

          Case Else
            Results = ParseError(TheParseError, Results)
            MoveToNext(Index)
            GoTo Expecting_Arghole_Index 
        End Select

      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Arghole_Index
    End Select
    Return Results
  End Function

  Private Function ArgHole_Expecting_ArgAlign( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
Expection_Arghole_Align:
    If EndOfText(Index, [End]) Then Return Results

    Dim current = Token(Index)

    Select Case current.Kind
      Case TokenKind.ArgHole_Align : Results = ArgAlign(DirectCast(current, FormatString.ArgHole.Align), Results)
                                     MoveToNext(Index)
                                     Return ArgHole_Expecting_ArgFormat(Index, [End], Token, Results)

      Case TokenKind.Brace_Closing : Return ArgHole_Expecting_ClosingBrace(Index, [End], Token, Results)

      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expection_Arghole_Align

    End Select
    Return Results
  End Function

  Private Function ArgHole_Expecting_ArgFormat( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
Expecting_Arghole_Format:
    If EndOfText(Index, [End]) Then Return Results
    Dim current = Token(Index)
    Select Case current.Kind
      Case TokenKind.ArgHole_Format : Results = ArgFormat(DirectCast(current, FSDv2.FormatString.ArgHole.Format), Results)
                                      MoveToNext(Index)
                                      Return ArgHole_Expecting_ClosingBrace(Index, [End], Token, Results)
      Case TokenKind.Brace_Closing  : Return ArgHole_Expecting_ClosingBrace(Index, [End], Token, Results)
      Case Else
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Arghole_Format
    End Select
    Return Results
  End Function

  Private Function ArgHole_Expecting_ClosingBrace( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
Expecting_Closing_Brace:
    If EndOfText(Index, [End]) Then Return Results
    Dim current = Token(Index)
    Select Case current.Kind

      Case TokenKind.Brace_Closing : MoveToNext(Index)
                                     Return ArgHole_Completed(Index, [End], Token, Results)
      Case Else
        Dim TheParseError = TryCast(current, ParseError)
        If TheParseError?(0).Kind = TokenKind.Brace_Closing Then GoTo Expecting_Closing_Brace
        Results.Result.Issues += Issue.Unexpected.Token(current.Span, current)
        GoTo Expecting_Closing_Brace
    End Select
    Return Results
  End Function

  Private Function ArgHole_Completed( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
    While Not EndOfText(Index, [End])
      Dim current = Token(Index)
      Results = WasUnexpected(Index, Current, Results)
    End While
    Return Results
  End Function

  Private Function ArgHole( TheArgHole As FormatString.ArgHole, Results As Parameters ) As Parameters
    Dim Index = 0, [End] = TheArgHole.InnerTokens.Count
    Results = ArgHole_Expecting_OpeningBrace(Index, [End], TheArgHole, Results)
    Results = ArgHole_Completed(Index, [End], TheArgHole, Results)
    Return Results
  End Function

End Class
