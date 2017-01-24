Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser

  Private Function Validate_ArgIndex(Token As Token, Results As Parameters) As Parameters

    Dim Digits = TryCast(Token, FSDv2.FormatString.Common.Digits)
    If Digits Is Nothing Then Results.Result.Issues += Issue.Unexpected.Token(Token.Span,Token) : Return Results
    Dim ValueOfArgIndex = Digits.GetValue
    Results.Arg = If(Results.Arg, New Arg())
    Results.Arg.Index = ValueOfArgIndex
    If Results.Arg.Index.HasValue = False Then
      Results.Result.Issues += Issue.Arg.Index.Missing(Token.Span)
    Else
      If Results.Arg.Index.Value >= Framework.UpperLimit Then
        Results.Result.Issues += Issue.Arg.Index.Framework.Lower_Limit_Exceeded(Token.Span)
      ElseIf Results.Arg.Index.Value >= Results.Args.Count Then
        Results.Result.Issues += Issue.Arg.Index.OutOfRange(Token.Span)
      Else
        Results.Args.MarkAsUsed(Results.Arg.Index.Value)
      End If
    End If
    Return Results
  End Function

  Private Function Expecting_Digits( ByRef Index As Integer, [End] As Integer, ArgIndex As Index, results As Parameters ) As Parameters
Expecting_Digits:
    If EndOfText(Index, [End]) Then results.Result.Issues += Issue.Unexpected.EoT(Nothing) : Return results
    If ArgIndex(Index).Kind = TokenKind.Digits Then
      results = Validate_ArgIndex(ArgIndex(Index), results)
      MoveToNext(Index)
      Return results
    End If
    ' An easy mistake to make is to have whitespaces after the opening brace.
    ' Eg: { 0}
    results = WasUnexpected(Index, ArgIndex(Index), results)
    GoTo Expecting_Digits
  End Function


  Private Function Expecting_Possible_Whitespaces(ByRef Index As Integer, [End] As Integer, ArgIndex As Index, Results As Parameters) As Parameters
    If Not EndOfText(Index, [End]) Then Index += If(ArgIndex(Index).Kind = TokenKind.Whitespaces, 1, 0)
    Return Results
  End Function

  Private Function ArgIndex( Src As Index, Results As Parameters ) As Analyser.Parameters
    '
    ' Arg.Index ::= Digits Whitespaces?
    '
    Dim Index = 0, [End] = Src.InnerTokens.Count
    Results = Expecting_Digits(Index, [End], Src, Results)
    Results = Expecting_Possible_Whitespaces(Index, [End], Src, Results)
    Results = AnyMoreTokensAreUnexpected(Index, [End], Src, Results)
    Return Results
  End Function

  Private Function AnyMoreTokensAreUnexpected(ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters) As Parameters
    While Not EndOfText(Index, [End])
      Results = WasUnexpected(Index, Token(Index), results)
    End While
    Return Results
  End Function

End Class
