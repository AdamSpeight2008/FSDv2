Imports FSDv2
Imports FSDv2.FormatString.ArgHole
Imports System.Numerics

Partial Public Class Analyser
  Private Function Expecting_Possible_Whitespaces( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
Expecting_Possible_Whitespaces:
    If EndOfText(Index, [End]) Then Return Results
    Dim current As Token = Token(Index)
    If current.Kind = TokenKind.Whitespaces Then Return Results
    Results = WasUnexpected(Index, Current, Results)
    GoTo Expecting_Possible_Whitespaces
  End Function

  Private Function Expecting_Comma( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
Expecting_Comma:
    If EndOfText(Index, [End]) Then Return Results
    Dim current = Token(Index)
    If current.Kind = TokenKind.Comma Then  MoveToNext(Index) : Return Results
    Results = WasUnexpected(Index, Current, Results)
    GoTo Expecting_Comma
  End Function

  Private Function ArgAlign_Head( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
    If EndOfText(Index, [End]) Then Return Results

    Dim idx0 = 0, edx0 = Token.InnerTokens.Count
    Results = Expecting_Comma(idx0, edx0, Token, Results)
    Results = Expecting_Possible_Whitespaces(idx0, edx0, Token, Results)
    Results = AnyMoreTokensAreUnexpected(idx0, edx0, Token, Results)
    Return Results
  End Function

  Private Function ArgAlign_Expecting_PossibleMinusSign( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
    If EndOfText(Index, [End]) Then Return Results
    Dim tkn = TryCast(Token, FSDv2.FormatString.ArgHole.Align.Body)
    Debug.Assert(tkn IsNot Nothing)
    Dim idx0 = 0, edx0 = tkn.InnerTokens.Count

Expecting_Possible_MinusSign:
    If EndOfText(idx0, edx0) Then Return AnyMoreTokensAreUnexpected(Index, [End], Token, Results)

    Dim Current = tkn(idx0)
    Select Case Current.Kind
      Case TokenKind.MinusSign : MoveToNext(Idx0) 
                                 Return ArgAlign_Expecting_Digits(idx0, edx0, Current, Results)
      Case TokenKind.Digits    : Return ArgAlign_Expecting_Digits(idx0, edx0, Current, Results)
      Case Else                : Results = WasUnexpected(idx0, Current, Results)
                                 GoTo Expecting_Possible_MinusSign
    End Select
  End Function

  Private Function Validate_ArgAlign(Token As Token, results As Parameters) As Parameters

    Dim Digits = TryCast(Token, FSDv2.FormatString.Common.Digits)
    If Digits Is Nothing Then results.Result.Issues += Issue.Unexpected.Token(Token.Span,Token) : Return results
    Dim ArgAlignValue = Digits.GetValue
    results.Arg = If(results.Arg, New Arg)
    results.Arg.Align = ArgAlignValue
    If results.Arg.Align.HasValue = False Then
        results.Result.Issues += Issue.Arg.Align.Missing(Token.Span)
    Else
      If results.Arg.Align.Value >= Framework.UpperLimit Then results.Result.Issues += Issue.Arg.Align.Framework.Upper_Limit_Exceeded(Token.Span)
      If results.Arg.Align.Value <= Framework.LowerLimit Then results.Result.Issues += Issue.Arg.Align.Framework.Lower_Limit_Exceeded(Token.Span)
    End If
    Return results
  End Function

  Private Function ArgAlign_Expecting_Digits(ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters) As Parameters
    Dim Digits = TryCast(Token, FSDv2.FormatString.Common.Digits)
    If Digits Is Nothing Then Results.Result.Issues += Issue.Unexpected.Token(Token.Span,Token) : Return Results
  
Expecting_Digits:
    If EndOfText(Index, [End]) Then Return Results
    If Digits.Kind = TokenKind.Digits Then
       Results = Validate_ArgAlign(Digits, Results)
       MoveToNext(Index)
       Return Expecting_Possible_Whitespaces(Index, [End], Token, Results)
    End if
    Results = WasUnexpected(Index, Token, Results)
    GoTo Expecting_Digits 
  End Function

  Private Function ArgAlign_Body( ByRef Index As Integer, [End] As Integer, Token As Token, Results As Parameters ) As Parameters
    If EndOfText(Index, [End]) Then Return Results
    Dim Current = TryCast(Token, FSDv2.FormatString.ArgHole.Align.Body)
    Debug.Assert(Current IsNot Nothing)
    Dim idx0 = 0, edx0 = Current.InnerTokens.Count
    Results = ArgAlign_Expecting_PossibleMinusSign(idx0, edx0, Current, Results)
    MoveToNext(Index) ' : If idx >= edx Then Return Results
    Return AnyMoreTokensAreUnexpected(Index, [End], Token, Results)
  End Function

  Private Function ArgAlign( TheArgAlign As Align, Results As Parameters ) As Analyser.Parameters
    '
    ' Arg.Align::= Comma Whitespace? MinusSign? Digits Whitespaces?
    '
    Dim Index = 0, [End] = TheArgAlign.InnerTokens.Count

Expecting_ArgAlign_Head:
    If EndOfText(Index, [End]) Then Results.Result.Issues += Issue.Unexpected.EoT(Nothing) : Return Results
    Dim current = TheArgAlign(Index)
    Select Case current.Kind

      Case TokenKind.ArgHole_Align_Head
        Results = ArgAlign_Head(Index, [End], current, Results)
        MoveToNext(Index) : If EndOfText(Index, [End]) Then Return Results
        current = TheArgAlign(Index)
        Results = ArgAlign_Body(Index, [End], current, Results)

      Case TokenKind.ArgHole_Align_Body
        ' missing argalign_head
        Return ArgAlign_Body(Index, [End], TheArgAlign, Results)

      Case Else
        Results = WasUnexpected(Index, Current, Results)

        GoTo Expecting_ArgAlign_Head

    End Select
    Return Results
  End Function

End Class
