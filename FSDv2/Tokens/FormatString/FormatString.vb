Partial Public Class FormatString : Inherits Token

  <DebuggerStepperBoundary>
  Friend Sub New(Span As Source.Span?, Inner As Tokens)
    MyBase.New(TokenKind.FormatString, Span, Inner)
  End Sub

  <DebuggerStepperBoundary>
  Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
    If Index?.IsInvalid Then Return ParseError.Make.NullParse(Index)
    Dim Start = Index, Tokens = FSDv2.Tokens.Empty, Token As Token, Begining As Source.Position? = Nothing
    While Index?.IsValid
      Token = Common.Brace.TryParse(Index, DoingResync)
      Select Case Token.Kind

        Case TokenKind.Esc_Brace_Opening,
             TokenKind.Esc_Brace_Closing : Tokens = Common.AddThenNext(Token, Tokens, Index, Begining)

        Case TokenKind.Brace_Closing     : Tokens = Common.AddThenNext(ParseError.Make.Invalid(Token.Span, Token), Tokens, Index, Begining)

        Case TokenKind.Brace_Opening
          Dim res = ArgHole.TryParse(Index, DoingResync)
          Tokens = Common.AddThenNext(if(res.Kind<>TokenKind.ArgHole, ParseError.Make.UnexpectedChars(Index?.ToUnitSpan, res, ""),res), Tokens, Index, Begining)

        Case Else
          If Begining Is Nothing Then Begining = Index
          Index = Index?.Next
      End Select

    End While
    Tokens = Common.AddThenNext(Nothing, Tokens, Index, Begining)
    Return New FormatString(Start?.To(Index), Tokens)
  End Function

End Class
