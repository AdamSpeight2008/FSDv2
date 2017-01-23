Partial Public Class FormatString

  Public Class Text : Inherits Token

    <DebuggerStepperBoundary>
    Friend Sub New(Span As Source.Span?, Inner As Tokens)
      MyBase.New(TokenKind.Text, Span, Inner)
    End Sub

    <DebuggerStepperBoundary>
    Public Shared Function TryParse(Index As Source.Position, DoingResync As Boolean) As Token
      Return _TryParse(Index, False, DoingResync)
    End Function

    <DebuggerStepperBoundary>
    Friend Shared Function _TryParse(
                                      Index As Source.Position?,
                              ParsingArgFormatText As Boolean,
                              DoingResync As Boolean
                                 ) As Token
      Dim Tokens = FSDv2.Tokens.Empty, Start = Index, Token As Token
      Dim TextStart As New Source.Position?
      While Index?.IsValid
#Region " Check to see if it is an escaped Brace Closing or Opening"
        Token = Common.Brace.Esc.Closing.TryParse(Index, DoingResync) : If Token.Kind = TokenKind.Esc_Brace_Closing Then Tokens = Common.AddThenNext(Token, Tokens, Index, TextStart) : Continue While
        Token = Common.Brace.Esc.Opening.TryParse(Index, DoingResync) : If Token.Kind = TokenKind.Esc_Brace_Opening Then Tokens = Common.AddThenNext(Token, Tokens, Index, TextStart) : Continue While
#End Region
#Region "Is it a Brace Closing } ?"
        Token = Common.Brace.Closing.TryParse(Index, DoingResync)
        If Token.Kind = TokenKind.Brace_Closing Then
          If ParsingArgFormatText Then Exit While
          Tokens = Common.AddThenNext(ParseError.Make.Invalid(Token.Span, Token, ""), Tokens, Index, TextStart)
        End If
#End Region
#Region "Is it a Brace Opeining { ?"
        Token = Common.Brace.Opening.TryParse(Index, DoingResync)
        If Token.Kind = TokenKind.Brace_Opening Then
          If ParsingArgFormatText Then Tokens = Common.AddThenNext(ParseError.Make.Invalid(Token.Span, Token, ""), Tokens, Index, TextStart) : Continue While
          Tokens = Common.AddThenNext(Nothing, Tokens, Index, TextStart) : Exit While
        End If
#End Region
#Region "Check to see it is a C# specific escape sequence"
        If Index?.Src.Kind = Source.SourceKind.CS_Standard AndAlso Index = "\"c Then
          Token = Common.Esc.Sequence.TryParse(Index, DoingResync)
          Select Case Token.Kind
            Case TokenKind.Esc_Seq_Simple, TokenKind.Esc_Seq_HexaDecimal, TokenKind.Esc_Seq_Unicode
              Tokens = Common.AddThenNext(Token, Tokens, Index, TextStart) : Continue While
            Case TokenKind.Partial
              Tokens = Common.AddThenNext(Token, Tokens, Index, TextStart) : Continue While
            Case TokenKind.ParseError
              If Token.IsNotNullParse Then Tokens = Common.AddThenNext(Token, Tokens, Index, TextStart) : Continue While
          End Select
        End If
#End Region
#Region "Otherwise treat it as just a text character."
        If TextStart Is Nothing Then TextStart = Index
        Index = Index?.Next
#End Region
      End While
      If TextStart IsNot Nothing Then Tokens = Common.AddThenNext(Nothing, Tokens, Index, TextStart)
      Return New Text(Start?.To(Index), Tokens)
    End Function

  End Class

End Class