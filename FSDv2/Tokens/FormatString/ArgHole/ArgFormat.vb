Imports System.Globalization
Partial Public Class FormatString
  Partial Public Class ArgHole : Inherits Token

    Public Class Format : Inherits Token

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span?, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Format, Span, Inner)
      End Sub

      '  <DebuggerStepperBoundary>
      Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
        If Index?.IsInvalid Then Return ParseError.Make.EoT(Index)
        Dim Token = Format.Head.TryParse(Index, DoingResync)
        If TypeOf Token Is ParseError Then Return Token
        Dim Start = Index, Tokens = Common.AddThenNext(Token, FSDv2.Tokens.Empty, Index)
        Token = ArgHole.Format.Body.TryParse(Index, DoingResync)
        If Token.Kind = TokenKind.ArgHole_Format_Body Then  Tokens = Common.AddThenNext(Token, Tokens, Index)
        Return New Format(Start?.To(Index), Tokens)
      End Function

      Public Class Head
        Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span?, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Head, Span, Inner)
        End Sub


        ' <DebuggerStepperBoundary>
        Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
          If Index?.IsInvalid Then Return ParseError.Make.EoT(Index)
          Dim Token = Colon.TryParse(Index, DoingResync)
          If Token.Kind = TokenKind.ParseError Then Return ParseError.Make.NullParse(Index)
          Return New Head(Token.Span, Tokens.Empty + Token)
        End Function

      End Class

      Public Class Body
        Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span?, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Body, Span, Inner)
        End Sub

        ' <DebuggerStepperBoundary>
        Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
          If Index?.IsInvalid Then Return ParseError.Make.EoT(Index)
          Dim Tokens = FSDv2.Tokens.Empty, Start = Index, Token As Token
          While Index?.IsValid

            Token = Common.Brace.Opening.TryParse(Index, DoingResync)
            Select Case Token.Kind
              Case TokenKind.Brace_Opening     : Token = ParseError.Make.Invalid(Token.Span, Token) : GoTo OnToNext
              Case TokenKind.Esc_Brace_Opening : GoTo OnToNext
            End Select

            Token = Common.Brace.Closing.TryParse(Index, DoingResync)
            Select Case Token.Kind
              Case TokenKind.Brace_Closing     : Exit While
              Case TokenKind.Esc_Brace_Closing : GoTo OnToNext
            End Select

            Token = Text._TryParse(Index, True, DoingResync)
            Select Case Token.Kind
              Case TokenKind.ParseError : Exit While
            End Select

OnToNext:
            Tokens = Common.AddThenNext(Token, Tokens, Index)

          End While
          Return New Format.Body(Start?.To(Index), Tokens)
        End Function

      End Class

      Public Class Colon : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span?)
          MyBase.New(TokenKind.Colon, Span)
        End Sub

        '    <DebuggerStepperBoundary>
        Public Shared Function TryParse(Index As Source.Position?, DoingResync As Boolean) As Token
          If Index?.IsInvalid Then Return ParseError.Make.EoT(Index)
          If (Index <> ":"c) Then Return ParseError.Make.NullParse(Index)
          Return New Colon(Source.Span.Create_UnitSpan(Index))
        End Function

      End Class

    End Class

  End Class

End Class