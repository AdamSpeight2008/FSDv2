Imports System.Globalization
Partial Public Class FormatString
  Partial Public Class ArgHole : Inherits Token

    Public Class Format : Inherits Token

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Format, Span, Inner)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim T As Token = Format.Head.TryParse(Ix, DoingResync)
        If TypeOf T Is ParseError Then Return T
        Dim sx = Ix
        Dim Txn = Tokens.Empty : Txn = Common.AddThenNext(T, Txn, Ix)
        T = ArgHole.Format.Body.TryParse(Ix, DoingResync)
        If T.Kind = TokenKind.ArgHole_Format_Body Then
          Txn = Common.AddThenNext(T, Txn, Ix)
        End If
        Return New Format(sx.To(Ix), Txn)
      End Function

      Public Class Head
        Inherits Token


        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Head, Span, Inner)
        End Sub


        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim T As Token = Colon.TryParse(Ix, DoingResync)
          If T.Kind = TokenKind.ParseError Then Return ParseError.Make.NullParse(Ix)
          Return New Head(T.Span, Tokens.Empty + T)
        End Function

      End Class

      Public Class Body
        Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Body, Span, Inner)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim Txn = Tokens.Empty
          Dim sx = Ix
          Dim T As Token
          While Ix.IsValid

            T = Common.Brace.Opening.TryParse(Ix, DoingResync)
            Select Case T.Kind
              Case TokenKind.Brace_Opening : T = ParseError.Make.Invalid(T.Span, T) : GoTo OnToNext
              Case TokenKind.Esc_Brace_Opening : GoTo OnToNext
            End Select

            T = Common.Brace.Closing.TryParse(Ix, DoingResync)
            Select Case T.Kind
              Case TokenKind.Brace_Closing : Exit While
              Case TokenKind.Esc_Brace_Closing : GoTo OnToNext
            End Select

            T = Text._TryParse(Ix, True, DoingResync)
            Select Case T.Kind
              Case TokenKind.ParseError : Exit While
            End Select

OnToNext:
            Txn = Common.AddThenNext(T, Txn, Ix)

          End While
          Return New Format.Body(sx.To(Ix), Txn)
        End Function

      End Class

      Public Class Colon : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Colon, Span)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If (Ix <> ":"c) Then Return ParseError.Make.NullParse(Ix)
          Return New Colon(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

    End Class

  End Class

End Class