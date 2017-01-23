Public MustInherit Class ParseError : Inherits Token

  Public Enum Reason As Integer
    NullParse = 0
    EoT
    UnexpectedCharacter
    Invalid
    Unsupported
    [Partial]
    ResyncSkipped
  End Enum

  Public ReadOnly Property Why As ParseError.Reason
  Public ReadOnly Property Additional As String

  Public Sub New(Span As Source.Span?, Reason As Reason, Token As Token, Optional Additional As String = Nothing)
    MyBase.New(TokenKind.ParseError, Span, Tokens.Empty + Token)
    Me.Why = Reason
    Me.Additional = If(Additional, String.Empty)
  End Sub

  Public Sub New(Span As Source.Span?, Reason As Reason, Tokens As Tokens, Optional Additional As String = Nothing)
    MyBase.New(TokenKind.ParseError, Span, Tokens)
    Me.Why = Reason
    Me.Additional = If(Additional, String.Empty)
  End Sub

  Public Overrides Function ToString() As String
    Return $"{MyBase.ToString()}.{Why.ToString}"
  End Function

  Public Class Make
    <DebuggerStepperBoundary>
    Public Shared Function EoT(Index As Source.Position?) As ParseError
      Return New EoT(Index?.ToZeroSpan, Tokens.Empty)
    End Function
    <DebuggerStepperBoundary>
    Public Shared Function NullParse(Index As Source.Position?, Optional Inner As Tokens = Nothing) As ParseError
      Return New NullParse(Index?.ToZeroSpan, Inner)
    End Function
    <DebuggerStepperBoundary>
    Public Shared Function Invalid(Index As Source.Position, Tokens As Tokens, Optional Additional As String = Nothing) As ParseError
      Return New Invalid(Index.ToZeroSpan, Tokens, Additional)
    End Function
    <DebuggerStepperBoundary>
    Public Shared Function Invalid(Index As Source.Span?, Tokens As Tokens, Optional Additional As String = Nothing) As ParseError
      Return New Invalid(Index, Tokens, Additional)
    End Function
    <DebuggerStepperBoundary>
    Public Shared Function Unsupported(Index As Source.Position?, Text As String) As ParseError
      Return New Unsupported(Index?.ToZeroSpan, Tokens.Empty, Text)
    End Function
    <DebuggerStepperBoundary>
    Public Shared Function Unsupported(Span As Source.Span?, Text As String) As ParseError
      Return New Unsupported(Span, Tokens.Empty, Text)
    End Function
    <DebuggerStepperBoundary>
    Public Shared Function UnexpectedChars(Span As Source.Span?, Tokens As Tokens, Text As String) As ParseError
      Return New UnexpectedChars(Span, Tokens, Text)
    End Function
  End Class

  Public Class EoT : Inherits ParseError
    <DebuggerStepperBoundary>
    Public Sub New(Span As Source.Span?, Tokens As Tokens, Optional Additional As String = Nothing)
      MyBase.New(Span, Reason.EoT, Tokens, Additional)
    End Sub
  End Class
  Public Class NullParse : Inherits ParseError
    <DebuggerStepperBoundary>
    Public Sub New(Span As Source.Span?, Tokens As Tokens, Optional Additional As String = Nothing)
      MyBase.New(Span, Reason.NullParse, Tokens, Additional)
    End Sub
  End Class
  Public Class Unsupported : Inherits ParseError
    <DebuggerStepperBoundary>
    Public Sub New(Span As Source.Span?, Tokens As Tokens, Optional Additional As String = Nothing)
      MyBase.New(Span, Reason.Unsupported, Tokens, Additional)
    End Sub
  End Class
  Public Class UnexpectedChars : Inherits ParseError
    <DebuggerStepperBoundary>
    Public Sub New(Span As Source.Span?, Tokens As Tokens, Optional Additional As String = Nothing)
      MyBase.New(Span, Reason.UnexpectedCharacter, Tokens, Additional)
    End Sub
  End Class
  Public Class Invalid : Inherits ParseError
    <DebuggerStepperBoundary>
    Public Sub New(Span As Source.Span?, Tokens As Tokens, Optional Additional As String = Nothing)
      MyBase.New(Span, Reason.Invalid, Tokens, Additional)
    End Sub
  End Class

  Public Class [Partial] : Inherits ParseError
    Public ReadOnly Property Target As TokenKind
    <DebuggerStepperBoundary>
    Public Sub New(Target As TokenKind, Span As Source.Span?, Tokens As Tokens, Optional Additional As String = Nothing)
      MyBase.New(Span, Reason.Partial, Tokens, Additional)
      Me.Target = Target
    End Sub
  End Class

  Public Class Resync : Inherits ParseError

    <DebuggerStepperBoundary>
    Public Sub New(Span As Source.Span?, Tokens As Tokens, Optional Additional As String = Nothing)
      MyBase.New(Span, Reason.ResyncSkipped, Tokens, Additional)
    End Sub

  End Class

End Class