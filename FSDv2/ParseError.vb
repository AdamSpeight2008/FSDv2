Public Class ParseError : Inherits Token

  Public Enum Reason
    UnexpectedCharacter
    Invalid
  End Enum

  Public ReadOnly Property Why As ParseError.Reason
  Public Sub New(Span As Source.Span, Reason As Reason, T As Token)
    MyBase.New(TokenKind.ParseError, Span, Tokens.Empty + T)
    Me.Why = Reason
  End Sub
  Public Sub New(Span As Source.Span, Reason As Reason, Tx As Tokens)
    MyBase.New(TokenKind.ParseError, Span, Tx)
    Me.Why = Reason
  End Sub
End Class
