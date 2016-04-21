Public Class ParseError : Inherits Token

  Public Enum Reason
    UnexpectedCharacter
    Invalid
  End Enum

  Public ReadOnly Property Why As ParseError.Reason
  Public Sub New(Span As Source.Span, Reason As Reason)
    MyBase.New(Span)
    Me.Why = Reason
  End Sub

End Class
