Imports FSD

Public MustInherit Class Token
  Public ReadOnly Property Span As Source.Span
  Public ReadOnly Property Inner As Tokens = Tokens.Empty

  Protected Sub New(span As Source.Span, Optional Inner As Tokens = Nothing)
    Me.Span = span : Me.Inner = If(Inner, Tokens.Empty)
  End Sub

  Public Shared Operator +(T0 As Token, T1 As Token) As Tokens
    Return Tokens.Create(T0, T1)
  End Operator

End Class

