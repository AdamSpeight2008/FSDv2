Public Class ParseError : Inherits Token

  Public Enum Reason As Integer
    NullParse = 0
    EoT
    UnexpectedCharacter
    Invalid
    Unsupported
  End Enum

  Public ReadOnly Property Why As ParseError.Reason
  Public ReadOnly Property Additional As String
  Public Sub New(Span As Source.Span, Reason As Reason, T As Token, Optional Additional As String = Nothing)
    MyBase.New(TokenKind.ParseError, Span, Tokens.Empty + T)
    Me.Why = Reason
    Me.Additional = If(Additional, String.Empty)
  End Sub
  Public Sub New(Span As Source.Span, Reason As Reason, Tx As Tokens, Optional Additional As String = Nothing)
    MyBase.New(TokenKind.ParseError, Span, Tx)
    Me.Why = Reason
    Me.Additional = If(Additional, String.Empty)
  End Sub

  Public Shared Function EoT(ix As Source.Position) As ParseError
    Return New ParseError(ix.ToZeroSpan, Reason.EoT, Tokens.Empty)
  End Function
  Public Shared Function NullParse(ix As Source.Position) As ParseError
    Return New ParseError(ix.ToZeroSpan, Reason.NullParse, Tokens.Empty)
  End Function

  Public Shared Function Unsupported(ix As Source.Position, Text As String) As ParseError
    Return New ParseError(ix.ToZeroSpan, Reason.Unsupported, Tokens.Empty, Text)
  End Function
  Public Shared Function Unsupported(sp As Source.Span, Text As String) As ParseError
    Return New ParseError(sp, Reason.Unsupported, Tokens.Empty, Text)
  End Function
End Class
