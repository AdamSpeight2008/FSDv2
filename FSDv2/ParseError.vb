Public MustInherit Class ParseError : Inherits Token

  Public Enum Reason As Integer
    NullParse = 0
    EoT
    UnexpectedCharacter
    Invalid
    Unsupported
    [Partial]
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
  Public Class Make
    Public Shared Function EoT(ix As Source.Position) As ParseError
      Return New EoT(ix.ToZeroSpan, Tokens.Empty)
    End Function
    Public Shared Function NullParse(ix As Source.Position) As ParseError
      Return New NullParse(ix.ToZeroSpan, Tokens.Empty)
    End Function
    Public Shared Function Invalid(ix As Source.Position, Tx As Tokens, Optional Additional As String = Nothing) As ParseError
      Return New Invalid(ix.ToZeroSpan, Tx, Additional)
    End Function
    Public Shared Function Invalid(ix As Source.Span, Tx As Tokens, Optional Additional As String = Nothing) As ParseError
      Return New Invalid(ix, Tx, Additional)
    End Function
    Public Shared Function Unsupported(ix As Source.Position, Text As String) As ParseError
      Return New Unsupported(ix.ToZeroSpan, Tokens.Empty, Text)
    End Function
    Public Shared Function Unsupported(sp As Source.Span, Text As String) As ParseError
      Return New Unsupported(sp, Tokens.Empty, Text)
    End Function
    Public Shared Function UnexpectedChars(sp As Source.Span, Tx As Tokens, Text As String) As ParseError
      Return New UnexpectedChars(sp, Tx, Text)
    End Function
  End Class

  Public Class EoT : Inherits ParseError
      Public Sub New(Span As Source.Span, Tx As Tokens, Optional Additional As String = Nothing)
        MyBase.New(Span, Reason.EoT, Tx, Additional)
      End Sub
    End Class
    Public Class NullParse : Inherits ParseError
      Public Sub New(Span As Source.Span, Tx As Tokens, Optional Additional As String = Nothing)
        MyBase.New(Span, Reason.NullParse, Tx, Additional)
      End Sub
    End Class
    Public Class Unsupported : Inherits ParseError
      Public Sub New(Span As Source.Span, Tx As Tokens, Optional Additional As String = Nothing)
        MyBase.New(Span, Reason.Unsupported, Tx, Additional)
      End Sub
    End Class
    Public Class UnexpectedChars : Inherits ParseError
      Public Sub New(Span As Source.Span, Tx As Tokens, Optional Additional As String = Nothing)
        MyBase.New(Span, Reason.UnexpectedCharacter, Tx, Additional)
      End Sub
    End Class
    Public Class Invalid : Inherits ParseError
      Public Sub New(Span As Source.Span, Tx As Tokens, Optional Additional As String = Nothing)
        MyBase.New(Span, Reason.Invalid, Tx, Additional)
      End Sub
    End Class

    Public Class [Partial] : Inherits ParseError
      Public ReadOnly Property Target As TokenKind
      Public Sub New(Target As TokenKind, Span As Source.Span, Txn As Tokens, Optional Additional As String = Nothing)
        MyBase.New(Span, Reason.Partial, Txn, Additional)
        Me.Target = Target
      End Sub
    End Class

    Public Class Resync : Inherits ParseError

      Public Sub New(Span As Source.Span, Txn As Tokens, Optional Additional As String = Nothing)
        MyBase.New(Span, Reason.Partial, Txn, Additional)
      End Sub
    End Class

  End Class