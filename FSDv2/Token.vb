Imports FSD

Public MustInherit Class Token
  Public ReadOnly Property Span As Source.Span
  ReadOnly Property InnerTokens As Tokens = Tokens.Empty
  Public ReadOnly Property Kind As TokenKind

  Protected Sub New(kind As TokenKind, span As Source.Span, Optional Inner As Tokens = Nothing)
    Me.Kind = kind : Me.Span = span : Me.InnerTokens = If(Inner, Tokens.Empty)
  End Sub

  Public Shared Operator +(T0 As Token, T1 As Token) As Tokens
    Return Tokens.Create(T0, T1)
  End Operator

  Public Shared Widening Operator CType(T As Token) As Tokens
    Return Tokens.Create(T)
  End Operator

  Public Overrides Function ToString() As String
    Return $"{Span.ToString()}  {Kind.ToString}"
  End Function

  Default Public ReadOnly Property Inner(ByVal Index As Integer) As Token
    Get
      Return InnerTokens(Index)
    End Get
  End Property

End Class

Public Enum TokenKind As Integer
  ParseError
  Whitespace
  Whitespaces
  Digit
  Digits
  HexDigit
  HexDigits
  Comma
  Colon
  MinusSign
  FormatString
  Text
  ArgHole
  ArgHole_Head
  ArgHole_Index
  ArgHole_Align
  ArgHole_Align_Head
  ArgHole_Align_Body
  ArgHole_Format
  ArgHole_Format_Head
  ArgHole_Format_Body
  Brace_Opening
  Brace_Closing
  Esc_Brace_Opening
  Esc_Brace_Closing
  Esc_Seq_Simple
  Esc_Seq_HexaDecimal
  Esc_Seq_Unicode
  Esc_Seq_Head
  Esc_Seq
  Backslash_UpperU
  Backslash_LowerU
  [Partial]
    Identifier
End Enum