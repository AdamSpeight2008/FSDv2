Partial Public Class FormatString

  Public Class Text : Inherits Token

    Friend Sub New(Span As Source.Span, Inner As Tokens)
      MyBase.New(TokenKind.Text, Span, Inner)
    End Sub

    Public Shared Function TryParse(Ix As Source.Position) As Token
      Return _TryParse(Ix, False)
    End Function

    Friend Shared Function _TryParse(Ix As Source.Position, Optional ParsingArgFormatText As Boolean = False) As Token
      Dim Txn = Tokens.Empty, sx = Ix, T As Token
      Dim TextStart As New Source.Position?
      While Ix.IsValid
#Region " Check to see if it is an escaped Brace Closing or Openind"
        T = Common.Brace.Esc.Closing.TryParse(Ix) : If T.Kind = TokenKind.Esc_Brace_Closing Then Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
        T = Common.Brace.Esc.Opening.TryParse(Ix) : If T.Kind = TokenKind.Esc_Brace_Opening Then Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
#End Region
#Region "Is it a Brace Closing } ?"
        T = Common.Brace.Closing.TryParse(Ix)
        If T.Kind = TokenKind.Brace_Closing Then
          If ParsingArgFormatText Then Exit While
          Txn = Common.AddThenNext(ParseError.Make.Invalid(T.Span, T, ""), Txn, Ix, TextStart)
        End If
#End Region
#Region "Is it a Brace Opeining { ?"
        T = Common.Brace.Opening.TryParse(Ix)
        If T.Kind = TokenKind.Brace_Opening Then
          If ParsingArgFormatText Then Txn = Common.AddThenNext(ParseError.Make.Invalid(T.Span, T, ""), Txn, Ix, TextStart) : Continue While
          Txn = Common.AddThenNext(Nothing, Txn, Ix, TextStart) : Exit While
        End If
#End Region
#Region "Check to see it is a C# specific escape sequence"
        If Ix.Source.Kind = Source.SourceKind.CS_Standard AndAlso Ix = "\"c Then
          T = Common.Esc.Sequence.TryParse(Ix)
          Select Case T.Kind
            Case TokenKind.Esc_Seq_Simple, TokenKind.Esc_Seq_HexaDecimal, TokenKind.Esc_Seq_Unicode
              Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
            Case TokenKind.Partial
              Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
            Case TokenKind.ParseError
              If T.IsNotNullParse Then Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
          End Select
        End If
#End Region
#Region "Otherwise treat it as just a text character."
        If TextStart Is Nothing Then TextStart = New Source.Position?(Ix)
        Ix = Ix.Next
#End Region
      End While
      If TextStart IsNot Nothing Then Txn = Common.AddThenNext(Nothing, Txn, Ix, TextStart)
      Return New Text(sx.To(Ix), Txn)
    End Function

  End Class

End Class