Partial Public Class FormatString

  Public Class Common

    Public Class Whitespace : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(TokenKind.Whitespace, Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        If (Ix <> " "c) Then Return ParseError.Make.NullParse(Ix)
        Return New Whitespace(Ix.ToUnitSpan)
      End Function

    End Class

    Public Class Whitespaces : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.Whitespaces, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim Txn = Tokens.Empty
        Dim Sx = Ix
        While Ix.IsValid
          Dim T = Whitespace.TryParse(Ix)
          If T.Kind = TokenKind.ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix)
        End While
        Dim s = Sx.To(Ix)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return ParseError.Make.NullParse(Ix)
        Return New Whitespaces(s.Value, Txn)
      End Function

    End Class

    Public Class Digit : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(TokenKind.Digit, Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid OrElse Ix.Value.HasValue = False Then Return ParseError.Make.EoT(Ix)
        Select Case Ix.Value.Value
          Case "0"c To "9"c
            Return New Digit(Ix.ToUnitSpan)
          Case Else
            Return ParseError.Make.NullParse(Ix)
        End Select
      End Function

      Public Function GetValue() As Numerics.BigInteger?
        Select Case Me.Span.Start.Value
          Case "0"c : Return Numerics.BigInteger.Zero
          Case "1"c : Return Numerics.BigInteger.One
          Case "2"c : Return New Numerics.BigInteger(2)
          Case "3"c : Return New Numerics.BigInteger(3)
          Case "4"c : Return New Numerics.BigInteger(4)
          Case "5"c : Return New Numerics.BigInteger(5)
          Case "6"c : Return New Numerics.BigInteger(6)
          Case "7"c : Return New Numerics.BigInteger(7)
          Case "8"c : Return New Numerics.BigInteger(8)
          Case "9"c : Return New Numerics.BigInteger(9)
        End Select
        Return Nothing
      End Function

    End Class

    Public Class Digits : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.Digits, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim Txn = Tokens.Empty()
        Dim Sx = Ix
        While Ix.IsValid
          Dim T = Digit.TryParse(Ix)
          If T.Kind = TokenKind.ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix)
        End While
        Dim s = Sx.To(Ix)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return ParseError.Make.NullParse(Ix)
        Return New Digits(s.Value, Txn)
      End Function

      Private _Value As Numerics.BigInteger?
      Private _First As Boolean = False
      Public Function GetValue() As Numerics.BigInteger?
        If Not _First Then
          Dim output = Numerics.BigInteger.Zero
          For Each d As FSDv2.FormatString.Common.Digit In Me.InnerTokens.GetEnumerator
            output = (10 * output) + d.GetValue
          Next
          _Value = output
          _First = True
        End If
        Return _Value
      End Function

    End Class

    Public Class HexDigit : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(TokenKind.HexDigit, Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid OrElse Ix.Value.HasValue = False Then Return ParseError.Make.EoT(Ix)
        Select Case Ix.Value.Value
          Case "0"c To "9"c, "a"c To "f"c, "A"c To "F"c
            Return New HexDigit(Ix.ToUnitSpan)
          Case Else
            Return ParseError.Make.NullParse(Ix)
        End Select
      End Function

    End Class

    Public Class HexDigits : Inherits Token

      Friend Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.HexDigits, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
        Dim Txn = Tokens.Empty()
        Dim Sx = Ix
        While Ix.IsValid
          Dim T = HexDigit.TryParse(Ix)
          If TypeOf T Is ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix)
        End While
        Dim s = Sx.To(Ix)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return ParseError.Make.NullParse(Ix)
        Return New HexDigits(s.Value, Txn)
      End Function

    End Class


    MustInherit Class Brace : Inherits Token

      Friend Sub New(Kind As TokenKind, Span As Source.Span, Optional Inner As Tokens = Nothing)
        MyBase.New(Kind, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid OrElse Ix.Value.HasValue = False Then Return ParseError.Make.EoT(Ix)
        Dim nx = Ix.Next
        If Ix.Value.Value = "{"c Then
          If nx.IsInvalid OrElse (nx <> "{") Then Return New Opening(Ix.ToUnitSpan)
          Return New Brace.Esc.Opening(Ix.To(nx.Next), New Opening(Ix.ToUnitSpan) + New Opening(nx.ToUnitSpan))
        ElseIf Ix.Value = "}"c Then
          If nx.IsInvalid OrElse (nx <> "}") Then Return New Closing(Ix.ToUnitSpan)
          Return New Brace.Esc.Closing(Ix.To(nx.Next), New Closing(Ix.ToUnitSpan) + New Closing(nx.ToUnitSpan))
        Else
          Return ParseError.Make.NullParse(Ix)
        End If
      End Function

      Public Class Opening : Inherits Brace

        Friend Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Brace_Opening, Span)
        End Sub

        Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim res = Brace.TryParse(Ix)
          If res.Kind <> TokenKind.Brace_Opening Then Return ParseError.Make.NullParse(Ix)
          Return res
        End Function

      End Class

      Public Class Closing : Inherits Brace

        Friend Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Brace_Closing, Span)
        End Sub

        Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim res = Brace.TryParse(Ix)
          If res.Kind <> TokenKind.Brace_Closing Then Return ParseError.Make.NullParse(Ix)
          Return res
        End Function

      End Class

      Public Class Esc

        Public Class Opening : Inherits Brace

          Friend Sub New(Span As Source.Span, Inner As Tokens)
            MyBase.New(TokenKind.Esc_Brace_Opening, Span, Inner)
          End Sub

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
            If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
            Dim res = Brace.TryParse(Ix)
            If res.Kind <> TokenKind.Esc_Brace_Opening Then Return ParseError.Make.NullParse(Ix)
            Return res
          End Function

        End Class

        Public Class Closing : Inherits Brace

          Friend Sub New(Span As Source.Span, Inner As Tokens)
            MyBase.New(TokenKind.Esc_Brace_Closing, Span, Inner)
          End Sub

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
            If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
            Dim res = Brace.TryParse(Ix)
            If res.Kind <> TokenKind.Esc_Brace_Closing Then Return ParseError.Make.NullParse(Ix)
            Return res
          End Function

        End Class

      End Class

    End Class

    Public Shared Function AddThenNext(T As Token, Tx As Tokens, ByRef Ix As Source.Position, Optional ByRef TextStart As Source.Position? = Nothing) As Tokens
      If TextStart IsNot Nothing Then Tx += New Text(TextStart.Value.To(Ix), Tokens.Empty) : TextStart = Nothing
      If T IsNot Nothing Then Ix = T.Span.Next : Tx = Tx.Add(T)
      Return Tx
    End Function

    Public Class Esc

      Public Class SeqHead : Inherits Token

        Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
          MyBase.New(TokenKind.Esc_Seq_Head, Span, Inner)
        End Sub

      End Class

      Public MustInherit Class Sequence : Inherits Token

        Friend Sub New(Kind As TokenKind, Span As Source.Span, Optional Inner As Tokens = Nothing)
          MyBase.New(Kind, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Make.Unsupported(Ix, $"{Ix.Source.Kind} doesn't support escape sequences")
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If Ix <> "\"c Then Return ParseError.Make.NullParse(Ix)
          Dim sx = Ix, nx = Ix.Next
          If nx.IsInvalid Then Return ParseError.Make.EoT(nx)
          Select Case nx
            Case "'"c, "\"c, "0"c, "a"c,
                 "b"c, "f"c, "n"c, "r"c,
                 "t"c, "v"c, """"c
              Return New Simple(Ix.To(nx.Next))
            Case "x"c
              Return HexaDecimal.TryParse(Ix)
            Case "u"c, "U"c
              Return Unicode.TryParse(Ix)
          End Select
          Return ParseError.Make.Unsupported(sx.To(nx.Next), "")
        End Function

        Public Class Simple : Inherits Esc.Sequence
          Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
            MyBase.New(TokenKind.Esc_Seq_Simple, Span, Inner)
          End Sub
        End Class

        Public Class Unicode : Inherits Esc.Sequence

          Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
            MyBase.New(TokenKind.Esc_Seq_Unicode, Span, Inner)
          End Sub

          Private Shared Function Parse_HexDigits(sx As Source.Position, ix As Source.Position, RequiredLength As Integer) As Token
            Dim T As Token, Hx = Tokens.Empty
            While ix.IsValid AndAlso Hx.Count < RequiredLength
              T = Common.HexDigit.TryParse(ix)
              If T.Kind = TokenKind.ParseError Then Exit While
              Hx = Common.AddThenNext(T, Hx, ix)
            End While
            If Hx.Count <> RequiredLength Then Return ParseError.Make.Invalid(sx.To(ix), Hx, $"Expecting {RequiredLength} Hexadecimal Digits")
            Return New HexDigits(Hx.First.Span.Start.To(Hx.Last.Span.Next), Hx)
          End Function

          Private Shared Function Backslash_UpperU(Ix As Source.Position) As Token
            ' unicode_escape_sequence ::= \U hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
            Dim Txn = Tokens.Empty, sx = Ix
            ' OK. At this stage we hace the start of a escape sequence for a unicode characeter \U
            If Ix.IsInvalid Then Return ParseError.Make.Invalid(sx.To(Ix), Txn, "Expecting 8 Hexadecimal Digits")
            ' Try and parse 4 HexaDecimal characters.
            Return Parse_HexDigits(sx, Ix, 8)
          End Function

          Private Shared Function Backslash_LowerU(Ix As Source.Position) As Token
            ' unicode_escape_sequence ::= \u hex_digit hex_digit hex_digit hex_digit
            Dim Txn = Tokens.Empty, sx = Ix
            ' OK. At this stage we hace the start of a escape sequence for a unicode characeter \u
            If Ix.IsInvalid Then Return ParseError.Make.Invalid(sx.To(Ix), Txn, "Expecting 4 Hexadecimal Digits")
            ' Try and parse 4 HexaDecimal characters.
            Return Parse_HexDigits(sx, Ix, 4)
          End Function

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
            '
            ' unicode_escape_sequence ::= \u hex_digit hex_digit hex_digit hex_digit
            '
            If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Make.Unsupported(Ix, $"{Ix.Source.Kind} doesn't support escape sequences")
            If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix) ' Already at the end of the text.
            If Ix <> "\"c Then Return ParseError.Make.NullParse(Ix) ' Character is not a blackslash
            Dim sx = Ix ' Start Index of this potential token.
            Dim txn = Tokens.Empty : Ix = Ix.Next
            If Ix.IsInvalid Then Return ParseError.Make.NullParse(sx) ' A lone backslash (\) at the end of the text.
            If Ix = "u"c Then
              txn = FormatString.Common.AddThenNext(New Esc.SeqHead(sx.To(Ix.Next)), txn, Ix)
              Dim hx = Backslash_LowerU(Ix)
              If hx.Kind = TokenKind.HexDigits Then Return New Unicode(sx.To(hx.Span.Next), txn + hx)
              Return ParseError.Make.Invalid(sx.To(hx.Span.Next), txn + hx, "")
            ElseIf Ix = "U"c Then
              txn = FormatString.Common.AddThenNext(New Esc.SeqHead(sx.To(Ix.Next)), txn, Ix)
              Dim hx = Backslash_UpperU(Ix)
              If hx.Kind = TokenKind.HexDigits Then Return New Unicode(sx.To(hx.Span.Next), txn + hx)
              Return ParseError.Make.Invalid(sx.To(hx.Span.Next), txn + hx, "")
            End If
            Return ParseError.Make.NullParse(sx) ' Doesn't have correct the start to an Unicode escape squence. (\c)
          End Function

        End Class

        Public Class HexaDecimal : Inherits Esc.Sequence

          Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
            MyBase.New(TokenKind.Esc_Seq_Unicode, Span, Inner)
          End Sub

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
            If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Make.Unsupported(Ix, $"{Ix.Source.Kind} doesn't support escape sequences")
            If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
            If Ix <> "\"c Then Return ParseError.Make.NullParse(Ix)
            Dim sx = Ix, txn = Tokens.Empty : Ix = Ix.Next
            If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix) ' A lone backslash (\) at the end of the text.
            If Ix <> "x"c Then Return ParseError.Make.NullParse(sx) ' Doesn't have correct the start to an Unicode escape squence. (\c)
            Ix = Ix.Next
            Dim T As Token = New Esc.SeqHead(sx.To(Ix))
            txn += T
            If Ix.IsInvalid Then Return New ParseError.[Partial](TokenKind.Esc_Seq_HexaDecimal, sx.To(Ix), txn, "Expecting 4 Hexadecimal Digits")
            Dim Hx = Tokens.Empty
            While Ix.IsValid AndAlso Hx.Count < 4
              T = Common.HexDigit.TryParse(Ix)
              If T.Kind = TokenKind.ParseError Then Exit While
              Hx = Common.AddThenNext(T, Hx, Ix)
            End While
            If Hx.Count = 0 Then Return New ParseError.[Partial](TokenKind.Esc_Seq_HexaDecimal, sx.To(Ix), txn + Hx, "Expecting 4 Hexadecimal Digits")
            Return New Esc.Sequence.HexaDecimal(sx.To(Ix), txn + New HexDigits(Hx.First.Span.Start.To(Hx.Last.Span.Next), Hx))
          End Function

        End Class

      End Class

    End Class

    End Class

  End Class