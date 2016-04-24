Partial Public Class FormatString

  Public Class Common

    Public Class Whitespace : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(TokenKind.Whitespace, Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.EoT(Ix)
        If (Ix <> " "c) Then Return ParseError.NullParse(Ix)
        Return New Whitespace(Ix.ToUnitSpan)
      End Function

    End Class

    Public Class Whitespaces : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.Whitespaces, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.EoT(Ix)
        Dim Txn = Tokens.Empty
        Dim Sx = Ix
        While Ix.IsValid
          Dim T = Whitespace.TryParse(Ix)
          If T.Kind = TokenKind.ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix)
        End While
        Dim s = Sx.To(Ix)
        If s.HasValue = False Then Return ParseError.NullParse(Ix)
        Return New Whitespaces(s.Value, Txn)
      End Function

    End Class

    Public Class Digit : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(TokenKind.Digit, Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid OrElse Ix.Value.HasValue = False Then Return ParseError.EoT(Ix)
        Select Case Ix.Value.Value
          Case "0"c To "9"c
            Return New Digit(Ix.ToUnitSpan)
          Case Else
            Return ParseError.NullParse(Ix)
        End Select
      End Function

    End Class

    Public Class Digits : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.Digits, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.EoT(Ix)
        Dim Txn = Tokens.Empty()
        Dim Sx = Ix
        While Ix.IsValid
          Dim T = Digit.TryParse(Ix)
          If T.Kind = TokenKind.ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix)
        End While
        Dim s = Sx.To(Ix)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return ParseError.NullParse(Ix)
        Return New Digits(s.Value, Txn)
      End Function

    End Class

    Public Class HexDigit : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(TokenKind.HexDigit, Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid OrElse Ix.Value.HasValue = False Then Return ParseError.EoT(Ix)
        Select Case Ix.Value.Value
          Case "0"c To "9"c, "a"c To "f"c, "A"c To "F"c
            Return New HexDigit(Ix.ToUnitSpan)
          Case Else
            Return ParseError.NullParse(Ix)
        End Select
      End Function

    End Class

    Public Class HexDigits : Inherits Token

      Friend Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.HexDigits, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.EoT(Ix)
        Dim Txn = Tokens.Empty()
        Dim Sx = Ix
        While Ix.IsValid
          Dim T = HexDigit.TryParse(Ix)
          If TypeOf T Is ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix)
        End While
        Dim s = Sx.To(Ix)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return ParseError.NullParse(Ix)
        Return New HexDigits(s.Value, Txn)
      End Function

    End Class


    MustInherit Class Brace : Inherits Token

      Friend Sub New(Kind As TokenKind, Span As Source.Span, Optional Inner As Tokens = Nothing)
        MyBase.New(Kind, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid OrElse Ix.Value.HasValue = False Then Return ParseError.EoT(Ix)
        Dim nx = Ix.Next
        If Ix.Value.Value = "{"c Then
          If nx.IsInvalid OrElse (nx <> "{") Then Return New Opening(Ix.ToUnitSpan)
          Return New Brace.Esc.Opening(Ix.To(nx.Next), New Opening(Ix.ToUnitSpan) + New Opening(nx.ToUnitSpan))
        ElseIf Ix.Value = "}"c Then
          If nx.IsInvalid OrElse (nx <> "}") Then Return New Closing(Ix.ToUnitSpan)
          Return New Brace.Esc.Closing(Ix.To(nx.Next), New Closing(Ix.ToUnitSpan) + New Closing(nx.ToUnitSpan))
        Else
          Return ParseError.NullParse(Ix)
        End If
      End Function

      Public Class Opening : Inherits Brace

        Friend Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Brace_Opening, Span)
        End Sub

        Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          Dim res = Brace.TryParse(Ix)
          If res.Kind <> TokenKind.Brace_Opening Then Return ParseError.NullParse(Ix)
          Return res
        End Function

      End Class

      Public Class Closing : Inherits Brace

        Friend Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Brace_Closing, Span)
        End Sub

        Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          Dim res = Brace.TryParse(Ix)
          If res.Kind <> TokenKind.Brace_Closing Then Return ParseError.NullParse(Ix)
          Return res
        End Function

      End Class

      Public Class Esc

        Public Class Opening : Inherits Brace

          Friend Sub New(Span As Source.Span, Inner As Tokens)
            MyBase.New(TokenKind.Esc_Brace_Opening, Span, Inner)
          End Sub

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
            If Ix.IsInvalid Then Return ParseError.EoT(Ix)
            Dim res = Brace.TryParse(Ix)
            If res.Kind <> TokenKind.Esc_Brace_Opening Then Return ParseError.NullParse(Ix)
            Return res
          End Function

        End Class

        Public Class Closing : Inherits Brace

          Friend Sub New(Span As Source.Span, Inner As Tokens)
            MyBase.New(TokenKind.Esc_Brace_Closing, Span, Inner)
          End Sub

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
            If Ix.IsInvalid Then Return ParseError.EoT(Ix)
            Dim res = Brace.TryParse(Ix)
            If res.Kind <> TokenKind.Esc_Brace_Closing Then Return ParseError.NullParse(Ix)
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
          If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Unsupported(Ix, $"{Ix.Source.Kind} doesn't support escape sequences")
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          Dim Txn = Tokens.Empty
          If Ix <> "\"c Then Return ParseError.NullParse(Ix)
          Dim nx = Ix.Next
          If nx.IsInvalid Then Return ParseError.EoT(Ix)
          Dim T As Token
          Select Case nx
            Case "'"c, """"c, "\"c, "0"c, "a"c, "b"c, "f"c, "n"c, "r"c, "t"c, "v"c
              Return New Simple(Ix.To(nx.Next))
            Case "x"c
              T = HexaDecimal.TryParse(Ix)

            Case "u"c, "U"c
              T = Unicode.TryParse(Ix)
          End Select
          Return Nothing
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

          Private Shared Function Backslash_UpperU(Ix As Source.Position) As Token
            '
            ' unicode_escape_sequence ::= \U hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
            '
            If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Unsupported(Ix, $"{Ix.Source.Kind} doesn't support escape sequences")
            If Ix.IsInvalid Then Return ParseError.EoT(Ix) '  Already at the end of the text.
            If Ix <> "\"c Then Return ParseError.NullParse(Ix) ' Character is not a blackslash
            Dim sx = Ix ' Start Index of this potential token.
            Dim txn = Tokens.Empty : Ix = Ix.Next
            If Ix.IsInvalid Then Return ParseError.NullParse(sx) ' A lone backslash (\) at the end of the text.
            If Ix <> "U"c Then Return ParseError.NullParse(sx) ' Doesn't have correct the start to an Unicode escape squence. (\U)
            Ix = Ix.Next
            Dim T As Token = New Esc.SeqHead(sx.To(Ix)) : txn += T
            ' OK. At this stage we hace the start of a escape sequence for a unicode characeter \u
            '
            ' unicode_escape_sequence ::= \U hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
            Dim Hx = Tokens.Empty
            Dim count = 0
            While Ix.IsInvalid AndAlso count < 8
              T = Common.HexDigit.TryParse(Ix)
              If T.Kind = TokenKind.ParseError Then Exit While
              Hx = Common.AddThenNext(T, Hx, Ix)
              count += 1
            End While
            If count <> 8 AndAlso count <> 0 Then Return ParseError.NullParse(sx)
            Return New Esc.Sequence.Unicode(sx.To(Ix), txn + New HexDigits(Hx.First.Span.Start.To(Hx.Last.Span.Next), Hx))
          End Function

          Private Shared Function Backslash_LowerU(Ix As Source.Position) As Token
            '
            ' unicode_escape_sequence ::= \u hex_digit hex_digit hex_digit hex_digit
            '
            If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Unsupported(Ix, $"{Ix.Source.Kind} doesn't support escape sequences")
            If Ix.IsInvalid Then Return ParseError.EoT(Ix) ' Already at the end of the text.
            If Ix <> "\"c Then Return ParseError.NullParse(Ix) ' Character is not a blackslash
            Dim sx = Ix ' Start Index of this potential token.
            Dim txn = Tokens.Empty : Ix = Ix.Next
            If Ix.IsInvalid Then Return ParseError.NullParse(sx) ' A lone backslash (\) at the end of the text.
            If Ix <> "u"c Then Return ParseError.NullParse(sx) ' Doesn't have correct the start to an Unicode escape squence. (\c)
            Ix = Ix.Next
            Dim T As Token = New Esc.SeqHead(sx.To(Ix)) : txn += T
            ' OK. At this stage we hace the start of a escape sequence for a unicode characeter \u
            '
            If Ix.IsInvalid Then ParseError.NullParse(sx)
            ' Try and parse 4 HexaDecimal characters.
            Dim Hx = Tokens.Empty
            Dim count = 0
            While Ix.IsInvalid AndAlso count < 4
              T = Common.HexDigit.TryParse(Ix)
              If T.Kind = TokenKind.ParseError Then Exit While
              Hx = Common.AddThenNext(T, Hx, Ix)
              count += 1
            End While
            If count <> 4 AndAlso count <> 0 Then Return ParseError.NullParse(sx)
            Return New Esc.Sequence.Unicode(sx.To(Ix), txn + New HexDigits(Hx.First.Span.Start.To(Hx.Last.Span.Next), Hx))
          End Function

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
            Dim T As Token
            T = Backslash_UpperU(Ix) : If T.Kind = TokenKind.Backslash_UpperU Then Return T
            T = Backslash_LowerU(Ix) : If T.Kind = TokenKind.Backslash_LowerU Then Return T
            Return ParseError.NullParse(Ix)
          End Function

        End Class

        Public Class HexaDecimal : Inherits Esc.Sequence
          Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
            MyBase.New(TokenKind.Esc_Seq_Unicode, Span, Inner)
          End Sub
          Public Shared Shadows Function TryParse(Ix As Source.Position) As Token
            If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Unsupported(Ix, $"{Ix.Source.Kind} doesn't support escape sequences")
            If Ix.IsInvalid Then Return ParseError.EoT(Ix)
            If Ix <> "\"c Then Return ParseError.NullParse(Ix)
            Dim sx = Ix, txn = Tokens.Empty : Ix = Ix.Next
            If Ix.IsInvalid Then Return ParseError.EoT(Ix) ' A lone backslash (\) at the end of the text.
            If Ix <> "x"c Then Return ParseError.NullParse(sx) ' Doesn't have correct the start to an Unicode escape squence. (\c)
            Ix = Ix.Next
            Dim T As Token = New Esc.SeqHead(sx.To(Ix))
            txn += T
            If Ix.IsInvalid Then Return ParseError.EoT(Ix)
            Dim Hx = Tokens.Empty
            Dim count = 0
            While Ix.IsInvalid AndAlso count < 4
              T = Common.HexDigit.TryParse(Ix)
              If T.Kind = TokenKind.ParseError Then Exit While
              Hx = Common.AddThenNext(T, Hx, Ix)
              count += 1
            End While
            If count = 0 Then Return ParseError.NullParse(sx)
            Return New Esc.Sequence.HexaDecimal(sx.To(Ix), txn + New HexDigits(Hx.First.Span.Start.To(Hx.Last.Span.Next), Hx))
          End Function
        End Class
      End Class

    End Class

    End Class

  End Class