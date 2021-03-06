﻿Partial Public Class FormatString

  Public Class Common

    Public Class Whitespace : Inherits Token

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span?)
        MyBase.New(TokenKind.Whitespace, Span)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
        If Idx?.IsInvalid Then Return ParseError.Make.EoT(Idx)
        If (Idx <> " "c) Then Return ParseError.Make.NullParse(Idx)
        Return New Whitespace(Idx?.ToUnitSpan)
      End Function

    End Class

    Public Class Whitespaces : Inherits Token

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span?, Inner As Tokens)
        MyBase.New(TokenKind.Whitespaces, Span, Inner)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
        If Idx?.IsInvalid Then Return ParseError.Make.EoT(Idx)
        Dim Txn = Tokens.Empty, Sx = Idx
        While Idx?.IsValid
          Dim T = Whitespace.TryParse(Idx, DoingResync)
          If T.Kind = TokenKind.ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Idx)
        End While
        Dim s = Sx?.To(Idx)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return ParseError.Make.NullParse(Idx)
        Return New Whitespaces(s.Value, Txn)
      End Function

    End Class

    Public Class Digit : Inherits Token

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span?)
        MyBase.New(TokenKind.Digit, Span)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
        If Idx?.IsInvalid OrElse Idx?.Value.HasValue = False Then Return ParseError.Make.EoT(Idx)
        Select Case Idx.Value.Value
          Case "0"c To "9"c
            Return New Digit(Idx?.ToUnitSpan)
          Case Else
            Return ParseError.Make.NullParse(Idx)
        End Select
      End Function
      Private Shared ReadOnly _Two As New Numerics.BigInteger(2)
      Private Shared ReadOnly _Three As New Numerics.BigInteger(3)
      Private Shared ReadOnly _Four As New Numerics.BigInteger(4)
      Private Shared ReadOnly _Five As New Numerics.BigInteger(5)
      Private Shared ReadOnly _Six As New Numerics.BigInteger(6)
      Private Shared ReadOnly _Seven As New Numerics.BigInteger(7)
      Private Shared ReadOnly _Eight As New Numerics.BigInteger(8)
      Private Shared ReadOnly _Nine As New Numerics.BigInteger(9)

      Public Function GetValue() As Numerics.BigInteger?
        Select Case Me.Span?.Start.Value
          Case "0"c : Return Numerics.BigInteger.Zero
          Case "1"c : Return Numerics.BigInteger.One
          Case "2"c : Return _Two
          Case "3"c : Return _Three
          Case "4"c : Return _Four
          Case "5"c : Return _Five
          Case "6"c : Return _Six
          Case "7"c : Return _Seven
          Case "8"c : Return _Eight
          Case "9"c : Return _Nine
        End Select
        Return Nothing
      End Function

    End Class

    Public Class Digits : Inherits Token

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.Digits, Span, Inner)
      End Sub

      Private Shared ReadOnly _Ten As New Numerics.BigInteger(10)

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
        If Idx?.IsInvalid Then Return ParseError.Make.EoT(Idx)
        Dim Txn = Tokens.Empty()
        Dim Sx = Idx
        While Idx?.IsValid
          Dim T = Digit.TryParse(Idx, DoingResync)
          If T.Kind = TokenKind.ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Idx)
        End While
        Dim s = Sx?.To(Idx)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return ParseError.Make.NullParse(Idx)
        Return New Digits(s.Value, Txn)
      End Function

      Private _Value As Numerics.BigInteger?
      Private _First As Boolean = False

      <DebuggerStepperBoundary>
      Public Function GetValue() As Numerics.BigInteger?
        If Not _First Then
          Dim output As Numerics.BigInteger? = Numerics.BigInteger.Zero
          For Each d As FSDv2.FormatString.Common.Digit In Me.InnerTokens.GetEnumerator
            output = (_Ten * output) + d.GetValue
          Next
          _Value = output
          _First = True
        End If
        Return _Value
      End Function

    End Class

    Public Class HexDigit : Inherits Token

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span?)
        MyBase.New(TokenKind.HexDigit, Span)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
        If Ix?.IsInvalid OrElse Ix?.Value.HasValue = False Then Return ParseError.Make.EoT(Ix)
        Select Case Ix.Value.Value
          Case "0"c To "9"c, "a"c To "f"c, "A"c To "F"c
            Return New HexDigit(Ix?.ToUnitSpan)
          Case Else
            Return ParseError.Make.NullParse(Ix)
        End Select
      End Function

    End Class

    Public Class HexDigits : Inherits Token

      <DebuggerStepperBoundary>
      Friend Sub New(Span As Source.Span?, Inner As Tokens)
        MyBase.New(TokenKind.HexDigits, Span, Inner)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
        If Idx?.IsInvalid Then Return ParseError.Make.EoT(Idx)
        Dim Txn = Tokens.Empty()
        Dim Sx = Idx
        While Idx?.IsValid
          Dim T = HexDigit.TryParse(Idx, DoingResync)
          If TypeOf T Is ParseError Then Exit While
          Txn = Common.AddThenNext(T, Txn, Idx)
        End While
        Dim s = Sx?.To(Idx)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return ParseError.Make.NullParse(Idx)
        Return New HexDigits(s.Value, Txn)
      End Function

    End Class


    MustInherit Class Brace : Inherits Token

      <DebuggerStepperBoundary>
      Friend Sub New(Kind As TokenKind, Span As Source.Span?, Optional Inner As Tokens = Nothing)
        MyBase.New(Kind, Span, Inner)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
        If Idx?.IsInvalid OrElse Idx?.Value.HasValue = False Then Return ParseError.Make.EoT(Idx)
        Dim nx = Idx?.Next
        If Idx.Value.Value = "{"c Then
          If nx?.IsInvalid OrElse (nx <> "{"c) Then Return New Opening(Idx?.ToUnitSpan)
          Return New Brace.Esc.Opening(Idx?.To(nx?.Next), New Opening(Idx?.ToUnitSpan) + New Opening(nx?.ToUnitSpan))
        ElseIf Idx.Value = "}"c Then
          If nx?.IsInvalid OrElse (nx <> "}"c) Then Return New Closing(Idx?.ToUnitSpan)
          Return New Brace.Esc.Closing(Idx?.To(nx?.Next), New Closing(Idx?.ToUnitSpan) + New Closing(nx?.ToUnitSpan))
        Else
          Return ParseError.Make.NullParse(Idx)
        End If
      End Function

      Public Class Opening : Inherits Brace

        <DebuggerStepperBoundary>
        Friend Sub New(Span As Source.Span?)
          MyBase.New(TokenKind.Brace_Opening, Span)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Shadows Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
          If Idx?.IsInvalid Then Return ParseError.Make.EoT(Idx)
          Dim res = Brace.TryParse(Idx, DoingResync)
          If res.Kind <> TokenKind.Brace_Opening Then Return ParseError.Make.NullParse(Idx)
          Return res
        End Function

      End Class

      Public Class Closing : Inherits Brace


        <DebuggerStepperBoundary>
        Friend Sub New(Span As Source.Span?)
          MyBase.New(TokenKind.Brace_Closing, Span)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Shadows Function TryParse(Idx As Source.Position?, DoingResync As Boolean) As Token
          If Idx?.IsInvalid Then Return ParseError.Make.EoT(Idx)
          Dim res = Brace.TryParse(Idx, DoingResync)
          If res.Kind <> TokenKind.Brace_Closing Then Return ParseError.Make.NullParse(Idx)
          Return res
        End Function

      End Class

      Public Class Esc

        Public Class Opening : Inherits Brace

          <DebuggerStepperBoundary>
          Friend Sub New(Span As Source.Span?, Inner As Tokens)
            MyBase.New(TokenKind.Esc_Brace_Opening, Span, Inner)
          End Sub

          <DebuggerStepperBoundary>
          Public Shared Shadows Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
            If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix)
            Dim res = Brace.TryParse(Ix, DoingResync)
            If res.Kind <> TokenKind.Esc_Brace_Opening Then Return ParseError.Make.NullParse(Ix)
            Return res
          End Function

        End Class

        Public Class Closing : Inherits Brace

          <DebuggerStepperBoundary>
          Friend Sub New(Span As Source.Span?, Inner As Tokens)
            MyBase.New(TokenKind.Esc_Brace_Closing, Span, Inner)
          End Sub

          <DebuggerStepperBoundary>
          Public Shared Shadows Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
            If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix)
            Dim res = Brace.TryParse(Ix, DoingResync)
            If res.Kind <> TokenKind.Esc_Brace_Closing Then Return ParseError.Make.NullParse(Ix)
            Return res
          End Function

        End Class

      End Class

    End Class


    <DebuggerStepperBoundary>
    Public Shared Function AddThenNext(T As Token, Tx As Tokens, ByRef Ix As Source.Position?,
                                       Optional ByRef TextStart As Source.Position? = Nothing) As Tokens
      If TextStart IsNot Nothing Then Tx += New Text(TextStart.Value.To(Ix), Tokens.Empty) : TextStart = Nothing
      If T IsNot Nothing Then Ix = T.Span?.Next : Tx = Tx.Add(T)
      Return Tx
    End Function

    Public Class Esc

      Public Class SeqHead : Inherits Token

        <DebuggerStepperBoundary>
        Friend Sub New(Span As Source.Span?, Optional Inner As Tokens = Nothing)
          MyBase.New(TokenKind.Esc_Seq_Head, Span, Inner)
        End Sub

      End Class

      Public MustInherit Class Sequence : Inherits Token

        <DebuggerStepperBoundary>
        Friend Sub New(Kind As TokenKind, Span As Source.Span?, Optional Inner As Tokens = Nothing)
          MyBase.New(Kind, Span, Inner)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
          If Ix?.Src.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Make.Unsupported(Ix, $"{Ix?.Src.Kind} doesn't support escape sequences")
          If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If Ix <> "\"c Then Return ParseError.Make.NullParse(Ix)
          Dim sx = Ix, nx = Ix?.Next
          If nx?.IsInvalid Then Return ParseError.Make.EoT(nx)
          Select Case nx
            Case "'"c, "\"c, "0"c, "a"c,
                 "b"c, "f"c, "n"c, "r"c,
                 "t"c, "v"c, """"c
              Return New Simple(Ix?.To(nx?.Next))
            Case "x"c
              Return HexaDecimal.TryParse(Ix, DoingResync)
            Case "u"c, "U"c
              Return Unicode.TryParse(Ix, DoingResync)
          End Select
          Return ParseError.Make.Unsupported(sx?.To(nx?.Next), "")
        End Function

        Public Class Simple : Inherits Esc.Sequence

          <DebuggerStepperBoundary>
          Friend Sub New(Span As Source.Span?, Optional Inner As Tokens = Nothing)
            MyBase.New(TokenKind.Esc_Seq_Simple, Span, Inner)
          End Sub
        End Class

        Public Class Unicode : Inherits Esc.Sequence

          <DebuggerStepperBoundary>
          Friend Sub New(Span As Source.Span?, Optional Inner As Tokens = Nothing)
            MyBase.New(TokenKind.Esc_Seq_Unicode, Span, Inner)
          End Sub

          <DebuggerStepperBoundary>
          Private Shared Function Parse_HexDigits(sx As Source.Position?, ix As Source.Position?, RequiredLength As Integer, DoingResync As Boolean) As Token
            Dim T As Token, Hx = Tokens.Empty
            While ix?.IsValid AndAlso Hx.Count < RequiredLength
              T = Common.HexDigit.TryParse(ix, DoingResync)
              If T.Kind = TokenKind.ParseError Then Exit While
              Hx = Common.AddThenNext(T, Hx, ix)
            End While
            If Hx.Count <> RequiredLength Then Return ParseError.Make.Invalid(sx?.To(ix), Hx, $"Expecting {RequiredLength} Hexadecimal Digits")
            Return New HexDigits(Hx.First.Span?.Start?.To(Hx.Last.Span?.Next), Hx)
          End Function

          <DebuggerStepperBoundary>
          Private Shared Function Backslash_UpperU(Ix As Source.Position?, DoingResync As Boolean) As Token
            ' unicode_escape_sequence ::= \U hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
            Dim Txn = Tokens.Empty, sx = Ix
            ' OK. At this stage we hace the start of a escape sequence for a unicode characeter \U
            If Ix?.IsInvalid Then Return ParseError.Make.Invalid(sx?.To(Ix), Txn, "Expecting 8 Hexadecimal Digits")
            ' Try and parse 4 HexaDecimal characters.
            Return Parse_HexDigits(sx, Ix, 8, DoingResync)
          End Function

          Private Shared Function Backslash_LowerU(Ix As Source.Position?, DoingResync As Boolean) As Token
            ' unicode_escape_sequence ::= \u hex_digit hex_digit hex_digit hex_digit
            Dim Txn = Tokens.Empty, sx = Ix
            ' OK. At this stage we hace the start of a escape sequence for a unicode characeter \u
            If Ix?.IsInvalid Then Return ParseError.Make.Invalid(sx?.To(Ix), Txn, "Expecting 4 Hexadecimal Digits")
            ' Try and parse 4 HexaDecimal characters.
            Return Parse_HexDigits(sx, Ix, 4, DoingResync)
          End Function

          <DebuggerStepperBoundary>
          Public Shared Shadows Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
            '
            ' unicode_escape_sequence ::= \u hex_digit hex_digit hex_digit hex_digit
            '
            If Ix?.Src.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Make.Unsupported(Ix, $"{Ix?.Src.Kind} doesn't support escape sequences")
            If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix) ' Already at the end of the text.
            If Ix <> "\"c Then Return ParseError.Make.NullParse(Ix) ' Character is not a blackslash
            Dim sx = Ix ' Start Index of this potential token.
            Dim txn = Tokens.Empty : Ix = Ix?.Next
            If Ix?.IsInvalid Then Return ParseError.Make.NullParse(sx) ' A lone backslash (\) at the end of the text.
            If Ix = "u"c Then
              txn = FormatString.Common.AddThenNext(New Esc.SeqHead(sx?.To(Ix?.Next)), txn, Ix)
              Dim hx = Backslash_LowerU(Ix, DoingResync)
              If hx.Kind = TokenKind.HexDigits Then Return New Unicode(sx?.To(hx.Span?.Next), txn + hx)
              Return ParseError.Make.Invalid(sx?.To(hx.Span?.Next), txn + hx, "")
            ElseIf Ix = "U"c Then
              txn = FormatString.Common.AddThenNext(New Esc.SeqHead(sx?.To(Ix?.Next)), txn, Ix)
              Dim hx = Backslash_UpperU(Ix, DoingResync)
              If hx.Kind = TokenKind.HexDigits Then Return New Unicode(sx?.To(hx.Span?.Next), txn + hx)
              Return ParseError.Make.Invalid(sx?.To(hx.Span?.Next), txn + hx, "")
            End If
            Return ParseError.Make.NullParse(sx) ' Doesn't have correct the start to an Unicode escape squence. (\c)
          End Function

        End Class

        Public Class HexaDecimal : Inherits Esc.Sequence

          <DebuggerStepperBoundary>
          Friend Sub New(Span As Source.Span?, Optional Inner As Tokens = Nothing)
            MyBase.New(TokenKind.Esc_Seq_Unicode, Span, Inner)
          End Sub

          <DebuggerStepperBoundary>
          Public Shared Shadows Function TryParse(Ix As Source.Position?, DoingResync As Boolean) As Token
            If Ix?.Src.Kind <> Source.SourceKind.CS_Standard Then Return ParseError.Make.Unsupported(Ix, $"{Ix?.Src.Kind} doesn't support escape sequences")
            If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix)
            If Ix <> "\"c Then Return ParseError.Make.NullParse(Ix)
            Dim sx = Ix, txn = Tokens.Empty : Ix = Ix?.Next
            If Ix?.IsInvalid Then Return ParseError.Make.EoT(Ix) ' A lone backslash (\) at the end of the text.
            If Ix <> "x"c Then Return ParseError.Make.NullParse(sx) ' Doesn't have correct the start to an Unicode escape squence. (\c)
            Ix = Ix?.Next
            Dim T As Token = New Esc.SeqHead(sx?.To(Ix))
            txn += T
            If Ix?.IsInvalid Then Return New ParseError.[Partial](TokenKind.Esc_Seq_HexaDecimal, sx?.To(Ix), txn, "Expecting 4 Hexadecimal Digits")
            Dim Hx = Tokens.Empty
            While Ix?.IsValid AndAlso Hx.Count < 4
              T = Common.HexDigit.TryParse(Ix, DoingResync)
              If T.Kind = TokenKind.ParseError Then Exit While
              Hx = Common.AddThenNext(T, Hx, Ix)
            End While
            If Hx.Count = 0 Then
              Return New ParseError.[Partial](TokenKind.Esc_Seq_HexaDecimal, sx?.To(Ix), txn + Hx, "Expecting 4 Hexadecimal Digits")
            End If
            Return New Esc.Sequence.HexaDecimal(sx?.To(Ix), txn + New HexDigits(Hx.First.Span?.Start?.To(Hx.Last.Span?.Next), Hx))
          End Function

        End Class

      End Class

    End Class

    End Class

  End Class