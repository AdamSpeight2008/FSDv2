Imports FSD

Module Module1

  Sub Main()
    '           0         1         2         3         4         5         6         7         8
    '           012345678901234567890123456789012345678901234567890123456789012345678901234567890
    Dim Text = "{}"
    'Text = "}  {1 , -123 : {{ABC}} }  {1 , -123 : {{ABC}} {} "
    'Text = " {0001234567   } {0001234567, 0007654321} {0001234567, -0007654321}{0001234567, 0007654321 : XX44} {0001234567, -7654321 : XX44} {0001234567 : XX44} "
    Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Standard)
    Dim Ix = TheSource.First
    'Dim T0 = FormatString.ArgHole.Index.TryParse(Ix)
    'Dim T1 = FormatString.ArgHole.Align.TryParse(T0.Span.Next)
    'Dim T2 = FormatString.ArgHole.Format.TryParse(T1.Span.Next)
    ' Dim T3 = ArgHole.TryParse(Ix)
    'Dim Txt = TryCast(T3.Inner.Tokens(3).Inner.Tokens(1).Inner.Tokens(0), ArgHole.Text)


    'Dim rp0 = New ResyncPoint(AddressOf FormatString.Common.Digits.TryParse, Nothing) + New ResyncPoint(AddressOf FormatString.ArgHole.Align.Comma.TryParse, Nothing) +
    '          New ResyncPoint(AddressOf FormatString.ArgHole.Format.Colon.TryParse, Nothing) + New ResyncPoint(AddressOf FormatString.Common.Brace.Closing.TryParse, Nothing)
    '  Dim res = rp0.TryToResync(Ix)
    Dim sw = Diagnostics.Stopwatch.StartNew
    Dim T4 = FormatString.TryParse(Ix)
    sw.Stop()
    Console.WriteLine(sw.Elapsed.TotalMilliseconds.ToString)

  End Sub

End Module



Public Structure ResyncPoint
  Public ReadOnly Property TryParse As Func(Of Source.Position, Token)

  Public Sub New(TryParse As Func(Of Source.Position, Token))
    Me.TryParse = TryParse
  End Sub

  Public Shared Operator +(Rp0 As ResyncPoint, Rp1 As ResyncPoint) As ResyncPoints
    Return ResyncPoints.CreateNew(Rp0, Rp1)
  End Operator
End Structure

Public Structure ResyncPoints
  'Public Shared ReadOnly Property Empty As New ResyncPoints()
  Private _ResyncPoints As ResyncPoint() '= Array.Empty(Of ResyncPoint)

  Private Sub New(Optional ResyncPoints As IEnumerable(Of ResyncPoint) = Nothing)
    'If ResyncPoints IsNot Nothing Then
    Me._ResyncPoints = ResyncPoints.ToArray
    'End If
  End Sub

  Public Shared Operator +(Rpx As ResyncPoints, Rp As ResyncPoint) As ResyncPoints
    Return New ResyncPoints(Rpx._ResyncPoints.AsEnumerable.Concat(Enumerable.Repeat(Rp, 1)))
  End Operator

  Public Shared Function CreateNew(Rp0 As ResyncPoint, Rp1 As ResyncPoint) As ResyncPoints
    Return New ResyncPoints(Enumerable.Repeat(Rp0, 1).Concat(Enumerable.Repeat(Rp1, 1)))
  End Function

  Public Function TryToResync(ix As Source.Position) As Token
    Dim c = _ResyncPoints.Count - 1
    If c < 0 Then Return Nothing
    Dim sx = ix
    While ix.IsValid
      For i = 0 To c
        Dim q = _ResyncPoints(i).TryParse(ix)
        If q IsNot Nothing Then Return q
      Next
      ix = ix.Next
    End While
    Return Nothing
  End Function

  'Public Function TryToResync(ix As Source.Position) As Token
  '  If Me._ResyncPoints.Count = 0 Then Return Nothing
  '  Dim tt = _ResyncPoints.AsParallel.Select(
  '    Function(rp As ResyncPoint)
  '      Dim _rp = rp
  '      Return Task.Run(Of Token)(Function()
  '                                  Dim lx = ix
  '                                  Dim res As Token
  '                                  While lx.IsValid
  '                                    res = _rp.TryParse(lx)
  '                                    If res IsNot Nothing Then Return res
  '                                    lx = lx.Next
  '                                  End While
  '                                  Return Nothing
  '                                End Function)
  '    End Function).ToArray
  '  Dim q = Task.WhenAll(Of Token)(tt)
  '  Dim output = tt.AsParallel.AsOrdered.FirstOrDefault(Function(rp) rp.Result IsNot Nothing)?.Result
  '  Return output
  'End Function
End Structure



Public Structure Source
  Friend ReadOnly Property ID As Guid
  Public ReadOnly Property Text As String
  Public ReadOnly Property Length As Integer
  Public ReadOnly Property Kind As SourceKind

  Private Sub New(Text As String, Kind As SourceKind)
    Me.ID = Guid.NewGuid
    Me.Text = If(Text, String.Empty)
    Me.Length = Me.Text.Length
  End Sub

  Public Function First() As Position?
    If Length = 0 Then Return Nothing
    Return Position.Create(Me, 0)
  End Function

  Default Friend ReadOnly Property Chars(Index As Integer) As Char?
    Get
      Return If((0 <= Index) AndAlso (Index < Me.Length), New Char?(Text(Index)), Nothing)
    End Get
  End Property

  Public Shared Function Create(Text As String, SourceKind As SourceKind) As Source
    Return New Source(Text, SourceKind)
  End Function

  Public Shared Operator =(S0 As Source, S1 As Source) As Boolean
    Return (S0.ID = S1.ID)
  End Operator
  Public Shared Operator <>(S0 As Source, S1 As Source) As Boolean
    Return (S0.ID <> S1.ID)
  End Operator

  Public Enum SourceKind As Integer
    VB_Standard
    CS_Standard
    CS_Verbatum
  End Enum

  <DebuggerDisplay("{Index}=[{Value}]")>
  Public Structure Position
    Public ReadOnly Property Source As Source
    Public ReadOnly Property Index As Integer
    Public ReadOnly Property IsValid As Boolean
    Public ReadOnly Property IsInvalid As Boolean

    Private Sub New(Source As Source, Index As Integer)
      Me.Source = Source : Me.Index = Bound(Index, 0, Source.Length) : Me.IsValid = IsWithin(Index, 0, Source.Length) : Me.IsInvalid = Not IsValid
    End Sub

    Public Function Value() As Char?
      Return Source(Index)
    End Function

    Private Function IsWithin(Value As Integer, Limit0 As Integer, Limit1 As Integer) As Boolean
      Return (Limit0 <= Value) AndAlso (Value < Limit1)
    End Function

    Private Function Bound(value As Integer, Limit0 As Integer, Limit1 As Integer) As Integer
      If value < Limit0 Then
        If Limit0 = Integer.MinValue Then Return Integer.MinValue Else Return Limit0 - 1
      ElseIf value > Limit1 Then
        If Limit1 = Integer.MaxValue Then Return Integer.MaxValue Else Return Limit1
      Else
        Return value
      End If
    End Function

    Public Function [Next]() As Position
      Return New Position(Me.Source, Index + 1)
    End Function
    Public Function AddOffset(Offset As Integer) As Position
      Return New Position(Me.Source, Index + Offset)
    End Function
    Public Function [To](p As Position) As Span?
      If Me.Source <> p.Source Then Return Nothing
      Return Source.Span.From(Me, p)
    End Function

    Public Shared Operator =(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value = Ch)
    End Operator
    Public Shared Operator >(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value > Ch)
    End Operator
    Public Shared Operator <(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value < Ch)
    End Operator
    Public Shared Operator <=(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value <= Ch)
    End Operator
    Public Shared Operator >=(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value >= Ch)
    End Operator
    Public Shared Operator <>(ByVal P As Position, Ch As Char) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso (P.Value.Value <> Ch)
    End Operator


    Public Shared Operator =(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch = P.Value.Value
    End Operator
    Public Shared Operator >(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch > P.Value.Value
    End Operator
    Public Shared Operator <(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch < P.Value.Value
    End Operator
    Public Shared Operator <=(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch <= P.Value.Value
    End Operator
    Public Shared Operator >=(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch >= P.Value.Value
    End Operator
    Public Shared Operator <>(Ch As Char, P As Position) As Boolean
      Return Not String.IsNullOrEmpty(P.Source.Text) AndAlso P.Value.HasValue AndAlso Ch <> P.Value.Value
    End Operator

    Public Shared Operator =(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index = P1.Index
    End Operator
    Public Shared Operator <>(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index <> P1.Index
    End Operator
    Public Shared Operator >=(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index >= P1.Index
    End Operator
    Public Shared Operator <=(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index <= P1.Index
    End Operator
    Public Shared Operator <(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index < P1.Index
    End Operator
    Public Shared Operator >(P0 As Position, P1 As Position) As Boolean
      Return (P0.Source = P1.Source) AndAlso P0.Index > P1.Index
    End Operator

    Public Function ToZeroSpan() As Span
      Return Source.Span.Create_ZeroSpan(Me)
    End Function
    Public Function ToUnitSpan() As Span
      Return Source.Span.Create_UnitSpan(Me)
    End Function

    Public Shared Function Create(Source As Source, Index As Integer) As Position
      Return New Position(Source, Index)
    End Function

    Public Overrides Function ToString() As String
      Return Me.Index.ToString
    End Function
  End Structure

  <DebuggerDisplay("({Start.Index},{Size}) =[{Me.Text}]")>
  Public Structure Span
    Public ReadOnly Property Start As Position
    Public ReadOnly Property Size As Integer

    Private Sub New(Start As Position, Size As Integer)
      Me.Start = Start : Me.Size = Size
    End Sub

    Public Shared Function Create(P As Position, Size As Integer) As Span
      Return New Span(P, Size)
    End Function
    Public Shared Function Create_ZeroSpan(p As Position) As Span
      Return New Span(p, 0)
    End Function
    Public Shared Function Create_UnitSpan(p As Position) As Span
      Return New Span(p, 1)
    End Function

    Public Function [Next]() As Source.Position
      Return Source.Position.Create(Me.Start.Source, Me.Start.Index + Me.Size)
    End Function

    Public Shared Function [From](p0 As Source.Position, p1 As Source.Position) As Span?
      If p0.Source <> p1.Source Then Return New Span?
      Return New Span(p0, p1.Index - p0.Index)
    End Function

    Public Overrides Function ToString() As String
      Return $"{Start} : {Size}"
    End Function

    Iterator Function GetChars() As IEnumerable(Of Char?)
      Dim cx = Start
      Dim fs = Me.Next
      While cx < fs
        Yield cx.Value
        cx = cx.Next
      End While
    End Function

    Public Function Text() As String
      Dim r = New String(GetChars.Where(Function(c) c.HasValue).Select(Function(c) c.Value).ToArray)
      Return r
    End Function
  End Structure

End Structure

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

Public Class Tokens
  Public Shared ReadOnly Property Empty As Tokens = New Tokens()
  Private ReadOnly Property _Tokens As Token()
  Private ReadOnly Property Count As Integer

  Private Sub New(Optional Tokens As IEnumerable(Of Token) = Nothing)
    If Tokens Is Nothing Then Me._Tokens = Array.Empty(Of Token) Else Me._Tokens = Tokens.ToArray
    Me.Count = _Tokens.Count
  End Sub

  Public Shared Function Create(T0 As Token, T1 As Token) As Tokens
    If (T0 Is Nothing) OrElse (T1 Is Nothing) Then Throw New Exception
    Return New Tokens(Enumerable.Repeat(T0, 1).Concat(Enumerable.Repeat(T1, 1)))
  End Function

  Public Function GetEnumerator() As IEnumerable(Of Token)
    Return _Tokens.AsEnumerable
  End Function

  Public Function Add(Token As Token) As Tokens
    If Token Is Nothing Then Return New Tokens(_Tokens) Else Return New Tokens(_Tokens.Concat(Enumerable.Repeat(Token, 1)))
  End Function

  Public Function First() As Token
    Return _Tokens.FirstOrDefault
  End Function
  Public Function Last() As Token
    Return _Tokens.LastOrDefault
  End Function

  Public Shared Operator +(Tx As Tokens, T As Token) As Tokens
    Return New Tokens(Tx._Tokens.Concat(Enumerable.Repeat(T, 1)))
  End Operator

  Public Shared Operator +(T As Token, Tx As Tokens) As Tokens
    Return New Tokens(Enumerable.Repeat(T, 1).Concat(Tx._Tokens))
  End Operator

  Default Public ReadOnly Property Tokens(ByVal Index As Integer) As Token
    Get
      If Index < 0 OrElse Index >= Count Then Return Nothing
      Return _Tokens(Index)
    End Get
  End Property
End Class

Public Class FormatString : Inherits Token

  Friend Sub New(Span As Source.Span, Inner As Tokens)
    MyBase.New(Span, Inner)
  End Sub

  Public Shared Function TryParse(Ix As Source.Position) As Token
    If Ix.IsInvalid Then Return Nothing
    Dim sx = Ix
    Dim Txn = Tokens.Empty
    Dim T As Token
    Dim TextStart As Source.Position? = Nothing
    While Ix.IsValid
      T = Common.Brace.TryParse(Ix)
      Select Case True
        Case TypeOf T Is Common.Brace.Esc.Opening,
             TypeOf T Is Common.Brace.Esc.Closing
          Txn = Common.AddThenNext(T, Txn, Ix, TextStart)
        Case TypeOf T Is Common.Brace.Closing
          Txn = Common.AddThenNext(New ParseError(T.Span, ParseError.Reason.Invalid), Txn, Ix, TextStart)
        Case TypeOf T Is Common.Brace.Opening
          Dim res = ArgHole.TryParse(Ix)
          If res IsNot Nothing Then
            Txn = Common.AddThenNext(res, Txn, Ix, TextStart)
          Else
            Txn = Common.AddThenNext(New ParseError(Ix.ToUnitSpan, ParseError.Reason.UnexpectedCharacter), Txn, Ix, TextStart)
          End If
        Case Else
          If TextStart Is Nothing Then TextStart = New Source.Position?(Ix)
          Ix = Ix.Next
      End Select
    End While
    Txn = Common.AddThenNext(Nothing, Txn, Ix, TextStart)
    Return New FormatString(sx.To(Ix), Txn)
  End Function

  Public Class Symbol : Inherits Token

    Friend Sub New(Span As Source.Span)
      MyBase.New(Span)
    End Sub

    Public Class BackSlash : Inherits Symbol

      Friend Sub New(Span As Source.Span)
        MyBase.New(Span)
      End Sub
    End Class
  End Class


  Public Class Common

    Public Class Whitespace : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Whitespace
        If Ix.IsInvalid OrElse (Ix <> " "c) Then Return Nothing
        Return New Whitespace(Ix.ToUnitSpan)
      End Function

    End Class

    Public Class Whitespaces : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Whitespaces
        If Ix.IsInvalid Then Return Nothing
        Dim Txn = Tokens.Empty
        Dim Sx = Ix
        While Ix.IsValid
          Dim T = Whitespace.TryParse(Ix)
          If T Is Nothing Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix)

        End While
        Dim s = Source.Span.From(Sx, Ix)
        If s.HasValue = False Then Return Nothing
        Return New Whitespaces(s.Value, Txn)
      End Function

    End Class

    Public Class Digit : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Digit
        If Ix.IsInvalid Then Return Nothing
        If Ix.Value.HasValue = False Then Return Nothing
        Select Case Ix.Value.Value
          Case "0"c To "9"c
            Return New Digit(Ix.ToUnitSpan)
          Case Else
            Return Nothing
        End Select
      End Function

    End Class

    Public Class Digits : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Digits
        If Ix.IsInvalid Then Return Nothing
        Dim Txn = Tokens.Empty()
        Dim Sx = Ix
        While Ix.IsValid
          Dim T = Digit.TryParse(Ix)
          If T Is Nothing Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix)
        End While
        Dim s = Sx.To(Ix)
        If s.HasValue = False OrElse s.Value.Size = 0 Then Return Nothing
        Return New Digits(s.Value, Txn)
      End Function

    End Class

    Public Class HexDigit : Inherits Token

      Private Sub New(Span As Source.Span)
        MyBase.New(Span)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As HexDigit
        If Ix.IsInvalid Then Return Nothing
        If Ix.Value.HasValue = False Then Return Nothing
        Select Case Ix.Value.Value
          Case "0"c To "9"c, "a"c To "f"c, "A"c To "F"c
            Return New HexDigit(Ix.ToUnitSpan)
          Case Else
            Return Nothing
        End Select
      End Function

    End Class

    Public Class HexDigits : Inherits Token

      Friend Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(Span, Inner)
      End Sub

      'Public Shared Function TryParse(Ix As Source.Position) As HexDigits
      '  If Ix.IsInvalid Then Return Nothing
      '  Dim Txn = Tokens.Empty()
      '  Dim Sx = Ix
      '  While Ix.IsValid
      '    Dim T = HexDigit.TryParse(Ix)
      '    If T Is Nothing Then Exit While
      '    Txn = Common.AddThenNext(T, Txn, Ix)
      '  End While
      '  Dim s = Sx.To(Ix)
      '  If s.HasValue = False OrElse s.Value.Size = 0 Then Return Nothing
      '  Return New HexDigits(s.Value, Txn)
      'End Function

    End Class


    MustInherit Class Brace : Inherits Token

      Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
        MyBase.New(Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return Nothing
        If Ix.Value.HasValue = False Then Return Nothing
        Dim nx = Ix.Next
        If Ix.Value.Value = "{"c Then
          If nx.IsInvalid OrElse (nx <> "{") Then Return New Opening(Ix.ToUnitSpan)
          Return New Brace.Esc.Opening(Ix.To(nx.Next), New Opening(Ix.ToUnitSpan) + New Opening(nx.ToUnitSpan))
        ElseIf Ix.Value = "}"c Then
          If nx.IsInvalid OrElse (nx <> "}") Then Return New Closing(Ix.ToUnitSpan)
          Return New Brace.Esc.Closing(Ix.To(nx.Next), New Closing(Ix.ToUnitSpan) + New Closing(nx.ToUnitSpan))
        Else
          Return Nothing
        End If
      End Function

      Public Class Opening : Inherits Brace

        Friend Sub New(Span As Source.Span)
          MyBase.New(Span)
        End Sub

        Public Shared Shadows Function TryParse(Ix As Source.Position) As Opening
          Return TryCast(Brace.TryParse(Ix), Opening)
        End Function

      End Class

      Public Class Closing : Inherits Brace

        Friend Sub New(Span As Source.Span)
          MyBase.New(Span)
        End Sub

        Public Shared Shadows Function TryParse(Ix As Source.Position) As Closing
          Return TryCast(Brace.TryParse(Ix), Closing)
        End Function

      End Class

      Public Class Esc

        Public Class Opening : Inherits Brace

          Friend Sub New(Span As Source.Span, Inner As Tokens)
            MyBase.New(Span, Inner)
          End Sub

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Esc.Opening
            Return TryCast(Brace.TryParse(Ix), Esc.Opening)
          End Function

        End Class

        Public Class Closing : Inherits Brace

          Friend Sub New(Span As Source.Span, Inner As Tokens)
            MyBase.New(Span, Inner)
          End Sub

          Public Shared Shadows Function TryParse(Ix As Source.Position) As Esc.Closing
            Return TryCast(Brace.TryParse(Ix), Esc.Closing)
          End Function

        End Class

        Public Class SeqHead : Inherits Token
          Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
            MyBase.New(Span, Inner)
          End Sub
        End Class

        Public MustInherit Class Sequence : Inherits Token
          Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
            MyBase.New(Span, Inner)
          End Sub

          Public Shared Function TryParse(Ix As Source.Position) As Esc.Sequence
            If Ix.IsInvalid Then Return Nothing
            If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return Nothing
            Dim Txn = Tokens.Empty
            If Ix <> "\"c Then Return Nothing
            Dim nx = Ix.Next
            If nx.IsInvalid Then Return Nothing
            Dim T As Token
            Select Case nx
              Case "'"c, """"c, "\"c, "0"c,
                   "a"c, "b"c, "f"c, "n"c,
                   "r"c, "t"c, "v"c
                Return New Simple(Ix.To(nx.Next))
              Case "x"c
                T = HexaDecimal.TryParse(Ix)
              Case "u"c
                T = Unicode.TryParse(Ix)
            End Select
            Return Nothing
          End Function

          Public Class Simple : Inherits Esc.Sequence
            Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
              MyBase.New(Span, Inner)
            End Sub
          End Class

          Public Class Unicode : Inherits Esc.Sequence
            Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
              MyBase.New(Span, Inner)
            End Sub

            Private Shared Function Backslash_UpperU(Ix As Source.Position) As Esc.Sequence
              '
              ' unicode_escape_sequence ::= \U hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
              '
              If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return Nothing
              If Ix.IsInvalid Then Return Nothing 'Already at the end of the text.
              If Ix <> "\"c Then Return Nothing ' Character is not a blackslash
              Dim sx = Ix ' Start Index of this potential token.
              Dim txn = Tokens.Empty : Ix = Ix.Next
              If Ix.IsInvalid Then Return Nothing ' A lone backslash (\) at the end of the text.
              If Ix <> "U"c Then Return Nothing ' Doesn't have correct the start to an Unicode escape squence. (\U)
              Ix = Ix.Next
              Dim T As Token = New Esc.SeqHead(sx.To(Ix)) : txn += T
              ' OK. At this stage we hace the start of a escape sequence for a unicode characeter \u
              '
              ' unicode_escape_sequence ::= \U hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
              Dim Hx = Tokens.Empty
              Dim count = 0
              While Ix.IsInvalid AndAlso count < 8
                T = Common.HexDigit.TryParse(Ix)
                If T Is Nothing Then Exit While
                Hx = Common.AddThenNext(T, Hx, Ix)
                count += 1
              End While
              If count <> 8 AndAlso count <> 0 Then Return Nothing
              Return New Esc.Sequence.Unicode(sx.To(Ix), txn + New HexDigits(Hx.First.Span.Start.To(Hx.Last.Span.Next), Hx))

            End Function
            Private Shared Function Backslash_LowerU(Ix As Source.Position) As Esc.Sequence
              '
              ' unicode_escape_sequence ::= \u hex_digit hex_digit hex_digit hex_digit
              '
              If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return Nothing
              If Ix.IsInvalid Then Return Nothing 'Already at the end of the text.
              If Ix <> "\"c Then Return Nothing ' Character is not a blackslash
              Dim sx = Ix ' Start Index of this potential token.
              Dim txn = Tokens.Empty : Ix = Ix.Next
              If Ix.IsInvalid Then Return Nothing ' A lone backslash (\) at the end of the text.
              If Ix <> "u"c Then Return Nothing ' Doesn't have correct the start to an Unicode escape squence. (\c)
              Ix = Ix.Next
              Dim T As Token = New Esc.SeqHead(sx.To(Ix)) : txn += T
              ' OK. At this stage we hace the start of a escape sequence for a unicode characeter \u
              '
              If Ix.IsInvalid Then Return Nothing
              ' 
              Dim Hx = Tokens.Empty
              Dim count = 0
              While Ix.IsInvalid AndAlso count < 4
                T = Common.HexDigit.TryParse(Ix)
                If T Is Nothing Then Exit While
                Hx = Common.AddThenNext(T, Hx, Ix)
                count += 1
              End While
              If count <> 4 AndAlso count <> 0 Then Return Nothing
              Return New Esc.Sequence.Unicode(sx.To(Ix), txn + New HexDigits(Hx.First.Span.Start.To(Hx.Last.Span.Next), Hx))

            End Function

            Public Shared Shadows Function TryParse(Ix As Source.Position) As Esc.Sequence
              Dim T As Token
              T = Backslash_UpperU(Ix) : If T IsNot Nothing Then Return T
              T = Backslash_LowerU(Ix) : If T IsNot Nothing Then Return T
              Return Nothing
            End Function
          End Class

          Public Class HexaDecimal : Inherits Esc.Sequence
            Friend Sub New(Span As Source.Span, Optional Inner As Tokens = Nothing)
              MyBase.New(Span, Inner)
            End Sub
            Public Shared Shadows Function TryParse(Ix As Source.Position) As Esc.Sequence.HexaDecimal
              If Ix.Source.Kind <> Source.SourceKind.CS_Standard Then Return Nothing
              If Ix.IsInvalid Then Return Nothing
              If Ix <> "\"c Then Return Nothing
              Dim sx = Ix
              Dim txn = Tokens.Empty
              Ix = Ix.Next
              If Ix.IsInvalid Then Return Nothing ' A lone backslash (\) at the end of the text.
              If Ix <> "x"c Then Return Nothing ' Doesn't have correct the start to an Unicode escape squence. (\c)
              Ix = Ix.Next
              Dim T As Token = New Esc.SeqHead(sx.To(Ix))
              txn += T
              If Ix.IsInvalid Then Return Nothing
              Dim Hx = Tokens.Empty
              Dim count = 0
              While Ix.IsInvalid AndAlso count < 4
                T = Common.HexDigit.TryParse(Ix)
                If T Is Nothing Then Exit While
                Hx = Common.AddThenNext(T, Hx, Ix)
                count += 1
              End While
              If count = 0 Then Return Nothing
              Return New Esc.Sequence.HexaDecimal(sx.To(Ix), txn + New HexDigits(Hx.First.Span.Start.To(Hx.Last.Span.Next), Hx))
            End Function
          End Class

        End Class

      End Class

    End Class

    Public Shared Function AddThenNext(T As Token, Tx As Tokens, ByRef Ix As Source.Position, Optional ByRef TextStart As Source.Position? = Nothing) As Tokens
      If TextStart IsNot Nothing Then Tx += New Text(TextStart.Value.To(Ix), Tokens.Empty) : TextStart = Nothing
      If T IsNot Nothing Then
        Ix = T.Span.Next
        Tx = Tx.Add(T)
      End If
      Return Tx
    End Function

  End Class

  Public Class ArgHole : Inherits Token

    Private Sub New(Span As Source.Span, Inner As Tokens)
      MyBase.New(Span, Inner)
    End Sub

    Public Shared Function TryParse(Ix As Source.Position) As Token
      If Ix.IsInvalid Then Return Nothing
      Dim T As Token = Common.Brace.Opening.TryParse(Ix)
      If T Is Nothing Then Return Nothing
      Dim sx = Ix
      Dim txn = Tokens.Empty : txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Index.TryParse(Ix) : If T Is Nothing Then Return Nothing
      txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Align.TryParse(Ix)
      If T IsNot Nothing Then txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Format.TryParse(Ix)
      If T IsNot Nothing Then txn = Common.AddThenNext(T, txn, Ix)
      T = Common.Brace.Closing.TryParse(Ix)
      If T Is Nothing Then Return Nothing
      txn = Common.AddThenNext(T, txn, Ix)
      Return New ArgHole(sx.To(Ix), txn)
    End Function

    Public Class Index : Inherits Token
      Private Shared RPX As ResyncPoints = New ResyncPoint(AddressOf Common.Digits.TryParse) + New ResyncPoint(AddressOf Align.Comma.TryParse) +
                                           New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)


      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return Nothing
        Dim Txn = Tokens.Empty
        Dim T As Token
        Dim sx = Ix
        T = Common.Digits.TryParse(Ix)
        If T IsNot Nothing Then
          Txn = Common.AddThenNext(T, Txn, Ix)
IsThereTrailingWhitespace:
          T = Common.Whitespaces.TryParse(Ix)
          If T IsNot Nothing Then Txn = Common.AddThenNext(T, Txn, Ix) : Return New Index(Source.Span.From(sx, Ix), Txn)
        Else
          Dim r = RPX.TryToResync(Ix)
          Select Case True
            Case r Is Nothing : Return Nothing
            Case TypeOf r Is Common.Digits
              Dim tmp As New ParseError(sx.To(r.Span.Start), Nothing)
              Txn = Txn + tmp + r
              Ix = r.Span.Next
              GoTo IsThereTrailingWhitespace
          End Select
        End If
        Return Nothing
      End Function

    End Class

    Public Class Align : Inherits Token
      Private Shared RPX0 As ResyncPoints = New ResyncPoint(AddressOf Head.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
      Private Shared RPX1 As ResyncPoints = New ResyncPoint(AddressOf Body.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Align
        Dim Txn = Tokens.Empty
        Dim sx = Ix
        Dim _Head = Head.TryParse(Ix)
        If _Head Is Nothing Then GoTo TryToResyncHead
        Txn = Common.AddThenNext(_Head, Txn, Ix)
IsThereABody:
        Dim _Body = Body.TryParse(Ix)
        If _Body Is Nothing Then GoTo TryToResyncBody
        Txn = Common.AddThenNext(_Body, Txn, Ix)
AfterBody:
        Return New Align(Txn.First.Span.Start.To(Txn.Last.Span.Next), Txn)

TryToResyncHead:
        Dim rp0 = RPX0.TryToResync(Ix)
        Select Case True
          Case rp0 Is Nothing : Return Nothing
          Case TypeOf rp0 Is Head
            Txn = Txn + New ParseError(sx.To(rp0.Span.Start), Nothing) + rp0
            GoTo IsThereABody
        End Select
        Return Nothing
TryToResyncBody:
        Dim rp1 = RPX1.TryToResync(Ix)
        Select Case True
          Case rp1 Is Nothing : Return Nothing
          Case TypeOf rp1 Is Body
            Txn = Txn + New ParseError(sx.To(rp1.Span.Start), Nothing) + rp1
            GoTo AfterBody
        End Select
        Return Nothing
      End Function

      Public Class Comma : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid OrElse (Ix.Value <> ","c) Then Return Nothing
          Return New Comma(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class MinusSign : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid OrElse (Ix.Value <> "-"c) Then Return Nothing
          Return New MinusSign(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class Head : Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return Nothing
          Dim Txn = Tokens.Empty
          Dim T = Comma.TryParse(Ix)
          If T IsNot Nothing Then
            Dim sx = Ix : Txn += T : Ix = T.Span.Next
            T = Common.Whitespaces.TryParse(Ix)
            If T IsNot Nothing Then
              Txn += T : Ix = T.Span.Next
              Return New Head(Source.Span.From(sx, Ix), Txn)
            End If
          End If
          Return Nothing
        End Function

      End Class

      Public Class Body : Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          Dim sx = Ix
          Dim Txn = Tokens.Empty
          Dim T As Token
          T = MinusSign.TryParse(Ix)
          If T IsNot Nothing Then
            Txn = Common.AddThenNext(T, Txn, Ix)
          End If
          T = Common.Digits.TryParse(Ix)
          If T IsNot Nothing Then
            Txn = Common.AddThenNext(T, Txn, Ix)
            T = Common.Whitespaces.TryParse(Ix)
            If T IsNot Nothing Then
              Txn = Common.AddThenNext(T, Txn, Ix)
            End If
            Return New Body(Source.Span.From(sx, Ix), Txn)
          Else
            Return Nothing
          End If
        End Function

      End Class

    End Class

    Public Class Format : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Format
        Dim T As Token
        T = Format.Head.TryParse(Ix)
        If T Is Nothing Then Return Nothing
        Dim sx = Ix
        Dim Txn = Tokens.Empty : Txn = Common.AddThenNext(T, Txn, Ix)
        T = ArgHole.Format.Body.TryParse(Ix)
        If T IsNot Nothing Then
          Txn = Common.AddThenNext(T, Txn, Ix)
        End If
        Return New Format(sx.To(Ix), Txn)
      End Function

      Public Class Colon : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Format.Colon
          If Ix.IsInvalid OrElse (Ix <> ":"c) Then Return Nothing Else Return New Colon(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class Head
        Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Format.Head
          Dim T As Token = Colon.TryParse(Ix)
          If T Is Nothing Then Return Nothing Else Return New Head(T.Span, Tokens.Empty + T)
        End Function

      End Class

      Public Class Body
        Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Format.Body
          If Ix.IsInvalid Then Return Nothing
          Dim Txn = Tokens.Empty
          Dim sx = Ix
          Dim T As Token
          While Ix.IsValid
            T = Common.Brace.Opening.TryParse(Ix)
            If T IsNot Nothing Then
              Dim pe As New ParseError(T.Span, ParseError.Reason.Invalid)
              Txn = Common.AddThenNext(pe, Txn, Ix)
              Continue While
            End If
            T = Common.Brace.Closing.TryParse(Ix) : If T IsNot Nothing Then Exit While
            T = Text._TryParse(Ix, True) : If T Is Nothing Then Exit While
            Txn = Common.AddThenNext(T, Txn, Ix)
          End While
          Return New Format.Body(sx.To(Ix), Txn)
        End Function

      End Class

    End Class

  End Class

  Public Class Text : Inherits Token

    Friend Sub New(Span As Source.Span, Inner As Tokens)
      MyBase.New(Span, Inner)
    End Sub

    Public Shared Function TryParse(Ix As Source.Position) As Token
      Return _TryParse(Ix, False)
    End Function

    Friend Shared Function _TryParse(Ix As Source.Position, Optional ArgFormatText As Boolean = False) As Text
      If Ix.IsInvalid Then Return Nothing
      Dim Txn = Tokens.Empty, sx = Ix, T As Token
      Dim TextStart As New Source.Position?
      While Ix.IsValid
        T = Common.Brace.Esc.Closing.TryParse(Ix) : If T IsNot Nothing Then Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
        T = Common.Brace.Esc.Opening.TryParse(Ix) : If T IsNot Nothing Then Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
        T = Common.Brace.Closing.TryParse(Ix) : If T IsNot Nothing Then Exit While
        T = Common.Brace.Opening.TryParse(Ix)
        If T IsNot Nothing Then
          If ArgFormatText Then Exit While
          Txn = Common.AddThenNext(T, Txn, Ix, TextStart)
          Continue While
        End If
        If TextStart Is Nothing Then TextStart = New Source.Position?(Ix)
        Ix = Ix.Next
      End While
      If TextStart IsNot Nothing Then Txn = Common.AddThenNext(Nothing, Txn, Ix, TextStart)
      Return New Text(sx.To(Ix), Txn)
    End Function

  End Class

End Class
