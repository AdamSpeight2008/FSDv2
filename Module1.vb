Imports FSD

Module Module1

  Sub Main()
    '           0         1         2         3         4         5         6         7         8
    '           012345678901234567890123456789012345678901234567890123456789012345678901234567890
    Dim Text = "}  {1 , -123 : {{ABC}} }  {1 , -123 : {{ABC}} } "
    Dim TheSource = Source.Create(Text)
    Dim Ix = TheSource.First
    'Dim T0 = ArgHole.Index.TryParse(Ix)
    'Dim T1 = ArgHole.Align.TryParse(T0.Span.Next)
    'Dim T2 = ArgHole.Format.TryParse(T1.Span.Next)
    ' Dim T3 = ArgHole.TryParse(Ix)
    'Dim Txt = TryCast(T3.Inner.Tokens(3).Inner.Tokens(1).Inner.Tokens(0), ArgHole.Text)
    Dim T4 = FormatString.TryParse(Ix)
  End Sub

End Module

Public Structure Source
  Friend ReadOnly Property ID As Guid
  Public ReadOnly Property Text As String
  Public ReadOnly Property Length As Integer

  Private Sub New(Text As String)
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

  Public Shared Function Create(Text As String) As Source
    Return New Source(Text)
  End Function

  Public Shared Operator =(S0 As Source, S1 As Source) As Boolean
    Return (S0.ID = S1.ID)
  End Operator
  Public Shared Operator <>(S0 As Source, S1 As Source) As Boolean
    Return (S0.ID <> S1.ID)
  End Operator

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

  Public Sub New(Span As Source.Span, Token As Token)
    MyBase.New(Span, Tokens.Empty + Token)
  End Sub

End Class

Public Class Tokens
  Public Shared ReadOnly Property Empty As Tokens = New Tokens()
  Public ReadOnly Property Tokens As Token()
  Private ReadOnly Property Count As Integer

  Private Sub New(Optional Tokens As IEnumerable(Of Token) = Nothing)
    If Tokens Is Nothing Then Me.Tokens = Array.Empty(Of Token) Else Me.Tokens = Tokens.ToArray
    Me.Count = Me.Tokens.Count
  End Sub

  Public Shared Function Create(T0 As Token, T1 As Token) As Tokens
    If (T0 Is Nothing) OrElse (T1 Is Nothing) Then Throw New Exception
    Return New Tokens(Enumerable.Repeat(T0, 1).Concat(Enumerable.Repeat(T1, 1)))
  End Function

  Public Function GetEnumerator() As IEnumerable(Of Token)
    Return Tokens.AsEnumerable
  End Function

  Public Function Add(Token As Token) As Tokens
    If Token Is Nothing Then Return New Tokens(Me.Tokens) Else Return New Tokens(Me.Tokens.Concat(Enumerable.Repeat(Token, 1)))
  End Function

  Public Shared Operator +(Tx As Tokens, T As Token) As Tokens
    Return New Tokens(Tx.Tokens.Concat(Enumerable.Repeat(T, 1)))
  End Operator

  Public Shared Operator +(T As Token, Tx As Tokens) As Tokens
    Return New Tokens(Enumerable.Repeat(T, 1).Concat(Tx.Tokens))
  End Operator

End Class


Public Class ResyncPoint
  Public ReadOnly Property TryParse As Func(Of Source.Position, Token)

  Public Sub New(TryParse As Func(Of Source.Position, Token))
    Me.TryParse = TryParse
  End Sub
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
        Case TypeOf T Is Common.Brace.Esc.Opening, TypeOf T Is Common.Brace.Esc.Closing
          Txn = Common.AddThenNext(T, Txn, Ix, TextStart)
        Case TypeOf T Is Common.Brace.Closing
          Dim pe As New ParseError(T.Span, T)
          Txn = Common.AddThenNext(pe, Txn, Ix, TextStart)
        Case TypeOf T Is Common.Brace.Opening
          Dim res = ArgHole.TryParse(Ix)
          If res IsNot Nothing Then
            Txn = Common.AddThenNext(res, Txn, Ix, TextStart)
          End If
        Case Else
          If TextStart Is Nothing Then TextStart = New Source.Position?(Ix)
          Ix = Ix.Next
      End Select

    End While
    Txn = Common.AddThenNext(Nothing, Txn, Ix, TextStart)
    Return New FormatString(sx.To(Ix), Txn)
  End Function

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
          Txn = Txn.Add(T)
          Ix = T.Span.Next
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
          Txn += T : Ix = T.Span.Next
        End While
        Dim s = Source.Span.From(Sx, Ix)
        If s.HasValue = False Then Return Nothing
        Return New Digits(s.Value, Txn)
      End Function

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

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return Nothing
        Dim T As Token = Common.Digits.TryParse(Ix)
        If T IsNot Nothing Then
          Dim sx = Ix
          Dim txn = Tokens.Empty + T
          Ix = T.Span.Next
          T = Common.Whitespaces.TryParse(Ix)
          If T IsNot Nothing Then txn += T : Ix = T.Span.Next : Return New Index(Source.Span.From(sx, Ix), txn)
        End If
        Return Nothing
      End Function

    End Class

    Public Class Align : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Align
        Dim Txn = Tokens.Empty
        Dim _Head = Head.TryParse(Ix)
        If _Head Is Nothing Then Return Nothing
        Txn += _Head
        Dim _Body = Body.TryParse(_Head.Span.Next)
        If _Body IsNot Nothing Then Txn += _Body
        Return New Align(Source.Span.From(_Head.Span.Start, _Body.Span.Next), Txn)
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
            T = Common.Brace.Closing.TryParse(Ix) : If T IsNot Nothing Then Exit While
            T = Text.TryParse(Ix) : If T Is Nothing Then Exit While
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

    Public Shared Function TryParse(Ix As Source.Position) As Text
      If Ix.IsInvalid Then Return Nothing
      Dim Txn = Tokens.Empty, sx = Ix, T As Token
      Dim TextStart As New Source.Position?
      While Ix.IsValid
        T = Common.Brace.Esc.Closing.TryParse(Ix) : If T IsNot Nothing Then Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
        T = Common.Brace.Esc.Opening.TryParse(Ix) : If T IsNot Nothing Then Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
        T = Common.Brace.Closing.TryParse(Ix) : If T IsNot Nothing Then Exit While
        T = Common.Brace.Opening.TryParse(Ix) : If T IsNot Nothing Then Txn = Common.AddThenNext(T, Txn, Ix, TextStart) : Continue While
        If TextStart Is Nothing Then TextStart = New Source.Position?(Ix)
        Ix = Ix.Next
      End While
      If TextStart IsNot Nothing Then Txn = Common.AddThenNext(Nothing, Txn, Ix, TextStart)
      Return New Text(sx.To(Ix), Txn)
    End Function

  End Class

End Class
