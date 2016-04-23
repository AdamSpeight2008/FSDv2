Partial Public Class FormatString

  Public Class ArgHole : Inherits Token

    Private Sub New(Span As Source.Span, Inner As Tokens)
      MyBase.New(TokenKind.ArgHole, Span, Inner)
    End Sub

    Public Shared Function TryParse(Ix As Source.Position) As Token
      If Ix.IsInvalid Then Return Nothing
      Dim T As Token = Common.Brace.Opening.TryParse(Ix)
      Dim sx = Ix
      Dim txn = Tokens.Empty : txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Index.TryParse(Ix)
      If T IsNot Nothing Then txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Align.TryParse(Ix)
      If T IsNot Nothing Then txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Format.TryParse(Ix)
      If T IsNot Nothing Then txn = Common.AddThenNext(T, txn, Ix)
      T = Common.Brace.Closing.TryParse(Ix)
      txn = Common.AddThenNext(T, txn, Ix)
      Return New ArgHole(sx.To(Ix), txn)
      ' Checking for the valid tokens is left the Analysis (TODO)
    End Function

    Public Class Index : Inherits Token
      Private Shared RPX As ResyncPoints = New ResyncPoint(AddressOf Common.Digits.TryParse) + New ResyncPoint(AddressOf Align.Comma.TryParse) +
                                         New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)


      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Index, Span, Inner)
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
          If T IsNot Nothing Then Txn = Common.AddThenNext(T, Txn, Ix)
          Return New Index(Source.Span.From(sx, Ix), Txn)
        Else
          Dim r = RPX.TryToResync(Ix)
          Select Case True
            Case r Is Nothing : Return Nothing
            Case TypeOf r Is Common.Digits
              Dim tmp As New ParseError(sx.To(r.Span.Start), Nothing, r)
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
        MyBase.New(TokenKind.ArgHole_Align, Span, Inner)
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
            Txn = Txn + New ParseError(sx.To(rp0.Span.Start), Nothing, rp0) + rp0
            GoTo IsThereABody
        End Select
        Return Nothing
TryToResyncBody:
        Dim rp1 = RPX1.TryToResync(Ix)
        Select Case True
          Case rp1 Is Nothing : Return Nothing
          Case TypeOf rp1 Is Body
            Txn = Txn + New ParseError(sx.To(rp1.Span.Start), Nothing, rp1) + rp1
            GoTo AfterBody
        End Select
        Return Nothing
      End Function

      Public Class Comma : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Comma, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid OrElse (Ix.Value <> ","c) Then Return Nothing
          Return New Comma(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class MinusSign : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.MinusSign, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid OrElse (Ix.Value <> "-"c) Then Return Nothing
          Return New MinusSign(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class Head : Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Head, Span, Inner)
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
          MyBase.New(TokenKind.ArgHole_Align_Body, Span, Inner)
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
        MyBase.New(TokenKind.ArgHole_Format, Span, Inner)
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
          MyBase.New(TokenKind.Colon, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Format.Colon
          If Ix.IsInvalid OrElse (Ix <> ":"c) Then Return Nothing Else Return New Colon(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class Head
        Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Head, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Format.Head
          Dim T As Token = Colon.TryParse(Ix)
          If T Is Nothing Then Return Nothing Else Return New Head(T.Span, Tokens.Empty + T)
        End Function

      End Class

      Public Class Body
        Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Body, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Format.Body
          If Ix.IsInvalid Then Return Nothing
          Dim Txn = Tokens.Empty
          Dim sx = Ix
          Dim T As Token
          While Ix.IsValid
            T = Common.Brace.Opening.TryParse(Ix)
            If T IsNot Nothing Then
              Dim pe As New ParseError(T.Span, ParseError.Reason.Invalid, T)
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
End Class