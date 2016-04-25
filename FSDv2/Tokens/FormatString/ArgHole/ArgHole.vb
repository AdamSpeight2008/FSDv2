Partial Public Class FormatString

  Public Class ArgHole : Inherits Token

    Private Sub New(Span As Source.Span, Inner As Tokens)
      MyBase.New(TokenKind.ArgHole, Span, Inner)
    End Sub

    Public Shared Function TryParse(Ix As Source.Position) As Token
      '
      '  ArgHole ::= Brace.Opening ArgHole.Index?
      '
      If Ix.IsInvalid Then Return ParseError.EoT(Ix)
      Dim sx = Ix
      Dim txn = Tokens.Empty
      Dim T As Token = Common.Brace.Opening.TryParse(Ix)
      If T.Kind = TokenKind.Brace_Opening Then txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Index.TryParse(Ix)
      If T.Kind = TokenKind.ArgHole_Index Then txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Align.TryParse(Ix)
      If T.Kind = TokenKind.ArgHole_Align Then txn = Common.AddThenNext(T, txn, Ix)
      T = ArgHole.Format.TryParse(Ix)
      If T.Kind = TokenKind.ArgHole_Format Then txn = Common.AddThenNext(T, txn, Ix)
      T = Common.Brace.Closing.TryParse(Ix)
      If T.Kind = TokenKind.Brace_Closing Then txn = Common.AddThenNext(T, txn, Ix)
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
        If Ix.IsInvalid Then Return ParseError.EoT(Ix)
        Dim Txn = Tokens.Empty
        Dim T As Token
        Dim sx = Ix
        T = Common.Digits.TryParse(Ix)
        If T.Kind = TokenKind.Digits Then
          Txn = Common.AddThenNext(T, Txn, Ix)
IsThereTrailingWhitespace:
          T = Common.Whitespaces.TryParse(Ix)
          If TypeOf T IsNot ParseError Then Txn = Common.AddThenNext(T, Txn, Ix)
          Return New Index(Source.Span.From(sx, Ix), Txn)
        Else
          Dim r = RPX.TryToResync(Ix)
          Select Case True
            Case r Is Nothing : Return Nothing
            Case TypeOf r Is Common.Digits
              Dim tmp As New ParseError(sx.To(r.Span.Start), ParseError.Reason.UnexpectedCharacter, r)
              Txn = Txn + tmp + r
              Ix = r.Span.Next
              GoTo IsThereTrailingWhitespace
          End Select
        End If
        Return ParseError.NullParse(Ix)
      End Function

    End Class

    Public Class Align : Inherits Token
      Private Shared RPX0 As ResyncPoints = New ResyncPoint(AddressOf Head.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
      Private Shared RPX1 As ResyncPoints = New ResyncPoint(AddressOf Body.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Align, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        '
        '  ArgHole.Index ::= ArgHole.Align.Head ArgHole.Align.Body
        '
        Dim Txn = Tokens.Empty
        Dim sx = Ix
        Dim _Head = Head.TryParse(Ix)
        If _Head.Kind = TokenKind.ParseError Then GoTo TryToResyncHead
        Txn = Common.AddThenNext(_Head, Txn, Ix)
IsThereABody:
        Dim _Body = Body.TryParse(Ix)
        If _Body.Kind = TokenKind.ParseError Then GoTo TryToResyncBody
        Txn = Common.AddThenNext(_Body, Txn, Ix)
AfterBody:
        Return New Align(Txn.First.Span.Start.To(Txn.Last.Span.Next), Txn)

TryToResyncHead:
        Dim rp0 = RPX0.TryToResync(Ix)
        Select Case True
          Case TypeOf rp0 Is Head
            Txn = Txn + New ParseError(sx.To(rp0.Span.Start), Nothing, rp0) + rp0
            GoTo IsThereABody
        End Select
        Return ParseError.NullParse(Ix)
TryToResyncBody:
        Dim rp1 = RPX1.TryToResync(Ix)
        Select Case rp1.Kind
          Case TokenKind.ParseError
          Case TokenKind.Colon
            Txn = Txn + New ParseError(sx.To(rp1.Span.Start), Nothing, rp1)
            GoTo AfterBody
          Case TokenKind.ArgHole_Align_Body
            Txn = Txn + New ParseError(sx.To(rp1.Span.Start), Nothing, rp1)
            GoTo AfterBody
          Case TokenKind.Brace_Closing
            Txn += New ParseError(sx.To(rp1.Span.Start), Nothing, rp1)
            GoTo AfterBody

        End Select
        Return ParseError.NullParse(Ix)
      End Function

      Public Class Comma : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Comma, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          If (Ix.Value <> ","c) Then Return ParseError.NullParse(Ix)
          Return New Comma(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class MinusSign : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.MinusSign, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          If (Ix.Value <> "-"c) Then Return ParseError.NullParse(Ix)
          Return New MinusSign(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class Head : Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Head, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          '
          ' ArgHole.Align.Head ::= Comma Whitespaces?
          '
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          Dim Txn = Tokens.Empty
          Dim T = Comma.TryParse(Ix)
          If T.Kind <> TokenKind.Comma Then Return ParseError.NullParse(Ix)
          Dim sx = Ix : Txn += T : Ix = T.Span.Next
          T = Common.Whitespaces.TryParse(Ix)
          If T.Kind = TokenKind.Whitespaces Then Txn += T : Ix = T.Span.Next
          Return New Head(sx.To(Ix), Txn)
        End Function

      End Class

      Public Class Body : Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Body, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          '
          '  ArgHole.Align.Body ::= MinusSign? Digits Whitespaces?
          '
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          Dim sx = Ix
          Dim Txn = Tokens.Empty
          Dim T As Token = MinusSign.TryParse(Ix)
          If T.Kind = TokenKind.MinusSign Then Txn = Common.AddThenNext(T, Txn, Ix)
          T = Common.Digits.TryParse(Ix)
          If T.Kind = TokenKind.ParseError Then Return ParseError.NullParse(Ix)
          Txn = Common.AddThenNext(T, Txn, Ix)
          T = Common.Whitespaces.TryParse(Ix)
          If T.Kind = TokenKind.Whitespaces Then Txn = Common.AddThenNext(T, Txn, Ix)
          Return New Body(sx.To(Ix), Txn)

        End Function

      End Class

    End Class

    Public Class Format : Inherits Token

      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Format, Span, Inner)
      End Sub

      Public Shared Function TryParse(Ix As Source.Position) As Token
        If Ix.IsInvalid Then Return ParseError.EoT(Ix)
        Dim T As Token = Format.Head.TryParse(Ix)
        If T.Kind = TokenKind.ParseError Then Return T
        Dim sx = Ix
        Dim Txn = Tokens.Empty : Txn = Common.AddThenNext(T, Txn, Ix)
        T = ArgHole.Format.Body.TryParse(Ix)
        If T.Kind = TokenKind.ArgHole_Format_Body Then Txn = Common.AddThenNext(T, Txn, Ix)
        Return New Format(sx.To(Ix), Txn)
      End Function
      Public Class Head
        Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Head, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          Dim T As Token = Colon.TryParse(Ix)
          If T.Kind = TokenKind.ParseError Then Return ParseError.NullParse(Ix)
          Return New Head(T.Span, Tokens.Empty + T)
        End Function

      End Class

      Public Class Body
        Inherits Token

        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Format_Body, Span, Inner)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          Dim Txn = Tokens.Empty
          Dim sx = Ix
          Dim T As Token
          While Ix.IsValid
            'T = Common.Brace.Opening.TryParse(Ix)
            'If T.Kind = TokenKind.Brace_Opening Then
            '  Dim pe As New ParseError(T.Span, ParseError.Reason.Invalid, T)
            '  Txn = Common.AddThenNext(pe, Txn, Ix)
            '  Continue While
            'End If
            T = Common.Brace.Closing.TryParse(Ix) : If T.Kind = TokenKind.Brace_Closing Then Exit While
            T = Text._TryParse(Ix, True) : If T.Kind = TokenKind.ParseError Then Exit While
            Txn = Common.AddThenNext(T, Txn, Ix)
          End While
          Return New Format.Body(sx.To(Ix), Txn)
        End Function

      End Class
      Public Class Colon : Inherits Token

        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Colon, Span)
        End Sub

        Public Shared Function TryParse(Ix As Source.Position) As Token
          If Ix.IsInvalid Then Return ParseError.EoT(Ix)
          If (Ix <> ":"c) Then Return ParseError.NullParse(Ix)
          Return New Colon(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class



    End Class

  End Class
End Class