Imports System.Globalization
Partial Public Class FormatString
  Partial Public Class ArgHole : Inherits Token

    Public Class Align : Inherits Token
#Region "ResyncePoints"
      Private Shared RPX0 As ResyncPoints = New ResyncPoint(AddressOf Head.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
      Private Shared RPX1 As ResyncPoints = New ResyncPoint(AddressOf Body.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
#End Region

      <DebuggerStepperBoundary>
      Private Sub New(Span As Source.Span, Inner As Tokens)
        MyBase.New(TokenKind.ArgHole_Align, Span, Inner)
      End Sub

      <DebuggerStepperBoundary>
      Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
        '
        '  ArgHole.Index ::= ArgHole.Align.Head ArgHole.Align.Body
        '
        If Ix.IsInvalid Then Return ParseError.Make.NullParse(Ix)
#Region "IsThereAHead"
IsThereAHead:
        Dim Txn = Tokens.Empty, sx = Ix, _Head = Head.TryParse(Ix, DoingResync)
        If _Head.Kind <> TokenKind.ArgHole_Align_Head Then GoTo TryToResyncHead
        Txn = Common.AddThenNext(_Head, Txn, Ix)

#End Region
#Region "IsThereABody"
IsThereABody:
        Dim _Body = Body.TryParse(Ix, DoingResync)
        If _Body.Kind <> TokenKind.ArgHole_Align_Body Then GoTo TryToResyncBody
        Txn = Common.AddThenNext(_Body, Txn, Ix)
#End Region
Done:
        Return New Align(Txn.First.Span.Start.To(Txn.Last.Span.Next), Txn)

#Region "TryToResyncHead"
TryToResyncHead:
        If Not DoingResync Then
          Dim ResultOfResyncing = RPX0.TryToResync(Ix, True)
          Dim Size = ResultOfResyncing.Span.Size
          If Size > 0 Then
            Txn += New ParseError.Resync(sx.To(ResultOfResyncing.Span.Start), ResultOfResyncing)
          End If
          Select Case ResultOfResyncing.Kind
            Case TokenKind.ArgHole_Align_Head
              GoTo IsThereAHead
            Case TokenKind.Colon
              GoTo IsThereABody
            Case TokenKind.ArgHole_Align_Body
              GoTo IsThereABody
            Case TokenKind.Brace_Closing
              GoTo Done
            Case TokenKind.ParseError
            Case Else
              Debug.Assert(False, "Unsupported Kind: " & ResultOfResyncing.Kind)
          End Select
        End If
        'Return ParseError.Make.Invalid(Ix, Txn)

        Return ParseError.Make.NullParse(Ix)
#End Region
#Region "TryToResyncBody"
TryToResyncBody:
        If Not DoingResync Then
          Dim ResultOfResyncing = RPX1.TryToResync(Ix, True)
          Dim size = ResultOfResyncing.Span.Size
          If size > 0 Then
            Txn += New ParseError.Resync(sx.To(ResultOfResyncing.Span.Start), ResultOfResyncing)
            GoTo Done
          End If
          Select Case ResultOfResyncing.Kind
            Case TokenKind.ParseError
            Case TokenKind.ArgHole_Align_Head,
               TokenKind.Colon,
               TokenKind.ArgHole_Align_Body,
               TokenKind.Brace_Closing
              GoTo Done
            Case Else
              Debug.Assert(False, "Unsupported Kind: " & ResultOfResyncing.Kind)

          End Select
        End If
        GoTo Done
#End Region
      End Function

      Public Class Comma : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.Comma, Span)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If (Ix.Value <> ","c) Then Return ParseError.Make.NullParse(Ix)
          Return New Comma(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class MinusSign : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span)
          MyBase.New(TokenKind.MinusSign, Span)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          If (Ix.Value <> "-"c) Then Return ParseError.Make.NullParse(Ix)
          Return New MinusSign(Source.Span.Create_UnitSpan(Ix))
        End Function

      End Class

      Public Class Head : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Head, Span, Inner)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
          '
          ' ArgHole.Align.Head ::= Comma Whitespaces?
          '
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim Txn = Tokens.Empty, T = Comma.TryParse(Ix, DoingResync)
          If T.Kind <> TokenKind.Comma Then Return ParseError.Make.NullParse(Ix)
          Dim sx = Ix : Txn += T : Ix = T.Span.Next
          T = Common.Whitespaces.TryParse(Ix, DoingResync)
          If T.Kind = TokenKind.Whitespaces Then Txn += T : Ix = T.Span.Next
          Return New Head(sx.To(Ix), Txn)
        End Function

      End Class

      Public Class Body : Inherits Token

        <DebuggerStepperBoundary>
        Private Sub New(Span As Source.Span, Inner As Tokens)
          MyBase.New(TokenKind.ArgHole_Align_Body, Span, Inner)
        End Sub

        <DebuggerStepperBoundary>
        Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
          '
          '  ArgHole.Align.Body ::= MinusSign? Digits Whitespaces?
          '
          If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
          Dim sx = Ix, Txn = Tokens.Empty
          Dim T As Token = MinusSign.TryParse(Ix, DoingResync)
          If T.Kind = TokenKind.MinusSign Then Txn = Common.AddThenNext(T, Txn, Ix)
          T = Common.Digits.TryParse(Ix, DoingResync)
          If T.Kind = TokenKind.ParseError Then Return ParseError.Make.NullParse(Ix)
          Txn = Common.AddThenNext(T, Txn, Ix)
          T = Common.Whitespaces.TryParse(Ix, DoingResync)
          If T.Kind = TokenKind.Whitespaces Then Txn = Common.AddThenNext(T, Txn, Ix)
          Return New Body(sx.To(Ix), Txn)
        End Function

      End Class

    End Class

  End Class
End Class