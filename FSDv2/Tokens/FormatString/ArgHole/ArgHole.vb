Imports System.Globalization
Partial Public Class FormatString
  Private Const Trace As Boolean = True

  Partial Public Class ArgHole : Inherits Token
#Region "Resync Points"
    Private Shared RPX0 As ResyncPoints = New ResyncPoint(AddressOf Align.Comma.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
#End Region

    <DebuggerStepperBoundary>
    Private Sub New(Span As Source.Span, Inner As Tokens)
      MyBase.New(TokenKind.ArgHole, Span, Inner)
    End Sub

    Private Shared Function TryFind_Brace_Opening(ByRef idx As Source.Position, ByRef txn As Tokens, DoingResync As Boolean) As Boolean
      Dim t As Token = Common.Brace.Opening.TryParse(idx, DoingResync)
      If t.Kind = TokenKind.Brace_Opening Then
        txn = Common.AddThenNext(t, txn, idx) : Return True
      End If
      Return False
    End Function

    Private Shared Function TryFind_ArgIndex(ByRef idx As Source.Position, ByRef txn As Tokens, DoingResync As Boolean) As Boolean
      Dim T As Token = ArgHole.Index.TryParse(idx, DoingResync)
      If TypeOf T Is ParseError.EoT Then
        txn += T
        Return False
      ElseIf T.Kind <> TokenKind.ArgHole_Index Then
        Return False
      Else
        txn = Common.AddThenNext(T, txn, idx)
      End If
      Return True
    End Function

    Private Shared Function TryFind_ArgAlign(ByRef idx As Source.Position, ByRef txn As Tokens, DoingResync As Boolean) As Boolean
      Dim T = ArgHole.Align.TryParse(idx, DoingResync)
      If TypeOf T Is ParseError.EoT Then txn += T : Return False
      If T.Kind <> TokenKind.ArgHole_Align Then Return False
      txn = Common.AddThenNext(T, txn, idx)
      Return True
    End Function

    Private Shared Function TryFind_ArgFormat(ByRef idx As Source.Position, txn As Tokens, DoingResync As Boolean) As Boolean
      Dim T As Token = ArgHole.Format.TryParse(idx, DoingResync)
      If TypeOf T Is ParseError.EoT Then txn += T : Return False
      If T.Kind <> TokenKind.ArgHole_Format Then Return False
      txn = Common.AddThenNext(T, txn, idx)
      Return True
    End Function

    Private Shared Function TryFind_Brace_Closing(ByRef idx As Source.Position, ByRef txn As Tokens, DoingResync As Boolean) As Boolean
      Dim T = Common.Brace.Closing.TryParse(idx, DoingResync)
      If TypeOf T Is ParseError.EoT Then txn += T : Return False
      If T.Kind <> TokenKind.Brace_Closing Then Return False
      txn = Common.AddThenNext(T, txn, idx)
      Return True
    End Function

    '<DebuggerStepperBoundary>
    Public Shared Function TryParse(Ix As Source.Position, DoingResync As Boolean) As Token
      '
      '  ArgHole ::= Brace.Opening ArgHole.Index?
      '
      If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
Find_Brace_Opening:
      Dim sx = Ix, txn = Tokens.Empty, T As Token
      If Not TryFind_Brace_Opening(Ix, txn, DoingResync) Then GoTo TryToResync

Find_Index:
      If Not TryFind_ArgIndex(Ix, txn, DoingResync) Then GoTo TryToResync

Find_Align:
      If Not TryFind_ArgAlign(Ix, txn, DoingResync) Then GoTo TryToResync

Find_Format:
      If Not TryFind_ArgFormat(Ix, txn, DoingResync) Then GoTo TryToResync

Find_Brace_Closing:
      If Not TryFind_Brace_Closing(Ix, txn, DoingResync) Then GoTo TryToResync

Done:
      Return New ArgHole(sx.To(Ix), txn)
      ' Checking for the valid tokens is left the Analysis (TODO)
#Region "TryToResync"
TryToResync:
      If Ix.IsInvalid Then GoTo Done
      If Not DoingResync Then
        Dim ResultOfResyncing = RPX0.TryToResync(Ix, True)
        Dim pe = TryCast(ResultOfResyncing, ParseError)
        If pe Is Nothing OrElse pe.Why = ParseError.Reason.NullParse Then GoTo Find_Brace_Closing
        Dim size = ResultOfResyncing.Span.Size
        If size > 0 Then
          txn = Common.AddThenNext(ResultOfResyncing, txn, Ix)
        End If
        Select Case ResultOfResyncing(0).Kind
            Case TokenKind.Comma
              GoTo Find_Align
            Case TokenKind.Colon
              GoTo Find_Format
            Case TokenKind.Brace_Closing
              GoTo Find_Brace_Closing
            Case Else
              Debug.Assert(False, "Unsupported Kind: " & ResultOfResyncing(0).Kind)
          End Select
        End If
        GoTo Find_Brace_Closing
#End Region
    End Function

  End Class

End Class