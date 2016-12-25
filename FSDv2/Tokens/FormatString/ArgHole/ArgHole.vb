﻿Imports System.Globalization
Partial Public Class FormatString

  Partial Public Class ArgHole : Inherits Token
#Region "Resync Points"
    Private Shared RPX0 As ResyncPoints = New ResyncPoint(AddressOf Align.Comma.TryParse) + New ResyncPoint(AddressOf Format.Colon.TryParse) + New ResyncPoint(AddressOf Common.Brace.Closing.TryParse)
#End Region

    <DebuggerStepperBoundary>
    Private Sub New(Span As Source.Span, Inner As Tokens)
      MyBase.New(TokenKind.ArgHole, Span, Inner)
    End Sub

    <DebuggerStepperBoundary>
    Public Shared Function TryParse(Ix As Source.Position) As Token
      '
      '  ArgHole ::= Brace.Opening ArgHole.Index?
      '
      If Ix.IsInvalid Then Return ParseError.Make.EoT(Ix)
#Region "Find Brace Opening"
Find_Brace_Opening:
      Dim sx = Ix, txn = Tokens.Empty, T As Token = Common.Brace.Opening.TryParse(Ix)
      If T.Kind = TokenKind.Brace_Opening Then txn = Common.AddThenNext(T, txn, Ix)
#End Region
#Region "Find Index"
Find_Index:
      T = ArgHole.Index.TryParse(Ix)
      If TypeOf T Is ParseError.EoT Then txn += T : GoTo Done
      '   If TypeOf T Is ParseError.NullParse Then GoTo TryToResync
      If T.Kind <> TokenKind.ArgHole_Index Then
        txn = Common.AddThenNext(T, txn, Ix)

        GoTo TryToResync
      End If
#End Region
#Region "Find Align"
Find_Align:
      T = ArgHole.Align.TryParse(Ix)
      If TypeOf T Is ParseError.EoT Then txn += T : GoTo Done
      If TypeOf T IsNot ParseError.NullParse Then txn = Common.AddThenNext(T, txn, Ix)
      If T.Kind <> TokenKind.ArgHole_Align Then GoTo TryToResync
      txn = Common.AddThenNext(T, txn, Ix)
#End Region
#Region "Find Format"
Find_Format:
      T = ArgHole.Format.TryParse(Ix)
      If TypeOf T Is ParseError.EoT Then txn += T : GoTo Done
      If TypeOf T IsNot ParseError.NullParse Then txn = Common.AddThenNext(T, txn, Ix)
      If T.Kind <> TokenKind.ArgHole_Format Then GoTo TryToResync
      txn = Common.AddThenNext(T, txn, Ix)
#End Region
#Region "Find Brace Closing"
Find_Brace_Closing:
      T = Common.Brace.Closing.TryParse(Ix)
      If TypeOf T Is ParseError.EoT Then txn += T : GoTo Done
      If T.Kind = TokenKind.Brace_Closing Then txn = Common.AddThenNext(T, txn, Ix)
#End Region
Done:
      Return New ArgHole(sx.To(Ix), txn)
      ' Checking for the valid tokens is left the Analysis (TODO)
#Region "TryToResync"
TryToResync:
      Dim rp1 = RPX0.TryToResync(Ix), pe = TryCast(rp1, ParseError)
      If pe Is Nothing OrElse pe.Why = ParseError.Reason.NullParse Then GoTo Find_Brace_Closing
      Select Case rp1(0).Kind
        Case TokenKind.Comma
          If Ix <> rp1.Span.Start Then txn = Common.AddThenNext(rp1, txn, Ix)
          GoTo Find_Align
        Case TokenKind.Colon
          If Ix <> rp1.Span.Start Then txn = Common.AddThenNext(rp1, txn, Ix)
          GoTo Find_Format
        Case TokenKind.Brace_Closing
          If Ix <> rp1.Span.Start Then txn = Common.AddThenNext(rp1, txn, Ix)
          GoTo Find_Brace_Closing
      End Select
      GoTo Find_Brace_Closing
#End Region
    End Function

  End Class

End Class