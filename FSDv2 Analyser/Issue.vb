Imports FSDv2

Partial Public Class Analyser

  <DebuggerStepThrough>
  Public Class Issue
    Public ReadOnly Property Kind As Issue.Kinds
    Public ReadOnly Property Span As Source.Span
    Public ReadOnly Property Additional As String

    Public Sub New(Kind As Kinds, Span As Source.Span, Optional Additional As String = Nothing)
      Me.Kind = Kind : Me.Span = Span : Me.Additional = If(Additional, String.Empty)
    End Sub

    Public Overrides Function ToString() As String
      Return $"{Span.ToString} {Kind.ToString} {Me.Additional}".TrimEnd(" "c)
    End Function
    Public Enum Kinds As Integer
      Unexpected_End
      Unexpected_Characters
      Arg_Index_Framework_Upper_Limit_Exceeded
      Arg_Index_FrameWork_Lower_Limit_Exceeded
      Arg_Index_OutOfRange
      Arg_Index_Missing
      Unexpected_Token
      Arg_Align_Missing
      Arg_Align_Framework_Upper_Limit_Exceeded
      Arg_Align_Framework_Lower_Limit_Exceeded
      Invalid
      Missing_Closing_Brace
      ResyncSkipped
    End Enum

    Public Shared Operator +(Issue0 As Issue, Issue1 As Issue) As Issues
      Return Issues.Empty + Issue0 + Issue1
    End Operator

    Public Class Arg
      Public Class Index
        Public Shared Function Missing(Span As Source.Span) As Issue
          Return New Issue(Kinds.Arg_Index_Missing, Span)
        End Function
        Public Shared Function OutOfRange(Span As Source.Span) As Issue
          Return New Issue(Kinds.Arg_Index_OutOfRange, Span, "")
        End Function
        Public Class Framework
          Public Shared Function Upper_Limit_Exceeded(Span As Source.Span) As Issue
            Return New Issue(Kinds.Arg_Index_Framework_Upper_Limit_Exceeded, Span)
          End Function
          Public Shared Function Lower_Limit_Exceeded(Span As Source.Span) As Issue
            Return New Issue(Kinds.Arg_Index_FrameWork_Lower_Limit_Exceeded, Span)
          End Function
        End Class
      End Class
      Public Class Align
        Public Shared Function Missing(Span As Source.Span) As Issue
          Return New Issue(Kinds.Arg_Align_Missing, Span)
        End Function
        Public Class Framework
          Public Shared Function Upper_Limit_Exceeded(Span As Source.Span) As Issue
            Return New Issue(Kinds.Arg_Align_Framework_Upper_Limit_Exceeded, Span)
          End Function
          Public Shared Function Lower_Limit_Exceeded(Span As Source.Span) As Issue
            Return New Issue(Kinds.Arg_Index_FrameWork_Lower_Limit_Exceeded, Span)
          End Function
        End Class
      End Class

    End Class

    Public Class Unexpected
      Public Shared Function Characters(Span As Source.Span) As Issue
        Return New Issue(Kinds.Unexpected_Characters, Span)
      End Function

      Public Shared Function Token(Span As Source.Span, Tkn As Token) As Issue
        Return New Issue(Kinds.Unexpected_Token, Span, Tkn.ToString)
      End Function

      Public Shared Function EoT(span As Source.Span) As Issue
        Return New Issue(Kinds.Unexpected_End, span)
      End Function
    End Class

    Public Class Missing
      Public Shared Function ClosingBrace(Span As Source.Span) As Issue
        Return New Issue(Kinds.Missing_Closing_Brace, Span)
      End Function
    End Class
  End Class
End Class