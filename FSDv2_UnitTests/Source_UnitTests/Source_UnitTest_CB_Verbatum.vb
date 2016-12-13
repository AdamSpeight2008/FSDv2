Imports System.Text
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports FSDv2

<TestClass>
Public Class Source_UnitTests_CS_Verbatum

  Const Cat0 = "Source (CS_Verbatum)"

  <TestMethod, TestCategory(Cat0)>
  Public Sub _00_NullText()
    Dim Text = Nothing
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Verbatum, Source.StringKind.StringFormat)
        Assert.AreEqual(0, TheSource.Length)
    Assert.AreEqual(Source.SourceKind.CS_Verbatum, TheSource.Kind)
    Assert.AreEqual("", TheSource.Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _01_StringEmpty()
    Dim Text = String.Empty
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Verbatum, Source.StringKind.StringFormat)
        Assert.AreEqual(0, TheSource.Length)
    Assert.AreEqual(Source.SourceKind.CS_Verbatum, TheSource.Kind)
    Assert.AreEqual("", TheSource.Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _02_EmptyStringLiteral()
    Dim Text = Nothing
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Verbatum, Source.StringKind.StringFormat)
        Assert.AreEqual(0, TheSource.Length)
    Assert.AreEqual(Source.SourceKind.CS_Verbatum, TheSource.Kind)
    Assert.AreEqual("", TheSource.Text)
  End Sub

  <TestMethod, TestCategory(Cat0)>
  Public Sub _03_SourceChar_Checks()
    Dim Text = "A"
        Dim TheSource = Source.Create(Text, Source.SourceKind.CS_Verbatum, Source.StringKind.StringFormat)
        Assert.AreEqual(1, TheSource.Length)
    Assert.AreEqual(Source.SourceKind.CS_Verbatum, TheSource.Kind)
    Assert.AreEqual("A", TheSource.Text)
    Dim res As Char?
    res = TheSource(-1)
    Assert.AreEqual(False, res.HasValue)
    res = TheSource(0)
    Assert.AreEqual(True, res.HasValue)
    Assert.AreEqual("A"c, res.Value)
    res = TheSource(1)
    Assert.AreEqual(False, res.HasValue)
  End Sub


End Class