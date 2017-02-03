Attribute VB_Name = "R_UDF"
Option Explicit

' ********** UDF (only async call is possible) ***************
Public Function RExec(Code As Variant, ParamArray Precedents() As Variant) As Variant
    Dim Dumb As String
    Dim ParamCounter As Long
    ' Strange manipulations with dumb for execution formulas in correct order
    If VarType(Code) = vbString Then
        ExecInR_async Code
        Dumb = Rnd(Len(Code) + 0.1)
    Else
        ExecRangeInR_async Code
        Dumb = Rnd(Len(Code.Cells(1, 1)) + 0.1)
    End If
    For ParamCounter = LBound(Precedents) To UBound(Precedents)
        If VarType(Precedents(ParamCounter)) = 8204 Then ' range
            Dumb = Dumb + Rnd(Len(Precedents(ParamCounter).Cells(1, 1)) + 0.1)
        Else
            Dumb = Dumb + Rnd(Len(CStr(Precedents(ParamCounter))) + 0.1)
        End If
    Next ParamCounter
    
    RExec = "#RExec#" & " " & Rnd(Dumb)
End Function




