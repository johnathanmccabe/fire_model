VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "Curve"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Private tiv_percs() As Double
Private values() As Double

' Get value from curve for a TIV percentage
' Where the TIV percentage lies between two points on the curve then a linear interpolated result will be returned
Public Function GetValue(tivPerc As Double)

 retVal = Interp1(tiv_percs, values, tivPerc)

GetValue = retVal


End Function


' Class initialiser
Public Function InitiateProperties(tivPercs() As Double, curveValues() As Double)

Set tiv_percs = tivPercs
Set values = curveValues

'sort the arrays according to the tiv percentages
Call sort2(tiv_percs, values)



End Function


