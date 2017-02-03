Attribute VB_Name = "utilities"
'Returns an interpolated value of x
'doing a lookup of xarr->yarr
Public Function Interp1(xArr As Variant, yArr As Variant, x As Double) As Double

If (UBound(xArr) <> UBound(yArr)) Then
    Exit Function
End If

If ((x < xArr(LBound(xArr))) Or (x > xArr(UBound(xArr)))) Then
  MsgBox "Interp1: x is out of bound"
  Exit Function
End If

If xArr(LBound(xArr)) = x Then
  Interp1 = yArr(LBound(yArr))
  Exit Function
End If
Dim I As Single
For I = LBound(xArr) To UBound(xArr)
  If xArr(I) >= x Then
    Interp1 = yArr(I - 1) + (x - xArr(I - 1)) / (xArr(I) - xArr(I - 1)) * (yArr(I) - yArr(I - 1))
    Exit Function
  End If
Next I
End Function



' QuickSort algorithm for 1-D arrays
'
' see http://stackoverflow.com/questions/4873182/sorting-a-multidimensionnal-array-in-vba
Public Sub QuickSortVector(ByRef SortArray As Variant, Optional lngMin As Long = -1, Optional lngMax As Long = -1)
    On Error Resume Next

    'Sort a 1-Dimensional array

    ' SampleUsage: sort arrData
    '
    '   QuickSortVector arrData

    '
    ' Originally posted by Jim Rech 10/20/98 Excel.Programming


    ' Modifications, Nigel Heffernan:
    '       ' Escape failed comparison with an empty variant in the array
    '       ' Defensive coding: check inputs

    Dim I As Long
    Dim J As Long
    Dim varMid As Variant
    Dim varX As Variant

    If IsEmpty(SortArray) Then
        Exit Sub
    End If
    If InStr(TypeName(SortArray), "()") < 1 Then  'IsArray() is somewhat broken: Look for brackets in the type name
        Exit Sub
    End If
    If lngMin = -1 Then
        lngMin = LBound(SortArray)
    End If
    If lngMax = -1 Then
        lngMax = UBound(SortArray)
    End If
    If lngMin >= lngMax Then    ' no sorting required
        Exit Sub
    End If

    I = lngMin
    J = lngMax

    varMid = Empty
    varMid = SortArray((lngMin + lngMax) \ 2)

    ' We  send 'Empty' and invalid data items to the end of the list:
    If IsObject(varMid) Then  ' note that we don't check isObject(SortArray(n)) - varMid *might* pick up a default member or property
        I = lngMax
        J = lngMin
    ElseIf IsEmpty(varMid) Then
        I = lngMax
        J = lngMin
    ElseIf IsNull(varMid) Then
        I = lngMax
        J = lngMin
    ElseIf varMid = "" Then
        I = lngMax
        J = lngMin
    ElseIf VarType(varMid) = vbError Then
        I = lngMax
        J = lngMin
    ElseIf VarType(varMid) > 17 Then
        I = lngMax
        J = lngMin
    End If

    While I <= J

        While SortArray(I) < varMid And I < lngMax
            I = I + 1
        Wend
        While varMid < SortArray(J) And J > lngMin
            J = J - 1
        Wend

        If I <= J Then
            ' Swap the item
            varX = SortArray(I)
            SortArray(I) = SortArray(J)
            SortArray(J) = varX

            I = I + 1
            J = J - 1
        End If

    Wend

    If (lngMin < J) Then Call QuickSortVector(SortArray, lngMin, J)
    If (I < lngMax) Then Call QuickSortVector(SortArray, I, lngMax)

End Sub


' Routine to sort two arrays
' The second array is sorted with reference to the first
' see http://stackoverflow.com/questions/29923742/vba-sort-array-based-on-another-array
Public Sub sort2(key() As Variant, other() As Variant)
Dim I As Long, J As Long, Low As Long
Dim Hi As Long, Temp As Variant
    Low = LBound(key)
    Hi = UBound(key)

    J = (Hi - Low + 1) \ 2
    Do While J > 0
        For I = Low To Hi - J
          If key(I) > key(I + J) Then
            Temp = key(I)
            key(I) = key(I + J)
            key(I + J) = Temp
            Temp = other(I)
            other(I) = other(I + J)
            other(I + J) = Temp
          End If
        Next I
        For I = Hi - J To Low Step -1
          If key(I) > key(I + J) Then
            Temp = key(I)
            key(I) = key(I + J)
            key(I + J) = Temp
            Temp = other(I)
            other(I) = other(I + J)
            other(I + J) = Temp
          End If
        Next I
        J = J \ 2
    Loop
End Sub
