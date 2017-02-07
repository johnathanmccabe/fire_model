Attribute VB_Name = "mdl_DatabaseConnection"

Option Explicit
 
Function fn_DatabaseConnect() As ADODB.Connection

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' pseudo-function as sets rst without it being an actual return value
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'recordset passed by reference so should amend existing recordset
'error handling - ensure quits and restores initial settings
'On Error GoTo Err_fn_DatabaseConnect

    Dim cnt As ADODB.Connection
    Dim stConn As String
    
     'Instantiate the Connectionobject.
    Set cnt = New ADODB.Connection
     
     'Create the connectionstring.
'    stConn = "Provider=Microsoft.Jet.OLEDB.4.0;" & _
'    "Data Source=" & stDB & ";"
     
     stConn = ConnString()
     
     'Create the SQL-statement.
     
    With cnt
        .CursorLocation = adUseClient 'Necesary for creating disconnected recordset.
        OpenConnection cnt, stConn 'Open connection.
        
    End With
    
    Set fn_DatabaseConnect = cnt
    
     'Close the connection.
    cnt.Close
    Set cnt = Nothing

Exit Function
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
     
Err_fn_DatabaseConnect:
     'Restore the settings.
    ImprovePerformanceStop
     
    'Release objects from memory - recordset must be cleared later
    'MsgBox wrn_ConnectionFailure
    Set cnt = Nothing
    End
End Function



Sub OpenConnection(con As ADODB.Connection, str As String)

'On Error GoTo OpenConn_Error

    con.Open str
    Exit Sub
    
OpenConn_Error:
    ImprovePerformanceStop
    End
End Sub

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'<comment>
'Procedures to retrieve data from Pricing database
'</comment>
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Function fn_RetrieveData(strSQL As String) As ADODB.Recordset

Dim con As New ADODB.Connection
Dim temp As ADODB.Recordset
Set temp = New ADODB.Recordset


Set con = fn_DatabaseConnect()

con.Open
With temp
    .ActiveConnection = con
    .CursorLocation = adUseClient
    .LockType = adLockOptimistic
    .Source = strSQL
    .Open
End With

Set fn_RetrieveData = temp


'clean up
Set temp = Nothing
Exit Function

Err_fn_RetrieveData:

'return Nothing if error
Set temp = Nothing
Set fn_RetrieveData = temp

End Function





Sub CheckADOReferenceEnabled()
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Adds ADO reference to enable saving to database
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
On Error GoTo Err_handle:

Dim ref As Reference

For Each ref In Application.VBE.ActiveVBProject.References
    If ref.Description = "Microsoft ActiveX Data Objects 2.8 Library" Then
        Exit Sub
    End If
Next

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Reference Hasn't been found so add
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
With Application.FileSearch
    .NewSearch
    .LookIn = "C:\Program Files\Common Files\System\ado"
    .Filename = "msado15.dll"
    If .Execute(SortBy:=msoSortByFileName, SortOrder:=msoSortOrderDescending) > 0 Then
        Application.VBE.ActiveVBProject.References.AddFromFile .FoundFiles(1)
    End If
End With

Exit Sub


Err_handle:
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Access to VB Projects is not trusted - Ask user to change settings then restart spreadsheet.
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

MsgBox wrn_ADOReferenceMissing
    
End Sub

Function ConnString() As String
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Initialise the database connection string - replace parameters with entries on the parameter info sheet
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Dim server As String
'Dim db As String
'Dim uid As String
'Dim pwd As String
'Dim temp As String
'
'With Sheets(sht_ParameterInfo)
'    server = .Range("pi_Server").Value
'    uid = .Range("pi_UID").Value
'    pwd = .Range("pi_PWD").Value
'    db = .Range("pi_DB").Value
'End With
'
'temp = SL_DB_CONNECTION_STRING
'temp = Replace(temp, "$server$", server)
'temp = Replace(temp, "$Db$", db)
'temp = Replace(temp, "$user$", uid)
'temp = Replace(temp, "$Password$", pwd)
'
'SLDBConnString = temp


ConnString = "Provider=Microsoft.Jet.OLEDB.4.0;" & _
    "Data Source=" & cnst_DatabasePathLocal & ";"

End Function


