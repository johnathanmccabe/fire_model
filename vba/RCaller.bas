Attribute VB_Name = "RCaller"
' (C) Gregory Demin, excel.link.feedback@gmail.com, 2015
' https://github.com/gdemin/excel.link
' MIT License
'
' Code for window hiding is borrowed from http://stackoverflow.com/questions/19166654/wscript-command-run-minimized-msaccess-vba
' and authored by Stackoverflow user B Hart
'
' Code for working with registry borrowed from http://www.slipstick.com/developer/read-and-change-a-registry-key-using-vba/
' and authored by Diane Poremsky

Option Explicit
Option Private Module

' It is possible to change consts to  public variables and set their values in macros


' Priority in search R
' 1. PathToR - if R not found here we will go further
' 2. If RNearThisWorkbook = True we search near workbook
' 3. Environement variable R_HOME
' 4. Registry
' If R not found in all above mentioned places error will raised.

Public Const RNearThisWorkbook = True ' If true we will seek R installation in the folder with the workbook (ThisWorkbook.Path & "/R") and after that in the system. It should be standard installation R/bin/i386
Public Const PathToR = "" ' Path to R folder (not R executable). If equals "" we will try to find R in environment variable and registry. Ignored if RNearThisWorkbook = TRUE
Public Const Use32bit = True ' Should we use 32 bit version of R. If we want max compatibility set it to TRUE


' R commands that will be executed just after initialization. By default we will try to load excel.link to have ability to return results from R back to Excel.
' If 'excel.link' not found we will prompt user to install it
' To workaround insuffiecient privileges we install package
' in temp folder which we create in VBA. That's why string %library%.
' We change %library% to path to temp folder on this machine.
' You can set InitString to empty to remove COM and excel.link dependency but in this case results or R process should be retreived only as R text output. So UDF function will be useless.
' InitString is used in Evaluate and Exec
' FinString is only used in Exec
Public Const InitString = "if(!require('excel.link')) {" & vbNewLine _
    & ".libPaths('%library%') " & vbNewLine _
    & "if(!require('excel.link')) {" & vbNewLine _
    & "library(tcltk); " & vbNewLine _
    & "res = tkmessageBox(message = 'Package \'excel.link\' not found. Would you like to install it?', title = 'R',type='okcancel'); " & vbNewLine _
    & "if (tclvalue(res) == 'ok') {" & vbNewLine _
    & "install.packages('excel.link', repos = c('http://cran.at.r-project.org/')); " & vbNewLine _
    & "if(!require('excel.link')) {tkmessageBox(message = 'Something going wrong. Quitting.', title = 'R'); q(); }} " & vbNewLine _
    & "else {q()} " & vbNewLine _
    & "}}"


Public Const FinString = ""



' In general there are no reasons to modify constants below
Public Const Use_hWnd = True ' if true we give to R Excel hWnd to correctly get exactly this Excel instance
' Command-line arguments for rterm.exe
Public Const rterm_params = "--no-save --no-restore --silent --ess" ' Params for rterm.exe for live session
' Integer. Delay in seconds before hiding console window. Try to increase it if window doesn't hide.
Public Const DelayBeforeHide = 1



' Public objects - it is not recommended to modify them.
Public objR As Object
Public objShell As Object ' WshShell
Public FO As Object ' FileSystemObject

' From http://stackoverflow.com/questions/19166654/wscript-command-run-minimized-msaccess-vba
' Copyright B Hart
'   ShowWindow() Commands
Public Const SW_HIDE = 0
Public Const SW_MINIMIZE = 6
'GetWindow Constants
Public Const GW_CHILD = 5
Public Const GW_HWNDFIRST = 0
Public Const GW_HWNDLAST = 1
Public Const GW_HWNDNEXT = 2
Public Const GW_HWNDPREV = 3
Public Const GW_OWNER = 4
'   API Functions
Public Declare PtrSafe Function ShowWindow Lib "User32" (ByVal hwnd As LongPtr, ByVal nCmdShow As LongPtr) As LongPtr
Public Declare PtrSafe Function GetWindow Lib "User32" (ByVal hwnd As LongPtr, ByVal wCmd As LongPtr) As LongPtr
Public Declare PtrSafe Function GetDesktopWindow Lib "User32" () As LongPtr
Public Declare PtrSafe Function GetWindowThreadProcessId Lib "User32" (ByVal hwnd As LongPtr, lpdwProcessId As LongPtr) As LongPtr
Declare PtrSafe Function FindWindowEx Lib "User32" Alias "FindWindowExA" (ByVal hWnd1 As LongPtr, ByVal hWnd2 As LongPtr, ByVal lpsz1 As String, ByVal lpsz2 As String) As LongPtr


' ***********************************************
' Stateless call
' R executes code and terminates. It doesn't keep
' objects and data between calls. There are two
' flavours - synchronious and asynchronious call.
' In synchronious call Excel waits for R. Also we
' can get R output and exception is raised if
' there is an error in R code.
' By now stateless synchronious call is most robust
' way of calling R.
' ***********************************************


' ***********************************************
' Sync call
' ***********************************************
' Lines as string - R Code
' Success as variant - if this argument is missing
'          then exception will raised in case of error
'          in R code. If argument is provided than it
'          will be True in case of succesfull execution
'          and False in opposite case.
' NeedOutput as boolean - if True then R output will be
'           returned as result of function call.
'           If False empty string will be returned.
Public Function ExecInR(ByVal Lines As String, Optional ByRef Success As Variant, Optional NeedOutput As Boolean = True) As String
    Dim strPath As String
    Dim ScriptPath As String
    Dim res As Long
    Dim TempFolder As Object
    Dim SourceFileName As String
    Dim ResultFileName As String
    Dim ResultFile As Object
    Dim SourceFile As Object
    Dim Arch As String
    Dim strCode As String
    If FO Is Nothing Then
        Set FO = CreateObject("Scripting.FileSystemObject")
    End If
    strPath = r_home()
    Set TempFolder = FO.GetSpecialFolder(2)
    SourceFileName = FO.GetTempName
    Set SourceFile = TempFolder.CreateTextFile(SourceFileName)
      
    'strCode = ".libPaths('" & Replace(strPath, "\", "/") & "/library')" & vbNewLine
    strCode = strCode & ProcessInitString(InitString) & vbNewLine
    If NeedOutput Then
        ResultFileName = Replace(TempFolder.Path & "/" & FO.GetTempName, "\", "/")
        strCode = strCode & "sink('" & ResultFileName & "')" & vbNewLine
    End If
    SourceFile.Write strCode & vbNewLine & Lines & vbNewLine & FinString
    SourceFile.Close
    Set objShell = CreateObject("Wscript.Shell")
    strPath = strPath & "/bin/" & "Rscript.exe "
    ScriptPath = TempFolder.Path & "/" & SourceFileName
    If Use32bit Then
        Arch = " --arch i386 "
    Else
        Arch = " --arch x64 "
    End If
    res = objShell.Run("""" & strPath & """ " & Arch & """" & ScriptPath & """", 0, True)
    If FO.FileExists(ScriptPath) Then FO.DeleteFile ScriptPath
    If IsMissing(Success) Then
        If res <> 0 Then
            If NeedOutput Then If FO.FileExists(ResultFileName) Then FO.DeleteFile ResultFileName
    
            Err.Raise vbObjectError + 1022, Source:="RCaller module.ExecInR", Description:="Error in R script."
        End If
    Else
        If (res <> 0) Then Success = False Else Success = True
    End If
    If NeedOutput Then
        If FO.FileExists(ResultFileName) Then
            Set ResultFile = FO.OpenTextFile(ResultFileName)
            If Not ResultFile.AtEndOfStream Then
                ExecInR = ResultFile.ReadAll
            Else
                ExecInR = ""
            End If
            ResultFile.Close
            FO.DeleteFile ResultFileName
        Else
           ExecInR = ""
        End If
    Else
        ExecInR = ""
    End If
End Function

' The same as ExecInR but  Range object with R code instead of string argument
Public Function ExecRangeInR(ByVal rArg As Range, Optional ByRef Success As Variant, Optional NeedOutput As Boolean = True) As String
    Dim aCell As Variant
    Dim strArg As String
    For Each aCell In rArg.Cells
        strArg = strArg & vbNewLine & aCell.Value
    Next
    If IsMissing(Success) Then
        ExecRangeInR = ExecInR(strArg, , NeedOutput)
    Else
        ExecRangeInR = ExecInR(strArg, Success, NeedOutput)
    End If
End Function



' ***********************************************
' Async call
' ***********************************************

Public Sub ExecInR_async(ByVal Lines As String, Optional ByVal OutputFilePath As String = "")
    Dim strPath As String
    Dim ScriptPath As String
    Dim res As Long
    Dim TempFolder As Object
    Dim SourceFileName As String
    Dim SourceFile As Object
    Dim Arch As String
    Dim strCode As String
    If FO Is Nothing Then
        Set FO = CreateObject("Scripting.FileSystemObject")
    End If
    strPath = r_home()
    Set TempFolder = FO.GetSpecialFolder(2)
    SourceFileName = FO.GetTempName
    Set SourceFile = TempFolder.CreateTextFile(SourceFileName)
    'strCode = ".libPaths('" & Replace(strPath, "\", "/") & "/library')" & vbNewLine
    strCode = strCode & ProcessInitString(InitString) & vbNewLine
    If OutputFilePath <> "" Then
        OutputFilePath = Replace(OutputFilePath, "\", "/")
        strCode = strCode & "sink('" & OutputFilePath & "')" & vbNewLine
    End If
    SourceFile.Write strCode & vbNewLine & Lines
    SourceFile.Close
    Set objShell = CreateObject("Wscript.Shell")
    strPath = strPath & "/bin/" & "Rscript.exe "
    ScriptPath = TempFolder.Path & "/" & SourceFileName
    If Use32bit Then
        Arch = " --arch i386 "
    Else
        Arch = " --arch x64 "
    End If
    objShell.Run """" & strPath & """ " & Arch & """" & ScriptPath & """", 0, False
    
End Sub

Public Sub ExecRangeInR_async(ByVal rArg As Range, Optional ByVal OutputFilePath As String = "")
    Dim aCell As Variant
    Dim strArg As String
    For Each aCell In rArg.Cells
        strArg = strArg & vbNewLine & aCell.Value
    Next
    ExecInR_async strArg, OutputFilePath
End Sub



' *******************************************
' HIGHLY UNSTABLE AND EXPERIMENTAL FUNCTIONS
' *******************************************

' ************************************************
' Live session with R
' R starts and work in the background. You can send commands to it .
' All objects and variables are kept between calls.
' It is usefull when large block of data should be transferred for different processing
' - it is possible to made this transfer only once.
' Also it is quickier than other methods - R starts only once.
' Drawbacks:
' 1. R blinks during its startup, method is not so stable
' 2. All calls are asynchronouos - it may cause problems
'   if you need some action (such as formatting results) just after actions in R.
' 3. No output, no error messages
'
' ************************************************

Public Sub InitR()
    Dim strPath
    Set objShell = CreateObject("Wscript.Shell")
    strPath = r_home() & "/bin/" & Use32path & "/Rterm.exe"
    Set objR = objShell.Exec("""" & strPath & """ " & rterm_params)
    objR.StdIn.WriteLine ProcessInitString(InitString)
    Application.Wait (Now + TimeValue("0:00:" & DelayBeforeHide))
    Call HideWindow(objR.ProcessID)
End Sub

Public Sub ShutdownR()
    Set objR = Nothing
End Sub

Public Sub EvaluateRangeInR(ByVal rArg As Range)
    Dim aCell As Variant
    For Each aCell In rArg
        EvaluateInR aCell.Value2
    Next aCell
End Sub

Public Sub EvaluateInR(ByVal Lines As String)
    Dim Line As Variant
    Dim arrLines As Variant
    If objR Is Nothing Then
        InitR
    Else
        If objR.Status <> 0 Then   ' WshRunning = 0
            InitR
        End If
    End If
    arrLines = Split(Lines, vbNewLine)
    For Each Line In arrLines
        objR.StdIn.WriteLine Line & vbNewLine
    Next
End Sub


' ********** UDF (only async call is possible) ***************

Public Function REvaluate(Code As Variant, ParamArray Precedents() As Variant) As Variant
    Dim Dumb As String
    Dim ParamCounter As Long
    ' Strange manipulations with dumb for execution in correct order
    If VarType(Code) = vbString Then
        EvaluateInR Code
        Dumb = Rnd(Len(Code) + 0.1)
    Else
        EvaluateRangeInR Code
        Dumb = Rnd(Len(Code.Cells(1, 1)) + 0.1)
    End If
    For ParamCounter = LBound(Precedents) To UBound(Precedents)
        If VarType(Precedents(ParamCounter)) = 8204 Then ' range
            Dumb = Dumb + Rnd(Len(Precedents(ParamCounter).Cells(1, 1)) + 0.1)
        Else
            Dumb = Dumb + Rnd(Len(CStr(Precedents(ParamCounter))) + 0.1)
        End If
    Next ParamCounter
    
    REvaluate = "#REvaluate#" & " " & Rnd(Dumb)
End Function




'**********************************************
' Auxilary functions
'**********************************************


' Build path to R home directory.
' If R not found will raise an error
Private Function r_home() As String
    Dim strPath As String
    
    If FO Is Nothing Then
        Set FO = CreateObject("Scripting.FileSystemObject")
    End If
    If PathToR <> "" Then
        If FO.FileExists(PathToR & "/bin/r.exe") Then
               r_home = PathToR
               Exit Function
        End If
    End If
    If RNearThisWorkbook Then
       strPath = ThisWorkbook.Path & "\R"
       If FO.FileExists(strPath & "/bin/r.exe") Then
           r_home = strPath
           Exit Function
       End If
    End If
    strPath = Environ("R_HOME")
    If strPath <> "" Then
        If FO.FileExists(strPath & "/bin/r.exe") Then
            r_home = strPath
            Exit Function
        End If
    End If
    strPath = GetPathFromRegistry()
    If Not FO.FileExists(strPath & "/bin/r.exe") Then
        Err.Raise vbObjectError + 1023, Source:="RCaller module", Description:="R not found. Please, install R (www.r-project.org) or set correct path."
    End If
    r_home = strPath
End Function

Private Function GetPathFromRegistry() As String
    Const usr = "HKEY_CURRENT_USER\"
    Const machine = "HKEY_LOCAL_MACHINE\"
    Dim temp As Object
    Dim strComputer As String
    Dim rPath As String
    Dim res As Variant
    Dim strValue As String
    
    rPath = "Software\R-core\R\InstallPath"
    If RegKeyExists(usr & rPath) Then
        GetPathFromRegistry = RegKeyRead(usr & rPath)
        Exit Function
    Else
        If RegKeyExists(machine & rPath) Then
            GetPathFromRegistry = RegKeyRead(machine & rPath)
            Exit Function
        End If
    End If
    GetPathFromRegistry = ""
End Function

' Return path fragment according to Use32bit flag
Private Function Use32path() As String
    If Use32bit Then
       Use32path = "i386"
    Else
       Use32path = "x64"
    End If
End Function

' Create library folder in temp
Private Function ProcessInitString(strInit As String) As String
    Dim TempFolder As Object
    Dim FullPathToLibrary As String
    Const RLibraryFolder = "RLibrary"
    If strInit = "" Then Exit Function
    If FO Is Nothing Then
        Set FO = CreateObject("Scripting.FileSystemObject")
    End If
    Set TempFolder = FO.GetSpecialFolder(2)
    FullPathToLibrary = Replace(TempFolder.Path & "\" & RLibraryFolder, "\", "/")
    If Not FO.FolderExists(FullPathToLibrary) Then FO.CreateFolder (FullPathToLibrary)
    If Use_hWnd Then
        strInit = strInit & vbNewLine & "options(excel_hwnd = " & GetThisExcelHwnd() & "L)"
    End If
    ProcessInitString = Replace(strInit, "%library%", FullPathToLibrary)

End Function

Private Function GetThisExcelHwnd() As LongPtr
    Dim hWndDesk As LongPtr
    Dim hWndDesk2 As LongPtr

    hWndDesk = FindWindowEx(Application.hwnd, 0&, "XLDESK", vbNullString)
    hWndDesk2 = FindWindowEx(hWndDesk, 0&, "EXCEL7", vbNullString)
    GetThisExcelHwnd = hWndDesk2
End Function

' From http://stackoverflow.com/questions/19166654/wscript-command-run-minimized-msaccess-vba
' Copyright by Stackoverflow user B Hart


Private Function HideWindow(iProcessID)
    Dim lngWinHwnd As LongPtr
    Do
        lngWinHwnd = GetHwndFromProcess(CLng(iProcessID))
        DoEvents
    Loop While lngWinHwnd = 0
    HideWindow = ShowWindow(lngWinHwnd, SW_HIDE)
End Function

Private Function GetHwndFromProcess(p_lngProcessId As LongPtr) As LongPtr
    Dim lngDesktop As LongPtr
    Dim lngChild As LongPtr
    Dim lngChildProcessID As LongPtr
    'On Error Resume Next
    lngDesktop = GetDesktopWindow()
    lngChild = GetWindow(lngDesktop, GW_CHILD)
    Do While lngChild <> 0
        Call GetWindowThreadProcessId(lngChild, lngChildProcessID)
        If lngChildProcessID = p_lngProcessId Then
            GetHwndFromProcess = lngChild
            Exit Do
        End If
        lngChild = GetWindow(lngChild, GW_HWNDNEXT)
    Loop
    'On Error GoTo 0
End Function

' End of code from Stackoverflow

' ####################





' #################################################
' From http://www.slipstick.com/developer/read-and-change-a-registry-key-using-vba/
' by Diane Poremsky

'reads the value for the registry key i_RegKey
'if the key cannot be found, the return value is ""
Private Function RegKeyRead(i_RegKey As String) As String
Dim myWS As Object
 
  'On Error Resume Next
  'access Windows scripting
  Set myWS = CreateObject("WScript.Shell")
  'read key from registry
  RegKeyRead = myWS.RegRead(i_RegKey)
End Function
 

 
'returns True if the registry key i_RegKey was found
'and False if not
Private Function RegKeyExists(i_RegKey As String) As Boolean
Dim myWS As Object
 

  'access Windows scripting
  Set myWS = CreateObject("WScript.Shell")
  'try to read the registry key
  On Error GoTo ErrorHandler
  myWS.RegRead i_RegKey
  'key was found
  RegKeyExists = True
  Exit Function
   
ErrorHandler:
  'key was not found
  RegKeyExists = False
End Function
