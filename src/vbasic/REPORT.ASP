
<%option explicit
  REM - Reporting Framework Demo    (build no. 2)
  REM - HTTP Parameters: rpt_prompt (yes,no), rpt_show (yes,no)
  REM - From: http://geocities.com/tablizer/chal06.htm
  REM - This software cannot be used for commercial purposes
  REM - without written permission from the author.
  REM - Copyright 2001 by Findy Services and B. Jacobs.
%>

<!--#include file="utils.asp"-->

<body bgcolor="white">

<%

' Declare module-level vars
dim promptCrit, showReport, saveCrit, userID, errMsgs, reportID

' Constants
dim DBconst_true   ' what the DB uses for True values (varies per vendor)
DBconst_true = -1  ' for MS-Jet
dim DBconst_dateDelim    ' date delimiter
DBconst_dateDelim = "#"  ' MS-Jet

' Initalize
promptCrit = (lcase(request("rpt_prompt")<>"no"))
saveCrit =   (lcase(request("rpt_saveValues")="yes"))
showReport = (lcase(request("rpt_show")="yes"))              
userID =     session("userID")
errMsgs =    space(0)
reportID =   request("reportID")

if isBlank(reportID) then
  hout "<p><b>** REPORT NOT FOUND ** Try the <a href=""rptlist.asp"">list</a><p>"
  response.end
end if

if saveCrit then
  saveCritValues()
end if

if showReport then
  validateCriteria
  if Not hasErrors() then
    genReport
  end if
end if

if promptCrit then
  criteriaPrompts
end if

' Bottom navigation

%>
<div align="center">
  <hr>
  <a href="rptlist.asp">Reports List</a>
  <hr>
</div>
<%

'----------------------------
sub criteriaPrompts()  ' display report criteria prompts
  dim rs, sql, theVal, useWidth, useValue, temp, fldRef

  if hasErrors() then     ' Display any validation error messages
     hout "<font color=red><b>** PLEASE NOTE THE FOLLOWING ERRORS **</b></font>"
     hout "<ul>" & errMsgs & "</ul><p>"
  end if
  initializeUserReport userID, reportID
  ' Use report and report field description tables to build prompts
  sql = reportFieldsSQL(userID, reportID)
  set rs = stdConn.execute(sql)
  if rs.eof then
    hout " SORRY, Report items not set up yet "
  else
    hout "<h3>" & rs("reportTitle") & " Report Criteria </h3>"
    hout "<form method=post action=""report.asp"">"
    do while not rs.eof           ' for each prompt field
      useWidth = clng("0" & trim(rs("width")))
      if useWidth = 0 then
        useWidth = 20             ' default width if blank or zero
      end if
      fldRef = "fld_" & rs("itemID")
      useValue = trim(rs("fldValue") & "")
      hout titlePadder(rs("fldTitle"), Not rs("keepwithPrior"), 20)
      select case ucase(rs("fmtType"))
      case "T","N","D"    ' text, number, or date
        hout inputBox("text", fldRef, useValue, useWidth, useWidth)
      case "Y"    ' Boolean
        temp = trim(lcase(rs("fldValue")) & "")
        if isBlank(temp) or temp="(either)" then
          useValue = "(either)"
        elseif contains("1,true,yes,on", temp) then
          useValue = "Yes"
        else
          useValue = "No"
        end if
        hout pickList1(fldRef, useValue, "(either),Yes,No")
      case "L"    ' List
        hout pickList1(fldRef, useValue, "(any)," & rs("theList"))
      end select

      rs.movenext()
    loop
    hout "<p>"
    hout inputBox("hidden","rpt_saveValues","yes",0,0)
    hout inputBox("hidden","rpt_show","yes",0,0)
    hout inputBox("hidden","rpt_prompt",request("rpt_prompt"),0,0)
    hout inputBox("hidden","reportID",reportID,0,0)
    hout inputBox("submit","btnSubmit","View Report",0,0)
    hout " &nbsp; <a href=""under.asp"">Clear Criteria</a>"
    hout "</form>"
  end if
  rs.close
end sub
'-------------------------------------
sub ValidateCriteria()
  dim sql, rs, fldValue, fmtType
  sql = reportFieldsSQL(userID, reportID)
  set rs = stdConn.execute(sql)
  do while not rs.eof    ' for each report crit field
    fldValue = trim(rs("fldValue") & "")
    fmtType = ucase(trim(rs("fmtType") & ""))
    if fmtType="N" and len(fldValue) > 0 then         
      if not isNumeric(fldValue) then
        appendErr "Invalid Number: " & fldValue
      end if
    end if
    if fmtType="D" and len(fldValue) > 0 then         
      if not isDate(fldValue) then
        appendErr "Invalid Date: " & fldValue
      end if
    end if
    if rs("Required") and len(fldValue)=0 then
      appendErr "Field is Required: '" & rs("fldTitle") & "'"
    end if
    rs.moveNext
  loop
  rs.close
end sub
'-------------------------------------
sub saveCritValues()  ' save criteria responses to table from HTTP
  dim sql, sql2, rs
  sql = reportFieldsSQL(userID, reportID)
  stdConn.beginTrans   ' MS-Jet bug workaround
  set rs = stdConn.execute(sql)
  do while not rs.eof    ' for each report crit field
    sql2 = "UPDATE userFields SET fldValue='" & trim(request("fld_" & rs("itemID"))) & "' "
    sql2 = sql2 & " WHERE userID=" & userID & " AND rptItemID=" & rs("itemID")
    stdConn.execute(sql2)
    rs.moveNext
  loop
  rs.close
  stdConn.commitTrans
  delay(1)  ' MS-Jet bug workaround
end sub
'-------------------------------------
sub genReport()       ' Generate report based on built-up SQL statement
  dim sql, rs, fldValue, useValue, fmtType, useComparer, orderByClause
  dim whereClause, groupByClause, temp, title

  set rs = stdConn.execute(reportFieldsSQL(userID, reportID))
  if rs.eof then
    hout " ** REPORT ERROR. Contact admin. ** "
  else
    sql = "SELECT " & rs("selectClause") & " FROM " & rs("fromClause")
    title = rs("reportTitle")
    whereClause =   rs("whereClause")
    groupByClause = rs("groupByClause")
    orderByClause = rs("orderByClause")
    do while not rs.eof    ' for each report crit field
      fldValue = trim(rs("fldValue") & "")
      fmtType = ucase(trim(rs("fmtType") & ""))
      useValue = fldValue

      select case fmtType
      case "T"
         if isBlank(rs("comparer")) then
           useValue = "'%" & useValue & "%'"        ' for LIKE
         else
           useValue = "'" & useValue & "'"        ' quote wrap
         end if
      case "D"
         useValue = DBconst_dateDelim & useValue & DBconst_dateDelim
      case "N"
         ' leave as is
      case "Y"
         select case lcase(fldValue)
         case "(either)",""
           useValue = space(0)
         case "yes"
           useValue = DBconst_true
         case "no"
           useValue = 0
         end select
      case "L"
         if lcase(useValue) = "(any)" then
           useValue = space(0)
         else
           useValue = "'" & useValue & "'"
         end if
      end select

      '---Comparer
      if isblank(rs("comparer")) then
        useComparer = " = "
        if fmtType = "T" then
          useComparer = " LIKE "
        end if
      else
        useComparer = space(1) & rs("comparer") & space(1)
      end if

      '---Append field as an AND clause
      if not isBlank(useValue) and not isBlank(fldValue) then
        temp = rs("fldName") & useComparer & useValue
        if not isBlank(whereClause) then
          temp = " AND " & temp
        end if
        concat whereClause, temp
      end if
      '---
      rs.moveNext
    loop
    if not isBlank(whereClause) then
      whereClause = " WHERE " & whereClause     ' filler start because of AND statements
    end if
    if not isBlank(groupByClause) then
      groupByClause = " GROUP BY " & groupByClause
    end if
    sql = sql & whereClause & groupByClause & " ORDER BY " & orderByClause
    hout "<BR><B>TEST:</B> " & sql & "<BR>"
    displayQuery  sql, title ,100
  end if
  rs.close
end sub
'-------------------------------------
sub displayQuery(sql, title, maxRows)  ' display SQL query as an HTML table
  dim lineCnt, i, temps, maxCell, rs

  maxCell = 100   ' max field size
  set rs = stdConn.execute(sql)
  if rs.eof then
    hout "<p>SORRY, no matching records were found. Please try again.<p>"
  else
    lineCnt = 0
    hout "<h3>" & title & "</h3>"
    hout "<TABLE border=1 cellpadding=2 cellspacing=0>"
    do While Not rs.EOF and lineCnt < maxRows
      lineCnt = lineCnt + 1
      if (lineCnt - 1) mod 20 = 0 then       'show column names every now and then
        columnNames rs
      end if
      hout "<TR>"
      For i = 0 to rs.Fields.Count - 1
         temps = trim(rs(i))
         if isBlank(temps) then
            temps = "&nbsp;"
         end if
         if isnull(temps) then
            temps = "<fontcolor=""#b0b0b0"">Null</font>"
         end if
         if len(temps) > maxCell then           
           temps = left(temp, maxCell) & " ..."
         end if
         hout "<TD>" & temps & "</TD>"
      Next
      hout "</TR>"
      rs.MoveNext
    Loop
    hout "</TABLE>"
    if lineCnt >= maxRows and not rs.eof then
        Hout "NOTE: row <b>quota</b> has been reached. Perhaps you can adjust your query to return a more specific result set.<br>"
    end if
    rs.close
  end if
end sub
'-------------------------------------
sub columnNames(rs)  ' used with DisplaySQLQuery to show column names
  dim i
  hout "<tr bgcolor=""#f0f0f0"">"
  For i = 0 to rs.Fields.Count - 1 
    hout "<th>" & rs(i).Name & "</th>"
  Next
  hout "</tr>"
end sub
'-------------------------------------
function reportFieldsSql(UserID, reportID)
' Returns report criteria fields for given report and user
  dim sql
  sql = "SELECT * FROM (Reports "
  sql = sql & " INNER JOIN ReportCriteria ON reports.ReportID = reportCriteria.ReportRef) "
  sql = sql & " INNER JOIN userFields  ON reportCriteria.ItemID = userFields.rptItemID "
  sql = sql & " WHERE reportID = " & reportID & " AND userID = " & userID
  sql = sql & " ORDER BY sequence "
  reportFieldsSql = sql     
end function
'-------------------------------------
sub initializeUserReport(userID, reportID)
' Make sure there are per-user value field records for given report
  dim sql
  sql = "INSERT INTO userFields "
  sql = sql & " SELECT " & userID & " as userID, ItemID as rptItemId, defaultVal as fldValue "
  sql = sql & " FROM ReportCriteria WHERE reportRef = " & reportID & " AND ItemID NOT IN "
  sql = sql & " (SELECT rptItemID FROM userfields "
  sql = sql & " WHERE userfields.userID = " & userID & ")"
  executeSQL sql
end sub
'-------------------------------------
function picklist1(fldName, current, theList)  ' create an HTML picklist
  ' fldName=the HTML field name, current=default item(if any), theList=comma-seperated list 
  dim i, r    
  r = "<select name=""" & fldname & """>"
  if not isBlank(current) then
    r = r & "<option>" & current
  end if
  for each i in split(theList,",")
    if trim(lcase(i)) <> trim(lcase(current) & "") then   ' dont repeat default
      r = r & "<option>" & i
    end if
  next
  r = r & "</select>"
  picklist1 = r
end function
'-------------------------------------
function inputBox(hType, fldName, fldValue, width, maxLength)
  ' Generate an HTML output box. Zero widths exclude tag
  dim r
  r = "<input type=""" & hType & """ "
  r = r & " value=""" & trim(fldValue) & """ "
  r = r & " name="""  & fldName & """ "
  if not width="0" and not width="" then
    r = r & " size=" & width
  end if
  if not maxLength="0" and not maxLength="" then
    r = r & " maxlength=" & maxLength
  end if
  inputBox = r & ">"
end function
'-------------------------------------
sub appendErr(msg)        ' Append an error message to the error list
  errMsgs = errMsgs & "<li>" & msg
end sub
'-------------------------------------
function hasErrors()      ' Returns True if any validation error messages
  hasErrors = (not isBlank(errMsgs))
end function
'-------------------------------------
function titlePadder(fldTitle, isBreak, minWidth)
' Format field title for consistent appearence.
' A future version will use HTML tables.
  dim r      ' r=result
  r = fldTitle
  if isBreak then
    if len(fldTitle) < minWidth then
      r = r & ".........................................."
      r = left(r, minWidth)
    end if
    r = "<br><tt>" & r & "</tt> "
  else
    r = space(1) & r & space(1)
  end if
  titlePadder = r 
end function
'-------------------------------------

%>
