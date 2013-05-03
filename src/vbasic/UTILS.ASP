
<%
rem - utilities include file, place at top of pages


' Define standard (default) database connection info
dim stdConn, stdConnString
stdConnString = "DBQ=" & server.mappath("reports.mdb") & ";Driver={Microsoft Access Driver (*.mdb)};"
set stdConn = server.createobject("adodb.connection")
stdConn.open  stdConnString

if isblank(session("userID")) then
  hout "* USER SESSION HAS EXPIRED *"
  ' perhaps login screen should be called here
  response.end
end if

'-----------------
function contains(withInMe, findMe)  ' case-ignoring string subset
  contains = (instr(1,withInMe,findMe,1) > 0)
end function
'-----------------
function isBlank(theString)
  isBlank = (len(trim(thestring & "")) = 0)
end function
'-----------------
sub hout(theString)  ' hide the bloated OOP syntax
  response.write theString
end sub
'-----------------
function Delay(byVal secs)   ' delay at least given seconds (used for MS bug workaround)
  dim cursec, prevsec, changecount
  changecount = 0
  prevsec = now()
  cursec = prevsec
  do while changecount <= secs
    if prevsec <> cursec then
      changecount = changecount + 1
      prevsec = cursec
    end if
    cursec = now()
  loop
end function
'----------------------------------
sub executeSQL(sql)
  stdConn.beginTrans   ' transactions are work-around for bug in Jet DB
  stdConn.execute(sql)
  stdConn.commitTrans
  delay(1)  ' work-around for bug in Jet DB
end sub
'----------------------------------
sub trace2(p1, p2)     ' debugging util
  response.write " Trace(" & p1 
  if not isBlank(p2) then
    response.write ", " & p2
  end if
  response.write ") "
end sub
'----------------------------------
sub concat(target, byVal addMe)   ' concatenation shortcut because VBS lacks "&="
  target = target & addMe
end sub
'----------------------------------

%>

