<%option explicit

session("userID") = 1   ' 1 for demo purposes only
%>

<!--#include file="utils.asp"-->

<% REM - Report Listing

dim rs, sql

hout "<h3> Reports </h3><ul>"
sql = "SELECT * FROM Reports ORDER BY rptSequence "
set rs = stdConn.execute(sql)
do while not rs.eof
  hout "<li><a href=""report.asp?reportID=" & rs("reportID") & """>"
  hout rs("reportTitle") & "</a><br>"
  rs.moveNext
loop

' Hard-coded items
hout "<li><a href=""report.asp?reportID=3&rpt_prompt=no&rpt_show=yes"">"
hout "(Test Report without prompts)</a>"

hout "</ul><hr>"

%>

