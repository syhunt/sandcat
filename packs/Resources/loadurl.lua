function trim(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

function PrintURLInfo()
  print(tab.url..' loaded.')
  local server = ctk.http.getheader(tab.rcvdheaders,'Server')
  server = trim(server)
  if server ~= '' then
   print('Server: '..server)
  end
end

if tab.statuscode ~= 0 then
 PrintURLInfo()
end