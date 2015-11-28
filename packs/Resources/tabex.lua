function tab:hasloadedurl(warnuser)
 local valid = false
 if slx.string.beginswith(self.url,'http') then
  valid = true
 elseif slx.string.beginswith(self.url,'file') then
  valid = true
 elseif slx.string.beginswith(self.url,'chrome') then
  valid = true
 end
 
 if valid == false then
  if warnuser then
   app.showmessage('No URL loaded.')
  end
 end
 return valid
end