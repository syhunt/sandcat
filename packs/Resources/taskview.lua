TaskView = {}
TaskView.uitable = 'TaskView.ui'

function TaskView:loadlist(newtab,tabtitle,list)
 local html = Sandcat:getfile('taskview.html')
 local slp = scl.listparser:new()
 local htmllist = scl.stringlist:new()
 local info = {}
 local tid = ''
 local pid = ''
 slp:load(list)
 while slp:parsing() do
  tid = slp.current
  info = browser.gettaskinfo(tid)
  pid = tostring(info.pid)
  if pid == '0' then
   pid = ''
  end
  htmllist:add('<tr role="option" style="context-menu: selector(#'..tid..'-menu);" ')
  if info.ondblclick ~= '' then
   htmllist:add('ondblclick="'..scop.html.escape(info.ondblclick)..'" ')
  end
  htmllist:add('>')
  htmllist:add('<td><img .lvfileicon src="'..info.progressicon..'"> <img .lvfileicon src="'..info.icon..'" filename="'..scop.html.escape(info.filename)..'">&nbsp;'..scop.html.escape(info.caption)..'</td>')
  htmllist:add('<td>'..scop.html.escape(info.progressdesc)..'</td>')
  htmllist:add('<td>'..scop.html.escape(info.status)..'</td>')
  htmllist:add('<td>'..tid..'</td>')
  htmllist:add('<td>'..pid..'</td>')
  if info.menuhtml ~= '' then
   htmllist:add('<menu.context id="'..tid..'-menu">')
   htmllist:add(info.menuhtml)
   htmllist:add('</menu>')
  end
  htmllist:add('</tr>')
 end
 html = stringop.replace(html,'%tasks%',htmllist.text)
 --app.showmessage(html)
 if newtab == nil then
  local j = {}
  j.title = tabtitle
  j.toolbar = 'Resources.pak#taskview_tb'..string.lower(tabtitle)..'.html'
  j.table = self.uitable
  j.tag = 'taskview'..string.lower(tabtitle)
  j.icon = 'url(Resources.pak#16/icon_'..string.lower(tabtitle)..'.png)'
  j.html = html
  browser.newtabx(j)
 else
  tab:loadx(html,self.uitable)
 end
 htmllist:release()
 slp:release()
end

function TaskView:loaddownloads(newtab)
 self:loadlist(newtab,'Downloads',browser.info.downloads)
end

function TaskView:loadtasks(newtab)
 self:loadlist(newtab,'Tasks',browser.info.tasks)
end