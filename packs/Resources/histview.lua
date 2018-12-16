local M = {}
M.listfilter = 'Syhunt URL List (*.sclist)|*.sclist'

function M:ViewHistory(newtab)
 local t = {}
 t.newtab = newtab
 t.toolbar = 'Resources.pak#histview_tbhistory.html'
 t.histname = 'History'
 t.style = [[
  tr.item { context-menu: selector(#historymenu); }
  ]] 
 t.menu = [[
  <li onclick="HistView:DeleteURLLogItem('%i','History')">Delete</li>
  ]]  
 t.showvisicol = true
 self:ViewURLLogFile(t)
end

function M:ViewBookmarks(newtab)
 local t = {}
 t.newtab = newtab
 t.toolbar = 'Resources.pak#histview_tbbookmarks.html'
 t.histname = 'Bookmarks'
 t.tabicon = 'url(Resources.pak#16/icon_bookmarks.png)'
 t.style = [[
  ]]
 t.menu = [[
  <li onclick="HistView:DeleteURLLogItem('%i','Bookmarks')">Delete</li>
  ]]  
 self:ViewURLLogFile(t)
end

-- ToDo WIP: This function should soon replace procedure
-- TSandcatSettings.AddToURLList() in uSettings.pas
function M:AddURLLogItem(item,logname)
 local logfile = browser.info.configdir..logname..'.sclist'
 local unixtime = os.time(os.date("!*t"))
 item.name = item.name or ''
 item.name = ctk.html.escape(item.name)
 item.url = ctk.html.escape(item.url)
 local sl = ctk.string.list:new()
 if ctk.file.exists(logfile) then
  sl:loadfromfile(logfile)
 end
 local id = tostring(unixtime)..'-'..tostring(sl.count)
 local linecontent = '<item id="'..id..'" url="'..item.url..'" name="'..item.name..'"/>'
 sl:insert(0, linecontent)
 sl:savetofile(logfile)
 sl:release()
end

function M:GetURLLogItem(itemid,logname)
 function getvalue(line, name)
    local s = ctk.string.after(line, ' '..name..'="')
    s = ctk.string.before(s, '"')
    s = ctk.html.unescape(s)
    return s
 end
 local logfile = browser.info.configdir..logname..'.sclist'
 local slp = ctk.string.loop:new()
 local i = nil
 local found = false
 if ctk.file.exists(logfile) then
  slp:loadfromfile(logfile)
  while slp:parsing() do
   if ctk.string.match(slp.current,'<item*id="'..itemid..'"*>') then
    i = {}
    i.url = getvalue(slp.current, 'url')
    i.id = itemid
   end
  end
 end
 slp:release()
 return i
end

function M:DeleteURLLogItem(itemid,logname)
 local logfile = browser.info.configdir..logname..'.sclist'
 local slp = ctk.string.loop:new()
 local found = false
 if ctk.file.exists(logfile) then
  slp:loadfromfile(logfile)
  while slp:parsing() do
   if ctk.string.match(slp.current,'<item*id="'..itemid..'"*>') then
    slp:curdelete()
    found = true
   end
  end
  if found == true then
   slp:savetofile(logfile)
  end
 end
 slp:release()
 tab.engine:eval('$("#'..itemid..'").remove()')
end

function M:GetSiteName(url)
 local jsonfile = prefs.getsiteprefsfilename(url)
 local name = ''
 if ctk.file.exists(jsonfile) then
   local j = ctk.json.object:new()
   j:loadfromfile(jsonfile)
   name = j['site.name']
   if name == nil then
     name = ''
   end
   j:release()
 end
 return name
end

function M:ClearURLLogFile(logname,onendlua)
 local histfile = browser.info.configdir..logname..'.sclist'
 local resp=app.ask_yn('Are you sure you want to clear "'..logname..'"?')
 if resp == true then
   if ctk.file.exists(histfile) == true then 
     ctk.file.delete(histfile)
     if onendlua ~= nil then
       browser.dostring(onendlua)
     end   
   end
 end
end

function M:ImportURLLogFile(logname,onendlua)
 local histfile = browser.info.configdir..logname..'.sclist'
 local srcfile = app.openfile(self.listfilter,'sclist')
 if ctk.file.exists(srcfile) == true then
   local l = ctk.string.list:new()
   local slp = ctk.string.loop:new()
   if ctk.file.exists(histfile) == true then
     l:loadfromfile(histfile)
   end
   slp:loadfromfile(srcfile)
   while slp:parsing() do
     if l:indexof(slp.current) == -1 then
       l:add(slp.current)
     end
   end
   l:savetofile(histfile)
   slp:release() 
   l:release()
  --ctk.file.copy(srcfile, histfile) 
  if onendlua ~= nil then
    browser.dostring(onendlua)
  end
 end 
end

function M:ExportURLLogFile(logname)
 local unixtime = os.time(os.date("!*t"))
 local histfile = browser.info.configdir..logname..'.sclist'
 local sugfn = ctk.file.getname(histfile)
 sugfn = ctk.string.before(sugfn, '.sclist')
 sugfn = sugfn..' '..tostring(unixtime)..'.sclist'
 local destfile = app.savefile(self.listfilter,'sclist',sugfn)
 if destfile ~= '' then
  ctk.file.copy(histfile, destfile)
 end 
end

function M:ViewURLLogFile(conf)
 local html = Sandcat:getfile('histview.html')
 local histname = conf.histname
 local histfile = browser.info.configdir..histname..'.sclist'
 local sl = ctk.string.list:new()
 if conf.newtab == nil then
  conf.newtab = true
 end
 conf.style = conf.style or ''
 conf.menu = conf.menu or ''
 conf.tabicon = conf.tabicon or '@ICON_BLANK'
 conf.showvisicol = conf.showvisicol or false
 conf.readsiteprefs = conf.readsiteprefs or false
 
 if conf.showvisicol == false then
  conf.style = conf.style..[[
  th.visited { display: none; }
  td.visited { display: none; }
  ]] 
 end
 
 if ctk.file.exists(histfile) then
  local p = ctk.html.parser:new()
  p:load(ctk.file.getcontents(histfile))
  while p:parsing() do
   if p.tagname == 'item' then
    local url = p:getattrib('url')
    local visited = p:getattrib('visited')
    local name = p:getattrib('name')
    local id = p:getattrib('id')
    if conf.readsiteprefs == true then
      name = ctk.html.escape(self:GetSiteName(url))
    end
    sl:add('<tr.item url="'..url..'" role="option" style="context-menu: selector(#menu'..id..');" id="'..id..'">')
    sl:add('<td>'..name..'</td>')
    sl:add('<td>'..url..'</td>')
    sl:add('<td.visited>'..visited..'</td>')
    sl:add('<menu.context id="menu'..id..'">'..ctk.string.replace(conf.menu,'%i',id)..'</menu>')
    sl:add('</tr>')
   end
  end
  p:release()
 end
 html = ctk.string.replace(html,'%style%',conf.style)
 html = ctk.string.replace(html,'%history%',sl.text)
 sl:release()
 if conf.newtab then
  local j = {}
  j.title = histname
  j.tag = string.lower(histname)..'view'
  j.toolbar = conf.toolbar
  j.icon = conf.tabicon
  j.html = html
  browser.newtabx(j)
 else
  tab:loadx(html)
 end
end

return M