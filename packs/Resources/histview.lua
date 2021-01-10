local M = {}
M.listfilter = 'Sandcat URL List (*.sclist)|*.sclist|List files (*.lst;*.list)|*.lst;*.list|CSV (Comma-Separated Values) files (*.csv)|*.csv'

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

function M:AddBookmark()
  local d = {}
  d.title = 'Add Bookmark'
  d.name_caption = 'Name (eg: MySite)'
  d.value_caption = 'URL'
  local r = Sandcat.Preferences:EditNameValue(d)
  if r.res == true then
    browser.bookmark(r.name, r.value)
    self:ViewBookmarks(false)
  end
end

function M:AddURLLogItemToList(sl, item)
 local unixtime = os.time(os.date("!*t"))
 local canadd = true
 item.name = item.name or ''
 item.name = ctk.html.escape(item.name)
 item.url = ctk.html.escape(item.url)
 local id = tostring(unixtime)..'-'..tostring(sl.count)
 local linecontent = '<item id="'..id..'" url="'..item.url..'" name="'..item.name..'"/>'
 if (item.repeaturlallow == false) and (ctk.string.occur(sl.text, 'url="'..item.url..'"') ~= 0) then
   canadd = false
   if (item.repeaturlwarn == true) then
     app.showmessage('URL already in list.')
   end   
 end 
 if (item.repeatnameallow == false) and (ctk.string.occur(sl.text, 'name="'..item.name..'"') ~= 0) then
   canadd = false
   if (item.repeatnamewarn == true) then
     app.showmessage(item.name..' already in list.')
   end
 end  
 if (canadd == true) then
   sl:insert(0, linecontent)
 end
 return canadd
end

function M:IsURLInList(item,logname)
 local inlist = false
 local logfile = browser.info.configdir..logname..'.sclist'
 local sl = ctk.string.list:new()
 if ctk.file.exists(logfile) then
  sl:loadfromfile(logfile)
 end
 if (ctk.string.occur(sl.text, 'url="'..item.url..'"') ~= 0) then
   inlist = true
 end 
 sl:release() 
 return inlist
end

-- ToDo WIP: This function should soon replace procedure
-- TSandcatSettings.AddToURLList() in uSettings.pas
function M:AddURLLogItem(item,logname)
 local added = false
 local logfile = browser.info.configdir..logname..'.sclist'
 local sl = ctk.string.list:new()
 if ctk.file.exists(logfile) then
  sl:loadfromfile(logfile)
 end
 added = self:AddURLLogItemToList(sl, item)
 sl:savetofile(logfile)
 sl:release()
 return added
end

-- Returns a table containing a list of ids and names
function M:GetURLLogLists(logname)
 local lists = {}
 local logfile = browser.info.configdir..logname..'.sclist'
 local names = ctk.string.list:new()
 local ids = ctk.string.list:new() 
 local slp = ctk.string.loop:new()
 if ctk.file.exists(logfile) then
  slp:loadfromfile(logfile)
  while slp:parsing() do
    names:add(self:GetValue(slp.current, 'name'))
    ids:add(self:GetValue(slp.current, 'id'))    
  end
 end
 lists.namelist = names.text
 lists.idlist = ids.text
 names:release()
 ids:release() 
 slp:release()
 return lists
end

function M:GetValue(line, name)
 local s = ctk.string.after(line, ' '..name..'="')
 s = ctk.string.before(s, '"')
 s = ctk.html.unescape(s)
 return s
end

function M:GetURLLogItem(itemid,logname)
 local logfile = browser.info.configdir..logname..'.sclist'
 local slp = ctk.string.loop:new()
 local i = nil
 local found = false
 if ctk.file.exists(logfile) then
  slp:loadfromfile(logfile)
  while slp:parsing() do
   if ctk.string.match(slp.current,'<item*id="'..itemid..'"*>') then
    i = {}
    i.url = self:GetValue(slp.current, 'url')
    i.name = self:GetValue(slp.current, 'name')
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

function M:MakeValidURL(url)
   local res = url
   if (ctk.string.occur(url, '://') == 0) then
     res = 'http://'..url
   end
   return res
end

function M:ImportURLLogFileItem(list, line, fileext)
   if fileext == '.sclist' then
     if list:indexof(line) == -1 then
       list:add(line)
     end
   end
   if fileext == '.lst' then
     item = {}
     item.url = self:MakeValidURL(line)
     self:AddURLLogItemToList(list, item)
   end
   if fileext == '.csv' then
     local csv = ctk.string.loop:new()
     csv.commatext = line
     item = {}
     item.url = csv:get(0)
     if csv.count >= 2 then
       item.name = csv:get(1)
     end     
     csv:release()
     if (ctk.string.occur(item.url, '://') ~= 0) then
       self:AddURLLogItemToList(list, item)
     end
   end
end

function M:ImportURLLogFile(logname,onendlua)
 local histfile = browser.info.configdir..logname..'.sclist'
 local srcfile = app.openfile(self.listfilter,'sclist')
 if ctk.file.exists(srcfile) == true then
   local fileext = ctk.file.getext(srcfile:lower())
   local l = ctk.string.list:new()
   local slp = ctk.string.loop:new()
   if ctk.file.exists(histfile) == true then
     l:loadfromfile(histfile)
   end
   slp:loadfromfile(srcfile)
   while slp:parsing() do
     self:ImportURLLogFileItem(l, slp.current, fileext)
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
 local html = conf.html or Sandcat:getfile('histview_listurl.html')
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
 conf.genurlfunc = conf.genurlfunc or nil
 
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
    if conf.genurlfunc ~= nil then
      url = conf.genurlfunc({name=name})
    end
    if conf.readsiteprefs == true then
      local sitename = ctk.html.escape(self:GetSiteName(url))
      if name == '' then
        name = sitename
      else
        if sitename ~= '' then
          name = name..' ('..sitename..')'
        end
      end
    end
    sl:add('<tr.item url="'..url..'" role="option" style="context-menu: selector(#menu'..id..');" id="'..id..'">')
    sl:add('<td>'..name..'</td>')
    sl:add('<td.address>'..url..'</td>')
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