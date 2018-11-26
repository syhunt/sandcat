--  Copyright (c) 2011-2014, Syhunt Informatica
--  License: 3-clause BSD license
--  See https://github.com/felipedaragon/sandcat/ for details.

PageMenu = {}
PageMenu.doc_url = 'http://www.syhunt.com/sandcat/?n='
PageMenu.livefilter = 'Sandcat Live Headers files (*.sclive)|*.sclive'
PageMenu.htmlfilter = 'HTML files (*.html;*.htm)|*.html;*.htm'
PageMenu.imagefilter = 'Image files|*.bmp;*.gif;*.jpg;*.jpe;*.jpeg;*.png;*.svg'
PageMenu.allfiles = 'All files (*.*)|*.*'
PageMenu.openfilter = PageMenu.htmlfilter..'|'..PageMenu.imagefilter..'|'..PageMenu.allfiles
PageMenu.newtabscript = ''

function PageMenu:NewTab()
 if PageMenu.newtabscript ~= '' then
  browser.dostring(PageMenu.newtabscript)
 else
  browser.newtab()
 end
end

function PageMenu:OpenPage()
 local f = app.openfile(self.openfilter,'html')
 if f ~= '' then
  tab:gotourl(f)
 end
end

function PageMenu:SaveCacheAs()
 local destfile = app.savefile('Sandcat Cache files (*.scc)|*.scc','scc')
 if destfile ~= '' then
  ctk.dir.packtotar(browser.info.cachedir,destfile)
 end
end

function PageMenu:OpenLiveHeaders()
 local lv = ''
 local f = app.openfile(self.livefilter,'sclive')
 if f ~= '' then
   tab:cache_import(f)
   lv = tab:cache_extractfile('\\Requests\\Headers')
   tab:loadheaders(lv)
   ctk.file.delete(lv)
 end
end

function PageMenu:SaveLiveHeadersAs()
 local destfile = app.savefile(self.livefilter,'sclive')
 if destfile ~= '' then
  tab:saveheaders(destfile)
  local lv = ctk.file.getcontents(destfile)
  if lv ~= '' then 
   tab:cache_storestring('\\Requests\\Headers',lv)
   tab:cache_export(destfile) -- overwrites initial export
  else
   app.showmessage('Failed to export live headers.')
  end
 end
end

function PageMenu:CloseAllTabs()
 if app.ask_yn('Close all tabs?') then
  browser.closetabs()
 end
end

function PageMenu:CloseAllButActiveTab()
 if app.ask_yn('Close all but active tab?') then
  browser.closetabs(tab.name)
 end
end

function PageMenu:SaveCachedAs_Ask(filename)
 debug.print('Saving cached resource from: '..filename)
 if ctk.file.exists(filename) then
  local name = ctk.url.getfilename(self.saveurl)
  local ext = ctk.url.getfileext(self.saveurl,false)
  if name == '' then
   name = 'Untitled.html'
   ext = 'html'
  end
  local sug = app.dir..name
  local destfile = app.savefile('All files (*.*)|*.*',ext,sug)
   if destfile ~= '' then
     ctk.file.copy(filename,destfile)
   end
  ctk.file.delete(filename)
 else
  self:SavePageAs() -- try online version
 end
end

function PageMenu:SaveCachedAs(url)
  if url == nil then
   url = tab.url
  end
  self.saveurl = url
  if url ~= '' then
   if self.crm == nil then
    self.crm = osr:new()
   end
   local c = self.crm
   c.onsetsource = function(s,h)
     PageMenu:SaveCachedAs_Ask(PageMenu.crm:savetofile())
   end
   c:loadcached(url)
  end
end

function PageMenu:SavePageAs()
 if tab:hasloadedurl(true) then
  debug.print('Saving Page from the cloud...')
  browser.navbar:eval('SandcatDownloader.SaveURL_As($(#url).value)')
 end
end

function PageMenu:TakeScreenshot()
 if tab:hasloadedurl(true) then
  local sf = tab.screenshot
  if sf ~= '' then
   local sug = app.dir..ctk.file.getname(sf)
   local destfile = app.savefile('PNG files (*.png)|*.png','png',sug)
   if destfile ~= '' then
    ctk.file.copy(sf,destfile)
   end
  end
  ctk.file.delete(sf)
 end
end

function PageMenu:ViewDevTools()
 if tab:hasloadedurl(true) then
   tab:viewdevtools()
 end
end

-- ToDo WIP: This function should soon replace procedure
-- TSandcatSettings.AddToURLList() in uSettings.pas
function PageMenu:AddURLLogItem(item,logname)
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

function PageMenu:GetURLLogItem(itemid,logname)
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

function PageMenu:DeleteURLLogItem(itemid,logname)
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

function PageMenu:GetSiteName(url)
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

function PageMenu:ViewURLLogFile(conf)
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

function PageMenu:ViewHistory(newtab)
 local t = {}
 t.newtab = newtab
 t.toolbar = 'Resources.pak#histview_tbhistory.html'
 t.histname = 'History'
 t.style = [[
  tr.item { context-menu: selector(#historymenu); }
  ]] 
 t.menu = [[
  <li onclick="PageMenu:DeleteURLLogItem('%i','History')">Delete</li>
  ]]  
 t.showvisicol = true
 self:ViewURLLogFile(t)
end

function PageMenu:ViewBookmarks(newtab)
 local t = {}
 t.newtab = newtab
 t.toolbar = 'Resources.pak#histview_tbbookmarks.html'
 t.histname = 'Bookmarks'
 t.tabicon = 'url(Resources.pak#16/icon_bookmarks.png)'
 t.style = [[
  ]]
 t.menu = [[
  <li onclick="PageMenu:DeleteURLLogItem('%i','Bookmarks')">Delete</li>
  ]]  
 self:ViewURLLogFile(t)
end

function PageMenu:ViewTasks()
 if browser.info.tasks ~= '' then
  browser.showtasks()
 else
  app.showmessage('No tasks to display.')
 end
 --[[ 
 TaskView = TaskView or Sandcat:require('taskview')
 TaskView:loadtasks()
 ]]
end

function PageMenu:ViewDownloads()
 TaskView = TaskView or Sandcat:require('taskview')
 TaskView:loaddownloads()
end