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

function PageMenu:ViewURLLogFile(newtab,histname)
 local html = Sandcat:getfile('histview.html')
 local style = ''
 local histfile = browser.info.configdir..histname..'.sclist'
 local tabicon = '@ICON_BLANK'
 local menu = ''
 local sl = ctk.string.list:new()
 if newtab == nil then
  newtab = true
 end
 if histname == 'History' then
  style = [[
  tr.item { context-menu: selector(#historymenu); }
  ]]
  menu = [[
  <li onclick="PageMenu:DeleteURLLogItem('%i','History')">Delete</li>
  ]]
 end
 if histname == 'Bookmarks' then
  style = [[
  th.visited { display: none; }
  td.visited { display: none; }
  ]]
  tabicon = 'url(Resources.pak#16/icon_bookmarks.png)'
  menu = [[
  <li onclick="PageMenu:DeleteURLLogItem('%i','Bookmarks')">Delete</li>
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
    sl:add('<tr.item url="'..url..'" role="option" style="context-menu: selector(#menu'..id..');" id="'..id..'">')
    sl:add('<td>'..name..'</td>')
    sl:add('<td>'..url..'</td>')
    sl:add('<td.visited>'..visited..'</td>')
    sl:add('<menu.context id="menu'..id..'">'..ctk.string.replace(menu,'%i',id)..'</menu>')
    sl:add('</tr>')
   end
  end
  p:release()
 end
 html = ctk.string.replace(html,'%style%',style)
 html = ctk.string.replace(html,'%history%',sl.text)
 sl:release()
 if newtab then
  local j = {}
  j.title = histname
  j.tag = string.lower(histname)..'view'
  j.toolbar = 'Resources.pak#histview_tb'..string.lower(histname)..'.html'
  j.icon = tabicon
  j.html = html
  browser.newtabx(j)
 else
  tab:loadx(html)
 end
end

function PageMenu:ViewHistory(newtab)
 self:ViewURLLogFile(newtab,'History')
end

function PageMenu:ViewBookmarks(newtab)
 self:ViewURLLogFile(newtab,'Bookmarks')
end

function PageMenu:ViewTasks()
 if browser.info.tasks ~= '' then
  browser.setactivepage('tasks')
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