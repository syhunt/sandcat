--[[

  Copyright (c) 2011-2015, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
  
]]

package.path = package.path .. ";"..app.dir.."/Lib/lua/?.lua"
package.path = package.path .. ";"..app.datadir.."/Lib/lua/?.lua"
package.cpath = package.cpath .. ";"..app.dir.."/Lib/clibs/?.dll"
slx = require "Selenite"

Sandcat = extensionpack:new()
Sandcat.filename = 'Resources.pak'
Sandcat.cfg_expextension = 'scpref'
Sandcat.cfg_expfilter = 'Preferences files (*.scpref)|*.scpref'

function Sandcat:Init()
 browser.info = _appinfo
 browser.jsvalues = _jsvalues
 browser.options = _appoptions
 browser.bottombar = self:GetUIZone("browser.bottombar")
 browser.navbar = self:GetUIZone("browser.navbar")
 browser.pagebar = self:GetUIZone("browser.pagebar")
 browser.pagex = self:GetUIZone("browser.pagex")
 browser.tabbar = self:GetUIZone("browser.tabbar")
 browser.statbar = self:GetUIZone("browser.statbar")
 reqbuilder.toolbar = self:GetUIZone("reqbuilder.toolbar")
 reqbuilder.edit = activecodeedit
 reqbuilder.request = _builderreq
 cmd = SandcatBrowserCommand:new()
 tab = SandcatBrowserTab:new()
 tab.liveheaders = self:GetUIZone("tab.liveheaders")
 tab.engine = self:GetUIZone("tab.engine")
 tab.toolbar = self:GetUIZone("tab.toolbar")
end

function Sandcat:AfterLoad()
 -- sets an initialization script to be executed when a Sandcat task is
 -- launched
 local initscript = Sandcat:getfile('task.lua')
 browser.addlua('task.init',initscript)
 
 self:require('pagemenu')
 self.reqbuildermenu = self:require('reqbuildmenu')
 self.Downloader = self:require('downloader')
 self.Preferences = self:require('dialog_prefs')
 
 -- adds Sandcat Console commands
 self.Commands = self:require('commands')
 self.Commands:AddCommands()
 
 -- registers response preview handlers
 self.Preview = self:require('preview')
 self:require('previewer')
 Previewer:Register()
end

function Sandcat:GetUIZone(name)
 local z = SandcatUIZone:new()
 z.name = name
 return z
end

function Sandcat:ClearPrivateData()
 local html = Sandcat:getfile('dialog_clear.html')
 app.showdialogx(html)
end

function Sandcat:EditPreferences()
 self.Preferences:Edit()
end

function Sandcat:EditList(key,caption,eg)
 if key ~= '' then
  local list = prefs.get(key,'')
  local s = app.editlist(list,caption,eg)
  prefs.set(key,s)
 end
end

function Sandcat:ImportPreferences()
 local file = app.openfile(self.cfg_expfilter,self.cfg_expextension)
 if file ~= '' then
  prefs.load(slx.file.getcontents(file))
  prefs.update()
 end
end

function Sandcat:ExportPreferences()
 local destfile = app.savefile(self.cfg_expfilter,self.cfg_expextension)
 if destfile ~= '' then
  local sl = slx.string.list:new()
  sl.text = prefs.getall()
  sl:savetofile(destfile)
  sl:release()
 end
end

function Sandcat:IsURLLoaded(warnuser)
 local valid = false
 if slx.string.beginswith(tab.url,'http') then
  valid = true
 elseif slx.string.beginswith(tab.url,'file') then
  valid = true
 elseif slx.string.beginswith(tab.url,'chrome') then
  valid = true
 end
 
 if valid == false then
  if warnuser then
   app.showmessage('No URL loaded.')
  end
 end
 return valid
end

function Sandcat:SetConsoleMode(mode,silent)
 mode = mode or 'sc'
 silent = silent or false
 if mode == 'sc' then
  if console.gethandler() ~= '' then 
   console.reset()
  end
 end
 if mode == 'js' then
   if console.gethandler() ~= mode then 
   console.sethandler(mode)
   console.clear()
   console.setcolor('#FFFFFF')
   console.setfontcolor('#0066bb')
    if silent == false then
     self.Commands:DisplayUserAgent()
    end
   end
 end
 if mode == 'lua' then
   if console.gethandler() ~= mode then 
   console.sethandler(mode)
   console.clear()
   console.setcolor('#455681')
   console.setfontcolor('#FFFFFF')
    if silent == false then
     console.writeln(_VERSION)
    end
   end
 end
end

function Sandcat:ShowAbout()
 self.about = self.about or self:require('dialog_about')
 self.about:show()
end

function Sandcat:ShowPreviewHandlers()
 local html = Sandcat:getfile('dialog_preview_handlers.html')
 html = slx.string.replace(html,'%handlerlist%',Sandcat.Preview.About.text)
 app.showdialogx(html)
end

function Sandcat:ShowErrorLog()
 local html = Sandcat:getfile('dialog_error.html')
 html = slx.string.replace(html,'%errorlist%',browser.info.errorlog)
 app.showdialogx(html)
 browser.statbar:eval('HideNotification()')
end

function Sandcat:ShowLicense(pak,file)
 self:ShowTextFile('License',pak,file)
end

function Sandcat:ShowText(tabtitle,text)
 local html = slx.string.list:new()
 html:add('<plaintext.editor readonly="true">')
 html:add(text)
 html:add('</plaintext>')
 local j = {}
 j.title = tabtitle
 j.tag = 'sandcattextfile'
 j.loadnew = true
 j.html = html.text
 browser.newtabx(j)
 html:release()
end

function Sandcat:ShowTextFile(tabtitle,pak,file)
 self:ShowText(tabtitle,browser.getpackfile(pak,file))
end

function Sandcat:ViewJSConsole()
 browser.options.showconsole = true
 self:SetConsoleMode('js',true)
end