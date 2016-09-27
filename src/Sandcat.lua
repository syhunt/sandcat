--[[
  Sandcat Startup script

  Copyright (c) 2011-2015, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
  
]]

package.path = package.path .. ";"..app.dir.."/Lib/lua/?.lua"
package.path = package.path .. ";"..app.datadir.."/Lib/lua/?.lua"
package.cpath = package.cpath .. ";"..app.dir.."/Lib/clibs/?.dll"

-- Creates a Sandcat expansionpack object allowing it to load resources
-- from the Resources pack file at any moment
-- expansionpacks are described in docs\objects.extensionpack.md
Sandcat = extensionpack:new()
Sandcat.filename = 'Resources.pak'

-- This method will be called before any Sandcat extensions are loaded
-- For startup speed reasons it will not try to read any compressed 
-- packs
function Sandcat:Init()
 -- Sets a global for acessing the Catarinka library
 -- Described in docs\libraries.catarinka.md
 ctk = require "Catarinka"
 
 -- Sets UI zones for quick manipulation by extensions
 -- Described in docs\objects.uizones.md
 browser.bottombar = self:GetUIZone("browser.bottombar")
 browser.navbar = self:GetUIZone("browser.navbar")
 browser.pagebar = self:GetUIZone("browser.pagebar")
 browser.pagex = self:GetUIZone("browser.pagex")
 browser.tabbar = self:GetUIZone("browser.tabbar")
 browser.statbar = self:GetUIZone("browser.statbar")
 reqbuilder.toolbar = self:GetUIZone("reqbuilder.toolbar")
 reqbuilder.edit = activecodeedit
 
 -- Creates the cmd object used by console commands
 -- Described in docs/libraries.console.md
 cmd = SandcatBrowserCommand:new()
 
 -- Creates the tab object for accessing the active tab
 -- Described in docs/objects.tab.md
 tab = SandcatBrowserTab:new()
 tab.liveheaders = self:GetUIZone("tab.liveheaders")
 tab.engine = self:GetUIZone("tab.engine")
 tab.toolbar = self:GetUIZone("tab.toolbar")
 
  -- Allows extensions to add custom response preview handlers
  self.Preview = self:GetPreview()
end

-- This method will be called after all Sandcat extensions have been
-- loaded
function Sandcat:AfterLoad()
 -- Sets an initialization script to be executed when a Sandcat task is
 -- launched. Sandcat tasks are described in docs/objects.task.md
 local initscript = Sandcat:getfile('task.lua')
 browser.addlua('task.init',initscript)
 
 self:require('pagemenu')
 self.reqbuildermenu = self:require('reqbuildmenu')
 self.Downloader = self:require('downloader')
 
 -- Extends the Preferences library
 -- Described in docs/libraries.prefs.md
 self.Preferences = self:require('dialog_prefs')
 prefs.editlist = function(...) Sandcat.Preferences:EditList(...) end
 
 -- Extends the tab object
 -- Described in docs/objects.tab.md
 self:require('tabex')
 
 -- Extends the console object and adds basic commands to it
 -- Described in docs/libraries.console.md
 self.Commands = self:require('consolecmds')
 self.Commands:AddCommands()
 self:require('consoleex')
 
 -- Registers response preview handlers
 self:require('previewer')
 Previewer:Register()
end

-- Creates and returns a simple module for storing or registering new
-- response preview handlers
function Sandcat:GetPreview()
 local M = {}
 M.Handlers = {}
 M.Types = {}
 M.Extensions = {}
 M.About = ctk.string.list:new()

 function M:RegisterHandler(id,func,extlist,typelist)
  if id ~= '' then
   self.Handlers[id]=func
   self.About:add('<tr role="option"><td>'..id..'</td><td>'..extlist..'</td></tr>')
   self.About:sort()
   local slp = ctk.string.loop:new()
   -- Associates extensions with handler
   local s = ''
   slp.commatext = extlist
   while slp:parsing() do
     s = ctk.string.trim(slp.current)
     if s ~= '' then
      self.Extensions[s]=id
     end
   end
   -- Associates types with handler
    if typelist ~= nil then
       slp:load(typelist)
       while slp:parsing() do
        s = ctk.string.trim(slp.current)
        if s ~= '' then
         self.Types[s]=id
        end
       end
    end
   slp:release()
  end
 end
 
 function M:ShowHandlers()
  local html = Sandcat:getfile('dialog_preview_handlers.html')
  html = ctk.string.replace(html,'%handlerlist%',self.About.text)
  app.showdialogx(html)
 end
 
 return M
end

-- Creates and returns an UI zone object
function Sandcat:GetUIZone(name)
 local zone = SandcatUIZone:new()
 zone.name = name
 return zone
end

-- Displays the dialog for clearing private data 
function Sandcat:ClearPrivateData()
 local html = Sandcat:getfile('dialog_clear.html')
 app.showdialogx(html)
end

-- Displays the about dialog
function Sandcat:ShowAbout()
 self.about = self.about or self:require('dialog_about')
 self.about:show()
end

-- Diplays the script error log dialog. Hides any notifications from the
-- statusbar after closing
function Sandcat:ShowErrorLog()
 local html = Sandcat:getfile('dialog_error.html')
 html = ctk.string.replace(html,'%errorlist%',browser.info.errorlog)
 app.showdialogx(html)
 browser.statbar:eval('HideNotification()')
end

-- Displays a license text file from a pack file
function Sandcat:ShowLicense(pak,file)
 self:ShowTextFile('License',pak,file)
end

-- Displays a text as a new tab
function Sandcat:ShowText(tabtitle,text,escape)
 local escape = escape or false
 if escape == true then
   text = ctk.html.escape(text)
 end
 local html = ctk.string.list:new()
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

-- Displays a text file from a pack file
function Sandcat:ShowTextFile(tabtitle,pak,file)
 self:ShowText(tabtitle,browser.getpackfile(pak,file),false)
end