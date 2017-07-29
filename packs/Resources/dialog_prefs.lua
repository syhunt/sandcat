--  Copyright (c) 2011-2017, Syhunt Informatica
--  License: 3-clause BSD license
--  See https://github.com/felipedaragon/sandcat/ for details.

local M = {}
M.backup = ''
M.confirmed = false
M.cfg_expextension = 'scpref'
M.cfg_expfilter = 'Preferences files (*.scpref)|*.scpref'

function M:LoadFromFile(file)
 file = file or app.openfile(self.cfg_expfilter,self.cfg_expextension)
 if file ~= '' then
  prefs.load(ctk.file.getcontents(file))
  prefs.update()
 end
end

function M:SaveToFile(destfile)
 destfile = destfile or app.savefile(self.cfg_expfilter,self.cfg_expextension)
 if destfile ~= '' then
  local sl = ctk.string.list:new()
  sl.text = prefs.getall()
  sl:savetofile(destfile)
  sl:release()
 end
end

function M:EditList(key,caption,eg)
 if key ~= '' then
  local list = prefs.get(key,'')
  local s = app.editlist(list,caption,eg)
  prefs.set(key,s)
 end
end

function M:Edit()
 local t = {}
 t.html = browser.getpackfile(Sandcat.filename,'dialog_prefs.html')
 t.html = ctk.string.replace(t.html,'%extensions%',browser.info.extensions)
 t.id = 'prefs'
 t.options = browser.info.options
 self:EditCustom(t)
end

function M:EditCancel()
 self.confirmed = false
end

function M:EditConfirm()
 self.confirmed = true
end

-- Expects a table as parameter containing the following keys:
-- html,id,options,jsonfile
function M:EditCustomFile(t)
 local ok = false
 self.confirmed = false
 t.iscustomfile = true
 if t.jsonfile ~= '' then
  local browsercfg = prefs.getall() -- Browser Preferences backup
  if ctk.file.exists(t.jsonfile) then
   prefs.load(ctk.file.getcontents(t.jsonfile))
  else
   prefs.load('')
  end
  self.backup = prefs.getall() -- Custom File Preferences backup
  local ok = self:EditCustom(t)
  if self.backup ~= prefs.getall() then
    if self.confirmed == true then
      prefs.savetofile(t.jsonfile)
    end
  end
  prefs.load(browsercfg) -- Restores Browser Preferences
 end
 return ok
end

-- Expects a table as parameter containing the following keys:
-- html,id,options
-- optional: iscustomfile, options_disabled
-- Returns true if the OK button has been pressed, false if the dialog
-- has been closed or Cancel has been pressed
function M:EditCustom(t)
 local html = t.html or '' --browser.getpackfile(t.pak,t.filename)
 local script = ''
 t.iscustomfile = t.iscustomfile or false
 self.backup = prefs.getall() -- Browser Preferences backup
 self.confirmed = false
 html = browser.var_replace(html) -- must be first
 script = self:GetImportScript(t.options,t.options_disabled)
 html = ctk.string.replace(html,'importsettings();',script)
 app.showdialogx(html,t.id)
 if t.iscustomfile == false then
  if self.backup ~= prefs.getall() then
   if self.confirmed == true then
     -- tells the browser that the settings have changed
     prefs.update()
   else
     -- dialog closed or cancel clicked; keep previous settings
     prefs.load(self.backup)
   end
  end
 end
 return self.confirmed
end

function M:GetOptionsImport(list,options,options_disabled)
 local slp = ctk.string.loop:new()
 local disabled = ctk.string.list:new()
 if options_disabled ~= nil then
  disabled.text = options_disabled
 end
 slp:load(options)
 while slp:parsing() do
  local s = slp.current
  local en = 'true'
  s = ctk.string.trim(s)
  if disabled:indexof(s) ~= -1 then
   en = 'false'
  end
  if s ~= '' then
   self:ImportOption(list,"[cid='"..s.."']",s,en)
  end
 end
 disabled:release()
 slp:release()
end

function M:GetOptionValue(cid)
 local s = ''
 local j = ctk.json.object:new()
 j.cid = cid
 j.value = prefs.get(cid)
 s = j:getjson_unquoted()
 j:release()
 return s
end

function M:GetImportScript(options,options_disabled)
 local s = ''
 local sl = ctk.string.list:new()
 self:GetOptionsImport(sl,options,options_disabled)
 s = sl.text
 sl:release()
 return s
end

-- Usage: self:ImportOption(list,'#plugins','chrome.options.plugins')
function M:ImportOption(list,selector,cid,enabled)
 local value = self:GetOptionValue(cid)
 list:add('import_option($("'..selector..'"),'..value..','..enabled..');')
end

return M