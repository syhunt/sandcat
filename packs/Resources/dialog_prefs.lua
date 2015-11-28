--  Copyright (c) 2011-2015, Syhunt Informatica
--  License: 3-clause BSD license
--  See https://github.com/felipedaragon/sandcat/ for details.

local M = {}
M.backup = ''
M.cfg_expextension = 'scpref'
M.cfg_expfilter = 'Preferences files (*.scpref)|*.scpref'

function M:LoadFromFile(file)
 file = file or app.openfile(self.cfg_expfilter,self.cfg_expextension)
 if file ~= '' then
  prefs.load(slx.file.getcontents(file))
  prefs.update()
 end
end

function M:SaveToFile(destfile)
 destfile = destfile or app.savefile(self.cfg_expfilter,self.cfg_expextension)
 if destfile ~= '' then
  local sl = slx.string.list:new()
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
 t.pak = Sandcat.filename
 t.filename = 'dialog_prefs.html'
 t.id = 'prefs'
 t.options = browser.info.options
 self:EditCustom(t)
end

function M:EditCancel()
 prefs.load(self.backup)
end

-- Expects a table as parameter containing the following keys:
-- pak,filename,id,options,jsonfile
function M:EditCustomFile(t)
 t.iscustomfile = true
 if t.jsonfile ~= '' then
  local browsercfg = prefs.getall() -- Preferences backup
  if slx.file.exists(t.jsonfile) then
   prefs.load(slx.file.getcontents(t.jsonfile))
  else
   prefs.load('')
  end
  self.backup = prefs.getall()
  self:EditCustom(t)
  prefs.savetofile(t.jsonfile)
  prefs.load(browsercfg)
 end
end

-- Expects a table as parameter containing the following keys:
-- pak,filename,id,options
-- optional: iscustomfile, options_disabled
function M:EditCustom(t)
 local html = browser.getpackfile(t.pak,t.filename)
 local script = ''
 if t.iscustomfile == nil then
  t.iscustomfile = false
 end
 self.backup = prefs.getall()
 html = browser.var_replace(html) -- must be first
 html = slx.string.replace(html,'%extensions%',browser.info.extensions)
 script = self:GetImportScript(t.options,t.options_disabled)
 html = slx.string.replace(html,'importsettings();',script)
 app.showdialogx(html,t.id)
 if t.iscustomfile == false then
  if self.backup ~= prefs.getall() then
   prefs.update()
  end
 end
end

function M:GetOptionsImport(list,options,options_disabled)
 local slp = slx.string.loop:new()
 local disabled = slx.string.list:new()
 if options_disabled ~= nil then
  disabled.text = options_disabled
 end
 slp:load(options)
 while slp:parsing() do
  local s = slp.current
  local en = 'true'
  s = slx.string.trim(s)
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
 local j = slx.json.object:new()
 j.cid = cid
 j.value = prefs.get(cid)
 s = j:getjson_unquoted()
 j:release()
 return s
end

function M:GetImportScript(options,options_disabled)
 local s = ''
 local sl = slx.string.list:new()
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