--  Copyright (c) 2011-2017, Syhunt Informatica
--  License: 3-clause BSD license
--  See https://github.com/felipedaragon/sandcat/ for details.

local M = {}
M.backup = ''
M.confirmed = false
M.cfg_expextension = 'scpbak'
M.cfg_expfilter = 'Preferences Backup files (*.scpbak)|*.scpbak|Basic Preferences files (*.scpref)|*.scpref'

function M:LoadFromFile(file)
 file = file or app.openfile(self.cfg_expfilter,self.cfg_expextension)
 if file ~= '' then
  if ctk.file.getext(file) == '.scpbak' then
    ctk.dir.unpackfromtar(file, browser.info.configdir)
  end 
  if ctk.file.getext(file) == '.scpref' then
    prefs.load(ctk.file.getcontents(file))
  end
  prefs.update()
 end
end

function M:SaveToFile(destfile)
 destfile = destfile or app.savefile(self.cfg_expfilter,self.cfg_expextension)
 if destfile ~= '' then
  if ctk.file.getext(destfile) == '.scpbak' then
    prefs.save()
    ctk.dir.packtotar(browser.info.configdir, destfile,'*.*')
  end
  if ctk.file.getext(destfile) == '.scpref' then
    local sl = ctk.string.list:new()
    sl.text = prefs.getall()
    sl:savetofile(destfile)
    sl:release()
  end
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
 return self:EditCustom(t)
end

-- Launches a simple name value input dialog
function M:EditNameValue(dlg)
 prefs.regdefault('sandcat.dialog.input.name', '')
 prefs.regdefault('sandcat.dialog.input.value', '')
 dlg.title = dlg.title or 'Input'
 dlg.name = dlg.name or ''
 dlg.value = dlg.value or ''
 dlg.name_caption = dlg.name_caption or 'Name'
 dlg.value_caption = dlg.value_caption or 'Value'
 prefs.set('sandcat.dialog.input.name',dlg.name)
 prefs.set('sandcat.dialog.input.value',dlg.value)
 local t = {}
 t.html = browser.getpackfile(Sandcat.filename,'dialog_input.html')
 t.html = ctk.string.replace(t.html,'%title%',ctk.html.escape(dlg.title))
 t.html = ctk.string.replace(t.html,'%name_caption%',ctk.html.escape(dlg.name_caption))
 t.html = ctk.string.replace(t.html,'%value_caption%',ctk.html.escape(dlg.value_caption))
 t.id = 'prefs_namevalue'
 t.options = browser.info.options
 dlg.res = self:EditCustom(t)
 if dlg.res == true then
   dlg.name = prefs.get('sandcat.dialog.input.name',dlg.name)
   dlg.value = prefs.get('sandcat.dialog.input.value',dlg.value)
 end
 return dlg
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
  ok = self:EditCustom(t)
  if self.backup ~= prefs.getall() then
    if self.confirmed == true then
      ctk.dir.create(ctk.file.getdir(t.jsonfile))
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