--  Copyright (c) 2011-2014, Syhunt Informatica
--  License: 3-clause BSD license
--  See https://github.com/felipedaragon/sandcat/ for details.

Preferences = {}
Preferences.backup = ''

function Preferences:Edit()
 self:EditCustom(Sandcat.filename,'dialog_prefs.html','prefs',browser.info.options)
end

function Preferences:EditCancel()
 prefs.load(self.backup)
end

function Preferences:EditCustomFile(pak,filename,id,options,jsonfile)
 if jsonfile ~= '' then
  local browsercfg = prefs.getall() -- Preferences backup
  if scop.file.exists(jsonfile) then
   prefs.load(scop.file.getcontents(jsonfile))
  else
   prefs.load('')
  end
  self.backup = prefs.getall()
  self:EditCustom(pak,filename,id,options,true)
  prefs.savetofile(jsonfile)
  prefs.load(browsercfg)
 end
end

function Preferences:EditCustom(pak,filename,id,options,iscustomfile)
 local html = browser.getpackfile(pak,filename)
 local script = ''
 if iscustomfile == nil then
  iscustomfile = false
 end
 self.backup = prefs.getall()
 html = browser.var_replace(html) -- must be first
 html = stringop.replace(html,'%extensions%',browser.info.extensions)
 script = self:GetImportScript(options)
 html = stringop.replace(html,'importsettings();',script)
 app.showdialogx(html,id)
 if iscustomfile == false then
  if self.backup ~= prefs.getall() then
   prefs.update()
  end
 end
end

function Preferences:GetOptionsImport(list,options)
 local slp = scl.listparser:new()
 slp:load(options)
 while slp:parsing() do
  local s = slp.current
  s = stringop.trim(s)
  if s ~= '' then
   self:ImportOption(list,"[cid='"..s.."']",s)
  end
 end
 slp:release()
end

function Preferences:GetOptionValue(cid)
 local s = ''
 local j = scl.json:new()
 j.cid = cid
 j.value = prefs.get(cid)
 s = j:getjson_unquoted()
 j:release()
 return s
end

function Preferences:GetImportScript(options)
 local s = ''
 local sl = scl.stringlist:new()
 self:GetOptionsImport(sl,options)
 s = sl.text
 sl:release()
 return s
end

-- Usage: self:ImportOption(list,'#plugins','chrome.options.plugins')
function Preferences:ImportOption(list,selector,cid)
 local value = self:GetOptionValue(cid)
 list:add('import_option($("'..selector..'"),'..value..');')
end