local M = {}
M.title = 'Download Manager'
function M:launch(f)
 if (f ~= '') then
   local resp=app.ask_yn('Are you sure you want to launch "'..ctk.file.getname(f)..'"?',self.title)
   if resp == true then
    ctk.file.exec(f)
   end
 end
end

return M