local M = {}
M.Handlers = {}
M.Types = {}
M.Extensions = {}
M.About = slx.string.list:new()

function M:RegisterPreviewHandler(id,func,extlist,typelist)
 if id ~= '' then
  self.Handlers[id]=func
  self.About:add('<tr role="option"><td>'..id..'</td><td>'..extlist..'</td></tr>')
  self.About:sort()
  local slp = slx.string.loop:new()
  -- associates extensions with handler
  local s = ''
  slp.commatext = extlist
  while slp:parsing() do
    s = slx.string.trim(slp.current)
    if s ~= '' then
     self.Extensions[s]=id
    end
  end
  -- associates types with handler
   if typelist ~= nil then
      slp:load(typelist)
      while slp:parsing() do
       s = slx.string.trim(slp.current)
       if s ~= '' then
        self.Types[s]=id
       end
      end
   end
  slp:release()
 end
end

return M