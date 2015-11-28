function console.setmode(mode,silent)
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
     Sandcat.Commands:DisplayUserAgent()
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