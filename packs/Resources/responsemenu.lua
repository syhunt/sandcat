--  Copyright (c) 2011-2014, Syhunt Informatica
--  License: 3-clause BSD license
--  See https://github.com/felipedaragon/sandcat/ for details.

local M = {}
M.reqfilter = 'Sandcat Request files (*.screq)|*.screq'

function M:editrequest(mode)
 local resp = tab:response_get()
 local postdata = ctk.http.crackrequest(resp.previewheaders).data
 if mode == 'http' then
  reqbuilder.request.url = resp.url
  reqbuilder.request.postdata = postdata
  browser.showreqbuilder()
 end
end

function M:fetchlivedone(r)
 local ui = M.ui
 local logfilename = ui.logfilename.value
 ui.resptext.value = r.responsetext
 if logfilename ~= '' then
  tab:cache_setreqresp(logfilename,r.responsetext)
 end
 ui.status_icon:setstyle('foreground-image','@ICON_BLANK')
 app.update()
end

-- ToDo: re-implement
function M:fetchliveversion()
 local ui = self.ui
 local url = ui.url.value
 ui.warn:setstyle('display','none')
 ui.status_icon:setstyle('foreground-image','@ICON_LOADING')
 app.update()
 debug.print('Fetching '..url)
 URLGet:get(url,M.fetchlivedone,true)
end

function M:reloadrequest()
 local resp = tab:response_get()
 tab:response_loadheaders(resp.headers)
end

function M:renderresponse()
 local resp = tab:response_get()
 browser.showurl(resp.url,resp.response)
end

function M:openrequest()
 local reqfile = app.openfile(self.reqfilter,'screq')
 if reqfile ~= '' then
  local s = ctk.file.getcontents(reqfile)
  tab:response_loadheaders(s)
 end
end

function M:saverequest()
 local resp = tab:response_get()
 local destfile = app.savefile(self.reqfilter,'screq')
 if destfile ~= '' then
  local sl = ctk.string.list:new()
  sl.text = resp.previewheaders
  sl:savetofile(destfile)
  sl:release()
 end
end

function M:sendrequest()
 local j = {}
 local resp = tab:response_get()
 local reqhead = resp.previewheaders
 local path = ctk.http.crackrequest(reqhead).path
 browser.options.showheaders = true
 j.url = ctk.url.combine(resp.url,path)
 j.method = ctk.string.before(reqhead,' ')
 j.details = 'Browser Request (Replay)'
 j.postdata = ctk.http.crackrequest(reqhead).data
 if ctk.url.crack(tab.url).host ~= ctk.url.crack(resp.url).host then
  tab:sendrequest(j)
 else
  tab:sendxhr(j)
 end
end

return M