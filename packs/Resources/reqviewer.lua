--  Copyright (c) 2011-2014, Syhunt Informatica
--  License: 3-clause BSD license
--  See https://github.com/felipedaragon/sandcat/ for details.

ReqViewer = {}
ReqViewer.reqfilter = 'Sandcat Request files (*.screq)|*.screq'

function ReqViewer:clearrequest()
 local ui = self.ui
 ui.reqhead.value = ''
 ui.resphead.value = ''
 ui.rt_reqhead.value = ''
 ui.rt_preview.value = ''
 ui.rt_resphead.value = ''
 ui.resptext.value = ''
 ui.hexview.value = ''
 ui.warn:setstyle('display','none')
end

function ReqViewer:handlecrmsource(source,headers)
 local ui = self.ui
 local urlext = slx.url.getfileext(self.crm.url)
 debug.print('Viewing cache resource...')
 if slx.re.match(urlext,'.bmp|.gif|.ico|.jpg|.jpeg|.png|.svg') == true then
  self.crm:savetofile()
 else
  ui.resptext.value = source
 end
 if ui.resphead.value == '' then
  ui.resphead.value = headers
 end
 if ui.method.value == '' then
  ui.method.value = 'Cached'
 end
 if ui.url.value == '' then
  ui.url.value = self.crm.url
 end
 --ui.hexview.value = not implemented GetChromeCacheRawData(s)
 self:loadheaders()
 self:handlepreview()
end

function ReqViewer:loadcachedurl(url)
 if self.crm == nil then
  self.crm = osr:new()
 end
 local c = self.crm
 c.onsetsource = function(s,h) ReqViewer:handlecrmsource(s,h) end
 if c.isloading == true then
  c:stop()
 end
 c:loadcached(url)
end

function ReqViewer:loadpreview(r)
 local std_load = function() ReqViewer:loadheaders() ReqViewer:handlepreview() end
 if r.response == '' then
  if r.islow == false then
   debug.print('Loading URL from cache...')
   self:loadcachedurl(r.url)
  else
   debug.print('Standard preview load (empty response)...')
   std_load()
  end
 else
  std_load()
 end
end

-- Loads a request from the tab live headers VFS
function ReqViewer:loadrequest(filename)
 local ui = self.ui
 self:clearrequest()
 if tab:cache_requestexists(filename) == true then
  local r = tab:cache_getrequestdetails(filename)
  ui.url.value = r.url
  ui.logfilename.value = filename
  ui.resphead.value = r.responseheaders
  ui.reqhead.value = r.headers
  ui.method.value = r.method
  --ui.headurl.value = slx.string.maxlen(r.url, 100, true)
  ui.resptext.value = r.response 
  self:loadpreview(r)
 end
end

function ReqViewer:editrequest(mode)
 local ui = self.ui
 local url = ui.url.value
 local postdata = slx.http.crackrequest(ui.reqhead.value).data
 local method = ui.method.value
 mode = mode or 'xhr'
 if mode == 'xhr' then
  PenTools:EditRequest()
  XHREditor.ui.url.value = url
  XHREditor.ui.postdata.value = postdata
  XHREditor.ui.method.value = method
 end
 if mode == 'http' then
  reqbuilder.request.url = url
  reqbuilder.request.postdata = postdata
  browser.showreqbuilder()
 end
end

function ReqViewer:fetchlivedone(r)
 local ui = ReqViewer.ui
 local logfilename = ui.logfilename.value
 ui.resptext.value = r.responsetext
 if logfilename ~= '' then
  tab:cache_setreqresp(logfilename,r.responsetext)
 end
 ui.status_icon:setstyle('foreground-image','@ICON_BLANK')
 app.update()
end

function ReqViewer:fetchliveversion()
 local ui = self.ui
 local url = ui.url.value
 ui.warn:setstyle('display','none')
 ui.status_icon:setstyle('foreground-image','@ICON_LOADING')
 app.update()
 debug.print('Fetching '..url)
 URLGet:get(url,ReqViewer.fetchlivedone,true)
end

function ReqViewer:hiderequestheaders()
 local ui = self.ui
 ui.reqheadeditor:setstyle('display','none')
 ui.tdreqhead:setstyle('width','0px')
 ui.tdresphead:setstyle('width','100%')
end

function ReqViewer:highlightrequest(s)
 s = slx.string.replace(s,'\n','<br>')
 s = slx.string.replace(s,'GET','<b>GET</b>')
 s = slx.string.replace(s,'POST','<b>POST</b>')
 s = slx.string.replace(s,'HEAD','<b>HEAD</b>')
 s = slx.string.replace(s,'HTTP/1.1','<b>HTTP/1.1</b>')
 s = slx.string.replace(s,'HTTP/1.0','<b>HTTP/1.0</b>')
 return '<font color="black">'..s..'</font>'
end

function ReqViewer:loadheaders()
 local ui = self.ui
 ui.rt_reqhead.value = self:highlightrequest(ui.reqhead.value)
 ui.rt_resphead.value = self:highlightrequest(ui.resphead.value)
end

function ReqViewer:load()
 local html = Sandcat:getfile('reqviewer.html')
 browser.loadpagex('response',html,'ReqViewer.ui')
end

function ReqViewer:loadinfuzzer(type)
 local ui = self.ui
 if type == 'low' then
  PenTools:ViewFuzzerLow()
  if ui.reqhead.value ~= '' then
   Fuzzer.ui.request.value = slx.string.replace(ui.reqhead.value,' HTTP/','{$1} HTTP/')
  end
  local url = slx.url.crack(ui.url.value)
  Fuzzer.ui.host.value = url.host
  Fuzzer.ui.port.value = url.port
 end
 if type == 'xhr' then
  PenTools:ViewFuzzer()
  Fuzzer.ui.url.value = ui.url.value..'{$1}'
  Fuzzer.ui.method.value = ui.method.value
 end
end

function ReqViewer:execpreviewhandler()
 local ui = self.ui
 local data = self.data
 local preview = ui.rt_preview.value
 self:handler(data)
 if preview ~= data.previewhtml then
  -- updates the preview html
  ui.rt_preview.value = data.previewhtml
 end
 if data.warnempty == true then
  ui.warn:setstyle('display','block')
 end
end

function ReqViewer:decidehandler(s)
 local r = ''
 if string.find(s,'<html') ~= nil then
  r = 'html'
 end
 return r
end

function ReqViewer:handlepreview()
 local ui = self.ui
 local urlext = slx.url.crack(ui.url.value).fileext
 local urlext = slx.string.after(urlext,'.')
 local data = {}
 data.handled = false
 data.warnempty = false
 data.url = ui.url.value
 data.previewhtml = ui.rt_preview.value
 data.responsetext = ui.resptext.value
 data.responseheaders = ui.resphead.value
 data.responsefilename = tab:cache_extractfile('\\Requests\\'..ui.logfilename.value..'.resp')
 data.requestheaders = ui.reqhead.value
 data.contenttype = slx.http.getheader(ui.resphead.value,'Content-Type')
 local ct = data.contenttype
 local ctsub = ''
 if slx.string.occur(ct,';') ~= 0 then
  ct = slx.string.before(ct,';')
 end
 ctsub = slx.string.after(ct,'/')
 local ph = Sandcat.Preview.Types[ct]
 if ph == nil then
  ph = Sandcat.Preview.Extensions[ctsub]
 end
 if ph == nil then
  ph = Sandcat.Preview.Extensions[urlext]
 end
 if ph == nil then
  ph = self:decidehandler(ui.resptext.value)
 end
 if ph ~= nil then
   if ph ~= '' then
    if Sandcat.Preview.Handlers[ph] ~= nil then
     debug.print('Found preview handler: '..ph)
     self.handler = Sandcat.Preview.Handlers[ph]
     self.data = data
     self:execpreviewhandler()
    end
   end
 end
end

function ReqViewer:renderresponse()
 local ui = self.ui
 browser.showurl(ui.url.value,ui.resptext.value)
end

function ReqViewer:saveresponse()
 local ui = self.ui
 browser.pagex:eval('SandcatDownloader.SaveURL_As($(#url).value)')
end

function ReqViewer:openrequest()
 local ui = self.ui
 local reqfile = app.openfile(self.reqfilter,'screq')
 if reqfile ~= '' then
  local s = slx.file.getcontents(reqfile)
  ui.reqhead.value = s
  ui.rt_reqhead.value = self:highlightrequest(s)
 end
end

function ReqViewer:saverequest()
 local ui = self.ui
 local destfile = app.savefile(self.reqfilter,'screq')
 if destfile ~= '' then
  local sl = slx.string.list:new()
  sl.text = ui.rt_reqhead.valueastext
  sl:savetofile(destfile)
  sl:release()
 end
end

function ReqViewer:sendrequest()
 local ui = self.ui
 local j = {}
 local reqhead = ui.rt_reqhead.valueastext
 local path = slx.http.crackrequest(reqhead).path
 j.url = slx.url.combine(ui.url.value,path)
 j.method = slx.string.before(reqhead,' ')
 j.details = 'Browser Request (Replay)'
 j.postdata = slx.http.crackrequest(reqhead).data
 if slx.url.crack(tab.url).host ~= slx.url.crack(ui.url.value).host then
  tab:sendrequest(j)
 else
  tab:sendxhr(j)
 end
end