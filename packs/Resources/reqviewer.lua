--  Copyright (c) 2011-2014, Syhunt Informatica
--  License: 3-clause BSD license
--  See https://github.com/felipedaragon/sandcat/ for details.

ReqViewer = {}
ReqViewer.reqfilter = 'Sandcat Request files (*.screq)|*.screq'

function ReqViewer:clearrequest()
 local ui = self.ui
 ui.reqhead.value = ''
 ui.resphead.value = ''
 ui.rt_preview.value = ''
 ui.resptext.value = ''
 ui.hexview.value = ''
 ui.warn:setstyle('display','none')
end

function ReqViewer:editrequest(mode)
 local ui = self.ui
 local url = ui.url.value
 local postdata = scop.http.crackrequest(ui.reqhead.value).data
 local method = ui.method.value
 mode = mode or 'xhr'
 if mode == 'xhr' then
  Syhunt:EditRequest()
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
 s = stringop.replace(s,'\n','<br>')
 s = stringop.replace(s,'GET','<b>GET</b>')
 s = stringop.replace(s,'POST','<b>POST</b>')
 s = stringop.replace(s,'HEAD','<b>HEAD</b>')
 s = stringop.replace(s,'HTTP/1.1','<b>HTTP/1.1</b>')
 s = stringop.replace(s,'HTTP/1.0','<b>HTTP/1.0</b>')
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
  Syhunt:ViewFuzzerLow()
  if ui.reqhead.value ~= '' then
   Fuzzer.ui.request.value = stringop.replace(ui.reqhead.value,' HTTP/','{$1} HTTP/')
  end
  local url = scop.url.crack(ui.url.value)
  Fuzzer.ui.host.value = url.host
  Fuzzer.ui.port.value = url.port
 end
 if type == 'xhr' then
  Syhunt:ViewFuzzer()
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
 local urlext = scop.url.crack(ui.url.value).fileext
 local urlext = stringop.after(urlext,'.')
 local data = {}
 data.handled = false
 data.warnempty = false
 data.url = ui.url.value
 data.previewhtml = ui.rt_preview.value
 data.responsetext = ui.resptext.value
 data.responseheaders = ui.resphead.value
 data.responsefilename = tab:cache_extractfile('\\Requests\\'..ui.logfilename.value..'.resp')
 data.requestheaders = ui.reqhead.value
 data.contenttype = scop.http.getheader(ui.resphead.value,'Content-Type')
 local ct = data.contenttype
 local ctsub = ''
 if stringop.occur(ct,';') ~= 0 then
  ct = stringop.before(ct,';')
 end
 ctsub = stringop.after(ct,'/')
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
  local s = scop.file.getcontents(reqfile)
  ui.reqhead.value = s
  ui.rt_reqhead.value = self:highlightrequest(s)
 end
end

function ReqViewer:saverequest()
 local ui = self.ui
 local destfile = app.savefile(self.reqfilter,'screq')
 if destfile ~= '' then
  local sl = scl.stringlist:new()
  sl.text = ui.rt_reqhead.valueastext
  sl:savetofile(destfile)
  sl:release()
 end
end

function ReqViewer:sendrequest()
 local ui = self.ui
 local j = {}
 local reqhead = ui.rt_reqhead.valueastext
 local path = scop.http.crackrequest(reqhead).path
 j.url = scop.url.combine(ui.url.value,path)
 j.method = stringop.before(reqhead,' ')
 j.details = 'Browser Request (Replay)'
 j.postdata = scop.http.crackrequest(reqhead).data
 if scop.url.crack(tab.url).host ~= scop.url.crack(ui.url.value).host then
  tab:sendrequest(j)
 else
  tab:sendxhr(j)
 end
end

function ReqViewer:viewcached(url)
 if url == nil then
  url = tab.url
 end
 if browser.bottombar.uix ~= 'ReqViewer.ui' then
  self:load()
 end
 local ui = self.ui
 ui.url.value = url
 ui.method.value = 'Cached'
 self:clearrequest()
 tab:loadcached(url)
end