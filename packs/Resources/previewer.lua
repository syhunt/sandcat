Previewer = {}
Previewer.msg_render = 'Click the <b>Render Response</b> button to see a preview.'
Previewer.msg_save = 'Click the <b>Save As</b> button to download this file.'

function Previewer:Register()
 local imagetypelist = [[
 image/bmp
 image/gif
 image/jpeg
 image/pjpeg
 image/png
 image/svg
 image/x-icon
 image/x-windows-bmp
 ]]
 local javascriptlist = [[
 text/javascript
 application/x-javascript
 ]]
 local jsonlist = [[
 application/json
 application/x-json
 ]]
 Sandcat:RegisterPreviewHandler('sc_archive',self.HandleArchive,'zip','application/zip')
 Sandcat:RegisterPreviewHandler('sc_css',self.HandleCSS,'css','text/css')
 Sandcat:RegisterPreviewHandler('sc_flash',self.HandleFlash,'swf','application/x-shockwave-flash')
 Sandcat:RegisterPreviewHandler('sc_html',self.HandleHTML,'htm,html','text/html')
 Sandcat:RegisterPreviewHandler('sc_image',self.HandleImage,'bmp,gif,ico,jpg,jpe,jpeg,png,svg',imagetypelist)
 Sandcat:RegisterPreviewHandler('sc_js',self.HandleJS,'js',javascriptlist)
 Sandcat:RegisterPreviewHandler('sc_json',self.HandleJSON,'json',jsonlist)
 Sandcat:RegisterPreviewHandler('sc_text',self.HandleText,'txt','text/plain')
 Sandcat:RegisterPreviewHandler('sc_xml',self.HandleXML,'xml','image/svg+xml')
end

--[[
function Previewer:HandleCode(f,format)
 local source = f.responsetext
 if source ~= '' then
  if string.len(source) < 1024*50 then
   f.previewhtml = src_highlight(source,format)
  else
   f.previewhtml = self:NoHighlight(source)
  end
 else
  f.warnempty = true
 end
end
]]

function Previewer:HandleCodeRay(f,format)
 require "RbUtils"
 local source = f.responsetext
 local html = ''
 if source ~= '' then
  if string.len(source) < 1024*50 then
   if format == 'java_script' then
    source = scop.html.beautifyjs(source)
   elseif format == 'css' then
    source = scop.html.beautifycss(source)
   end
   html = rbutils.coderay_highlight(source,format)
   html = stringop.replace(html,'<pre>','<pre style="background-color:white;border:0;">')
   f.previewhtml = html
  else
   f.previewhtml = self:NoHighlight(source)
  end
 else
  f.warnempty = true
 end
end

function Previewer:HandleText(f)
 Previewer:HandleCodeRay(f,'text')
end

function Previewer:HandleHTML(f)
 Previewer:HandleCodeRay(f,'html')
end

function Previewer:HandleCSS(f)
 Previewer:HandleCodeRay(f,'css')
end

function Previewer:HandleXML(f)
 Previewer:HandleCodeRay(f,'xml')
end

function Previewer:HandleJS(f)
 Previewer:HandleCodeRay(f,'java_script')
end

function Previewer:HandleJSON(f)
 Previewer:HandleCodeRay(f,'json')
end

function Previewer:HandleFlash(f)
 local resp=app.ask_yn('Render "'..scop.url.crack(f.url).filename..'"?','Previewer')
 if resp == true then
  browser.showurl(f.url)
 end
end

function Previewer:HandleImage(f)
 f.previewhtml = '<pre style="background-color:white;padding:5px;border:0;"><img src="'..scop.html.escape(f.url)..'"></pre>'
end

function Previewer:HandleArchive(f)
 f.previewhtml = '<pre style="background-color:white;padding:5px;border:0;"><font color="blue">'..Previewer.msg_save..'</font></pre>'
end

function Previewer:NoHighlight(s)
 local html = scop.html.escape(s)
 html = '<pre style="background-color:white;border:0;">'..html..'</pre>'
 return html
end