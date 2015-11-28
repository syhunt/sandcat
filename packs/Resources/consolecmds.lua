local M = {}

function M:AddCommands()
 console.addcmd('clear','console.clear()','Clears the console')
 console.addcmd('cll','tab:clearlog()','Clears the contents of the Log tab')
 console.addcmd('cookie','Sandcat.Commands:DisplayCookie()','Displays the page cookie')
 console.addcmd('debug','debug.enable(true)','Enables the debug mode')
 console.addcmd('delay [ms]','slx.utils.delay(cmd.params)','Waits a specific number of milliseconds before proceeding')
 console.addcmd('devtools','PageMenu:ViewDevTools(false)','Displays the Developer Tools')
 console.addcmd('echo [str]','print(cmd.params)','Prints a string of text')
 console.addcmd('exit','browser.exit()','Exits the application')
 console.addcmd('go back|fwd','Sandcat.Commands:Go(s)','Goes back or forward in history')
 console.addcmd('google [query]','Sandcat.Commands:Google(cmd.params)','Searches Google')
 console.addcmd('help','browser.bottombar:loadx(browser.info.commands)','Displays a screen with the list of commands available')
 console.addcmd('hide','browser.options.showconsole = false','Hides the console')
 console.addcmd('ip','print(slx.net.nametoip(slx.url.crack(tab.url).host))','Displays the server IP address')
 console.addcmd('js [code]','tab:runjs(cmd.params,tab.url,0)','Runs JavaScript Code in Loaded Page')
 console.addcmd('jscs','console.setmode("js")','JS Console')
 console.addcmd('load [url]','Sandcat.Commands:LoadURL(cmd.params)','Goes to URL')
 console.addcmd('lp [path]','tab:gotourl(slx.url.combine(tab.url,cmd.params))','Goes to URL path (eg: lp /admin)')
 console.addcmd('lua [code]','assert(loadstring(cmd.params))()','Runs Lua Code')
 console.addcmd('luacs','console.setmode("lua")','Lua Console')
 console.addcmd('new tab|console|window','Sandcat.Commands:New(cmd.params)','Opens a new tab, console tab or window')
 console.addcmd('newtab [optional:url]','browser.newtab(cmd.params)','Opens a new tab')
 console.addcmd('reload','tab:reload()','Reloads the page')
 console.addcmd('relic','tab:reload(true)','Reloads the page (ignores the cache)')
 console.addcmd('server','print(slx.http.getheader(tab.rcvdheaders,"Server"))','Displays the server software')
 console.addcmd('start [filename] [optional:params]','slx.file.exec(cmd.params)','Executes a file (eg: exec Notepad.exe)')
 console.addcmd('tab clone|close|new','Sandcat.Commands:Tab(cmd.params)','Creates, clones or closes a tab')
 console.addcmd('useragent','Sandcat.Commands:DisplayUserAgent()','Displays the current user-agent')
end

function M:DisplayCookie()
 tab:runluaonlog('done','print(tab.lastjslogmsg)')
 tab:runjs("console.log(document.cookie);console.log('done');",tab.url,0)
end

function M:DisplayUserAgent()
 tab:runluaonlog('done','print(tab.lastjslogmsg)')
 tab:runjs("console.log(navigator.userAgent);console.log('done');",tab.url,0)
end

function M:Go(s)
 if s == 'back' then tab:goback() end
 if s == 'fwd' then tab:goforward() end
end

function M:Google(query)
 if query ~= '' then
  browser.showurl('https://www.google.com/search?q='..query)
 end
end

function M:LoadURL(url)
 tab.loadend = Sandcat:getfile('loadurl.lua')
 tab:gotourl(url)
end

function M:New(s)
 if s == 'window' then browser.newwindow() end
 if s == 'tab' then browser.newtab() end
 if s == 'console' then browser.newtabcs() end
end

function M:Tab(s)
 if s == 'clone' then browser.newtab(tab.url) end
 if s == 'close' then browser.closetab() end
 if s == 'new' then browser.newtab() end
end

return M