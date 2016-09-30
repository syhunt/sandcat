local about = {}

function about:addcredits()
  browser.addlibinfo('Catarinka libraries','','Syhunt and others','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_Catarinka.txt]])')
  browser.addlibinfo('Cromis','1.6.1','Iztok Kacin','browser.newtab([[http://www.opensource.org/licenses/bsd-license.php]])')
  if browser.info.ceflibrary == 'dcef' then
    browser.addlibinfo('DCEF library','3','Henri Gourvest')
  end
  browser.addlibinfo('DCPcrypt','v2 beta 2','David Barton','browser.newtab([[http://opensource.org/licenses/mit-license.php]])')
  browser.addlibinfo('Ext JS wrapper','0.9.8','Wanderlan Santos dos Anjos','browser.newtab([[http://www.opensource.org/licenses/bsd-license.php]])')
  browser.addlibinfo('FatCow Icons','3.5.0','<a href="#" onclick="browser.newtab([[http://www.fatcow.com/free-icons]])">FatCow</a>','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_FatCowIcons.txt]])')
  browser.addlibinfo('Fugue Icons','3.3.5','<a href="#" onclick="browser.newtab([[http://p.yusukekamiyamane.com/]])">Yusuke Kamiyamane</a>','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_FugueIcons.txt]])')
  browser.addlibinfo('ICU library','','IBM Corporation and others','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_icudt.txt]])')
  browser.addlibinfo('libcef library','file:libcef.dll','The Chromium Embedded Framework Authors','browser.newtab([[chrome://license/]])')
  browser.addlibinfo('Lua','file:lua5.1.dll','Tecgraf/PUC-Rio','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_Lua5.txt]])')
  browser.addlibinfo('Custom Lua icon','','Yarin Kaul')
  browser.addlibinfo('ObjectCache library','1.0','Colin Wilson')
  browser.addlibinfo('RegExpr library','0.944','Andrey V. Sorokin','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_RegExpr.txt]])')
  browser.addlibinfo('Ruby','file:msvcrt-ruby18.dll','Yukihiro Matsumoto','browser.newtab([[https://www.ruby-lang.org]])')
  browser.addlibinfo('Sciter Engine','file:sciter-x.dll','<a href="#" onclick="browser.newtab([[http://terrainformatica.com]])">Terra Informatica</a>','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_Sciter.txt]])')
  browser.addlibinfo('Structured Storage library','2.0c','Primoz Gabrijelcic','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_StructuredStorage.txt]])')
  browser.addlibinfo('Super Object Toolkit','1.2','Henri Gourvest')
  browser.addlibinfo('Synapse library','39','Lukas Gebauer','Sandcat:ShowLicense(Sandcat.filename,[[docs\\License_Synapse.txt]])')
  if browser.info.ceflibrary == 'wacef' then
    browser.addlibinfo('WACEF library','3','WaspAce Architector')
  end
end

function about:show()
  self.aboutdisplayed = self.aboutdisplayed or false
  if self.aboutdisplayed == false then
    self.aboutdisplayed = true
    self:addcredits()
  end
  local html = Sandcat:getfile('dialog_about.html')
  html = ctk.string.replace(html,'%iconfilename%',ctk.html.escape(browser.info.iconfilename))
  html = ctk.string.replace(html,'%ver%',ctk.html.escape(browser.info.version))
  html = ctk.string.replace(html,'%appurl%',ctk.html.escape(browser.info.abouturl))
  html = ctk.string.replace(html,'%appname%',ctk.html.escape(browser.info.name))
  html = ctk.string.replace(html,'%libs%',browser.info.libraries)
  html = ctk.string.replace(html,'%extensions%',browser.info.extensions)
  app.showdialogx(html,'about')
end

return about