html = Sandcat:getfile('dialog_about.html')
html = stringop.replace(html,'%iconfilename%',scop.html.escape(browser.info.iconfilename))
html = stringop.replace(html,'%ver%',scop.html.escape(browser.info.version))
html = stringop.replace(html,'%appurl%',scop.html.escape(browser.info.abouturl))
html = stringop.replace(html,'%appname%',scop.html.escape(browser.info.name))
html = stringop.replace(html,'%libs%',browser.info.libraries)
html = stringop.replace(html,'%extensions%',browser.info.extensions)

app.showdialogx(html,'about')