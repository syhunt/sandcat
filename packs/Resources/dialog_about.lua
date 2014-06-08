html = Sandcat:getfile('dialog_about.html')
html = slx.string.replace(html,'%iconfilename%',slx.html.escape(browser.info.iconfilename))
html = slx.string.replace(html,'%ver%',slx.html.escape(browser.info.version))
html = slx.string.replace(html,'%appurl%',slx.html.escape(browser.info.abouturl))
html = slx.string.replace(html,'%appname%',slx.html.escape(browser.info.name))
html = slx.string.replace(html,'%libs%',browser.info.libraries)
html = slx.string.replace(html,'%extensions%',browser.info.extensions)

app.showdialogx(html,'about')