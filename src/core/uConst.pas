unit uConst;

{
  Sandcat - Constants
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

var
  vAppNameShort: string = 'Syhunt Sandcat';
  vAppNameShortNoCompany: string = 'Sandcat Browser';
  vAppNameShortest: string = 'Sandcat';
  vAppURL: string = 'http://www.syhunt.com/sandcat';
  vConfigFile: string = 'Sandcat.json';
  vDebugFile: string = 'SandcatDebug.log';
  vSearchEngine_Name: string = 'Google';
  vSearchEngine_QueryURL: string = 'https://www.google.com/search?q=';
  vSearchEngine_Icon: string = '@ICON_GOOGLE';
  vExeFileName: string;
  vSCRIDHeader: string = 'X-SCRID';

const
  cBgTaskPrefix = 'sandtask:';
  cNewWinParam = 'newwin:true';
  cModeParam = 'mode:';
  cExecutableFile = 'Sandcat.exe';
  cMainClass = 'TSandBrowser';
  cPakExtension = '.scx';
  cTabNamePrefix = 'tab-';
  cBookmarksFile = 'Bookmarks.sclist';
  cHistoryFile = 'History.sclist';
  cSourceActiveLineColor = $00E6E6E6;
  cFavIconFileName = 'favicon.ico';

const // Resource related
  cResourcesPak = 'Resources.pak';
  ICON_BLANK = cResourcesPak + '#16\icon_blank.png';
  ICON_URL = cResourcesPak + '#16\icon_url.png';
  ICON_LOADING = cResourcesPak + '#16\icon_loading.png';
  ICON_CHECKED = cResourcesPak + '#16\icon_checked.png';
  ICON_TASK_RUNNING = cResourcesPak + '#16\icon_stat_green.png';
  ICON_LUA = cResourcesPak + '#16\icon_lua.png';
  ICON_DOWNLOADS = cResourcesPak + '#16\icon_downloads.png';

const // Index of images from Live Headers imagelist
  ICONIDX_UNKNOWN = 0;
  ICONIDX_IMAGE = 1;
  ICONIDX_JAVASCRIPT = 2;
  ICONIDX_JSON = 3;
  ICONIDX_FLASH = 4;
  ICONIDX_HTML = 5;
  ICONIDX_CSS = 6;
  ICONIDX_XML = 7;
  ICONIDX_VIDEO = 8;
  ICONIDX_AUDIO = 9;
  ICONIDX_PHP = 16;
  ICONIDX_FOLDER_CLOSED = 17;
  ICONIDX_FOLDER_OPEN = 18;
  ICONIDX_PHPSCRIPT = 19;
  ICONIDX_PHPSCRIPT_VULNERABLE = 20;
  ICONIDX_RISK_HIGH = 21;
  ICONIDX_RISK_MEDIUM = 22;
  ICONIDX_RISK_LOW = 23;
  ICONIDX_RISK_INFO = 24;
  ICONIDX_SCRIPT = 25;
  ICONIDX_SCRIPT_VULNERABLE = 26;
  ICONIDX_SERVER_CLOUD = 27;
  ICONIDX_FORM_FIELD = 28;

const // Sandcat sub directories
  SCDIR_LOGS = 1;
  SCDIR_HEADERS = 2;
  SCDIR_TEMP = 3;
  SCDIR_PREVIEW = 4;
  SCDIR_CONFIG = 5;
  SCDIR_PLUGINS = 6;
  SCDIR_CACHE = 7;
  SCDIR_TASKS = 8;
  SCDIR_CONFIGSITE = 9;

const // Sciter related
  // Do not remove &nbsp - workaround for a weird AxSciter error when loading a blank html
  cBlank_Htm = '<html>&nbsp;</html>';

implementation

// ------------------------------------------------------------------------//
end.
