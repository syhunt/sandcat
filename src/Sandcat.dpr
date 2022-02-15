program Sandcat;

uses
  {$IF CompilerVersion >= 23}
  vcl.Forms,
  winapi.Windows,
  {$ELSE}
  Forms,
  Windows,
  {$IFEND}
  uMain in 'core\uMain.pas' {SandBrowser} ,
  uZones in 'core\uZones.pas',
  uTaskMon in 'core\uTaskMon.pas',
  uUIComponents in 'core\uUIComponents.pas',
  uRequests in 'core\uRequests.pas',
  uLiveHeaders in 'core\uLiveHeaders.pas',
  uConst in 'core\uConst.pas',
  uSettings in 'core\uSettings.pas',
  uTabMan in 'core\uTabMan.pas',
  uTab in 'core\uTab.pas',
  uTabV8 in 'core\uTabV8.pas',
  uTabResources in 'core\uTabResources.pas',
  uTabResponse in 'core\uTabResponse.pas',
  uCodeInspect in 'core\uCodeInspect.pas',
  uSynEditExtended in 'core\uSynEditExtended.pas',
  uExtensions in 'core\uExtensions.pas',
  uTaskMan in 'core\uTaskMan.pas',
  uReqBuilder in 'core\uReqBuilder.pas',
  uMisc in 'core\uMisc.pas',
  LAPI in 'lua\LAPI.pas',
  LAPI_App in 'lua\LAPI_App.pas',
  LAPI_Task in 'lua\LAPI_Task.pas',
  LAPI_TaskMan in 'lua\LAPI_TaskMan.pas',
  LAPI_Browser in 'lua\LAPI_Browser.pas',
  LAPI_CodeEdit in 'lua\LAPI_CodeEdit.pas',
  LAPI_Console in 'lua\LAPI_Console.pas',
  LAPI_Element in 'lua\LAPI_Element.pas',
  LAPI_Cmd in 'lua\LAPI_Cmd.pas',
  LAPI_Tab in 'lua\LAPI_Tab.pas',
  LAPI_CEF in 'lua\LAPI_CEF.pas',
  LAPI_SCX in 'lua\LAPI_SCX.pas';

{$R *.res}
{$R Browser.res}
//{$R uac.res}

 // Reduces executable size
{$IFDEF RELEASE}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}
{$O+} {$SetPEFlags IMAGE_FILE_RELOCS_STRIPPED}
 // Reduces exe size end

begin
  if ExitBeforeInitializing = true then
    Halt(0);
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TSandBrowser, SandBrowser);
  Application.Run;

end.
