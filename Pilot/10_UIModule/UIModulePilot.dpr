program UIModulePilot;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  DevMax.Module.Intf in 'Sources\DevMax.Module.Intf.pas',
  DevMax.UI.Item in 'Sources\DevMax.UI.Item.pas',
  DevMax.UI.TestFrame in 'UIItrems\DevMax.UI.TestFrame.pas' {Frame1: TFrame},
  DevMax.UI.RButtonToolbar in 'UIItrems\DevMax.UI.RButtonToolbar.pas' {frRButtonToolbar: TFrame},
  DevMax.UI.Factory in 'Sources\DevMax.UI.Factory.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
