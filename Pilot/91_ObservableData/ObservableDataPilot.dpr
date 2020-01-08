program ObservableDataPilot;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  DevMax.ObservableData in 'Sources\DevMax.ObservableData.pas',
  DataModule in 'DataModule.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
