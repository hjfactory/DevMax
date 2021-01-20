program ObservableDataPilot;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  DataModule in 'DataModule.pas' {DataModule1: TDataModule},
  DMX.ObservableData in '..\..\Sources\DMX.ObservableData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
