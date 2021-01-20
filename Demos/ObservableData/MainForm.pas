unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses DataModule, DMX.ObservableData;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DataModule1.IntegerValue.AddBind(Label1, 'Caption');
  DataModule1.IntegerValue.AddBind(ProgressBar1, 'Position');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DataModule1.IntegerValue.Value := 10;
end;

end.
