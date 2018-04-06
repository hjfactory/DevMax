unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Rectangle1: TRectangle;
    Layout1: TLayout;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    FTestFrame: TControl;
    procedure ClearChild;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses DevMax.UI.Factory;

procedure TForm1.Button1Click(Sender: TObject);
var
  Control: TControl;
begin
  ClearChild;

  Control := TUIItemFactory.Instance.CreateControl('TestFrame', Layout1);
  if not Assigned(Control) then
    Exit;
  Control.Parent := Layout1;
  Control.Align := TAlignLayout.Client;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Control: TControl;
begin
  ClearChild;

  Control := TUIItemFactory.Instance.CreateControl('RButtonToolbar', Layout1);
  Control.Parent := Layout1;
  Control.Align := TAlignLayout.Top;

  Control := TUIItemFactory.Instance.CreateControl('TestFrame', Layout1);
  Control.Parent := Layout1;
  Control.Align := TAlignLayout.Client;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ClearChild;
end;

procedure TForm1.ClearChild;
var
  I: Integer;
  Control: TControl;
begin
  for I := Layout1.ChildrenCount - 1 downto 0 do
    Layout1.Children.Items[I].DisposeOf;
end;

end.
