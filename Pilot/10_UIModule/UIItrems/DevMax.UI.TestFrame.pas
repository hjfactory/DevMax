unit DevMax.UI.TestFrame;

interface

uses
  DevMax.Module.Intf,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects;

type
  TFrame1 = class(TFrame, TUIItem)
    Circle1: TCircle;
    Rectangle1: TRectangle;
    Line1: TLine;
    Edit1: TEdit;
    Button1: TButton;
  private
    { Private declarations }
    function GetDataControls: TUIItemDataControls;
  public
    { Public declarations }
  end;

implementation

uses
  DevMax.UI.Factory;

{$R *.fmx}

{ TFrame1 }

function TFrame1.GetDataControls: TUIItemDataControls;
begin
  Result := [];
end;

initialization
  TUIItemFactory.Instance.Regist('TestFrame', TFrame1);

end.
