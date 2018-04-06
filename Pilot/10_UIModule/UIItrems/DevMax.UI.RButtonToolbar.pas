unit DevMax.UI.RButtonToolbar;

interface

uses
  DevMax.Module.Intf,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TfrRButtonToolbar = class(TFrame, TUIItem)
    ToolBar1: TToolBar;
    Button1: TButton;
    lblTitle: TLabel;
    procedure FrameClick(Sender: TObject);
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

{ TFrame2 }

procedure TfrRButtonToolbar.FrameClick(Sender: TObject);
begin
  Height := 40;
end;

function TfrRButtonToolbar.GetDataControls: TUIItemDataControls;
begin
  Result := [
    TUIItemDataControl.Create('ButtonCaption',  Button1,  'Text'),
    TUIItemDataControl.Create('Title',          lblTitle, 'Text')
  ];
end;

initialization
  TUIItemFactory.Instance.Regist('RButtonToolbar', TfrRButtonToolbar);

end.
