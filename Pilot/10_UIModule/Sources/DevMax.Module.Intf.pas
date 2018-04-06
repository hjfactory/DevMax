unit DevMax.Module.Intf;

interface

uses
  System.Generics.Collections,
  FMX.Types, FMX.Controls;

type
  // UI Module
  TUIItemClass = class of FMX.Controls.TControl;

  TUIItemDataControl = record
    Name: string;
    Control: TControl;
    PropertyName: string;

    constructor Create(AName: string; AControl: TControl; APropertyName: string);
  end;
  TUIItemDataControls = TArray<TUIItemDataControl>;

  TUIItem = interface
    ['{8AC5C92C-7FA0-47B4-A65C-D8B156F5EB1F}']
    function GetDataControls: TUIItemDataControls;
  end;


implementation

{ TViewItemDataControl }

constructor TUIItemDataControl.Create(AName: string; AControl: TControl;
  APropertyName: string);
begin
  Name := AName;
  Control := AControl;
  PropertyName := APropertyName;
end;

end.
