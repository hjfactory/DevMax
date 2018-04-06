unit DevMax.UI.Factory;

interface

uses
  System.Classes,
  FMX.Controls,
  System.Generics.Collections,
  DevMax.Module.Intf;

type
  TUIItemFactory = class
  private
    class var FInstance: TUIItemFactory;
  public
    class function Instance: TUIItemFactory;
    class procedure ReleaseInstance;
  private
    FUIItems: TDictionary<string, TUIItemClass>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Regist(AId: string; AItemClass: TUIItemClass);

    function GetClass(AId: string): TUIItemClass;
    function CreateControl(AId: string; AOwner: TComponent = nil): TControl;
  end;

implementation

{ TUIItemFactory }

constructor TUIItemFactory.Create;
begin
    FUIItems := TDictionary<string, TUIItemClass>.Create;
end;

destructor TUIItemFactory.Destroy;
begin
  FUIItems.Free;

  inherited;
end;

function TUIItemFactory.CreateControl(AId: string;
  AOwner: TComponent): TControl;
var
  ItemClass: TUIItemClass;
begin
  ItemClass := GetClass(AId);
  if not Assigned(ItemClass) then
    Exit(nil);
  Result := ItemClass.Create(AOwner);
end;

function TUIItemFactory.GetClass(AId: string): TUIItemClass;
begin
  if not FUIItems.TryGetValue(AId, Result) then
    Result := Default(TUIItemClass);
end;

procedure TUIItemFactory.Regist(AId: string; AItemClass: TUIItemClass);
begin
  FUIItems.Add(AId, AItemClass);
end;

class function TUIItemFactory.Instance: TUIItemFactory;
begin
  if not Assigned(FInstance) then
    FInstance := Create;
  Result := FInstance;
end;

class procedure TUIItemFactory.ReleaseInstance;
begin
  if Assigned(FInstance) then
    FInstance.Free;
end;

initialization
finalization
  TUIItemFactory.ReleaseInstance;

end.
