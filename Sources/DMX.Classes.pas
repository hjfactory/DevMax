unit DMX.Classes;

interface

type
  TdxInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
  end;


implementation

{ TdxInterfacedObject }

function TdxInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
var
  Intt: TInterfacedObject;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TdxInterfacedObject._AddRef: Integer;
begin
  Result := 0;
end;

function TdxInterfacedObject._Release: Integer;
begin
  Result := 0;
end;

end.
