unit DMX.DesignPattern;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs;

type
  TSingleton<T: class> = class(TInterfacedObject)
  private
    class var
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: T;
  end;

  function Singleton_GetInstance(AClass: TClass): TObject;
  procedure Singleton_AddInstance(AObject: TObject);

implementation

uses
  System.Generics.Collections;

{ TSingleton<T> }

var
  Singleton_Dict: TObjectDictionary<TClass, TObject>;

function Singleton_GetInstance(AClass: TClass): TObject;
begin
  if not Assigned(Singleton_Dict) then
    Singleton_Dict := TObjectDictionary<TClass, TObject>.Create([doOwnsValues]);
  Result := nil;
  if Singleton_Dict.ContainsKey(AClass) then
    Result := Singleton_Dict.Items[AClass];
end;

procedure Singleton_AddInstance(AObject: TObject);
begin
  if not Singleton_Dict.ContainsKey(AObject.ClassType) then
    Singleton_Dict.Add(AObject.ClassType, AObject);
end;

procedure Singleton_ReleaseInstances;
var
  Obj: TObject;
begin
  if not Assigned(Singleton_Dict) then
    Exit;

//  for Obj in Singleton_Dict.Values do
//    Obj.Free;
  Singleton_Dict.Free;
end;

constructor TSingleton<T>.Create;
begin
  raise Exception.Create('This object is a singleton. Use the Instance property instead of the Create method.');
end;

destructor TSingleton<T>.Destroy;
begin
  FCriticalSection.Free;

  inherited;
end;

class function TSingleton<T>.Instance: T;
var
  Inst: TObject;
begin
  if not Assigned(FCriticalSection) then
    FCriticalSection := TCriticalSection.Create;


  Inst := Singleton_GetInstance(Self);
  if not Assigned(Inst) then
  begin
    FCriticalSection.Enter;
    try
      Inst := inherited Create;
      Singleton_AddInstance(Inst);
    finally
      FCriticalSection.Leave;
    end;
  end;
  Result := T(Inst);
end;

initialization

finalization
  Singleton_ReleaseInstances;

end.
