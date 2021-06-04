unit DMX.DesignPattern;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  System.Generics.Collections;

type
{$REGION 'Singleton'}
  TSingleton<T: class> = class(TInterfacedObject)
  private
    class var
    FCriticalSection: TCriticalSection;
  protected
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    class function Instance: T;
  end;

  function Singleton_GetInstance(AClass: TClass): TObject;
  procedure Singleton_AddInstance(AObject: TObject);
{$ENDREGION 'Singleton'}

{$REGION 'Factory'}
type
  TClassFactory<TKey; TCls> = class(TSingleton<TClassFactory<TKey, TCls>>)
  private
    FList: TDictionary<TKey, TCls>;
  protected
    procedure Initialize; override;
    procedure Finalize; override;

    function CalcKey(ACls: TCls): TKey; virtual;
  public
    procedure Regist(AKey: TKey; ACls: TCls); overload;
    procedure Regist(ACls: TCls); overload;
    function GetClass(AKey: TKey): TCls;
    // The type of instance cannot be specified and cannot be implemented.
//    function GetInstance(AKey: TKey): TValue;
//    function GetInstance<T: class, constructor>(AKey: TKey): TValue;
  end;
{$ENDREGION 'Factory'}


{$REGION 'Observer'}
type
  ICustomCommandObserver<T> = interface;
  ICommandSubject<T> = interface
    procedure Notification(ACommand: T);
    procedure RegistObserver(AObserver: ICustomCommandObserver<T>);
    procedure UnregistObserver(AObserver: ICustomCommandObserver<T>);
  end;
  ICustomCommandObserver<T> = interface
    procedure UpdateNotify(Sender: TObject; ACommand: T);
  end;

  TCustomCommandSubject<T> = class(TInterfacedObject, ICommandSubject<T>)
  private
    FLock: TLightweightMREW;
    FObservers: TList<ICustomCommandObserver<T>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegistObserver(AObserver: ICustomCommandObserver<T>);
    procedure UnregistObserver(AObserver: ICustomCommandObserver<T>);

    procedure Notification(ACommand: T);
    procedure NotificationTargetIntf(const IID: TGUID; ACommand: T);
  end;
{$ENDREGION 'Observer'}


implementation

{$REGION 'Singleton'}
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
begin
  if not Assigned(Singleton_Dict) then
    Exit;

  Singleton_Dict.Free;
end;

constructor TSingleton<T>.Create;
begin
  raise Exception.Create('This object is a singleton. Use the Instance property instead of the Create method.');
end;

destructor TSingleton<T>.Destroy;
begin
  FCriticalSection.Free;
  Finalize;

  inherited;
end;

procedure TSingleton<T>.Initialize;
begin
end;

procedure TSingleton<T>.Finalize;
begin
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
      TSingleton<T>(Inst).Initialize;
      Singleton_AddInstance(Inst);
    finally
      FCriticalSection.Leave;
    end;
  end;
  Result := T(Inst);
end;
{$ENDREGION 'Singleton'}

{ TFactory<TKey, TValue> }

procedure TClassFactory<TKey, TCls>.Initialize;
begin
  inherited;
  FList := TDictionary<TKey, TCls>.Create;
end;

procedure TClassFactory<TKey, TCls>.Regist(ACls: TCls);
var
  Key: TKey;
begin
  Key := CalcKey(ACls);
  Regist(Key, ACls);
end;

function TClassFactory<TKey, TCls>.CalcKey(ACls: TCls): TKey;
begin
  raise Exception.Create('Override this method.');
end;

procedure TClassFactory<TKey, TCls>.Finalize;
begin
  FList.Free;
  inherited;
end;

procedure TClassFactory<TKey, TCls>.Regist(AKey: TKey; ACls: TCls);
begin
  if FList.ContainsKey(AKey) then
    raise Exception.Create('Duplicate item.');

  FList.Add(AKey, ACls);
end;

function TClassFactory<TKey, TCls>.GetClass(AKey: TKey): TCls;
begin
  FList.TryGetValue(AKey, Result);
end;

{ TCustomCommandSubject<T> }

constructor TCustomCommandSubject<T>.Create;
begin
  FObservers := TList<ICustomCommandObserver<T>>.Create;
end;

destructor TCustomCommandSubject<T>.Destroy;
begin
  FObservers.Free;

  inherited;
end;

procedure TCustomCommandSubject<T>.Notification(ACommand: T);
var
  O: ICustomCommandObserver<T>;
begin
  FLock.BeginRead;
  try
    for O in FObservers do
      O.UpdateNotify(Self, ACommand);
  finally
    FLock.EndRead;
  end;
end;

procedure TCustomCommandSubject<T>.NotificationTargetIntf(const IID: TGUID;
  ACommand: T);
var
  O: ICustomCommandObserver<T>;
begin
  FLock.BeginRead;
  try
    for O in FObservers do
    begin
      if Supports(O, IID) then
        O.UpdateNotify(Self, ACommand);
    end;
  finally
    FLock.EndRead;
  end;
end;

procedure TCustomCommandSubject<T>.RegistObserver(
  AObserver: ICustomCommandObserver<T>);
begin
  FLock.BeginWrite;
  try
    FObservers.Add(AObserver);
  finally
    FLock.EndWrite;
  end;
end;

procedure TCustomCommandSubject<T>.UnregistObserver(
  AObserver: ICustomCommandObserver<T>);
begin
  FLock.BeginWrite;
  try
    FObservers.Remove(AObserver)
  finally
    FLock.EndWrite;
  end;
end;

initialization

finalization
  Singleton_ReleaseInstances;

end.
