unit DMX.ObservableData;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo,
  System.Generics.Collections;

type
  TObservableBase = class
  end;

  TObservableField<T> = class(TObservableBase)
  private type
    TBindInfo = record
      Obj: TObject;
      Prop: string;
      CastStr: Boolean;
      constructor Create(AObject: TObject; AProperty: string); overload;
      constructor Create(AObject: TObject; AProperty: string; ACastStr: Boolean); overload;
    end;
    TBindInfoList = TList<TBindInfo>;
  private
    FValue: T;

    FOnChange: TNotifyEvent;

    { TODO : Support to BindingList }
    { TODO : Separate to Data Object and Binding Object }
    FBindInfoList: TBindInfoList;

//    FBindObject: TObject;
//    FBindProperty: string;

    function GetValue: T;
    procedure SetValue(const Value: T);

    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;

    property Value: T read GetValue write SetValue;

    procedure AddBind(AObject: TObject; AProperty: string); overload;
    procedure AddBind<TCastType>(AObject: TObject; AProperty: string); overload;
    procedure RemoveBind(AObject: TObject; AProperty: string);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TObservableString = TObservableField<string>;
  TObservableInteger = TObservableField<Integer>;
  TObservableBoolean = TObservableField<Boolean>;

  TObservableArray<T> = class
  private
    FValue: TArray<T>;

    FTmpObj: TObject;
    FTmpProp: string;

    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);

    procedure DoChange(Index: Integer);
  public
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    procedure Bind(AIndex: Integer; AObject: TObject; AProperty: string);

    constructor Create;
    destructor Destroy; override;
  end;

  EObservableDataError = class(Exception);

resourcestring
  sPropertyNotFound = 'Property %s not found';
  STypeMisMatch = 'Type mismatch between %s and %s';

implementation

uses
  System.Rtti, System.RTLConsts,
  System.Generics.Defaults;

{ TObservableField<T>.TBindInfo }

constructor TObservableField<T>.TBindInfo.Create(AObject: TObject;
  AProperty: string);
begin
  Create(Obj, Prop, False);
end;

constructor TObservableField<T>.TBindInfo.Create(AObject: TObject;
  AProperty: string; ACastStr: Boolean);
begin
  Obj := AObject;
  Prop := AProperty;
  CastStr := ACastStr;
end;

{ TObservableField<T> }

constructor TObservableField<T>.Create;
begin
  FBindInfoList := TBindInfoList.Create;
end;

destructor TObservableField<T>.Destroy;
begin
  FBindInfoList.Free;

  inherited;
end;

procedure TObservableField<T>.AddBind(AObject: TObject; AProperty: string);
var
  Context: TRttiContext;
  Dummy: T;
  PropTypeInfo: PTypeInfo;
  WantCastString: Boolean;
begin
  if not Assigned(AObject) then
    Exit;

  if not Assigned(Context.GetType(AObject.ClassType).GetProperty(AProperty)) then
    raise EObservableDataError.CreateFmt(sPropertyNotFound, [AProperty]);

  // Check data type of property and value.
  WantCastString := False;
  PropTypeInfo := Context.GetType(AObject.ClassType).GetProperty(AProperty).PropertyType.Handle;
  if PropTypeInfo <> TypeInfo(T) then
  begin
    if PropTypeInfo.Kind in [tkUString, tkString, tkWString, tkLString] then
      WantCastString := True
    else
      raise EObservableDataError.CreateFmt(STypeMisMatch, [PTypeInfo(TypeInfo(T)).Name, PropTypeInfo.Name]);
  end;

  FBindInfoList.Add(TBindInfo.Create(AObject, AProperty, WantCastString));
end;

procedure TObservableField<T>.AddBind<TCastType>(AObject: TObject;
  AProperty: string);
begin

end;

procedure TObservableField<T>.DoChange;
var
  Context: TRttiContext;
  BindInfo: TBindInfo;
  LValue: TValue;
begin
  for BindInfo in FBindInfoList do
  begin
    LValue := TValue.From<T>(FValue);
    if BindInfo.CastStr then
      LValue := TValue.From<string>(LValue.ToString);//LValue := LValue.Cast(BindInfo.CastType);

    { TODO : if not assigned FBindObject? }
    Context
      .GetType(BindInfo.Obj.ClassType)
      .GetProperty(BindInfo.Prop)
      .SetValue(BindInfo.Obj, LValue);
  end;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TObservableField<T>.GetValue: T;
begin
  Result := FValue;
end;

procedure TObservableField<T>.SetValue(const Value: T);
var
  Comparer: IEqualityComparer<T>;
begin
  Comparer := TEqualityComparer<T>.Default;
  if Comparer.Equals(FValue, Value) then
    Exit;

  FValue := Value;

  DoChange;
end;

procedure TObservableField<T>.RemoveBind(AObject: TObject; AProperty: string);
begin
end;

{ TObservableArray<T> }

procedure TObservableArray<T>.Bind(AIndex: Integer; AObject: TObject;
  AProperty: string);
begin
  FTmpObj := AObject;
  FTmpProp := AProperty;
end;

constructor TObservableArray<T>.Create;
begin

end;

destructor TObservableArray<T>.Destroy;
begin
  inherited;
end;

procedure TObservableArray<T>.DoChange(Index: Integer);
var
  s: string;
  Context: TRttiContext;
begin
  s := 'Items[' + Index.ToString + ']';
//  TBindings.Notify(Self, s);
  Context.GetType(FTmpObj.ClassType).GetProperty(FTmpProp).SetValue(FTmpObj, TValue.From<T>(FValue[Index]));
end;

function TObservableArray<T>.GetItem(Index: Integer): T;
begin
  if Length(FValue) >= Index then
    Exit(Default(T));

  Result := FValue[Index];
end;

procedure TObservableArray<T>.SetItem(Index: Integer; const Value: T);
var
  Comparer: IEqualityComparer<T>;
begin
  if Length(FValue) >= Index then
    SetLength(FValue, Index+1);

  Comparer := TEqualityComparer<T>.Default;
  if Comparer.Equals(FValue[Index], Value) then
    Exit;

  FValue[Index] := Value;

  DoChange(Index);
end;

end.
