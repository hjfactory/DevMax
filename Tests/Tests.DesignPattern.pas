unit Tests.DesignPattern;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework;

type
  [TestFixture]
  TDesignPatternTests = class(TObject)
  private
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestSingleton;
    [Test] procedure TestFactory;
    [Test] procedure TestObserver;
  end;

implementation

uses
  DMX.DesignPattern;

{ TDesignPatternTests }

procedure TDesignPatternTests.Setup;
begin

end;

procedure TDesignPatternTests.TearDown;
begin

end;

type
  TSTObj = class(TSingleton<TSTObj>)
  private
    FData: Integer;
    function GetRndData: string;
  public
    property RndData: string read GetRndData;
  end;

{ TSTObj }

function TSTObj.GetRndData: string;
begin
  if FData = 0 then
  begin
    RandSeed := DateTimeToTimeStamp(Now).Time;
    FData := Random(1000);
  end;
  Result := FData.ToString;
end;

procedure TDesignPatternTests.TestSingleton;
var
  S1, S2: string;
  O1, O2: TSTObj;
begin
  O1 := TSTObj.Instance;
  O2 := TSTObj.Instance;

  Assert.AreEqual(O1, O2);

  S1 := TSTObj.Instance.RndData;
  S2 := TSTObj.Instance.RndData;

  Assert.AreEqual(S1, S2);
end;

type
  TBaseCls = class(TInterfacedObject)
  end;
  TSubCls1 = class(TBaseCls)
  end;
  TSubCls2 = class(TBaseCls)
  end;

  TBaseClsClass = class of TBaseCls;
  TClsFactory = TClassFactory<string, TBaseClsClass>;

procedure TDesignPatternTests.TestFactory;
var
  Cls: TBaseClsClass;
  Obj: TBaseCls;
begin
  TClsFactory.Instance.Regist('sub1', TSubCls1);
  TClsFactory.Instance.Regist('sub2', TSubCls2);

  Cls := TClsFactory.Instance.GetClass('sub1');
  Assert.AreEqual(Cls.ClassName, TSubCls1.ClassName);
  Cls := TClsFactory.Instance.GetClass('sub2');
  Assert.AreEqual(Cls.ClassName, TSubCls2.ClassName);

  Obj := Cls.Create;
  Assert.AreEqual(Obj.ClassName, TSubCls2.ClassName);
  Obj.Free;
end;

procedure TDesignPatternTests.TestObserver;
begin

end;

end.
