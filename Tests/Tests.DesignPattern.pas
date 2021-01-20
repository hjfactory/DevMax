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

{ TDesignPatternTests }

procedure TDesignPatternTests.Setup;
begin

end;

procedure TDesignPatternTests.TearDown;
begin

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

procedure TDesignPatternTests.TestFactory;
begin

end;

procedure TDesignPatternTests.TestObserver;
begin

end;

end.
