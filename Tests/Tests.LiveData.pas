unit Tests.LiveData;

interface

uses
  DUnitX.TestFramework;

type
  TMyData = class
  private
    FData: Integer;
  public
    property Data: Integer read FData write FData;
  end;

  [TestFixture]
  TLiveDataTests = class(TObject)
  private
    procedure TestAssign<T>(AVal1, AVal2: T);
    procedure TestEqual<T>(AVal1, AVal2: T);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestAssignInteger;
    [Test] procedure TestAssignString;
    [Test] procedure TestAssignDateTime;

    [Test] procedure TestAssignObject;

    [Test] procedure TestEqualInteger;
    [Test] procedure TestEqualString;
    [Test] procedure TestEqualDateTime;
  end;

implementation

uses DMX.LiveData, System.Generics.Collections,
  System.SysUtils, System.DateUtils;

procedure TLiveDataTests.Setup;
begin
end;

procedure TLiveDataTests.TearDown;
begin
end;

procedure TLiveDataTests.TestAssign<T>(AVal1, AVal2: T);
var
  OV: T;
  LV: TLiveData<T>;
begin
  OV := AVal1;
  LV := AVal2;
  Assert.AreEqual(LV.Value, AVal2);
  LV := OV;
  Assert.AreEqual(LV.Value, AVal1);
  LV := AVal2;
  OV := LV;
  Assert.AreEqual(OV, AVal2);
end;

procedure TLiveDataTests.TestEqual<T>(AVal1, AVal2: T);
var
  OV: T;
  LV1, LV2: TLiveData<T>;
begin
  OV := AVal1;
  LV1 := AVal1;
  LV2 := AVal1;
  Assert.IsTrue(OV = LV1);
  Assert.IsTrue(LV1 = LV2);
  LV1 := AVal2;
  LV2 := AVal1;
  Assert.IsTrue(OV <> LV1);
  Assert.IsTrue(LV1 <> LV2);
end;

procedure TLiveDataTests.TestAssignInteger;
begin
  TestAssign<Integer>(10, 20);
  TestAssign<Integer>(-1, 100);
end;

procedure TLiveDataTests.TestAssignObject;
var
  O1, O2, O3: TMyData;
  LO: TLiveData<TMyData>;
begin
//  LO.Data := 100;
  O1 := TMyData.Create;
  O1.Data := 100;
  O2 := TMyData.Create;
  O2.Data := 200;
  O3 := O1;

  Assert.IsFalse(O1 = O2);
  Assert.IsTrue(O1 = O3);

  LO := O1;
//  LO := TMyData.Create;
//  LO.Value := O3;
  Assert.IsTrue(O1 = LO);
end;

procedure TLiveDataTests.TestAssignString;
begin
  TestAssign<string>('abc', '가나다');
end;

procedure TLiveDataTests.TestAssignDateTime;
begin
  TestAssign<TDateTime>(
    EncodeDate(2019, 10, 10) + EncodeTime(10, 30, 20, 0),
    EncodeDate(2019, 11, 14) + EncodeTime(17, 20, 50, 100)
  );
end;

procedure TLiveDataTests.TestEqualInteger;
begin
  TestEqual<Integer>(10, 20);
end;

procedure TLiveDataTests.TestEqualString;
begin
  TestEqual<string>('abc', '가나다');
end;

procedure TLiveDataTests.TestEqualDateTime;
begin
  TestEqual<TDateTime>(
    EncodeDate(2019, 10, 10) + EncodeTime(10, 30, 20, 0),
    EncodeDate(2019, 11, 14) + EncodeTime(17, 20, 50, 100)
  );
end;

initialization
  TDUnitX.RegisterTestFixture(TLiveDataTests);
end.
