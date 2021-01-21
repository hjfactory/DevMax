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
    [Test] procedure TestObserverFilterd;
  end;

implementation

uses
  DMX.Classes,
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

type
  IIntf = interface
  ['{E82F415E-14DA-4D1C-BA08-03C48C024290}']
  end;
  IIntf1 = interface(IIntf)
  ['{E4B2B43D-E5C5-4200-BFB3-E684C31F556F}']
  end;
  IIntf2 = interface(IIntf)
  ['{EDC0180B-00DD-45EC-BFCA-7C6ADFD93ADC}']
  end;
  TCommand = class
    Data: string;
  end;
  TCommand1 = class(TCommand) end;
  TCommand2 = class(TCommand) end;

  // Intf
  ICommandObserver = ICustomCommandObserver<TCommand>;

  // Impl
  TSubject = class(TCustomCommandSubject<TCommand>) end;
  TObserver1 = class(TdxInterfacedObject, ICommandObserver, IIntf1)
    procedure UpdateNotify(Sender: TObject; ACommand: TCommand);
  end;
  TObserver2 = class(TdxInterfacedObject, ICommandObserver, IIntf2)
    procedure UpdateNotify(Sender: TObject; ACommand: TCommand);
  end;

var
  NotiCount: Integer = 0;
  NotiData: string = '';

procedure TObserver1.UpdateNotify(Sender: TObject; ACommand: TCommand);
begin
  Inc(NotiCount);
  NotiData := ACommand.Data;
end;
procedure TObserver2.UpdateNotify(Sender: TObject; ACommand: TCommand);
begin
  Inc(NotiCount);
  NotiData := ACommand.Data;
end;

procedure TDesignPatternTests.TestObserver;
var
  Sub: TSubject;
  Ob1: TObserver1;
  Ob2: TObserver2;
  Cmd1: TCommand1;
  Cmd2: TCommand2;
begin
  Sub := TSubject.Create;
  Ob1 := TObserver1.Create;
  Ob2 := TObserver2.Create;
  Cmd1 := TCommand1.Create;
  Cmd2 := TCommand2.Create;

  Sub.RegistObserver(Ob1);
  Sub.RegistObserver(Ob2);

  NotiCount := 0;
  Sub.Notification(Cmd1);
  Assert.AreEqual(NotiCount, 2);

  NotiCount := 0;
  Sub.UnregistObserver(Ob2);
  Sub.Notification(Cmd1);
  Assert.AreEqual(NotiCount, 1);

  Sub.RegistObserver(Ob2);

  NotiCount := 0;
  Sub.Notification(Cmd1);
  Assert.AreEqual(NotiCount, 2);

  Cmd1.Data := 'C#1';
  Sub.Notification(Cmd1);
  Assert.AreEqual(NotiData, Cmd1.Data);

  Cmd2.Data := 'C#2';
  Sub.Notification(Cmd2);
  Assert.AreEqual(NotiData, Cmd2.Data);

  Sub.Free;
  Ob1.Free;
  Ob2.Free;
  Cmd1.Free;
  Cmd2.Free;
end;

procedure TDesignPatternTests.TestObserverFilterd;
var
  Sub: TSubject;
  Ob1: TObserver1;
  Ob2: TObserver2;
  Cmd: TCommand1;

  Intf: IInterface;
begin
  Sub := TSubject.Create;
  Ob1 := TObserver1.Create;
  Ob2 := TObserver2.Create;
  Cmd := TCommand1.Create;

  Sub.RegistObserver(Ob1);
  Sub.RegistObserver(Ob2);

  NotiCount := 0;
  Sub.NotificationTargetIntf(IIntf1, Cmd);
  Assert.AreEqual(NotiCount, 1);

  NotiCount := 0;
  Sub.NotificationTargetIntf(IIntf2, Cmd);
  Assert.AreEqual(NotiCount, 1);

  // Not support the ancestor of that interface
//  NotiCount := 0;
//  Sub.NotificationTargetIntf(IIntf, Cmd);
//  Assert.AreEqual(NotiCount, 2);

  Sub.Free;
  Ob1.Free;
  Ob2.Free;
  Cmd.Free;
end;

end.
