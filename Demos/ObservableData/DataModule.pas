unit DataModule;

interface

uses
  System.SysUtils, System.Classes, DMX.ObservableData;

type
  TDataModule1 = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FIntegerValue: TObservableField<Integer>;
  public
    property IntegerValue: TObservableField<Integer> read FIntegerValue write FIntegerValue;
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  FIntegerValue := TObservableField<Integer>.Create;
end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
  FIntegerValue.Free;
end;

end.
