unit DMX.LiveData;

interface

type
  TLiveData<T> = record
    Value: T;
    class operator Implicit(const AValue: T): TLiveData<T>;
    class operator Implicit(const AValue: TLiveData<T>): T;
    class operator Explicit(const AValue: T): TLiveData<T>;
    class operator Explicit(const AValue: TLiveData<T>): T;

    class operator Add(const A, B: TLiveData<T>): TLiveData<T>;

    class operator Equal(A, B: TLiveData<T>): Boolean;
//    class operator Equal(A: TLiveData<T>; B: T): Boolean;
    class operator NotEqual(const A, B: TLiveData<T>): Boolean;
  end;

implementation

uses
 System.Generics.Defaults;

{ TLiveData<T> }

class operator TLiveData<T>.Add(const A, B: TLiveData<T>): TLiveData<T>;
begin
//  Result := A.Value + B.Value;
end;

//function TLiveData<T>.GetValue: T;
//begin
//  Result := FValue;
//end;

class operator TLiveData<T>.Explicit(const AValue: TLiveData<T>): T;
begin
  Result := AValue.Value;
end;

class operator TLiveData<T>.Explicit(const AValue: T): TLiveData<T>;
begin
  Result.Value := AValue;
end;

class operator TLiveData<T>.Implicit(const AValue: TLiveData<T>): T;
begin
  Result := AValue.Value;
end;

class operator TLiveData<T>.Implicit(const AValue: T): TLiveData<T>;
begin
  Result.Value := AValue;
end;

class operator TLiveData<T>.Equal(A, B: TLiveData<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  Comparer := TEqualityComparer<T>.Default;

  Result := Comparer.Equals(A.Value, B.Value);
end;

class operator TLiveData<T>.NotEqual(const A, B: TLiveData<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  Comparer := TEqualityComparer<T>.Default;

  Result := not Comparer.Equals(A.Value, B.Value);
end;

end.
