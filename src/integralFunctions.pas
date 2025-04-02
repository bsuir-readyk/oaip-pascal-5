unit integralFunctions;

interface

uses
  Math, integralTypes;

// Функция 1: (√(x²+5))/(2x+√(x²+0.5))
function Function1(x: Real): Real;

// Функция 2: (sin(2x+0.5))/(2+cos(x²+1))
function Function2(x: Real): Real;

// Функция 3: 1/√(2x²+1)
function Function3(x: Real): Real;

// Функция 4: (ln(x+2))/x
function Function4(x: Real): Real;

// Функция-селектор для выбора нужной функции по номеру
function GetFunction(functionNumber: Integer): TFunction;

implementation

function Function1(x: Real): Real;
begin
  Result := Sqrt(x*x + 5) / (2*x + Sqrt(x*x + 0.5));
end;

function Function2(x: Real): Real;
begin
  Result := Sin(2*x + 0.5) / (2 + Cos(x*x + 1));
end;

function Function3(x: Real): Real;
begin
  Result := 1 / Sqrt(2*x*x + 1);
end;

function Function4(x: Real): Real;
begin
  Result := Ln(x + 2) / x;
end;

function GetFunction(functionNumber: Integer): TFunction;
begin
  case functionNumber of
    1: Result := @Function1;
    2: Result := @Function2;
    3: Result := @Function3;
    4: Result := @Function4;
    else Result := nil;
  end;
end;

end.