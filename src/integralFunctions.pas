unit integralFunctions;

{$mode objfpc}

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

function GetFunction(functionNumber: Int64): TFunction;

implementation

function Function1(x: Real): Real;
begin
  Function1 := Sqrt(x*x + 5) / (2*x + Sqrt(x*x + 0.5));
end;

function Function2(x: Real): Real;
begin
  Function2 := Sin(2*x + 0.5) / (2 + Cos(x*x + 1));
end;

function Function3(x: Real): Real;
begin
  Function3 := 1 / Sqrt(2*x*x + 1);
end;

function Function4(x: Real): Real;
begin
  Function4 := Ln(x + 2) / x;
end;

function GetFunction(functionNumber: Int64): TFunction;
begin
  case functionNumber of
    1: GetFunction := @Function1;
    2: GetFunction := @Function2;
    3: GetFunction := @Function3;
    4: GetFunction := @Function4;
    else GetFunction := nil;
  end;
end;

end.