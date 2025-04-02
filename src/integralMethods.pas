unit integralMethods;

interface

uses
  integralTypes;

// Метод левых прямоугольников
function LeftRectangleMethod(f: TFunction; a, b: Real; n: Integer): Real;

// Метод трапеций
function TrapezoidMethod(f: TFunction; a, b: Real; n: Integer): Real;

// Вычисление интеграла с заданной точностью
function CalculateWithPrecision(method: TIntegrationMethod; f: TFunction; a, b: Real; eps: Real; var iterations: Integer): Real;

implementation

function LeftRectangleMethod(f: TFunction; a, b: Real; n: Integer): Real;
var
  h, sum: Real;
  i: Integer;
begin
  h := (b - a) / n;
  sum := 0;
  for i := 0 to n - 1 do
    sum := sum + f(a + i * h);
  Result := h * sum;
end;

function TrapezoidMethod(f: TFunction; a, b: Real; n: Integer): Real;
var
  h, sum: Real;
  i: Integer;
begin
  h := (b - a) / n;
  sum := (f(a) + f(b)) / 2;
  for i := 1 to n - 1 do
    sum := sum + f(a + i * h);
  Result := h * sum;
end;

function CalculateWithPrecision(method: TIntegrationMethod; f: TFunction; a, b: Real; eps: Real; var iterations: Integer): Real;
var
  prevResult, currResult: Real;
  n: Integer;
begin
  n := 1;
  currResult := method(f, a, b, n);
  
  repeat
    prevResult := currResult;
    n := n * 2;
    currResult := method(f, a, b, n);
  until Abs(currResult - prevResult) <= eps;
  
  iterations := n;
  Result := currResult;
end;

end.