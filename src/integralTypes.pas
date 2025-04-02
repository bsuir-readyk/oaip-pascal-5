unit integralTypes;

interface

type
  TFunction = function(x: Real): Real;
  TIntegrationMethod = function(f: TFunction; a, b: Real; n: Integer): Real;

const
  // Границы интегрирования
  A1 = 0.6; B1 = 1.4; // Для первого интеграла
  A2 = 0.2; B2 = 0.8; // Для второго интеграла
  A3 = 0.8; B3 = 1.6; // Для третьего интеграла
  A4 = 1.2; B4 = 2.0; // Для четвертого интеграла
  
  // Точности
  EPS1 = 1E-2;
  EPS2 = 1E-3;

implementation

end.