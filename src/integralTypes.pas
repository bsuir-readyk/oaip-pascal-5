unit integralTypes;

{$mode objfpc}

interface

type
  TFunction = function(x: Real): Real;
  TIntegrationMethod = function(f: TFunction; a, b: Real; n: Int64): Real;

const
  // Границы интегрирования
  A1 = 0.6; B1 = 1.4;
  A2 = 0.2; B2 = 0.8;
  A3 = 0.8; B3 = 1.6;
  A4 = 1.2; B4 = 2.0;

  EPS1 = 1E-2;
  EPS2 = 1E-3;

implementation

end.