program solve;

{ MODE $DELPHI }

uses
  SysUtils, Math,
  integralTypes in 'src/integralTypes.pas',
  integralFunctions in 'src/integralFunctions.pas',
  integralMethods in 'src/integralMethods.pas',
  tableOutput in 'src/tableOutput.pas';

var
  f: TFunction;
  value1, value2, value3, value4: Real;
  n1, n2, n3, n4: Int64;
  i: Int64;

begin
  PrintTableHeader;
  
  for i := 1 to 4 do
  begin
    f := GetFunction(i);
    
    case i of
      1: begin
        // Метод левых прямоугольников с eps=10^-2
        value1 := CalculateWithPrecision(@LeftRectangleMethod, f, A1, B1, EPS1, n1);
        // Метод левых прямоугольников с eps=10^-3
        value2 := CalculateWithPrecision(@LeftRectangleMethod, f, A1, B1, EPS2, n2);
        // Метод трапеций с eps=10^-2
        value3 := CalculateWithPrecision(@TrapezoidMethod, f, A1, B1, EPS1, n3);
        // Метод трапеций с eps=10^-3
        value4 := CalculateWithPrecision(@TrapezoidMethod, f, A1, B1, EPS2, n4);
      end;
      2: begin
        // Метод левых прямоугольников с eps=10^-2
        value1 := CalculateWithPrecision(@LeftRectangleMethod, f, A2, B2, EPS1, n1);
        // Метод левых прямоугольников с eps=10^-3
        value2 := CalculateWithPrecision(@LeftRectangleMethod, f, A2, B2, EPS2, n2);
        // Метод трапеций с eps=10^-2
        value3 := CalculateWithPrecision(@TrapezoidMethod, f, A2, B2, EPS1, n3);
        // Метод трапеций с eps=10^-3
        value4 := CalculateWithPrecision(@TrapezoidMethod, f, A2, B2, EPS2, n4);
      end;
      3: begin
        // Метод левых прямоугольников с eps=10^-2
        value1 := CalculateWithPrecision(@LeftRectangleMethod, f, A3, B3, EPS1, n1);
        // Метод левых прямоугольников с eps=10^-3
        value2 := CalculateWithPrecision(@LeftRectangleMethod, f, A3, B3, EPS2, n2);
        // Метод трапеций с eps=10^-2
        value3 := CalculateWithPrecision(@TrapezoidMethod, f, A3, B3, EPS1, n3);
        // Метод трапеций с eps=10^-3
        value4 := CalculateWithPrecision(@TrapezoidMethod, f, A3, B3, EPS2, n4);
      end;
      4: begin
        // Метод левых прямоугольников с eps=10^-2
        value1 := CalculateWithPrecision(@LeftRectangleMethod, f, A4, B4, EPS1, n1);
        // Метод левых прямоугольников с eps=10^-3
        value2 := CalculateWithPrecision(@LeftRectangleMethod, f, A4, B4, EPS2, n2);
        // Метод трапеций с eps=10^-2
        value3 := CalculateWithPrecision(@TrapezoidMethod, f, A4, B4, EPS1, n3);
        // Метод трапеций с eps=10^-3
        value4 := CalculateWithPrecision(@TrapezoidMethod, f, A4, B4, EPS2, n4);
      end;
    end;
    
    PrintTableRow(Format('%d-ый интеграл', [i]), value1, n1, value2, n2, value3, n3, value4, n4);
  end;
  
  PrintTableFooter;
end.
