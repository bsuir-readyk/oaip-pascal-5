unit tableOutput;

interface

procedure PrintTableHeader;
procedure PrintTableRow(rowName: string; 
                        value1, n1, value2, n2, 
                        value3, n3, value4, n4: Real);
procedure PrintTableFooter;

implementation

uses SysUtils;

procedure PrintTableHeader;
begin
  WriteLn('+---------------+----------+----------+----------+----------+----------+----------+----------+----------+');
  WriteLn('|               | 1ый метод | 1ый метод | 1ый метод | 1ый метод | 2ой метод | 2ой метод | 2ой метод | 2ой метод |');
  WriteLn('+---------------+----------+----------+----------+----------+----------+----------+----------+----------+');
  WriteLn('|               | eps=10^-2 | eps=10^-2 | eps=10^-3 | eps=10^-3 | eps=10^-2 | eps=10^-2 | eps=10^-3 | eps=10^-3 |');
  WriteLn('|               | значение  | N         | значение  | N         | значение  | N         | значение  | N         |');
  WriteLn('+---------------+----------+----------+----------+----------+----------+----------+----------+----------+');
end;

procedure PrintTableRow(rowName: string; 
                        value1, n1, value2, n2, 
                        value3, n3, value4, n4: Real);
begin
  WriteLn(Format('| %-14s | %8.5f | %8.0f | %8.5f | %8.0f | %8.5f | %8.0f | %8.5f | %8.0f |',
                [rowName, value1, n1, value2, n2, value3, n3, value4, n4]));
end;

procedure PrintTableFooter;
begin
  WriteLn('+----------------+----------+----------+----------+----------+----------+----------+----------+----------+');
end;

end.