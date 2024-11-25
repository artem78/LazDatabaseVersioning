unit Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function AddTrailingSemicolon(const ASQL: String): String;

implementation

uses
  StrUtils;

// note: обрезает пробельные символы в конце (вроде влиять не должно)
function AddTrailingSemicolon(const ASQL: String): String;
begin
    Result := TrimRight(ASQL);
    if (Result <> '') and (not EndsStr(';', Result)) then
        Result := Result + ';';
end;

end.

