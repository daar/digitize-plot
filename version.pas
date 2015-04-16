unit Version;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetVersion: string;

implementation

function GetVersion: string;
begin
  Result := '14.12';
end;

end.

