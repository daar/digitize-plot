unit FrameCounter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TFrameCounter }

  TFrameCounter = object
  private
    sFPS: string;
    lasttime: double;
    frames: cardinal;
    function GetTicks: double;
  public
    function FPS: string; overload;
    procedure RenderFrame;
  end;

implementation

{ TFrameCounter }

function TFrameCounter.GetTicks: double;
begin
  Result := Now * 24 * 3600;
end;

function TFrameCounter.FPS: string;
var
  tmp: double;
begin
  tmp := GetTicks;

  if tmp - lasttime > 0.5 then
  begin
    Str(frames / (tmp - lasttime): 0: 0, sFPS);
    frames := 0;
    lasttime := tmp;
  end;

  Result := sFPS;
end;

procedure TFrameCounter.RenderFrame;
begin
  Inc(frames);
end;

end.

