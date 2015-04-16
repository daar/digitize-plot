unit Camera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Transform;

type

  { TCamera }

  TCamera = object
  private
    old_camera_x: Double;
    old_camera_y: Double;
    tmp_x: LongInt;
    tmp_y: LongInt;
    dragging: Boolean;
  public
    mouse: TVector2i;
    camera_x: double;
    camera_y: double;
    camera_zoom: double;
    rotation: double;
    constructor Init;
    procedure on_mouse_scroll(x, y: Integer; WheelDelta: Integer);
    procedure on_mouse_drag(Shift: TShiftState; X, Y: Integer);
    procedure on_mouse_down(x, y: Integer);
    procedure on_mouse_up;
  end;


implementation

uses
  Math;

constructor TCamera.Init;
begin
  //initialize camera values
  camera_x := 0;
  camera_y := 0;
  camera_zoom := 1;

  dragging := false;
end;

procedure TCamera.on_mouse_scroll(x, y: Integer; WheelDelta: Integer);
//zooming constants
const
  ZOOM_FACTOR = 1.2;
var
  f: double;
begin
  //get scale factor
  if WheelDelta < 0 then
    f := ZOOM_FACTOR
  else
    f := 1 / ZOOM_FACTOR;

  //if zoom_level is in the proper range
  if (camera_zoom * f > 0.02) and (camera_zoom * f < 50) then
  begin
    //zoom camera
    camera_zoom *= f;
  end;
end;

procedure TCamera.on_mouse_drag(Shift: TShiftState; X, Y: Integer);
begin
  mouse[0] := X;
  mouse[1] := Y;

  if not dragging then exit;

  if Shift = [ssCtrl, ssLeft] then
  begin
    rotation := radtodeg(arctan2(tmp_y - y, x - tmp_x));
    if rotation < 0 then rotation += 360;
    if rotation > 360 then rotation -= 360;
  end;

  if Shift = [ssShift, ssLeft] then
  begin
    //move camera
    camera_x := old_camera_x + (x - tmp_x);
    camera_y := old_camera_y + (y - tmp_y);
  end;
end;

procedure TCamera.on_mouse_down(x, y: Integer);
begin
  old_camera_x := camera_x;
  old_camera_y := camera_y;
  tmp_x := x;
  tmp_y := y;
  dragging := true;
end;

procedure TCamera.on_mouse_up;
begin
  dragging := false;
end;

end.

