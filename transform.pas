unit Transform;

{$mode objfpc}{$H+}

interface

uses
  GL, GLu,
  Matrix;

type
  TMatrix4d = array [0..15] of GLdouble;

  TVector2d = array [0..1] of GLdouble;
  TVector2i = array [0..1] of GLint;
  TVector3d = array [0..2] of GLdouble;
  TVector4i = array [0..3] of GLint;
  TVector4d = array [0..3] of GLdouble;
  TVector4f = array [0..3] of GLfloat;

//  { TTransform }
//
//  TTransform = object
//  private
//    world_center: Tvector2_double_data;
//    world_coord: Tvector2_double_data;
//    zoom: double;
//  public
//    constructor init;
//
//    procedure set_world_center(coord: Point);
//    procedure set_world_coord(coord: Point);
//  end;
//
//function RotatePoint(const pt, org: Point; const angle: Double): Point;
//
//function world_to_internal_space(world_coord, world_center, pan: Point; zoom: double): Point;
//function internal_to_object_space(internal_coord: Point; angle: double): Point;
//function world_to_object_space(world_coord, world_center, pan: Point; zoom, angle: double): Point;
//function world_to_screen_space(world_coord, world_center, pan: Point; zoom, angle: double): Point;
function screen_to_object_space(wind: TVector2i; screen_coord: TVector2d; img_size: TVector2i; camera_x, camera_y, rotation, camera_zoom: double): TVector2d;

implementation

const
  PIDiv180 = PI / 180;

//Rotate a Cartesian point
function RotatePoint(const pt: TVector2d; const angle: double): TVector2d;
var
  SinVal: double;
  CosVal: double;
begin
  SinVal := Sin(angle * PIDiv180);
  CosVal := Cos(angle * PIDiv180);
  Result[0] := Round(pt[0] * CosVal - pt[0] * SinVal);
  Result[1] := Round(pt[1] * CosVal + pt[1] * SinVal);
end;

//Rotate a Cartesian point about an arbitrary origin
function RotatePoint(const pt, org: TVector2d; const angle: Double): TVector2d;
var
  tmp: TVector2d;
begin
  tmp[0] := pt[0] - org[0];
  tmp[1] := pt[1] - org[1];

  Result := RotatePoint(tmp, angle);
  Result[0] += org[0];
  Result[1] += org[1];
end;

function screen_to_object_space(wind: TVector2i; screen_coord: TVector2d; img_size: TVector2i; camera_x, camera_y, rotation, camera_zoom: double): TVector2d;
var
  tmp: TVector2d;
  //viewport: TVector4i;
  //modelview: array [0..15] of GLdouble;
  //projection: array [0..15] of GLdouble;
  //x: integer;
  //y: integer;
  //winZ: Single;
  //res: array [1..3] of Double;
begin

//  glGetIntegerv(GL_VIEWPORT, viewport);
//  glGetDoublev(GL_MODELVIEW_MATRIX, modelview);
//  glGetDoublev(GL_PROJECTION_MATRIX, projection);
//
//  x := round(screen_coord[0]);
//  y := round(screen_coord[1]);
//  if( Y = 0 )then Y := 1;
//
//  glReadPixels(   X, -Y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @winZ );
//  gluUnProject(   X, viewport[4]-Y, winZ,
//          @modelview, @projection, @viewport,
//          @res[1], @res[2], @res[3]);
//
//  Result[0] := res[0];
//  Result[1] := res[1];
//
//exit;



  //scale
  tmp[0] := screen_coord[0] * camera_zoom;
  tmp[1] := screen_coord[1] * camera_zoom;

  //translate
  tmp[0] := tmp[0] - camera_x - (-img_size[0] + wind[0]) div 2;
  tmp[1] := tmp[1] - camera_y - (-img_size[1] + wind[1]) div 2;


  result := tmp;

  exit;

  tmp[0] := screen_coord[0] - camera_x - wind[0] div 2;
  tmp[1] := screen_coord[1] - camera_y - wind[1] div 2;


  result[0] := tmp[0] - img_size[0] div 2;
  result[1] := tmp[1] - img_size[1] div 2;

  //rotate
  result := rotatepoint(result, tmp, 360-rotation);

end;


//function world_to_internal_space(world_coord, world_center, pan: Point; zoom: double): Point;
//begin
//  Result.Point(Round((world_coord.x - world_center.x + pan.x) * zoom),
//               Round((world_coord.y - world_center.y + pan.y) * zoom));
//end;
//
//function internal_to_object_space(internal_coord: Point; angle: double): Point;
//begin
//  Result := RotatePoint(internal_coord, angle);
//end;
//
//function world_to_object_space(world_coord, world_center, pan: Point; zoom, angle: double): Point;
//var
//  internal_space: Point;
//begin
//  internal_space := world_to_internal_space(world_coord, world_center, pan, zoom);
//  Result := internal_to_object_space(internal_space, angle);
//end;
//
//function world_to_screen_space(world_coord, world_center, pan: Point; zoom, angle: double): Point;
//begin
//  Result := world_to_object_space(world_coord, world_center, pan, zoom, angle);
//
//  Result.x += world_center.x;
//  Result.y += world_center.y;
//end;
//
//{ TTransform }
//
//constructor TTransform.init;
//begin
//  world_center[0] := 0;
//  world_center[1] := 0;
//
//  world_coord[0] := 0;
//  world_coord[1] := 0;
//end;
//
//procedure TTransform.set_world_center(coord: Point);
//begin
//  world_center[0] := coord.x;
//  world_center[1] := coord.y;
//end;
//
//procedure TTransform.set_world_coord(coord: Point);
//begin
//  world_coord[0] := coord.x;
//  world_coord[1] := coord.y;
//end;

end.

