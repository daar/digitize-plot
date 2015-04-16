unit UAxis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Transform;

type
  TAxisPoint = record
    pixel: TVector2d;
    value: double;
  end;

  TAxis = object
    x_axis: array [0..1] of TAxisPoint;
  end;

implementation

end.

