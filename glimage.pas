(*
 * GLImage
 *
 * Basic image class with extended filetype support.
 *
 *)

unit GLImage;

{$mode objfpc}

interface

uses
  Classes, fpImage,
  //readers
  fpReadBMP, fpReadGIF, fpReadJPEG, fpReadPCX, fpReadPNG, fpReadPNM, fpReadPSD,
  fpReadTGA, fpReadTIFF, fpReadXPM, fpReadXWD,
  //writers
  fpWriteBMP, fpWriteJPEG, fpWritePCX, fpWritePNG, fpWritePNM, fpWriteTGA,
  fpWriteTIFF, fpWriteXPM,
  Transform;

type
  { TGLImage }

  TGLImage = class(TFPMemoryImage)
  private
    FOnChange: TNotifyEvent;

  protected
    procedure Changed(Sender: TObject);

  public
    Size: TVector2i;
    procedure LoadImage(const filename: string);
    property Data: PFPIntegerArray read FData write FData;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TGLImage }

procedure TGLImage.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TGLImage.LoadImage(const filename: string);
begin
  LoadFromFile(filename);
  Size[0] := Width;
  Size[1] := Height;
  Changed(Self);
end;

end.
