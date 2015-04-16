unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OpenGLContext, Forms, Controls,
  ComCtrls, Menus, ExtDlgs, FrameCounter, Dialogs, StdCtrls, ExtCtrls,
  GL, GLu, GLimage, GLdrawing, Camera, About, UAxis, Version, FPImage,
  Transform;

type
  TApplicationMode = (amNone, amSelectXMin, amSelectXMax, amSelectYMin,
    amSelectYMax, amDigitizeData);

  { TMainForm }

  TMainForm = class(TForm)
    MessageImage: TImage;
    ImageList: TImageList;
    MessageLabel: TLabel;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    AboutMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    OpenGLControl: TOpenGLControl;
    OpenPictureDialog: TOpenPictureDialog;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    OpenToolButton: TToolButton;
    XAxisToolButton: TToolButton;
    YAxisToolButton: TToolButton;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OpenGLControlResize(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure OpenGLControlPaint(Sender: TObject);
    procedure ActionLoadGLImage(Sender: TObject);
    procedure XAxisToolButtonClick(Sender: TObject);
    procedure YAxisToolButtonClick(Sender: TObject);
  private
    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Matrix;

var
  img: TGLImage = nil;
  cam: TCamera;
  Frame_Counter: TFrameCounter;
  axis: TAxis;
  wind: TVector2i;
  AppMode:  TApplicationMode;

{ TMainForm }

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AppMode := amNone;

  MainForm.Caption := 'digitize ' + GetVersion;

  Application.AddOnIdleHandler(@OnAppIdle);

  //testing
  axis.x_axis[0].pixel[0] := 0;
  axis.x_axis[0].pixel[1] := 0;
end;

procedure TMainForm.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  model: TMatrix4d;
  proj: TMatrix4d;
  view: TVector4i;
  objCoords : TVector3d;
begin
  case AppMode of
    amSelectXMin:
    begin
      AppMode := amSelectXMax;
      MessageLabel.Caption := 'Select the end of the X-axis';
    end;
    amSelectXMax:
    begin
      axis.x_axis[1].pixel[0] := X;
      axis.x_axis[1].pixel[1] := Y;
      AppMode := amNone;
      axis.x_axis[0].value := StrToFloat(InputBox('Enter value',
        'Enter the start value of the X axis', '0'));
      axis.x_axis[1].value := StrToFloat(InputBox('Enter value',
        'Enter the end value of the X axis', '100'));
      MessageLabel.Visible := False;
      MessageImage.Visible := False;
    end;
    amSelectYMin:
    begin
      //Ystart  := MousePos;
      AppMode := amSelectYMax;
      MessageLabel.Caption := 'Select the end of the Y-axis';
    end;
    amSelectYMax:
    begin
      //Yend    := MousePos;
      AppMode := amNone;
      //Yaxis.StartValue := StrToFloat(InputBox('Enter value',
      //  'Enter the start value of the Y axis', '0'));
      //Yaxis.EndValue := StrToFloat(InputBox('Enter value',
      //  'Enter the end value of the Y axis', '100'));
      MessageLabel.Visible := False;
      MessageImage.Visible := False;
    end;
    amDigitizeData:
    begin
      //AddDataPoint;
    end;
    amNone:
      cam.on_mouse_down(x, y);
  end;
end;

procedure TMainForm.OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  cam.on_mouse_drag(Shift, X, Y);
end;

procedure TMainForm.OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  cam.on_mouse_up;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if assigned(img) then
    img.Free;
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.OpenGLControlMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  cam.on_mouse_scroll(mousepos.x, mousepos.y, WheelDelta);
end;

procedure TMainForm.OpenGLControlResize(Sender: TObject);
begin
  wind[0] := OpenGLControl.Width;
  wind[1] := OpenGLControl.Height;
  glOrtho(0, 0, wind[0], wind[1], 1, 1);
  glViewport(0, 0, wind[0], wind[1]);
end;

procedure TMainForm.OnAppIdle(Sender: TObject; var Done: Boolean);
begin
  Done:=false;
  OpenGLControl.Invalidate;
end;

var
  tmp: TVector2d;

procedure TMainForm.OpenGLControlPaint(Sender: TObject);
var
  coord_array: Tmatrix2_double_data = ((10, 20), (0, 0));
begin
  glClearColor(0.447, 0.447, 0.447, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glEnable2D;

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glPushMatrix;

  //move the origin to the window center
  glTranslatef(cam.camera_x + wind[0] div 2, cam.camera_y + wind[1] div 2, 0);
  glRotatef(cam.rotation, 0, 0, -1);
  glScalef(cam.camera_zoom, -cam.camera_zoom, 1);

  //if hover then
  //begin
  //  m_painter.drawFrame(SetRect(-img.Width div 2, -img.Height div 2, img.Width, img.Height), 3, 0);
  //  Include(Result, peHover);
  //end;

  //draw the image
  if assigned(img) then
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, 13);
    glColor4us($ffff, $ffff, $ffff, $ffff);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 1);
      glVertex3f(-img.Width div 2, -img.Height div 2, 0);
      glTexCoord2f(1, 1);
      glVertex3f(img.Width div 2, -img.Height div 2, 0);
      glTexCoord2f(1, 0);
      glVertex3f(img.Width div 2, img.Height div 2, 0);
      glTexCoord2f(0, 0);
      glVertex3f(-img.Width div 2, img.Height div 2, 0);
    glEnd;
  end;


  //draw X axis line
  if assigned(img) then
  begin
    //coord_array[0] := screen_to_object_space(wind, axis.x_axis[0].pixel, img.Size, cam.camera_x, cam.camera_y, cam.rotation, cam.camera_zoom);
    coord_array[0][0] := img.Size[0] div 2;
    coord_array[0][1] := img.Size[1] div 2;
    coord_array[1][0] -= img.Size[0] div 2;
    coord_array[1][1] += img.Size[1] div 2;

    ui_draw_line_straight(coord_array, true, false);
    ui_draw_single_handle(coord_array[0]);
    ui_draw_single_handle(coord_array[1]);

    ui_draw_single_handle(tmp);
  end;

  glDisable(GL_TEXTURE_2D);
  glPopMatrix;

  //draw point starting from screen coordinates
  if assigned(img) then
  begin
    tmp[0] := cam.mouse[0];
    tmp[1] := cam.mouse[1];
    tmp := screen_to_object_space(wind, tmp, img.Size, cam.camera_x, cam.camera_y, cam.rotation, cam.camera_zoom);
    StatusBar.Panels[3].Text := Format('x:%d y:%d', [round(tmp[0]), round(tmp[1])]);
  end;

  //frame is rendered, increment counter
  Frame_Counter.RenderFrame;
  StatusBar.Panels[0].Text := Format('FPS: %s', [Frame_Counter.FPS]);
  StatusBar.Panels[1].Text := Format('Zoom: %d%%', [Round(cam.camera_zoom * 100)]);
  StatusBar.Panels[2].Text := Format('Angle: %ddeg', [Round(360 - cam.rotation)]);

  OpenGLControl.SwapBuffers;
end;

procedure TMainForm.ActionLoadGLImage(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    if not assigned(img) then
      img := TGLImage.Create(0, 0);

    img.UsePalette := False;
    img.LoadImage(OpenPictureDialog.FileName);

    //setup the texture
    glBindTexture(GL_TEXTURE_2D, 13);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 2);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    //glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, img.Width, img.Height, 0, GL_RGBA, GL_UNSIGNED_SHORT, img.Data);

    cam.Init;
  end;
end;

procedure TMainForm.XAxisToolButtonClick(Sender: TObject);
begin
  AppMode := amSelectXMin;
  MessageLabel.Caption := 'Select the start of the X-axis';
  MessageLabel.Visible := True;
  MessageImage.Visible := True;
end;

procedure TMainForm.YAxisToolButtonClick(Sender: TObject);
begin
  AppMode := amSelectYMin;
  MessageLabel.Caption := 'Select the start of the Y-axis';
  MessageLabel.Visible := True;
  MessageImage.Visible := True;
end;

end.

