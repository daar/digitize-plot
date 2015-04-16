unit About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, LCLIntf;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    DeveloperLabel1: TLabel;
    URLLabel: TLabel;
    Image1: TImage;
    Label1: TLabel;
    VersionLabel: TLabel;
    LicenseMemo: TMemo;
    OpenGLMemo: TMemo;
    PageControl: TPageControl;
    LicenseTabSheet: TTabSheet;
    GLInfoTabSheet: TTabSheet;
    DeveloperLabel: TLabel;
    procedure URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure URLLabelMouseEnter(Sender: TObject);
    procedure URLLabelMouseLeave(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses
  GLinfo, Version;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key = #27 then Close;
end;

procedure TAboutForm.URLLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
end;

procedure TAboutForm.URLLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  sURL: String;
begin
  sURL := TLabel(Sender).Caption;

  if pos('@', sURL) > 0 then
     sURL := 'mailto:' + sURL;
  OpenURL(sURL);
end;

procedure TAboutForm.URLLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
end;

procedure TAboutForm.FormShow(Sender: TObject);
var
  OpenGLInfo: TGLInfo;
begin
  PageControl.ActivePage := LicenseTabSheet;

  OpenGLInfo := TGLInfo.Create;
  OpenGLMemo.Clear;
  OpenGLMemo.Lines.Add('Version: ' + OpenGLInfo.Version);
  OpenGLMemo.Lines.Add('Vendor: ' + OpenGLInfo.Vendor);
  OpenGLMemo.Lines.Add('Renderer: ' + OpenGLInfo.Renderer);
  OpenGLMemo.Lines.Add('');
  OpenGLMemo.Lines.Add('Extensions:');
  OpenGLMemo.lines.AddStrings(OpenGLInfo.Extensions);
  OpenGLInfo.Free;

  VersionLabel.Caption := 'Version: ' + GetVersion;
end;

end.

