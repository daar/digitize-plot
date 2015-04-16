(*
 * glInfo
 *
 * Class that parses all information from OpenGL in a usable format.
 *
 * Copyright (c) 2010 by the grape3D developers team
 * Originally written by Darius Blaszyk, <darius@grape3D.org>
 * Creation date: 11-Dec-2010
 * Website: www.grape3D.org
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *)

unit glInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, GL;

type

  { TGLInfo }

  TGLInfo = class(TObject)
  private
    FVersion: string;
    FVendor: string;
    FRenderer: string;
    FExtensions: TStringList;
  protected
    function GetIntegerForIndex(const Index: GLEnum): integer;

  public
    constructor Create;
    destructor Destroy; override;

    function SupportsExtension(const AExtension: string): boolean;
    function SupportsVersion(const ACheckVersion: string): boolean;

    property Version: string read FVersion;
    property Vendor: string read FVendor;
    property Renderer: string read FRenderer;

    property Extensions: TStringList read FExtensions;
    property Integers[const anIndex: GLEnum]: integer read GetIntegerForIndex;
  end;

implementation

uses
  SysUtils, Types;

constructor TGLInfo.Create;
begin
  inherited Create;

  FVersion := glGetString(GL_VERSION);
  FVendor := glGetString(GL_VENDOR);
  FRenderer := glGetString(GL_RENDERER);

  FExtensions := TStringList.Create;
  FExtensions.Delimiter := ' ';
  FExtensions.DelimitedText := glGetString(GL_EXTENSIONS);
end;

destructor TGLInfo.Destroy;
begin
  FreeAndNil(FExtensions);

  inherited Destroy;
end;

function TGLInfo.SupportsExtension(const AExtension: string): boolean;
begin
  Result := (FExtensions.IndexOf(AExtension) <> -1);
end;

function TGLInfo.SupportsVersion(const ACheckVersion: string): boolean;
var
  versionComponents: TStringList;
  checkVersionComponents: TStringList;
  nrVersionComponents: integer;
  nrCheckVersionComponents: integer;
  index: integer;
begin
  try
    versionComponents := TStringList.Create;
    versionComponents.Delimiter := '.';
    versionComponents.DelimitedText := FVersion;

    checkVersionComponents := TStringList.Create;
    checkVersionComponents.Delimiter := '.';
    checkVersionComponents.DelimitedText := ACheckVersion;

    nrVersionComponents := versionComponents.Count;
    nrCheckVersionComponents := checkVersionComponents.Count;

    Result := True;
    index := 0;

    while Result and (index < nrVersionComponents) and (index < nrCheckVersionComponents) do
    begin
      Result := (checkVersionComponents[index] <= versionComponents[index]);
      index := index + 1;
    end;
  finally
    FreeAndNil(versionComponents);
    FreeAndNil(checkVersionComponents);
  end;
end;


function TGLInfo.GetIntegerForIndex(const Index: GLEnum): integer;
begin
  glGetIntegerV(Index, @Result);
end;

end.

