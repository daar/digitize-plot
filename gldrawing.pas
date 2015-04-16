//Additional OpenGL drawing routines
unit glDrawing;

{$mode objfpc}{$H+}

interface

uses
  GL, fpImage,
  Transform, Matrix;

procedure glEnable2D;
procedure glDisable2D;

procedure glRect(x, y, w, h: double; AColor: TFPColor);
procedure glFillRect(x, y, w, h: double; AColor: TFPColor);
procedure glFillRoundRect(x, y, w, h, radius: integer; AColor: TFPColor);

procedure ui_draw_line_straight(coord_array: Tmatrix2_double_data; do_triple, do_shaded: boolean);
procedure draw_circle(const x, y, size: GLfloat;
                        const fill: boolean;
                        const xscale, yscale: GLfloat);
procedure ui_draw_single_handle(coord: TVector2d);

implementation

procedure glEnable2D;
var
  vPort: array [0..3] of integer;
begin
   glGetIntegerv(GL_VIEWPORT, vPort);

   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;

   glOrtho(0, vPort[2], vPort[3], 0, -1, 1);
   glMatrixMode(GL_MODELVIEW);
   glViewPort(0, 0, vPort[2], vPort[3]);              // Set the viewport for the OpenGL window
   glPushMatrix;
   glLoadIdentity;
end;

procedure glDisable2D;
begin
   glMatrixMode(GL_PROJECTION);
   glPopMatrix;
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;
end;

procedure glRect(x, y, w, h: double; AColor: TFPColor);
begin
  glColor4us(AColor.red, AColor.green, AColor.blue, AColor.alpha);
  glBegin(GL_LINE_LOOP);
    glVertex2f(x, y);
    glVertex2f(x + w, y);
    glVertex2f(x + w, y + h);
    glVertex2f(x, y + h);
  glEnd();
end;

//a filled rectangle
procedure glFillRect(x, y, w, h: double; AColor: TFPColor);
begin
  glColor4us(AColor.red, AColor.green, AColor.blue, AColor.alpha);
  glRectf(x, y, x + w, y + h);
end;

//a filled round rectangle
procedure glFillRoundRect(x, y, w, h, radius: integer; AColor: TFPColor);
var
  t: double;
  sx, sy: double;
  step: double;
begin
  //calculate the stepsize
  step := 1 / radius;

  glColor4us(AColor.red, AColor.green, AColor.blue, AColor.alpha);

  glBegin(GL_POLYGON);
  //glBegin(GL_LINE_LOOP);

  glVertex2f(x + radius, y);
  glVertex2f(x + w - radius, y);
  t := PI * 1.5;
  while t < PI * 2 do
  begin
    sx := x + w - radius + cos(t) * radius;
    sy := y + radius + sin(t) * radius;
    glVertex2f(sx, sy);
    t += step;
  end;

  glVertex2f(x + w, y + radius);
  glVertex2f(x + w, y + h - radius);
  t := 0;
  while t < PI * 0.5 do
  begin
    sx := x + w - radius + cos(t) * radius;
    sy := y + h - radius + sin(t) * radius;
    glVertex2f(sx, sy);
    t += step;
  end;

  glVertex2f(x + w - radius, y + h);
  glVertex2f(x + radius, y + h);
  t := PI * 0.5;
  while t < PI do
  begin
    sx := x + radius + cos(t) * radius;
    sy := y + h - radius + sin(t) * radius;
    glVertex2f(sx, sy);
    t += step;
  end;

  glVertex2f(x, y + h - radius);
  glVertex2f(x, y + radius);
  t := PI;
  while t < PI * 1.5 do
  begin
    sx := x + radius + cos(t) * radius;
    sy := y + radius + sin(t) * radius;
    glVertex2f(sx, sy);
    t += step;
  end;

  glEnd;
end;

const
  LINK_RESOL = 24;

procedure ui_draw_line_straight(coord_array: Tmatrix2_double_data; do_triple, do_shaded: boolean);
var
  linew: GLFloat;
  i: integer;
  t: double;
begin
	//store current linewidth
	glGetFloatv(GL_LINE_WIDTH, @linew);

	glEnable(GL_LINE_SMOOTH);

	if do_triple then begin
		//UI_ThemeColorShadeAlpha(th_col3, -80, -120);
                glColor4us($0000, $0000, $0000, $ffff);
		glLineWidth(4.0);

		glBegin(GL_LINES);
		glVertex2f(coord_array[0,0], coord_array[0,1]);
		glVertex2f(coord_array[1,0], coord_array[1,1]);
		glEnd();
	end;

	//UI_ThemeColor(th_col1);
        glColor4us($ffff, $ffff, $ffff, $ffff);
	glLineWidth(1.5);

	(* XXX using GL_LINES for shaded node lines is a workaround
	 * for Intel hardware, this breaks with GL_LINE_STRIP and
	 * changing color in begin/end blocks.
	 *)
	if do_shaded then begin
		glBegin(GL_LINES);
		for i := 0 to LINK_RESOL - 1 do begin
			t := i / (LINK_RESOL - 1);
			//UI_ThemeColorBlend(th_col1, th_col2, t);
			glVertex2f((1 - t) * coord_array[0][0] + t * coord_array[1][0],
			           (1 - t) * coord_array[0][1] + t * coord_array[1][1]);

			t := (i + 1) / (LINK_RESOL - 1);
			//UI_ThemeColorBlend(th_col1, th_col2, t);
			glVertex2f((1 - t) * coord_array[0][0] + t * coord_array[1][0],
			           (1 - t) * coord_array[0][1] + t * coord_array[1][1]);
		end;
		glEnd();
	end
	else begin
		glBegin(GL_LINE_STRIP);
		for i := 0 to LINK_RESOL - 1 do begin
			t := i / (LINK_RESOL - 1);
			glVertex2f((1 - t) * coord_array[0][0] + t * coord_array[1][0],
			           (1 - t) * coord_array[0][1] + t * coord_array[1][1]);
		end;
		glEnd();
	end;

	glDisable(GL_LINE_SMOOTH);

	//restore previuos linewidth
	glLineWidth(linew);
end;

var
  wire_displist: GLuint = 0;
  fill_displist: GLuint = 0;

procedure draw_circle(const x, y, size: GLfloat;
                        const fill: boolean;
                        const xscale, yscale: GLfloat);
var
  displist: GLuint;
  i: Integer;
  cosine: GLfloat;
  sine: GLfloat;
begin
  if fill then
    displist := fill_displist
  else
    displist := wire_displist;

  //Initialize round circle shape.
  if displist = 0 then
  begin
    displist := glGenLists(1);
    glNewList(displist, GL_COMPILE);

    glBegin(GL_POLYGON);
    for i := 0 to 99 do
    begin
      cosine := cos(i*2*PI/100.0);
      sine := sin(i*2*PI/100.0);
      glVertex2f(cosine,sine);
    end;

    glEnd();

    glEndList();

    if fill then
      fill_displist := displist
    else
      wire_displist := displist;
  end;

  glPushMatrix();
  glTranslatef(x, y, 0);
  glScalef(1 / xscale * size, 1 / yscale * size, 1);
  glCallList(displist);
  glPopMatrix();
end;

procedure ui_draw_single_handle(coord: TVector2d);
const
  HANDLE_SIZE = 10;
begin
  glRect(coord[0] - HANDLE_SIZE / 2, coord[1] - HANDLE_SIZE / 2, HANDLE_SIZE, HANDLE_SIZE, colBlue);
end;

end.

