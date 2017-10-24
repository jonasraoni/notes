unit XGComps;

interface

uses Windows, SysUtils, Classes, StdCtrls, Controls, Graphics,
Forms, Messages, ExtCtrls;

type

  TXgScrollBox = class(TScrollBox)
  private
    FCanvas: TControlCanvas;
    fOnPaint: TNotifyEvent;
    function getCanvas: TCanvas;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read getCanvas;
  published
    property OnPaint: TNotifyEvent read fOnPaint write fOnPaint;
  end;

  TXgMemo = class(TMemo)
  private
    procedure setText(const Value: string);
    function getText: string;
  public
    Constructor Create(Owner: TComponent); override;
  published
    property Text: string read getText write setText;
  end;

  TXgComboBox = Class(TComboBox)
  private
    function getEditable: boolean;
    procedure setEditable(const Value: boolean);
  public
    constructor Create(Owner: TComponent); override;
  published
    property Editable: boolean read getEditable write setEditable;
  end;


  TXgHyperLink = class(TLabel)
  private
    FSaveFont: TFont;
    FSaveColor: TColor;
    fUri: string;
    fHoverColor: TColor;
    FHoverFont: TFont;
    procedure SetHoverFont(const Value: TFont);
    procedure setUri(const Value: string);
  protected
    procedure Click; override;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Uri: string read fUri write setUri;
    property HoverFont: TFont read FHoverFont write SetHoverFont;
    property HoverColor: TColor read fHoverColor write fHoverColor;
  end;

  TXgImage = class(TImage)
  private
    fSrc: string;
    fHoverSrc: string;
    fBmp: TBitmap;
    fHoverBmp: TBitmap;
    fHoverStc: string;
    procedure setHoverSrc(const Value: string);
    procedure setSrc(const Value: string);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;    
  public
    constructor Create(Owner: TComponent); override;
    destructor  Destroy; override;
  published
    property src: string read fSrc write setSrc;
    property hoverSrc: string read fHoverStc write setHoverSrc;
  end;

  TXgCustomShape = class(TGraphicControl)
  private
    function getFill: TColor;
    function getStroke: TColor;
    function getStrokeWidth: integer;
    procedure setFill(const Value: TColor);
    procedure setStroke(const Value: TColor);
    procedure setStrokeWidth(const Value: integer);
  protected
    FPen: TPen;
    FBrush: TBrush;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property stroke: TColor read getStroke write setStroke;
    property fill: TColor read getFill write setFill;
    property strokeWidth: integer read getStrokeWidth write setStrokeWidth;
 end;

  TXgRect = class(TXgCustomShape)
  private
    fRy: integer;
    fRx: integer;
    procedure setX(const Value: integer);
    procedure setY(const Value: integer);
    function getX: integer;
    function getY: integer;
  protected
    procedure Paint; override;
  published
    property X: integer read getX write setX;
    property Y: integer read getY write setY;
    property rx: integer read fRx write fRx;
    property Ry: integer read fRy write fRy;
  end;

  TXgEllipse = class(TXgCustomShape)
  private
    fcy: integer;
    fcx: integer;
    fry: integer;
    frx: integer;
    procedure setcx(const Value: integer);
    procedure setcy(const Value: integer);
    procedure setrx(const Value: integer);
    procedure setry(const Value: integer);
  protected
    procedure Paint; override;
  published
    property cx: integer read fcx write setcx;
    property cy: integer read fcy write setcy;
    property rx: integer read frx write setrx;
    property ry: integer read fry write setry;
  end;

  TXgCircle = class(TXgEllipse)
  private
    function getR: integer;
    procedure setR(const Value: integer);
  published
    property r: integer read getR write setR;
  end;

  TXgLine = class(TXgCustomShape)
  private
    fY1: integer;
    fY2: integer;
    fX1: integer;
    fX2: integer;
    procedure setX1(const Value: integer);
    procedure setX2(const Value: integer);
    procedure setY1(const Value: integer);
    procedure setY2(const Value: integer);
  protected
    procedure Paint; override;
    procedure CalcSize; dynamic;
  published
    property X1: integer read fX1 write setX1;
    property X2: integer read fX2 write setX2;
    property Y1: integer read fY1 write setY1;
    property Y2: integer read fY2 write setY2;
  end;

implementation

uses ShellApi, XGUtils, Math;

{ TXgMemo }

constructor TXgMemo.Create(Owner: TComponent);
begin
  inherited;
  ScrollBars:= ssBoth;
  WordWrap:= false;
end;

function TXgMemo.getText: string;
begin
  Result:= Lines.Text;
end;

procedure TXgMemo.setText(const Value: string);
begin
  Lines.Text:= Value;
end;

{ TXgScrollBox }

constructor TXgScrollBox.Create(Owner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  HorzScrollBar.Tracking:= true;
  VertScrollBar.Tracking:= true;
  BorderStyle:= bsNone;
end;

destructor TXgScrollBox.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

function TXgScrollBox.getCanvas: TCanvas;
begin
  Result:= fCanvas;
end;

procedure TXgScrollBox.Paint;
begin
  if assigned(fOnPaint) then fOnPaint(self);
end;

procedure TXgScrollBox.PaintWindow(DC: HDC);
begin
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

{ TXgComboBox }



{ TXgComboBox }

constructor TXgComboBox.Create(Owner: TComponent);
begin
  inherited;
  Style := csDropDownList;
end;

function TXgComboBox.getEditable: boolean;
begin
  Result:= Style <> csDropDownList;
end;

procedure TXgComboBox.setEditable(const Value: boolean);
begin
  if Value then
    Style:= csDropDown
  else
    Style := csDropDownList;
end;


{ TXgHyperLink }

procedure TXgHyperLink.Click;
begin
  inherited;

  if Uri <> '' then
  begin
    if Owner.GetInterfaceEntry(StringToGUID('{01FB7AFE-C5E4-4597-9CD3-9BF9E1006E74}')) <> nil then
    begin
      (Owner AS IXgOwner).openURI(URI);
    end else
      ShellExecute(0, 'open', PChar(URI), '', '', SW_SHOW);
  end;
end;

procedure TXgHyperLink.CMMouseEnter(var Msg: TMessage);
begin
  if csDesigning in ComponentState then Exit;
  if FSaveFont <> Font then FSaveFont.Assign(Font);
  FSaveColor:= Color;
  if fHoverColor <> clNone then
  begin
    self.Transparent:= false;
    Color:= fHoverColor;
  end;
  Font.Assign(FHoverFont);
end;

procedure TXgHyperLink.CMMouseLeave(var Msg: TMessage);
begin
  if csDesigning in ComponentState then Exit;
  Font.Assign(FSaveFont);
  Color:= fSavecolor;
end;

constructor TXgHyperLink.Create(aOwner: TComponent);
begin
  inherited;
  FHoverFont := TFont.Create;
  FSaveFont := TFont.Create;
  FSaveFont.Assign(Font);
  ShowHint:= true;
  ShowAccelChar:= true;
  Cursor:= crHandPoint;
  Font.Color:= clNavy;
  Font.Style:= [fsUnderline];
  fHoverFont.Color:= clBlue;
  fHoverFont.Style:= [fsUnderline];
  fHoverColor:= clNone;
  self.Transparent:= true;
end;

destructor TXgHyperLink.Destroy;
begin
  FSaveFont.Free;
  FHoverFont.Free;
  inherited;
end;

procedure TXgHyperLink.SetHoverFont(const Value: TFont);
begin
  FHoverFont.Assign(Value);
end;

procedure TXgHyperLink.setUri(const Value: string);
begin
  fUri := Value;
  if Hint = '' then
    Hint:= Uri;
end;

{ TXgImage }

procedure TXgImage.CMMouseEnter(var Msg: TMessage);
begin
  if (fHoverBmp.Width > 0) and (fHoverBmp.Height > 0) then
    picture.Assign(fHoverBmp);
end;

procedure TXgImage.CMMouseLeave(var Msg: TMessage);
begin
  if (fHoverBmp.Width > 0) and (fHoverBmp.Height > 0) then
    picture.Assign(fBmp);
end;

constructor TXgImage.Create(Owner: TComponent);
begin
  inherited;
  fBmp:= TBitmap.Create;
  fHoverBmp:= TBitmap.Create;
end;

destructor TXgImage.Destroy;
begin
  fBmp.Free;
  fHoverBmp.Free;
  inherited;
end;

procedure TXgImage.setHoverSrc(const Value: string);
var
  S: string;
begin
  S:= value;
  if Owner.GetInterfaceEntry(StringToGUID('{01FB7AFE-C5E4-4597-9CD3-9BF9E1006E74}')) <> nil then
    S:= (Owner AS IXgOwner).getURIPath(Value);

  if fileExists(S) then
  begin
    fHoverSrc:= S;
    fHoverBmp.LoadFromFile(S);
  end;
end;

procedure TXgImage.setSrc(const Value: string);
var
  S: string;
begin
  S:= value;
  if Owner.GetInterfaceEntry(StringToGUID('{01FB7AFE-C5E4-4597-9CD3-9BF9E1006E74}')) <> nil then
    S:= (Owner AS IXgOwner).getURIPath(Value);

  if fileExists(S) then
  begin
    fSrc:= S;
    fBmp.LoadFromFile(S);
    Width:= fBmp.Width;
    Height:= fBmp.Height;
    Picture.Assign(fBmp);
  end;
end;

{ TXgCustomShape }

constructor TXgCustomShape.Create(Owner: TComponent);
begin
  inherited;
  fPen:= TPen.Create;
  fBrush:= TBrush.Create;
end;

destructor TXgCustomShape.Destroy;
begin
  fPen.Free;
  fBrush.Free;
  inherited;
end;

function TXgCustomShape.getFill: TColor;
begin
  Result:= fBrush.Color;
end;

function TXgCustomShape.getStroke: TColor;
begin
  Result:= fPen.Color;
end;

function TXgCustomShape.getStrokeWidth: integer;
begin
  Result:= fPen.Width;
end;

procedure TXgCustomShape.setFill(const Value: TColor);
begin
  fBrush.Color:= Value;
  if Value = clNone then
    fBrush.Style:= bsClear
  else
    fBrush.Style:= bsSolid;
end;

procedure TXgCustomShape.setStroke(const Value: TColor);
begin
  fPen.Color:= Value;
  if Value = clNone then
    fPen.Style:= psClear
  else
    fPen.Style:= psSolid;
end;

procedure TXgCustomShape.setStrokeWidth(const Value: integer);
begin
  fPen.Width:= Value;
end;

{ TXgRect }

function TXgRect.getX: integer;
begin
  Result:= Left;
end;

function TXgRect.getY: integer;
begin
  Result:= Top;
end;

procedure TXgRect.Paint;
begin
  inherited;
  With Canvas do
  begin
    Pen:= FPen;
    Brush:= FBrush;
    if (frx > 0) and (fry > 0) then
      RoundRect(0, 0, Width, height, frx, fry)
    else
      Rectangle(0, 0, Width, height);
  end;
end;

procedure TXgRect.setX(const Value: integer);
begin
  Left:= Value;
end;

procedure TXgRect.setY(const Value: integer);
begin
  Top:= Value;
end;

{ TXgEllipse }

procedure TXgEllipse.Paint;
begin
  inherited;
  Canvas.Pen:= fPen;
  Canvas.Brush:= fBrush;
  Canvas.Ellipse(0, 0, Width, Height);
end;

procedure TXgEllipse.setcx(const Value: integer);
begin
  fcx := Value;
  Left:= fcx - frx;
end;

procedure TXgEllipse.setcy(const Value: integer);
begin
  fcy := Value;
  Top:= fcy - fry;
end;

procedure TXgEllipse.setrx(const Value: integer);
begin
  frx := Value;
  Left:= fcx - frx;
  Width:= frx * 2;
end;

procedure TXgEllipse.setry(const Value: integer);
begin
  fry := Value;
  Top:= fcy - fry;
  Height:= fry * 2;
end;

{ TXgCircle }

function TXgCircle.getR: integer;
begin
  Result:= rx;
end;

procedure TXgCircle.setR(const Value: integer);
begin
  rx:= Value;
  ry:= Value;
end;

{ TXgLine }

procedure TXgLine.CalcSize;
begin
  Width:= Max(X1, X2) - Min(X1, X2);
  Height:= Max(Y1, Y2) - Min(Y1, Y2);
  Left:= Min(X1, X2);
  Top:= Min(Y1, Y2);

  if Width = 0 then
    Width:= fPen.Width;
  if height = 0 then
    height:= fPen.Width;
end;

procedure TXgLine.Paint;
begin
  inherited;
  With Canvas do
  begin
    Pen:= fPen;
    MoveTo(X1-Left, Y1-Top);
    LineTo(X2-Left, Y2-Top);
  end;
end;

procedure TXgLine.setX1(const Value: integer);
begin
  fX1 := Value;
  CalcSize;
end;

procedure TXgLine.setX2(const Value: integer);
begin
  fX2 := Value;
  CalcSize;
end;

procedure TXgLine.setY1(const Value: integer);
begin
  fY1 := Value;
  CalcSize;
end;

procedure TXgLine.setY2(const Value: integer);
begin
  fY2 := Value;
  CalcSize;
end;

end.


