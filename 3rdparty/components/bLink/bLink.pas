unit bLink;

interface


uses
 Windows,  Messages, Classes, Graphics, Controls, Forms, StdCtrls, SysUtils,
  ShellAPI;

type
  TbLink = class(TLabel)
  private
    FFocusFont, FSaveFont: TFont;
    FOnMouseEnter, FOnMouseLeave: TNotifyEvent;

    procedure SetFocusFont(Value: TFont);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MouseEnter;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MouseLeave;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetNewFont(Value: TFont);
  published
    property FocusFont: TFont read FFocusFont write SetFocusFont;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

procedure Register;

implementation

constructor TbLink.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFocusFont := TFont.Create;
  FSaveFont := TFont.Create;
  FSaveFont.Assign(Font);
  Cursor:= crHandPoint;
end;

destructor TbLink.Destroy;
begin
  FSaveFont.Free;
  FFocusFont.Free;
  inherited Destroy;
end;

procedure TbLink.CMMouseEnter(var Msg: TMessage);
begin
  if csDesigning in ComponentState then Exit;
  if FSaveFont <> Font then FSaveFont.Assign(Font);
  Font.Assign(FFocusFont);
  if Assigned(FOnMouseEnter) then OnMouseEnter(Self);
end;

procedure TbLink.CMMouseLeave(var Msg: TMessage);
begin
  if csDesigning in ComponentState then Exit;
  Font.Assign(FSaveFont);
  if Assigned(FOnMouseLeave) then OnMouseLeave(Self);
end;

procedure TbLink.SetFocusFont(Value: TFont);
begin
  FFocusFont.Assign(Value);
end;

procedure TbLink.SetNewFont(Value: TFont);
begin
  Font:= Value; 
  FSaveFont.Assign(Value);
end;

procedure Register;
begin
  RegisterComponents('Samples', [TbLink]);
end;

end.