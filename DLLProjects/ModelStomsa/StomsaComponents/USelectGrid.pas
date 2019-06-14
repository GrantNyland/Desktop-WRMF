unit USelectGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Grids, Vcl.ImgList;

//{$R *.RES}

type
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TCellState = (csChecked,csUnchecked,csComplete);
  TCellType = (ctCheckable,ctCompleteIndicator,ctBlank,ctNormal);

  //need to create as a default of csUnchecked, ctNormal
  {$M+}
  TCellParameter = class(TObject)
  private
    FCellState : TCellState;
    FCellType : TCellType;
  public
    constructor Create;
  published
    property CellState: TCellState read FCellState write FCellState;
    property CellType: TCellType read FCellType write FCellType;
  end;//TCellParameter

  TSelectGrid = class(TStringGrid)
  private
    FSelectImage : TImageList;
  protected
    property SelectImage: TImageList read FSelectImage write FSelectImage;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;  
  public
    constructor Create(AOwner : TComponent); override;

    procedure SetCellParameters(ACol, ARow : Integer; ACellState : TCellState; ACellType : TCellType);
    procedure SetCellState(ACol, ARow : Integer; ACellState : TCellState);

    function GetCellState(ACol, ARow : Integer) : TCellState;
  published
    //
  end;//TSelectGrid

procedure Register;

implementation

{ TSelectGrid }

constructor TSelectGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectImage := TImageList.Create(Self);
  Options := [goFixedVertLine,goFixedHorzLine];
  FSelectImage.Height := 13;
  FSelectImage.Width := 13;
//  FSelectImage.ResourceLoad(rtBitmap,'SELECTGRID',clWhite);   Commented out because could not find RES file.
end;

procedure TSelectGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var
  ImageRect : TRect;
begin
  Inherited DrawCell(ACol,ARow,ARect,AState);
  if NOT(gdFixed in AState) then//do nothing for fixed cells?
  begin
    if (Objects[ACol,ARow] = nil) then
      Objects[ACol,ARow] := TCellParameter.Create;

    case (Objects[ACol,ARow] as TCellParameter).CellType of
      ctCompleteIndicator,
      ctCheckable : begin
                      self.Canvas.Brush.Color := self.Color;
                      self.Canvas.FillRect(ARect);

                      ImageRect.Left := 0;
                      ImageRect.Top := 0;
                      ImageRect.Right := FSelectImage.Width - 1;
                      ImageRect.Bottom := FSelectImage.Height - 1;

                      with ARect do
                      begin
                        Left := Round(Left + (Right - Left)/2 - (FSelectImage.Width/2));
                        Right := Left + FSelectImage.Width;
                        Top := Round(Top + (Bottom - Top)/2 - (FSelectImage.Height/2));
                        Bottom := Top + FSelectImage.Height;
                      end;

                      if (Objects[ACol,ARow] as TCellParameter).CellType = ctCheckable then
                      begin
                        if GetCellState(ACol,ARow) = csChecked then
                          FSelectImage.Draw(self.Canvas,ARect.Left,ARect.Top,1)
                        else
                          FSelectImage.Draw(self.Canvas,ARect.Left,ARect.Top,0)
                      end
                      else
                      begin
                        if GetCellState(ACol,ARow) = csComplete then
                          FSelectImage.Draw(self.Canvas,ARect.Left,ARect.Top,2);
                      end;
                    end;
      ctBlank     : begin
                      Canvas.Brush.Color := FixedColor;
                      Canvas.FillRect(ARect);
                    end;
    end;//case
  end;
end;

function TSelectGrid.GetCellState(ACol, ARow: Integer): TCellState;
begin
  if Objects[ACol,ARow] = nil then
    SetCellParameters(ACol,ARow,csUnchecked,ctNormal);
  Result := (Objects[ACol,ARow] as TCellParameter).CellState;
end;

function TSelectGrid.SelectCell(ACol, ARow: Integer): Boolean;

  procedure SetCellValue(TheCol, TheRow : Integer);
  begin
    if GetCellState(TheCol,TheRow) = csChecked then
      SetCellState(TheCol,TheRow,csUnchecked)
    else
      SetCellState(TheCol,TheRow,csChecked);
  end;

begin
  Result := inherited SelectCell(ACol,ARow);
  
  //if these are fixed cells take no action
  if Objects[ACol,ARow] = nil then
    SetCellParameters(ACol,ARow,csUnchecked,ctNormal);

  if (Objects[ACol,ARow] as TCellParameter).CellType = ctCheckable then
    SetCellValue(ACol,ARow);
end;

procedure TSelectGrid.SetCellParameters(ACol, ARow: Integer;
  ACellState: TCellState; ACellType: TCellType);
begin
  //if no object then create and set, otherwise adjust
  if Objects[ACol,ARow] = nil then
    Objects[ACol,ARow] := TCellParameter.Create;
  (Objects[ACol,ARow] as TCellParameter).CellState := ACellState;
  (Objects[ACol,ARow] as TCellParameter).CellType := ACellType;

  InvalidateCell(ACol,ARow);
end;

procedure TSelectGrid.SetCellState(ACol, ARow: Integer;
  ACellState: TCellState);
begin
  if Objects[ACol,ARow] = nil then
    Objects[ACol,ARow] := TCellParameter.Create;
  (Objects[ACol,ARow] as TCellParameter).CellState := ACellState;

  InvalidateCell(ACol,ARow);
end;

{ TCellParameter }

constructor TCellParameter.Create;
begin
  inherited;
  FCellState := csUnchecked;
  FCellType := ctNormal;
end;

procedure Register;
begin
  RegisterComponents('Stomsa', [TSelectGrid]);
end;

end.
