{
  MIT License

  Copyright (c) 2018 Hélio S. Ribeiro and Anderson J. Gado da Silva

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}
unit DataGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types,
  Graphics,
  Controls;

type

  /// Forward declaration
  TCustomDataGrid = class;

  TColumnFormat = (cfBoolean, cfDataTime, cfNumber, cfString);

  { TDataColumn }

  TDataColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FColor: TColor;
    FDisplayMask: string;
    FFont: TFont;
    FFormat: TColumnFormat;
    FHint: string;
    FName: string;
    FTag: integer;
    FTitle: string;
    FUpdateCount: NativeInt;
    FValueChecked: string;
    FValueUnchecked: string;
    FVisible: boolean;
    FWidth: NativeInt;
    function GetGrid: TCustomDataGrid;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetColor(AValue: TColor);
    procedure SetDisplayMask(AValue: string);
    procedure SetFont(AValue: TFont);
    procedure SetFormat(AValue: TColumnFormat);
    procedure SetName(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetValueChecked(AValue: string);
    procedure SetValueUnchecked(AValue: string);
    procedure SetVisible(AValue: boolean);
    procedure SetWidth(AValue: NativeInt);
  protected
    procedure ColumnChanged; virtual;
    function GetDisplayName: string; override;
    procedure FillDefaultFont; virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function GetDefaultValueChecked: string; virtual;
    function GetDefaultValueUnchecked: string; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property Grid: TCustomDataGrid read GetGrid;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Color: TColor read FColor write SetColor;
    property DisplayMask: string read FDisplayMask write SetDisplayMask;
    property Font: TFont read FFont write SetFont;
    property Format: TColumnFormat read FFormat write SetFormat;
    property Hint: string read FHint write FHint;
    property Name: string read FName write SetName;
    property Tag: integer read FTag write FTag;
    property Title: string read FTitle write SetTitle;
    property ValueChecked: string read FValueChecked write SetValueChecked;
    property ValueUnchecked: string read FValueUnchecked write SetValueUnchecked;
    property Visible: boolean read FVisible write SetVisible;
    property Width: NativeInt read FWidth write SetWidth;
  end;

  { TDataColumns }

  TDataColumns = class(TCollection)
  private
    FGrid: TCustomDataGrid;
    function GetColumn(AIndex: NativeInt): TDataColumn;
    procedure SetColumn(AIndex: NativeInt; AValue: TDataColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(AItem: TCollectionItem); override;
  public
    constructor Create(AGrid: TCustomDataGrid); reintroduce;
    function Add: TDataColumn; reintroduce;
    function HasIndex(const AIndex: integer): boolean;
    property Grid: TCustomDataGrid read FGrid;
    property Items[AIndex: NativeInt]: TDataColumn read GetColumn write SetColumn; default;
  end;

  TSortOrder = (soAscending, soDescending);

  TOnClickEvent = procedure(ASender: TObject; ACol, ARow: NativeInt) of object;
  TOnHeaderClick = procedure(ASender: TObject; ACol: NativeInt) of object;

  { TCustomDataGrid }

  TCustomDataGrid = class(TCustomControl)
  private
    FAutoCreateColumns: boolean;
    FColumnClickSorts: boolean;
    FColumns: TDataColumns;
    FDefColWidth: NativeInt;
    FDefRowHeight: NativeInt;
    FShowHeader: boolean;
    FSortColumn: NativeInt;
    FSortOrder: TSortOrder;
    FOnCellClick: TOnClickEvent;
    FOnHeaderClick: TOnHeaderClick;
    procedure SetColumnClickSorts(AValue: boolean);
    procedure SetColumns(AValue: TDataColumns);
    procedure SetDefColWidth(AValue: NativeInt);
    procedure SetDefRowHeight(AValue: NativeInt);
    procedure SetShowHeader(AValue: boolean);
  protected
    procedure VisualChange; virtual;
    procedure ColumnsChanged({%H-}AColumn: TDataColumn); virtual;
    function CalcDefaultRowHeight: NativeInt; virtual;
    procedure Paint; override;
  protected
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoCreateColumns: boolean read FAutoCreateColumns write FAutoCreateColumns;
    property Columns: TDataColumns read FColumns write SetColumns;
    property ColumnClickSorts: boolean read FColumnClickSorts write SetColumnClickSorts;
    property DefaultColWidth: NativeInt read FDefColWidth write SetDefColWidth;
    property DefaultRowHeight: NativeInt read FDefRowHeight write SetDefRowHeight;
    property SortColumn: NativeInt read FSortColumn;
    property SortOrder: TSortOrder read FSortOrder;
    property ShowHeader: boolean read FShowHeader write SetShowHeader;
    property OnCellClick: TOnClickEvent read FOnCellClick write FOnCellClick;
    property OnHeaderClick: TOnHeaderClick read FOnHeaderClick write FOnHeaderClick;
  end;

  TOnPageEvent = procedure(ASender: TObject; APage: NativeInt) of object;

  { TCustomPagination }

  TCustomPagination = class(TCustomControl)
    { TODO: Add keys navigations }
  private
    FCurrentPage: NativeInt;
    FOnPageClick: TOnPageEvent;
    FRecordsPerPage: NativeInt;
    FTotalPages: NativeInt;
    FTotalRecords: NativeInt;
    procedure SetCurrentPage(AValue: NativeInt);
    procedure SetRecordsPerPage(AValue: NativeInt);
    procedure SetTotalRecords(AValue: NativeInt);
  protected
    procedure Changed; virtual;
    procedure CalculatePages; virtual;
    procedure Paint; override;
  protected
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property CurrentPage: NativeInt read FCurrentPage write SetCurrentPage;
    property RecordsPerPage: NativeInt read FRecordsPerPage write SetRecordsPerPage;
    property TotalPages: NativeInt read FTotalPages;
    property TotalRecords: NativeInt read FTotalRecords write SetTotalRecords;
    property OnPageClick: TOnPageEvent read FOnPageClick write FOnPageClick;
  end;

implementation

uses
  Math;

{ TDataColumn }

function TDataColumn.GetGrid: TCustomDataGrid;
begin
  if (Collection is TDataColumns) then
  begin
    Result := TDataColumns(Collection).Grid;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TDataColumn.SetAlignment(AValue: TAlignment);
begin
  if (FAlignment <> AValue) then
  begin
    FAlignment := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetColor(AValue: TColor);
begin
  if (FColor <> AValue) then
  begin
    FColor := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetDisplayMask(AValue: string);
begin
  if (FDisplayMask <> AValue) then
  begin
    FDisplayMask := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetFont(AValue: TFont);
begin
  if (not FFont.IsEqual(AValue)) then
  begin
    FFont.Assign(AValue);
  end;
end;

procedure TDataColumn.SetFormat(AValue: TColumnFormat);
begin
  if (FFormat <> AValue) then
  begin
    FFormat := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetName(AValue: string);
begin
  if (FName <> AValue) then
  begin
    FName := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetTitle(AValue: string);
begin
  if (FTitle <> AValue) then
  begin
    FTitle := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetValueChecked(AValue: string);
begin
  if (FValueChecked <> AValue) then
  begin
    FValueChecked := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetValueUnchecked(AValue: string);
begin
  if (FValueUnchecked <> AValue) then
  begin
    FValueUnchecked := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetVisible(AValue: boolean);
begin
  if (FVisible <> AValue) then
  begin
    FVisible := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.SetWidth(AValue: NativeInt);
begin
  if (FWidth <> AValue) then
  begin
    FWidth := AValue;
    ColumnChanged;
  end;
end;

procedure TDataColumn.ColumnChanged;
begin
  if (FUpdateCount = 0) then
  begin
    Changed(False);
  end;
end;

function TDataColumn.GetDisplayName: string;
begin
  if (FTitle <> '') then
  begin
    Result := FTitle;
  end
  else
  begin
    Result := 'Column ' + IntToStr(Index);
  end;
end;

procedure TDataColumn.FillDefaultFont;
begin
  if (Assigned(Grid)) then
  begin
    FFont.Assign(Grid.Font);
  end;
end;

procedure TDataColumn.FontChanged(Sender: TObject);
begin
  ColumnChanged;
end;

function TDataColumn.GetDefaultValueChecked: string;
begin
  Result := '1';
end;

function TDataColumn.GetDefaultValueUnchecked: string;
begin
  Result := '0';
end;

constructor TDataColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FAlignment := taLeftJustify;
  FColor := clWhite;
  FDisplayMask := '';
  FFormat := cfString;
  FHint := '';
  FName := '';
  FTag := 0;
  FTitle := '';
  FUpdateCount := 0;
  FValueChecked := GetDefaultValueChecked;
  FValueUnchecked := GetDefaultValueUnchecked;
  FVisible := True;
  FWidth := 0;
  FillDefaultFont;
end;

destructor TDataColumn.Destroy;
begin
  FFont.Destroy;
  FFont := nil;
  inherited Destroy;
end;

procedure TDataColumn.Assign(Source: TPersistent);
var
  VColumn: TDataColumn;
begin
  if (Assigned(Source)) and (Source is TDataColumn) then
  begin
    BeginUpdate;
    try
      VColumn := TDataColumn(Source);
      FAlignment := VColumn.Alignment;
      FColor := VColumn.Color;
      FDisplayMask := VColumn.DisplayMask;
      FFont.Assign(VColumn.FFont);
      FFormat := VColumn.Format;
      FHint := VColumn.Hint;
      FName := VColumn.Name;
      FTag := VColumn.Tag;
      FTitle := VColumn.Title;
      FValueChecked := VColumn.ValueChecked;
      FValueUnchecked := VColumn.ValueUnchecked;
      FVisible := VColumn.Visible;
      FWidth := VColumn.Width;
    finally
      EndUpdate;
    end;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

procedure TDataColumn.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDataColumn.EndUpdate;
begin
  if (FUpdateCount > 0) then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) then
    begin
      ColumnChanged;
    end;
  end;
end;

{ TDataColumns }

function TDataColumns.GetColumn(AIndex: NativeInt): TDataColumn;
begin
  Result := TDataColumn(inherited Items[AIndex]);
end;

procedure TDataColumns.SetColumn(AIndex: NativeInt; AValue: TDataColumn);
begin
  Items[AIndex].Assign(AValue);
end;

function TDataColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TDataColumns.Update(AItem: TCollectionItem);
begin
  FGrid.ColumnsChanged(TDataColumn(AItem));
end;

constructor TDataColumns.Create(AGrid: TCustomDataGrid);
begin
  inherited Create(TDataColumn);
  FGrid := AGrid;
end;

function TDataColumns.Add: TDataColumn;
begin
  Result := TDataColumn(inherited Add);
end;

function TDataColumns.HasIndex(const AIndex: integer): boolean;
begin
  Result := (Aindex > -1) and (AIndex < Count);
end;


{ TCustomDataGrid }

procedure TCustomDataGrid.SetColumnClickSorts(AValue: boolean);
begin
  if (FColumnClickSorts <> AValue) then
  begin
    FColumnClickSorts := AValue;
  end;
end;

procedure TCustomDataGrid.SetColumns(AValue: TDataColumns);
begin
  FColumns.Assign(AValue);
end;

procedure TCustomDataGrid.SetDefColWidth(AValue: NativeInt);
begin
  if (FDefColWidth <> AValue) then
  begin
    FDefColWidth := AValue;
  end;
end;

procedure TCustomDataGrid.SetDefRowHeight(AValue: NativeInt);
begin
  if (FDefRowHeight <> AValue) then
  begin
    FDefRowHeight := AValue;
  end;
end;

procedure TCustomDataGrid.SetShowHeader(AValue: boolean);
begin
  if (FShowHeader <> AValue) then
  begin
    FShowHeader := AValue;
  end;
end;

procedure TCustomDataGrid.VisualChange;
begin
  Invalidate;
end;

procedure TCustomDataGrid.ColumnsChanged(AColumn: TDataColumn);
begin
  if (csDestroying in ComponentState) then
  begin
    exit;
  end;
  VisualChange;
end;

function TCustomDataGrid.CalcDefaultRowHeight: NativeInt;
begin
  Result := Font.GetTextHeight('Fj') + 10;
end;

procedure TCustomDataGrid.Paint;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Color := Color;
    with Canvas do
    begin
      Pen.Style := psSolid;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(0, 0, Self.Width - 1, Self.Height - 1);
      Line(0, 0, Self.Width - 1, Self.Height - 1);
      Line(Self.Width - 1, 0, 0, Self.Height - 1);
    end;
    Exit;
  end;
end;

class function TCustomDataGrid.GetControlClassDefaultSize: TSize;
begin
  Result.Cx := 200;
  Result.Cy := 100;
end;

constructor TCustomDataGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
  end;
  FAutoCreateColumns := True;
  FColumns := TDataColumns.Create(Self);
  FColumnClickSorts := True;
  FDefColWidth := -1;
  FDefRowHeight := -1;
  FShowHeader := True;
end;

destructor TCustomDataGrid.Destroy;
begin
  FColumns.Destroy;
  FColumns := nil;
  inherited Destroy;
end;

{ TCustomPagination }

procedure TCustomPagination.SetCurrentPage(AValue: NativeInt);
begin
  if (FCurrentPage <> AValue) then
  begin
    FCurrentPage := AValue;
    Changed;
  end;
end;

procedure TCustomPagination.SetRecordsPerPage(AValue: NativeInt);
begin
  if (FRecordsPerPage <> AValue) then
  begin
    FRecordsPerPage := AValue;
    Changed;
  end;
end;

procedure TCustomPagination.SetTotalRecords(AValue: NativeInt);
begin
  if (FTotalRecords <> AValue) then
  begin
    FTotalRecords := AValue;
    Changed;
  end;
end;

procedure TCustomPagination.Changed;
begin
  CalculatePages;
end;

procedure TCustomPagination.CalculatePages;
begin
  FTotalPages := Ceil64(FTotalRecords / FRecordsPerPage);
end;

procedure TCustomPagination.Paint;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Color := Color;
    with Canvas do
    begin
      Pen.Style := psSolid;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(0, 0, Self.Width - 1, Self.Height - 1);
      Line(0, 0, Self.Width - 1, Self.Height - 1);
      Line(Self.Width - 1, 0, 0, Self.Height - 1);
    end;
    Exit;
  end;
end;

class function TCustomPagination.GetControlClassDefaultSize: TSize;
begin
  Result.Cx := 150;
  Result.Cy := 30;
end;

constructor TCustomPagination.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  with GetControlClassDefaultSize do
  begin
    SetInitialBounds(0, 0, CX, CY);
  end;
  FCurrentPage := 1;
  FRecordsPerPage := 10;
  FTotalPages := 0;
  FTotalRecords := 0;
end;

end.
