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
unit NumCtrls;

{$mode objfpc}{$H+}

interface
uses
  Classes,
  SysUtils,
  Graphics,
  Controls,
  StdCtrls;

type

  { TCustomNumericEdit }

  TCustomNumericEdit = class(TCustomEdit)
    { TODO: Max Min value }
    { TODO: Add spin }
  private
    FDecimals: NativeInt;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: char); override;
  public
    constructor Create(AOwner: TComponent); override;
    property DecimalPlaces: NativeInt read FDecimals write FDecimals;
  end;

implementation

{ TCustomNumericEdit }

procedure TCustomNumericEdit.DoEnter;
begin
  inherited DoEnter;
  RealSetText(RealGetText);
end;

procedure TCustomNumericEdit.DoExit;
begin
  inherited DoExit;
  RealSetText(RealGetText);
end;

procedure TCustomNumericEdit.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if (Key = DefaultFormatSettings.DecimalSeparator) then
  begin
    if (FDecimals = 0) then
    begin
      Key := #0;
    end;
    if (Pos(Key, RealGetText) > 0) then
    begin
      Key := #0;
    end;
  end;
  if (not (Key in ['0'..'9', DefaultFormatSettings.DecimalSeparator])) then
  begin
    Key:= #0;
  end;
end;

constructor TCustomNumericEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Alignment := taRightJustify;
  FDecimals := 2;
end;

end.
