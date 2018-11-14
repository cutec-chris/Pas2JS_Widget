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
unit Pas2JS_IDE_Options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, EditBtn, StdCtrls;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    CompilerLabel: TLabel;
    CompilerEdit: TEditButton;    
    CompilerDialog: TOpenDialog; 
    BuilderLabel: TLabel;
    BuilderEdit: TEditButton;
    BuilderDialog: TOpenDialog;
    OutputLabel: TLabel;
    OutputEdit: TEditButton;   
    OuputDialog: TSelectDirectoryDialog;  
    TemplateLabel: TLabel;
    TemplateEdit: TEditButton;
    TemplateDialog: TOpenDialog;
    BrowserLabel1: TLabel;
    BrowserEdit: TEditButton;
    BrowseriDialog: TOpenDialog;
    CustomOptionsMemo: TMemo; 
    CustomOptionsLabel: TLabel;
    ButtonsPanel: TButtonPanel;         
    procedure BuilderEditButtonClick(Sender: TObject);   
    procedure CompilerEditButtonClick(Sender: TObject);  
    procedure OutputEditButtonClick(Sender: TObject);
    procedure BrowserEditButtonClick(Sender: TObject);
    procedure TemplateEditButtonClick(Sender: TObject);
  public
    procedure AfterConstruction; override;
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

uses
  LazUTF8;

{ TOptionsForm }

procedure TOptionsForm.BuilderEditButtonClick(Sender: TObject);
begin
  if (BuilderDialog.Execute) then
  begin
    BuilderEdit.Text := BuilderDialog.FileName;
  end;
end;         

procedure TOptionsForm.CompilerEditButtonClick(Sender: TObject);
begin       
  if (CompilerDialog.Execute) then
  begin
    CompilerEdit.Text := CompilerDialog.FileName;
    if (BuilderEdit.Text = '') then
    begin
      {$IfDef Windows}
      BuilderEdit.Text := UTF8Copy(CompilerEdit.Text, 1, UTF8Pos('.exe', CompilerEdit.Text)-1) + '_build.exe';
      {$EndIf}
      {$IfDef Linux}
      BuilderEdit.Text := CompilerEdit.Text + '_build';
      {$EndIf}
    end;
  end;
end;

procedure TOptionsForm.OutputEditButtonClick(Sender: TObject);
begin
  if (OuputDialog.Execute) then
  begin
    OutputEdit.Text := OuputDialog.FileName;
  end;
end;    

procedure TOptionsForm.BrowserEditButtonClick(Sender: TObject);
begin
  if (BrowseriDialog.Execute) then
  begin
    BrowserEdit.Text := BrowseriDialog.FileName;
  end;
end;

procedure TOptionsForm.TemplateEditButtonClick(Sender: TObject);
begin
  if (TemplateDialog.Execute) then
  begin
    TemplateEdit.Text := TemplateDialog.FileName;
  end;
end;

procedure TOptionsForm.AfterConstruction;
begin
  inherited AfterConstruction;
  {$IfDef Windows}  
  BrowserEdit.Text:= 'C:\Program Files\Mozilla Firefox\firefox.exe';
  {$EndIf}                    
  {$IfDef Linux}
  BrowserEdit.Text:= '/opt/google/chrome/google-chrome';   
  {$EndIf}
  CustomOptionsMemo.Append('-Jminclude');
  CustomOptionsMemo.Append('-O1');
end;

end.

