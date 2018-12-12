program pas2js_build;

{$mode objfpc}{$H+}

uses
  Interfaces,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, CompWriterJS, fpjson, jsonparser,LResources,WebCtrls, process
  { you can add units after this };

type

  { TPas2JSBuild }

  TPas2JSBuild = class(TCustomApplication)
  private
    procedure FindComponentClass(Reader: TReader; const aClassName: string;
      var ComponentClass: TComponentClass);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConvertLFM(aName : string);
    procedure WriteHelp; virtual;
  end;

{ TPas2JSBuild }

procedure TPas2JSBuild.FindComponentClass(Reader: TReader;
  const aClassName: string; var ComponentClass: TComponentClass);
begin
  if copy(AClassName,0,6)= 'TWForm' then
    ComponentClass := TWForm
  else if copy(AClassName,0,7)= 'TWFrame' then
    ComponentClass := TWFrame
  else if copy(AClassName,0,11)= 'TDataModule' then
    ComponentClass := TWDataModule
  else if CompareText(AClassName, 'TWButton') = 0 then
    ComponentClass := TWButton
  else if CompareText(AClassName, 'TWMemo') = 0 then
    ComponentClass := TWMemo
  else if CompareText(AClassName, 'TWPanel') = 0 then
    ComponentClass := TWPanel
  else if CompareText(AClassName, 'TWDataGrid') = 0 then
    ComponentClass := TWDataGrid
  else if CompareText(AClassName, 'TWPagination') = 0 then
    ComponentClass := TWPagination
  ;
end;

procedure TPas2JSBuild.DoRun;
var
  ErrorMsg,aStr: String;
  aFile, sl: TStringList;
  aParser: TJSONParser;
  aOptions: TJSONObject;
  searchResult: TRawByteSearchRec;
  aProc: TProcess;
  i: Integer;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { read config }

  if not FileExists(ParamStr(ParamCount)) then
    raise(Exception.Create('ERROR: Project File not Found !'));
  aFile := TStringList.Create;
  aFile.LoadFromFile(ParamStr(ParamCount));
  while aFile.Count>0 do
    if aFile[0]<>'{@PAS2JS_BEGIN}' then
      aFile.Delete(0)
    else
      begin
        aFile.Delete(0);
        break;
      end;
  while aFile.Count>0 do
    if aFile[aFile.Count-1]<>'{@PAS2JS_END}' then
      aFile.Delete(aFile.Count-1)
    else
      begin
        aFile.Delete(aFile.Count-1);
        break;
      end;
  aParser := TJSONParser.Create(aFile.Text);
  aOptions := aParser.Parse as TJSONObject;

  { convert lfm }

  //TODO: parse Project Units and not convert only Units from Project Dir
  if findfirst(ExtractFileDir(ParamStr(ParamCount))+DirectorySeparator+'*.lfm', faAnyFile, searchResult) = 0 then
  begin
    repeat
      ConvertLFM(ExtractFileDir(ParamStr(ParamCount))+DirectorySeparator+searchResult.Name);
    until FindNext(searchResult) <> 0;
    FindClose(searchResult);
  end;

  aProc := TProcess.Create(nil);
  aProc.CommandLine:='pas2js';
  for i := 0 to aOptions.Arrays['CustomOptions'].Count-1 do
    aProc.CommandLine:=aProc.CommandLine+' '+aOptions.Arrays['CustomOptions'].Strings[i];
  aProc.CommandLine:=aProc.CommandLine+' '+ParamStr(ParamCount);
  aProc.Options:=aProc.Options+[poWaitOnExit,poUsePipes];
  aProc.Execute;
  sl := TStringList.Create;
  sl.LoadFromStream(AProc.Output);
  aProc.Free;
  for i := 0 to sl.Count-1 do writeln(sl[i]);
  // stop program loop
  Terminate;
end;

constructor TPas2JSBuild.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TPas2JSBuild.Destroy;
begin
  inherited Destroy;
end;

procedure TPas2JSBuild.ConvertLFM(aName: string);
var
  aComponent : TComponent;
  aStream, bStream: TFileStream;
begin
  writeln('INFO: Converting "'+aName+'"');
  aStream := TFileStream.Create(aName,fmOpenRead);
  ReadComponentFromTextStream(aStream,aComponent,@FindComponentClass);
  aStream.Free;
  bStream := TFileStream.Create(ChangeFileExt(aName,'.wfm'),fmCreate);
  WriteComponentToJSStream(aComponent,bStream);
  bStream.Free;
end;

procedure TPas2JSBuild.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TPas2JSBuild;
begin
  Application:=TPas2JSBuild.Create(nil);
  Application.Title:='Pas2JSBuild';
  Application.Run;
  Application.Free;
end.

