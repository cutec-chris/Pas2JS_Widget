program pas2js_build;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, CompWriterJS
  { you can add units after this };

type

  { TPas2JSBuild }

  TPas2JSBuild = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TPas2JSBuild }

procedure TPas2JSBuild.DoRun;
var
  ErrorMsg: String;
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

  { convert lfm }

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

