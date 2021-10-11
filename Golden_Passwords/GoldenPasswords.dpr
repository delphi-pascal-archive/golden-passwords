program GoldenPasswords;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  LEA in 'LEA.pas',
  SEA in 'SEA.pas',
  Cryptosystem in 'Cryptosystem.pas',
  About in 'About.pas' {AboutBox},
  EditPassword in 'EditPassword.pas' {EditForm},
  SearchUnit in 'SearchUnit.pas' {SearchForm},
  TrayIcon in 'TrayIcon.pas';

{$R *.res}
{$R WINDOWSXP.res}

begin
  Application.Initialize;
  Application.Title := 'Golden Passwords';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.Run;
end.
