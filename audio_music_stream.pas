program audio_music_stream;

{$MODE objfpc}

uses raylib, SysUtils;

const
  screenWidth = 800;
  screenHeight = 450;

var
  mus          : TMusic;
  timePlayed   : Single;
  pause        : Boolean;
begin
  InitWindow(screenWidth, screenHeight, StrToPChar('raylib [audio] example - music playing (streaming)'));
  InitAudioDevice();
  mus := LoadMusicStream(StrToPChar('res/music/guitar_noodling.ogg'));
  PlayMusicStream(mus);
  timePlayed := 0.0;
  pause := false;
  SetTargetFPS(60);
  while  not WindowShouldClose() do
  begin
    UpdateMusicStream(mus);
    if IsKeyPressed(KEY_SPACE) then
    begin
      StopMusicStream(mus);
      PlayMusicStream(mus);
    end;
    if IsKeyPressed(KEY_P) then
    begin
      pause := not pause;
      if pause then PauseMusicStream(mus)
      else ResumeMusicStream(mus);
    end;
    timePlayed := GetMusicTimePlayed(mus)/GetMusicTimeLength(mus)*400;
    BeginDrawing();
      ClearBackground(RAYWHITE);
      DrawText(StrToPChar('MUSIC SHOULD BE PLAYING!'), 255, 150, 20, LIGHTGRAY);
      DrawRectangle(200, 200, 400, 12, LIGHTGRAY);
      DrawRectangle(200, 200, Trunc(timePlayed), 12, MAROON);
      DrawRectangleLines(200, 200, 400, 12, GRAY);
      DrawText(StrToPChar('PRESS SPACE TO RESTART MUSIC'), 215, 250, 20, LIGHTGRAY);
      DrawText(StrToPChar('PRESS P TO PAUSE/RESUME MUSIC'), 208, 280, 20, LIGHTGRAY);
    EndDrawing();
  end;
  UnloadMusicStream(mus);
  CloseAudioDevice();
  CloseWindow();
end.
