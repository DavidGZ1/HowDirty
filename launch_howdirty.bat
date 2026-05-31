@echo off
setlocal

set IMAGE=ghcr.io/davidgz1/howdirty:app
set CONTAINER=howdirty_app
set PORT=3838
set URL=http://localhost:%PORT%

echo.
echo  ================================================
echo    HowDirty -- LC-MS Contamination Reporter
echo  ================================================
echo.

:: --- Check Docker is installed -----------------------------------------
where docker >nul 2>&1
if errorlevel 1 goto no_docker

:: --- Start Docker Desktop if daemon is not running ---------------------
docker info >nul 2>&1
if not errorlevel 1 goto docker_ready

echo  Docker Desktop is not running. Starting it...
if exist "%ProgramFiles%\Docker\Docker\Docker Desktop.exe" (
    start "" "%ProgramFiles%\Docker\Docker\Docker Desktop.exe"
    goto wait_for_docker
)
if exist "%LocalAppData%\Docker\Docker Desktop.exe" (
    start "" "%LocalAppData%\Docker\Docker Desktop.exe"
    goto wait_for_docker
)
echo  Could not locate Docker Desktop. Please start it manually and try again.
goto error_exit

:wait_for_docker
echo  Waiting for Docker to be ready (up to 90 seconds)...
:wait_docker
timeout /t 5 /nobreak >nul
docker info >nul 2>&1
if errorlevel 1 goto wait_docker
echo  Docker is ready.
echo.

:docker_ready
:: --- Stop any leftover HowDirty container ------------------------------
docker rm -f %CONTAINER% >nul 2>&1

:: --- Pull latest image (falls back to cache if offline) ----------------
echo  Checking for updates...
docker pull %IMAGE%
if errorlevel 1 echo  (Could not reach registry -- using cached image if available.)
echo.

:: --- Start container ---------------------------------------------------
echo  Starting HowDirty...
docker run --rm -d --name %CONTAINER% -p %PORT%:3838 %IMAGE% >nul
if errorlevel 1 goto run_error

:: --- Wait for Shiny and open browser -----------------------------------
echo  Waiting for the app to start...
timeout /t 5 /nobreak >nul
echo  Opening %URL% in your browser...
start "" %URL%
echo.
echo  ================================================
echo    HowDirty is running at %URL%
echo.
echo    Press any key to stop the app.
echo  ================================================
echo.
pause >nul

:: --- Shut down ---------------------------------------------------------
echo  Stopping HowDirty...
docker stop %CONTAINER% >nul 2>&1
echo  Done. Goodbye!
timeout /t 2 /nobreak >nul
goto end

:no_docker
echo  ERROR: Docker not found.
echo  Please install Docker Desktop from:
echo    https://www.docker.com/products/docker-desktop
echo  then try again.
goto error_exit

:run_error
echo  ERROR: Failed to start HowDirty.
echo  Make sure port %PORT% is not in use and the image has been
echo  downloaded at least once (requires an internet connection).

:error_exit
echo.
pause
exit /b 1

:end
endlocal
