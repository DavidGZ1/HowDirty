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
if errorlevel 1 (
    echo  ERROR: Docker not found.
    echo  Please install Docker Desktop from https://www.docker.com/products/docker-desktop
    echo  then try again.
    echo.
    pause
    exit /b 1
)

:: --- Start Docker Desktop if daemon is not running ---------------------
docker info >nul 2>&1
if errorlevel 1 (
    echo  Docker Desktop is not running. Starting it...
    if exist "%ProgramFiles%\Docker\Docker\Docker Desktop.exe" (
        start "" "%ProgramFiles%\Docker\Docker\Docker Desktop.exe"
    ) else if exist "%LocalAppData%\Docker\Docker Desktop.exe" (
        start "" "%LocalAppData%\Docker\Docker Desktop.exe"
    ) else (
        echo  Could not locate Docker Desktop. Please start it manually and try again.
        echo.
        pause
        exit /b 1
    )
    echo  Waiting for Docker to be ready (up to 90 seconds)...
    :wait_docker
    timeout /t 5 /nobreak >nul
    docker info >nul 2>&1
    if errorlevel 1 goto wait_docker
    echo  Docker is ready.
    echo.
)

:: --- Stop any leftover HowDirty container ------------------------------
docker rm -f %CONTAINER% >nul 2>&1

:: --- Pull latest image (graceful: if offline, cached image is used) ---
echo  Checking for updates...
docker pull %IMAGE% 2>nul
if errorlevel 1 (
    echo  (Could not reach registry -- using cached image if available.)
)
echo.

:: --- Start container ---------------------------------------------------
echo  Starting HowDirty...
docker run --rm -d --name %CONTAINER% -p %PORT%:3838 %IMAGE% >nul
if errorlevel 1 (
    echo.
    echo  ERROR: Failed to start HowDirty.
    echo  Make sure port %PORT% is not in use and the image was downloaded at least once.
    echo.
    pause
    exit /b 1
)

:: --- Wait for Shiny to be ready ----------------------------------------
timeout /t 5 /nobreak >nul

:: --- Open browser -------------------------------------------------------
echo  Opening %URL% in your browser...
start "" %URL%
echo.
echo  ================================================
echo    HowDirty is running at %URL%
echo.
echo    Press any key here to stop the app.
echo  ================================================
echo.
pause >nul

:: --- Shut down ----------------------------------------------------------
echo  Stopping HowDirty...
docker stop %CONTAINER% >nul 2>&1
echo  Done. Goodbye!
timeout /t 2 /nobreak >nul
endlocal
