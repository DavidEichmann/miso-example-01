* Build with nix-build
* Run server with ./result/bin/server
* Run client with ./result/bin/client
* Open http://localhost:8080 in chrome
  * open the debug console with Ctl+Shift+I and go to the Consol tab
  * Click Login. Either:
    * JS gets stuck in an infinite loop and the websocket connection gets closed. The console shows an error "jsaddle.js:9 WebSocket connection to 'ws://localhost:8080/' failed: Invalid frame header"
    * OR no error, so reload and try hit Login again.