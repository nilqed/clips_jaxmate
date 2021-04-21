#!/bin/bash
echo "Press CTRL-C to stop the server"
sensible-browser http://localhost:3010 & 
node clips_server.js