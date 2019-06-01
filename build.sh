# Build script

elm make src/Main.elm --optimize --output=out/elm.js
cp static/index.html out/index.html