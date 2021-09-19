# Werewolf

Werewolf is a helper tool for moderating groups playing werewolf. It should work as a flexible notepad
that helps the moderator keep track of role assignments and choices made at night.

I hope to include some automated suggestions, but the interface should not force any rules on the moderator.

The strings are currently in german and will remain german until I implement propper translations.

## Development

This tool does not use a server and is written as an [Elm](https://elm-lang.org) single page app.

For development I use the [Elm Live](https://github.com/wking-io/elm-live) development server:

    elm-live src/Main.elm --start-page=index.html --dir=static -- --output=static/elm.js

When you update the types file, make sure to run the encoder / decoder generator:

    npx elm-auto-encoder-decoder src/Types.elm
