docs/index.html: elm-stuff
	elm make --output=$@ Main.elm

elm-stuff:
	elm install
