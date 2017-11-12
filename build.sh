SOURCE=src/JsonEditorApp.elm
DEST=dist

echo "Building $SOURCE..."

elm-make $SOURCE --yes --output=$DEST/elm.readable.js &> /dev/null

echo "Raw build size:"
wc -c $DEST/elm.readable.js

echo ""
echo "Pruning and minifying code..."

uglifyjs \
       	$DEST/elm.readable.js \
       	-mc 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9",pure_getters=true,keep_fargs=false,unsafe_comps=true' \
	2> /dev/null \
	1> $DEST/elm.min.js

echo "Minified build size:"
wc -c $DEST/elm.min.js

echo ""
echo "Deflating (gzip)..."

gzip -c $DEST/elm.min.js > $DEST/elm.js

echo "Compressed size:"
wc -c $DEST/elm.js

rm dist/elm.readable.js
rm dist/elm.min.js

cp index.html dist/index.html
