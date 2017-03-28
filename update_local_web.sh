
rm -f web/*
rm -f palm_web/*
mkdir -p palm_web
stack build && stack exec apostovia
