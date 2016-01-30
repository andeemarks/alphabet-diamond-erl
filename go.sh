#~/bin/bash

erl -pa _build/default/lib/alphadiamond/ebin/ -noshell -s alphadiamond_app diamond $1 -s init stop
