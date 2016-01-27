#~/bin/bash

erl -pa ebin/ -noshell -s alphadiamond_app diamond $1 -s init stop
