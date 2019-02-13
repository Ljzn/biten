#! /bin/sh


POSITIONAL=()
NETWORK="mainnet"

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
    -n|--network)
    NETWORK="$2"
    shift # past argument
    shift # past value
    ;;
    # -s|--searchpath)
    # SEARCHPATH="$2"
    # shift # past argument
    # shift # past value
    # ;;
    # -l|--lib)
    # LIBPATH="$2"
    # shift # past argument
    # shift # past value
    # ;;
    # --default)
    # DEFAULT=YES
    # shift # past argument
    # ;;
    *)    # unknown option
    POSITIONAL+=("$1") # save it in an array for later
    shift # past argument
    ;;
esac
done
set -- "${POSITIONAL[@]}" # restore positional parameters

export BITEN_NETWORK="include/$NETWORK"

rebar3 compile && \

erl -pa _build/default/lib/*/ebin -boot start_sasl -s biten_app
