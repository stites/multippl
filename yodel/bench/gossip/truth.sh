#!/usr/bin/env bash
GDIR=${PWD##*/}
FORCE=0
NUM_NODES=${PWD##*/}
NUM_NODES=${NUM_NODES#g}
CACHE_DIR=./tmp/
CACHE_FILE=${CACHE_DIR}truth.log

while [[ "$1" =~ ^- && ! "$1" == "--" ]]; do case $1 in
  -f | --force )
    FORCE=1
    ;;
esac; shift; done
if [[ "$1" == '--' ]]; then shift; fi


if [ "${GDIR:0:1}" == "g" ] &&
     (  [ "${GDIR##g}" == "4" ] ||
        [ "${GDIR##g}" == "10" ] ||
        [ "${GDIR##g}" == "20" ] )
then

  if [ $FORCE == 1 ] || [ ! -f ../truth ]; then
    ghc -O3 ../truth.hs
    rm ../truth.{o,hi}
  fi

  test ! -d $CACHE_DIR && mkdir -p $CACHE_DIR

  if [ $FORCE == 1 ] || [ ! -f $CACHE_FILE ]; then
    AVG=0
    for i in 5 6 7 8; do
      STEP_TRUTH=$(../truth "$NUM_NODES" "$i" | tee "$CACHE_DIR/truth-$NUM_NODES-$i.log")
      STEP_TRUTH=${STEP_TRUTH##*= }
      AVG="$( bc <<< "scale=5;$AVG + $STEP_TRUTH" )"
    done
    (bc <<< "scale=5;$AVG / 4") > $CACHE_FILE
  fi

  cat $CACHE_FILE


else
  echo "not in a gossip experiment folder, aborting!"
  echo "needs to be run an a folder g4 g10 or g20"
  echo "${GDIR##g}"
  exit 1
fi
