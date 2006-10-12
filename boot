PYTHONPATH=.:$PYTHONPATH
export PYTHONPATH
echo "Booting Felix"
FLX_LPARCHIVE=${FLX_LPARCHIVE:-.}
PREFIX=$PREFIX
ARGS=""

grab=1
while  [ "$grab" -eq 1 ]
do
  case x$1 in
  x--prefix=*)
     #PREFIX="`echo \"$1\" | sed 's/--prefix=\(.*\)/\\1/'`"
     PREFIX=${1:9}
     shift
  ;;

  x--prefix)
    shift
    PREFIX="$1"
    shift
  ;;

  x--lparchive=*)
   FLX_LPARCHIVE=${1:12}
   shift
  ;;

  x--lparchive)
   shift
   FLX_LPARCHIVE="$1"
   shift
  ;;

  x)
    grab=0
  ;;

  x*)
    ARGS="${ARGS} $1"
    shift
  ;;

  esac
done

echo FLX_LPARCHIVE at $FLX_LPARCHIVE

rm -f $FLX_LPARCHIVE/lpsrc/*.cache
python interscript/bin/iscr.py --break-on-error $FLX_LPARCHIVE/lpsrc/flx_config.pak
if [ $? != 0 ]
then
  echo "ERROR EXTRACTING CONFIGURATION PROGRAM"
  exit 1
fi

python interscript/bin/iscr.py --break-on-error $FLX_LPARCHIVE/lpsrc/flx_maker.pak
if [ $? != 0 ]
then
  echo "ERROR EXTRACTING MAKER PROGRAM"
  exit 1
fi

echo PLEASE RUN ./configure  && ./mk extract && ./mk

