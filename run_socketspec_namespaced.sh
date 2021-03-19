#/bin/sh

cabal build socketspec && unshare -rn ./socketspec.sh
