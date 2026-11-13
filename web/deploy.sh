#!/bin/bash
# The built page (./build.sh) to poetry.awx.at: tatooine:~/docker/poetry/dist, served by the
# nginx container there (compose and nginx.conf beside it; pob.awx.at's nginx sends a login's
# callback on). Characters are never uploaded: the site reads them with the visitor's login.
set -e
here="$(cd "$(dirname "$0")" && pwd)"
rsync -a --delete --exclude builds --info=stats1 "$here/dist/" tatooine:docker/poetry/dist/
