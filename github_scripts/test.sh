#!/bin/bash
(
exec 2>&1
set -x -v
ffpm          
ffpm  help    
ffpm  --help  
ffpm  fpm     --help
ffpm  build   --help
ffpm  run     --help
ffpm  test    --help
ffpm  help    --help
ffpm  list    --help

ffpm  help    fpm
ffpm  help    build
ffpm  help    run
ffpm  help    test
ffpm  help    help
ffpm  help    list

ffpm  help    manual
ffpm  help    tan

(
ffpm new BB
cd BB
ffpm build
ffpm build --list
ffpm build --release
ffpm build --release --list
)

ffpm new
ffpm new no-no
ffpm new A
ffpm new B --lib
ffpm new C --app
ffpm new D --test
ffpm new E --lib --test 
ffpm new F --lib --app
ffpm new G --test --app
ffpm new B
ffpm new B --backfill

ffpm list
ffpm --list

(
cd A
ffpm run 
ffpm --release run
ffpm run --list
ffpm run main
)

(
cd A
ffpm test 
ffpm --release test
ffpm test --list
)

)|tee $0.log
