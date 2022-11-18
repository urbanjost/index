#!/bin/bash
echo LIST
for NAME in fortran-intrinsic-manpages fpm fpm-registry FUNIX github_scripts M_calculator M_change M_CLI M_CLI2 M_color M_commandline M_draw M_history M_io M_kracken2015 M_kracken95 M_msg M_pixel M_process M_strings M_system M_time stdlib
do
   (
   exec 2>&1
   set +x
   cd $NAME
   echo "TESTING test --list in $NAME"
   ffpm test --list
   echo "TESTING run --list in $NAME"
   ffpm run --list
   )
done |tee $0.out

echo BUILD
for NAME in M_calculator M_change M_CLI2 M_CLI M_color M_draw M_escape M_history M_io M_kracken2015 M_kracken95 M_msg M_pixel M_process M_strings M_system M_time
do
   echo '>>>>>>' $NAME
   banner $NAME
   grep -i dependent $NAME/fpm.toml
   grep -i mk $NAME/fpm.toml
   (
      cd $NAME
      rm -rfv build
      ffpm build
      ffpm build -release
   )
done
