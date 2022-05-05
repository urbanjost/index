#!/bin/bash
(
echo '|Repository|Issues|Stars|Build FORD(1) docs|ubuntu with intel|ubuntu with gfortran|macos with gfortran|windows with gfortran|windows with mingw64 gfortran|windows with msys gfortran|'
echo '| -------- | ---- | --- | ---------------- | --------------- | ------------------ | ------------------ | ------------------- | --------------------------- | ----------------------- |'
for NAME in \
general-purpose-fortran \
prep \
fortran-intrinsic-descriptions \
M_anything \
M_args \
M_attr \
M_blas \
M_calcomp \
M_calculator \
M_CLI \
M_CLI2 \
M_color \
M_display \
M_draw \
M_escape \
M_factor \
M_graph \
M_hashkeys \
M_history \
M_intrinsics \
M_io \
M_kracken \
M_kracken95 \
M_LA \
M_list \
M_match \
M_matrix \
M_msg \
M_ncurses \
M_OS \
M_overload \
M_path \
M_pixel \
M_process \
M_random \
M_readline \
M_slices \
M_sort \
M_stopwatch \
M_strings \
M_system \
M_time \
M_uuid \
orderpack \
easy \
fpm-man \
fpm-search \
fpm-tools \
paranoia \
plugins \
scripts \
index \
$NULL
do
echo "\
|[$NAME](http://github.com/urbanjost/$NAME)|\
[![GitHub issues open](https://img.shields.io/github/issues/urbanjost/$NAME.svg?maxAge=2)](https://github.com/urbanjost/$NAME/issues)|\
[![GitHub stars](https://img.shields.io/github/stars/urbanjost/$NAME.svg)](https://urbanjost.github.io/$NAME/man3.html)\
| [![Build FORD(1) docs]\
   (https://github.com/urbanjost/$NAME/actions/workflows/deploy_api_docs.yml/badge.svg)\
   ](https://github.com/urbanjost/$NAME/actions/workflows/deploy_api_docs.yml)\
| [![test ubuntu with intel]\
   (https://github.com/urbanjost/$NAME/actions/workflows/test_intel_ubuntu.yml/badge.svg)\
   ](https://github.com/urbanjost/$NAME/actions/workflows/test_intel_ubuntu.yml)\
| [![test ubuntu with gfortran]\
   (https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_ubuntu.yml/badge.svg)\
   ](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_ubuntu.yml)\
| [![test macos with gfortran]\
   (https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_macos.yml/badge.svg)\
   ](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_macos.yml)\
| [![test windows with gfortran]\
   (https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_windows.yml/badge.svg)\
   ](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_windows.yml)\
| [![test windows with mingw64 gfortran]\
   (https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_mingw64_windows.yml/badge.svg)\
   ](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_mingw64_windows.yml)\
| [![test windows with msys gfortran]\
   (https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_msys_windows.yml/badge.svg)\
   ](https://github.com/urbanjost/$NAME/actions/workflows/test_gfortran_msys_windows.yml)\
|"
done
cat <<\EOF
## See Also:
```bash
w3m -dump 'https://api.github.com/users/urbanjost/repos?per_page=1000'
```
EOF
) >ISSUES.md
git add ISSUES.md ISSUES.sh
git commit -m 'refresh ISSUES'
git push
exit
