#!/bin/bash
(
echo '|Repository|Issues|Stars|'
echo '| -------- | ---- | --- |'
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
echo "
|[$NAME](http://github.com/urbanjost/$NAME)|\
[![GitHub issues open](https://img.shields.io/github/issues/urbanjost/$NAME.svg?maxAge=2)](https://github.com/urbanjost/$NAME/issues)|\
[![GitHub stars](https://img.shields.io/github/stars/urbanjost/$NAME.svg)](https://github.com/urbanjost/$NAME/docs/man3.html)\
|
"
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
