for NAME in M_commandline M_ncurses M_calculator M_change M_CLI M_color M_draw M_history M_io M_kracken95 M_msg M_pixel M_process M_strings M_system M_time M_escape M_CLI2
do
(
cd $NAME
echo $NAME
git log --pretty='format:%Creset%H %s' --no-merges master -n 1
)|xargs
#vi fpm.toml
done
exit
https://github.com/fortran-lang/fpm-registry
