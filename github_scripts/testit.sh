for NAME in funix-fpm M_change M_CLI2 M_CLI M_color M_draw M_escape M_history M_intrinsics M_io M_kracken2015 M_kracken95 M_msg M_pixel M_process
do
(
   cd $NAME
   #ffpm build
   ffpm run
   ffpm test
)
done
exit

funix-fpm
M_calculator . OK. Need to add tests
M_change
M_CLI2
M_CLI
M_color
M_draw
M_escape
M_history
M_intrinsics
M_io
M_kracken2015
M_kracken95
M_msg
M_pixel
M_process
