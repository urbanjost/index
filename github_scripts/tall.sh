for NAME in M_calculator M_change M_CLI2 M_CLI M_color M_draw M_escape M_history M_intrinsics M_io M_kracken2015 M_kracken95 M_msg M_pixel M_process M_strings M_system M_time FUNIX
do
(
cd $NAME
banner $NAME
rm -rfv build
ffpm build
ffpm test
)
done
