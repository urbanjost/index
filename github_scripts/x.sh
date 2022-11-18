for NAME in alphabet anagrams area_of_simple_polygon asa2pdf base change colors compute cprint days2sec degrees digest dtu esc exchange factors ffmt findll fseq gcd hasher huegif lcm lsup magic_square makeout manup match mat month note now numdiff paws pendulum penv planets quadratic rep reverse sec2days sha3 shell slice splitname table2html tabulate target today topic triangle ttee ufpp what xauth_key
do
   mkdir ../app/$NAME
   mv $NAME.f90 ../app/$NAME
   echo '[[executable]]  name="'$NAME'"     source-dir="app/'$NAME'"     main="'$NAME'.f90"'
done
