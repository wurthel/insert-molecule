#!/bin/bash


filename='N'
zamm=1
while read p; do
    echo $p > C_$zamm
    zamm=$(($zamm+1))
done < $filename

prosa=""
for ((jok=1; jok <= zamm-1; jok++)) do
    tropp=$(cat C_$jok)
    fropp=$(($tropp + 1))
    cropp=$(($tropp - 1))
    fropp1=$(($tropp + 2))
    cropp1=$(($tropp + 2))
    prosa="$tropp $fropp $cropp $fropp1 $cropp1 $prosa"
done
echo $prosa > finale
#sed -i "s/and$//" finale
resnum=$(cat finale)
rm C_* finale

sed "s/NNN/$resnum/g" fixate_ret.pgn > fixate_h.pgn
tail -1 1M0L.pdb > hitme
awk '{print $3}' hitme >  hitme1
base=$(cat hitme1)
natom=8000
caatom=8001
catom=8002
oatom=8003
cbatom=8004
cgatom=8005
cdatom=8006
ceatom=8007
nzatom=8008
c15=8009
c14=8010
c13=8011
c20=8012
c12=8013
c11atom=8014
c10=8015
c9=8016
c19=8017
c8=8018
c7=8019
c6=8020
c5=8021
c18=8022
c4=8023
c3=8024
c2=8025
c1atom=8026
c17=8027
c16=8028
case $base in 
    "CB") 
        echo "CB is our case"
        rename=RDZ
    ;;
    "CG")
	echo "CG is our case"
        rename=RCZ
    ;;
    "CD")
	echo "CD is our case"
	rename=RBZ
    ;;
    "CE")
	echo "CE is our case"
	rename=RAZ
    ;;
    "NZ")
        echo "NZ is our case"
        rename=REZ
    ;;
    "C15")
        echo "C15 is our case"
        rename=REA
    ;;
    "C14")
        echo "C14 is our case"
        rename=REB
    ;;
    "C13")
        echo "C13 is our case"
        rename=REC
    ;;
    "C20")
        echo "C20 is our case"
        rename=RED
    ;;
    "C12") 
        echo "C12 is our case"
        rename=REE
    ;;
    "C11")
        echo "C11 is our case"
        rename=REF
    ;;
    "C10")
        echo "C10 is our case"
        rename=REG
    ;;
    "C9")
        echo "C9 is our case"
        rename=REH
    ;;
    "C19")
        echo "C19 is our case"
        rename=REI
    ;;
    "C8")
        echo "C8 is our case"
        rename=REJ
    ;;
    "C7") 
        echo "C7 is our case"
        rename=REK
    ;;
    "C6")
        echo "C6 is our case"
        rename=REM
    ;;
    "C5") 
        echo "C5 is our case"
        rename=REN
    ;;
    "C18")
        echo "C18 is our case"
        rename=REO
    ;;
    "C4")
        echo "C4 is our case"
        rename=REP
    ;;
    "C3")
        echo "C3 is our case"
        rename=REQ
    ;;
    "C2")
        echo "C2 is our case"
        rename=RER
    ;;
    "C1")
        echo "C1 is our case"
        rename=RES
    ;;
    "C17")
        echo "C17 is our case"
        rename=REU
    ;;
    "C16")
        echo "C16 is our case, the retinal is finally inserted"
        rename=REL
    ;;
    *)
        echo "Something got completely wrong!!!"
esac

sed "s/REL/${rename}/g" 1M0L.pdb > 1M0L_a.pdb
vmd -dispdev text -e create.pgn
vmd -dispdev text -e fixate_h.pgn
vmd -dispdev text -e size.pgn > size.log
awk 'c&&!--c; /atomselect0/{c=1}' size.log > A
sed 's/{//g; s/}//g' A > AA
awk 'c&&!--c; /atomselect0/{c=2}' size.log > B
awk '{print $1}' AA > X1
awk '{print $4}' AA > X2
x1=$(cat X1)
x2=$(cat X2)
dx=$(awk "BEGIN {print $x2 - $x1 + 0.5; exit}")
ddx=$(awk -v v="$dx" 'BEGIN{printf "%d", v}')
Dx=$(awk "BEGIN {print $ddx + 1.2 ; exit}")
awk '{print $2}' AA > Y1
awk '{print $5}' AA > Y2
y1=$(cat Y1)
y2=$(cat Y2)
dy=$(awk "BEGIN {print $y2 - $y1 + 0.5; exit}")
ddy=$(awk -v v="$dy" 'BEGIN{printf "%d", v}')
Dy=$(awk "BEGIN {print $ddy + 1.2 ; exit}")
awk '{print $3}' AA > Z1
awk '{print $6}' AA > Z2
z1=$(cat Z1)
z2=$(cat Z2)
dz=$(awk "BEGIN {print $z2 - $z1 + 0.5; exit}")
ddz=$(awk -v v="$dz" 'BEGIN{printf "%d", v}')
Dz=$(awk "BEGIN {print $ddz + 1.2 ; exit}")
awk '{print $1}' B > C1
awk '{print $2}' B > C2
awk '{print $3}' B > C3
q1=$(cat C1)
q2=$(cat C2)
q3=$(cat C3)
sed "s/xxxx/${Dx}/; s/yyyy/${Dy}/; s/zzzz/${Dz}/; s/aaaa/${q1}/; s/bbbb/${q2}/; s/cccc/${q3}/"  m1.conf > m11.conf
rm A AA B C1 C2 C3 X1 X2 Z1 Z2 Y1 Y2

/home/wurthel-linux/Desktop/NAMD_2.13_Linux-x86_64-multicore/namd2 +p6 m11.conf > m1.log

vmd -dispdev text -e mout.pgn
cp 1M0L_0.pdb 1M0L_A.pdb

rm 1M0L_min-0.* 
rm hitme hitme1 hitme2
rm m1.log 

grep "N   $rename" 1M0L_0.pdb > N-atom
grep "CA  $rename" 1M0L_0.pdb > CA-atom
grep "C   $rename" 1M0L_0.pdb > C-atom
grep "OT1 $rename" 1M0L_0.pdb > O-atom
grep "CB  $rename" 1M0L_0.pdb > CB-atom
grep "CG  $rename" 1M0L_0.pdb > CG-atom
grep "CD  $rename" 1M0L_0.pdb > CD-atom
grep "CE  $rename" 1M0L_0.pdb > CE-atom
grep "NZ  $rename" 1M0L_0.pdb > NZ-atom 
grep "C15 $rename" 1M0L_0.pdb > C15-atom
grep "C14 $rename" 1M0L_0.pdb > C14-atom
grep "C13 $rename" 1M0L_0.pdb > C13-atom
grep "C20 $rename" 1M0L_0.pdb > C20-atom
grep "C12 $rename" 1M0L_0.pdb > C12-atom
grep "C11 $rename" 1M0L_0.pdb > C11-atom
grep "C10 $rename" 1M0L_0.pdb > C10-atom
grep "C9  $rename" 1M0L_0.pdb > C9-atom
grep "C19 $rename" 1M0L_0.pdb > C19-atom
grep "C8  $rename" 1M0L_0.pdb > C8-atom
grep "C7  $rename" 1M0L_0.pdb > C7-atom
grep "C6  $rename" 1M0L_0.pdb > C6-atom
grep "C5  $rename" 1M0L_0.pdb > C5-atom
grep "C18 $rename" 1M0L_0.pdb > C18-atom
grep "C4  $rename" 1M0L_0.pdb > C4-atom
grep "C3  $rename" 1M0L_0.pdb > C3-atom
grep "C2  $rename" 1M0L_0.pdb > C2-atom
grep "C1  $rename" 1M0L_0.pdb > C1-atom
grep "C17 $rename" 1M0L_0.pdb > C17-atom
grep "C16 $rename" 1M0L_0.pdb > C16-atom

sed -i "/ $rename/d" 1M0L_0.pdb
sed -i "/END/d" 1M0L_0.pdb

awk '{print $2}' N-atom > N-atom-num
awk '{print $2}' CA-atom > CA-atom-num
awk '{print $2}' C-atom > C-atom-num
awk '{print $2}' O-atom > O-atom-num
awk '{print $2}' CB-atom >  CB-atom-num
awk '{print $2}' CG-atom >  CG-atom-num
awk '{print $2}' CD-atom >  CD-atom-num
awk '{print $2}' CE-atom >  CE-atom-num
awk '{print $2}' NZ-atom >  NZ-atom-num
awk '{print $2}' C15-atom > C15-atom-num
awk '{print $2}' C14-atom > C14-atom-num
awk '{print $2}' C13-atom > C13-atom-num
awk '{print $2}' C20-atom > C20-atom-num
awk '{print $2}' C12-atom > C12-atom-num
awk '{print $2}' C11-atom > C11-atom-num
awk '{print $2}' C10-atom > C10-atom-num
awk '{print $2}' C9-atom  > C9-atom-num 
awk '{print $2}' C19-atom > C19-atom-num
awk '{print $2}' C8-atom  > C8-atom-num 
awk '{print $2}' C7-atom  > C7-atom-num 
awk '{print $2}' C6-atom  > C6-atom-num 
awk '{print $2}' C5-atom  > C5-atom-num 
awk '{print $2}' C18-atom > C18-atom-num
awk '{print $2}' C4-atom  > C4-atom-num 
awk '{print $2}' C3-atom  > C3-atom-num 
awk '{print $2}' C2-atom  > C2-atom-num 
awk '{print $2}' C1-atom  > C1-atom-num 
awk '{print $2}' C17-atom > C17-atom-num
awk '{print $2}' C16-atom > C16-atom-num

n1=$(cat N-atom-num)
ca1=$(cat CA-atom-num)
c1=$(cat C-atom-num)
o1=$(cat O-atom-num)
cb1=$(cat CB-atom-num)
cg1=$(cat CG-atom-num)
cd1=$(cat CD-atom-num)
ce1=$(cat CE-atom-num)
nz1=$(cat NZ-atom-num)
c151=$(cat C15-atom-num)
c141=$(cat C14-atom-num)
c131=$(cat C13-atom-num)
c201=$(cat C20-atom-num)
c121=$(cat C12-atom-num)
c111=$(cat C11-atom-num)
c101=$(cat C10-atom-num)
c91=$(cat C9-atom-num)
c191=$(cat C19-atom-num)
c81=$(cat C8-atom-num)
c71=$(cat C7-atom-num)
c61=$(cat C6-atom-num)
c51=$(cat C5-atom-num)
c181=$(cat C18-atom-num)
c41=$(cat C4-atom-num)
c31=$(cat C3-atom-num)
c21=$(cat C2-atom-num)
c11=$(cat C1-atom-num)
c171=$(cat C17-atom-num)
c161=$(cat C16-atom-num)

sed -i "s/$n1/$natom/g" N-atom
sed -i "s/$ca1/$caatom/g" CA-atom
sed -i "s/$c1/$catom/g" C-atom
sed -i "s/$o1/$oatom/g" O-atom
sed -i "s/OT1/O  /g" O-atom
sed -i "s/$cb1/$cbatom/g" CB-atom
sed -i "s/$cg1/$cgatom/g" CG-atom
sed -i "s/$cd1/$cdatom/g" CD-atom
sed -i "s/$ce1/$ceatom/g" CE-atom
sed -i "s/$nz1/$nzatom/g" NZ-atom
sed -i "s/$c151/$c15/g" C15-atom
sed -i "s/$c141/$c14/g" C14-atom
sed -i "s/$c131/$c13/g" C13-atom
sed -i "s/$c201/$c20/g" C20-atom
sed -i "s/$c121/$c12/g" C12-atom
sed -i "s/$c111/$c11atom/g" C11-atom
sed -i "s/$c101/$c10/g" C10-atom
sed -i "s/$c91/$c9/g" C9-atom
sed -i "s/$c191/$c19/g" C19-atom
sed -i "s/$c81/$c8/g" C8-atom
sed -i "s/$c71/$c7/g" C7-atom
sed -i "s/$c61/$c6/g" C6-atom
sed -i "s/$c51/$c5/g" C5-atom
sed -i "s/$c181/$c18/g" C18-atom
sed -i "s/$c41/$c4/g" C4-atom
sed -i "s/$c31/$c3/g" C3-atom
sed -i "s/$c21/$c2/g" C2-atom
sed -i "s/$c11/$c1atom/g" C1-atom
sed -i "s/$c171/$c17/g" C17-atom
sed -i "s/$c161/$c16/g" C16-atom

cat N-atom >> 1M0L_0.pdb
cat CA-atom >> 1M0L_0.pdb
cat C-atom >> 1M0L_0.pdb
cat O-atom >> 1M0L_0.pdb
cat CB-atom >> 1M0L_0.pdb
cat CG-atom >> 1M0L_0.pdb
cat CD-atom >> 1M0L_0.pdb
cat CE-atom >> 1M0L_0.pdb
cat NZ-atom >> 1M0L_0.pdb
cat C15-atom >> 1M0L_0.pdb
cat C14-atom >> 1M0L_0.pdb
cat C13-atom >> 1M0L_0.pdb
cat C20-atom >> 1M0L_0.pdb
cat C12-atom >> 1M0L_0.pdb
cat C11-atom >> 1M0L_0.pdb
cat C10-atom >> 1M0L_0.pdb
cat C9-atom >> 1M0L_0.pdb
cat C19-atom >> 1M0L_0.pdb
cat C8-atom >> 1M0L_0.pdb
cat C7-atom >> 1M0L_0.pdb
cat C6-atom >> 1M0L_0.pdb
cat C5-atom >> 1M0L_0.pdb
cat C18-atom >> 1M0L_0.pdb
cat C4-atom >> 1M0L_0.pdb
cat C3-atom >> 1M0L_0.pdb
cat C2-atom >> 1M0L_0.pdb
cat C1-atom >> 1M0L_0.pdb
cat C17-atom >> 1M0L_0.pdb
cat C16-atom >> 1M0L_0.pdb

sed -i "s/$rename/REL/g" 1M0L_0.pdb

rm *-atom*


