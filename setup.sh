#!/bin/bash
apt update
apt install g++ gfortran mpich freeglut3 freeglut3-dev cmake emacs25 libgsl0-dev libxmu-dev libxi-dev  libmpich-dev 
flag=1
lang=1
echo "\nChoose language to be used when installing the script: rus or eng (r or e)
Выберите язык, который будет использоваться при установке скрипта:  rus or eng (r or e)"
read lan
while [ $lang -eq 1 ]
do
if [ "$lan" = "r" -o "$lan" = "rus" ]; then
lang=0
echo "\nПользователи доступные в системе:" 
awk -F: '$3 >= 1000 && $1 != "nobody" {print $1}' /etc/passwd
echo "\nВведите имя пользователя, для которого необходимо установить систему?"
read usr
while [ $flag -eq 1 ]
do
grep "$usr:" /etc/passwd >/dev/null
if [ $? -ne 0 ];
then
echo "\nПользователь не найден. Установить для пользователья root? yes or no "
read cont
if [ "$cont" = "y" -o "$cont" = "yes" ]; then
user=$HOME
flag=0
else
echo "\nВведите заного имя пользователя"
read usr
flag=1
fi
else user=/home/$usr
flag=0
fi
done
elif [ "$lan" = "e" -o "$lan" = "eng" ]; then
lang=0
echo "\nUsers are available in the system:" 
awk -F: '$3 >= 1000 && $1 != "nobody" {print $1}' /etc/passwd
echo "\nEnter the username for which the system will be installed?"
read usr
while [ $flag -eq 1 ]
do
grep "$usr:" /etc/passwd >/dev/null
if [ $? -ne 0 ];
then
echo "\nUser is not found. Install for user root? yes or no "
read cont
if [ "$cont" = "y" -o "$cont" = "yes" ]; then
user=$HOME
flag=0
else
echo "\nEnter your username again"
read usr
flag=1
fi
else user=/home/$usr
flag=0
fi
done
else echo "The language is not correct, select again
Язык введен не верно, введите снова"
read lan
lang=1
fi
done

cp ./.emacs $user/
cp -R ./.emacs.d $user/
cd ./NumLib/Build/Release/
if [ -e CMakeCache.txt ]
then
rm CMakeCache.txt
fi
sh ./build.sh
mkdir -p $user/.local/share/Library/
cp ./inc/*.mod $user/.local/share/Library
cp ./lib/*.a $user/.local/share/Library
cd ../../../
cd Plot/
rm -rf ./build
mkdir -p build
cd build
cmake ..
make
cp -R ./inc/* $user/.local/share/Library/
cp ./lib/* $user/.local/share/Library/
cd ../../
grep "$usr:" /etc/passwd >/dev/null
if [ $? -eq 0 ];
then
chown $usr $user/.emacs
chown -R $usr $user/.emacs.d
chown -R $usr $user/.local/share/Library
fi

if [ "$lan" = "r" -o "$lan" = "rus" ]; then
echo "Система успешно установлена"
elif [ "$lan" = "e" -o "$lan" = "eng" ]; then
echo "System successfully installed"
fi
if ! grep -q 'export LIBRARY_PATH=$LIBRARY_PATH:"'$user'/.local/share/Library/"' $user/.bashrc;
then echo 'export LIBRARY_PATH=$LIBRARY_PATH:"'$user'/.local/share/Library/"' >> $user/.bashrc
fi

