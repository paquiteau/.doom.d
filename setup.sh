cd /tmp
curl -o "hunspell-en-custom.zip" 'http://app.aspell.net/create?max_size=80&spelling=GBs&spelling=US&max_variant=0&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=hunspell'
unzip "hunspell-en-custom.zip"

sudo chown root:root en-custom.*
sudo mv en-custom.{aff,dic} /usr/share/myspell/

cd /tmp
curl -o "aspell6-en-custom.tar.bz2" 'http://app.aspell.net/create?max_size=80&spelling=GBs&spelling=US&max_variant=0&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=aspell'
tar -xjf "aspell6-en-custom.tar.bz2"

cd aspell6-en-custom
./configure && make && sudo make install
