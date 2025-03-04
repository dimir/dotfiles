```
cd
wget https://github.com/dimir/dotfiles/archive/refs/heads/master.zip
unzip master.zip
rm master.zip
color_prompt=yes . dotfiles-master/.ps1
for i in .emacs.el .screenrc .bash_aliases .ps1; do ln -s ~/dotfiles-master/$i; done
```
