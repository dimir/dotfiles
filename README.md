```
mkdir -p ~/git
cd !$
git clone https://github.com/dimir/dotfiles.git
color_prompt=yes . ~/git/dotfiles/.ps1
cd
for i in .emacs.el .screenrc .bash_aliases; do ln -s ~/git/dotfiles/$i; done
```
