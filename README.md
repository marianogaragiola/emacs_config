<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. emacs configuration</a>
<ul>
<li><a href="#sec-1-1">1.1. Installation</a>
<ul>
<li><a href="#sec-1-1-1">1.1.1. Known issues</a></li>
</ul>
</li>
</ul>
</li>
</ul>
</div>
</div>

# emacs configuration<a id="sec-1" name="sec-1"></a>

This is my emacs configuration

## Installation<a id="sec-1-1" name="sec-1-1"></a>

Back up you `\~/.emacs.d/` first:

    $ mv ~/.emacs.d ~/emacs_back_up

Clone the repository as the new `\~/.emacs.d/`:

    $ git clone https://github.com/marianogaragiola/emacs_config.git ~/.emacs.d

Open `emacs` and wait until the installation is finished.

### Known issues<a id="sec-1-1-1" name="sec-1-1-1"></a>

If you get and error with an *Expired ELPA GPG Keys*, you can fix it with:

    $ gpg --homedir ~/.emacs.d/elpa/gnupg --receive-keys 066DAFCB81E42C40

Alternatively you can modify the expiration date of the old key with something like:

    $ gpg --homedir ~/.emacs.d/elpa/gnupg --quick-set-expire 474F05837FBDEF9B 1y
