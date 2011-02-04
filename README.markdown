MochiWeb setup
=============================

First retrieve the Mochiweb source code

    cd dev
    git clone https://github.com/mochi/mochiweb.git

Then generate our 'mochusi' sample application using the mochiweb's script, quick and easy:

	cd mochiweb
    make app PROJECT=mochusi PREFIX=~/Projects/

Let's build it! and run it:

    cd ~/Project/mochusi
    make
    ./start-dev.sh

Open the browser on localhost:8080 and here it is!

	
mbtree.erl
=============================

Simple and basic in-memory b+-tree implementation.
Developped using a strong tdd approach.


