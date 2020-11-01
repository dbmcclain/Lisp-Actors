
A little background about this project...

I come from Ground-based Observational Astronomoy, many decades
ago. My first "real" language was Forth. We used Forth to control the
world's first completely computer-controlled telescope, atop Mt. Jelm,
located about 25 miles SouthWest of Laramie, Wyoming, USA.

From there I migrated to the then-largest telescope in the world on
Mt. Hopkins, about 60 km south of Tucson, AZ, USA. At the time, the
MMT (Multiple Mirror Telescope) had 6 x 72in mirrors, all operating
toward a common focus. We used Forth to run that beast too. I
deveveloped MMTO SuperForth for that purpose. My job was to teach that
monster to point and track stars to astonishing accuracy, using a
Nova-800 computer.

Forth, in those early days, had a characteristic of about 10 minutes
MTR (Mean Time between Reboots). It was so easy to crash the
machines. My goal was to make a Forth system that could withstand
operator mistakes and live for days on end without rebooting. And I
succeeded.

That was all back in the late '70s and early '80s. Fast forward
another decade, a number of other languages learned, and some graduate
CompSci, and I immersed myself into Common Lisp, at first with
Allegro, then Harlequin Common Lisp.

That is where I have remained, mostly, for the past 30 years.

This Actors project was an outgrowth of earlier work, called
Butterfly, whose aim was to control various pieces of laboratory gear
for making precision frequency measurements between 1-30 MHz. It is
laboratory distributed computing with realtime instrumentation
control, data acquisition, and data reduction.

Common Lisp is the glue that binds all of these together, better than
any mishmash of individual tools that are most commonly seen in such
applications. But Common Lisp is just modeling clay. It needs
paradigms to wrap around, and Actors has emerged over time as a very
useful method of performing distributed computing.

Actors gives the programming the sense of single-threaded semantics,
regardless of how many CPU cores are all operating concurrently. Safer
programming without running into a blizzard of Locking/Mutexes.

So the code you see here is actual running code, used daily in my
laboratory. It is evolving over time as I learn more. But the present
code base has the benefit of having been hammered into shape over 3
years of field use.

It is still evolving, but the core is quite stable. Actors have fully
replaced my earlier Butterfly network, as being more capable, smaller,
more efficient, and more easily understood. The codebase is still
evolving around the edges.

And now with that description of where I came from, and what I use
them for, you can perhaps appreciate the lack of polish evident in the
code.

- DM 10/20
