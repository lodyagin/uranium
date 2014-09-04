Uranium (mostly) is a test framework written in SWI-Prolog.

Before using Uranium you should define the Uranium path 
in you .plrc like this:

```
:- dynamic user:file_search_path/2.
:- assertz(user:file_search_path(u,
          '/home/serg/uranium-test/U-238/u')).
```