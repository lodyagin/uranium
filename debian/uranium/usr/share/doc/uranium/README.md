Uranium is a prolog library originally created as
a test framework. 

Before using Uranium you should define the Uranium path 
in your .plrc like this:

```
:- dynamic user:file_search_path/2.
:- assertz(user:file_search_path(u,
          '/home/serg/uranium-test/U-238/u')).
```