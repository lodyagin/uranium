%  This file is a part of Uranium, a general-purpose
%  functional test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
%
%  This library is free software; you can redistribute it
%  and/or modify it under the terms of the GNU Lesser
%  General Public License as published by the Free
%  Software Foundation; either version 2.1 of the License,
%  or (at your option) any later version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the
%  implied warranty of MERCHANTABILITY or FITNESS FOR A
%  PARTICULAR PURPOSE.  See the GNU Lesser General Public
%  License for more details.
%
%  You should have received a copy of the GNU Lesser
%  General Public License along with this library; if not,
%  write to the Free Software Foundation, Inc., 51
%  Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

%  This module describes db_properties_v - the object
%  which can turns various properties of an uranium db.

:- module(db_properties_v, []).

/** <module> db_properties_v

  It is an object with special meaning when put into Uranium
  DB. It is used for change per-DB key resolution policy.

  ---+++ Parent
  db_singleton_v.pl

  ---+++ New static fields
   * key_policy
   For possible key policy values see ../vd.pl.

  --
  
  The example of usage (change the per-DB policy to ignore)

  ==
  % Define DB key policy by the first time
  db_construct(my_db, db_properties_v, [key_policy], [ignore]).
  ==

  To change the policy you need use, for example, db_put_object/4
  with per-predicate overwrite policy because db_properties_v is
  a db_singleton_v descendant:

  ==
  % Change the existing DB key policy
  obj_construct(db_properties_v, [key_policy], [throw], Obj),
  db_put_object(my_db, overwrite, Obj, _).
  ==
*/

new_class(db_properties_v, db_singleton_v,
          [key_policy]).

