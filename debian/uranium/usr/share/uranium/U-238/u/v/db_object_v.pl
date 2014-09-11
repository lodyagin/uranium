%  -*-fill-column: 65-*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2011, Sergei Lodyagin
%  Copyright (C) 2012, Kogorta OOO Ltd
%
%  This library is free software; you can redistribute it and/or
%  modify it under the terms of the GNU Lesser General Public
%  License as published by the Free Software Foundation; either
%  version 2.1 of the License, or (at your option) any later
%  version.
%
%  This library is distributed in the hope that it will be
%  useful, but WITHOUT ANY WARRANTY; without even the implied
%  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
%  PURPOSE.  See the GNU Lesser General Public License for more
%  details.

%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(db_object_v, []).

/** <module> db_object_v

  It is a special parent object for using with Uranium object database (see
  vd.pl). Normally any object which is not db_object_v descendant
  is rebased to it by the expression =|object_v -> db_object_v|=
  (see obj_rebase/3). Objects which are used only with DB can be
  directly inherited from _db_object_v_.

  ---++ Parent

  object_v.pl

  ---++ New static fields

   $ db_key : See vd.pl, DB_Key.

   $ db_ref : A "physical" DB address, it is opaque. All Uranium
   DB implementations must guarantee different (db_key, db_ref)
   for any object stored in Uranium object database.

  ---++ Key
  No key.
*/

new_class(db_object_v, object_v,
          [db_key, % uniq identify the db
           db_ref  % "physical" address in db, it is opaque
           ]).


