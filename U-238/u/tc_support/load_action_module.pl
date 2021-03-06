% -*- fill-column: 65; -*-
%
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

% Загрузка модуля в зависимости от конфигурации back end.
%

:- module(load_action_module,
          [load_action_module/3]).

:- module_transparent load_action_module/3.

load_action_module(Backend, Name, Module_Name) :-

    member(Place, [tc, u]),
    file_search_path(Place, Base_Path),
    concat_atom([Base_Path, '/action/', Backend, '/',
                 Backend, '_', Name, '.pl'],
                '', Module_Path),
    exists_file(Module_Path), !,

    use_module(Module_Path),

    concat_atom(List, '/', Module_Path),
    last(List, Name_With_Ext),
    atom_concat(Module_Name, '.pl', Name_With_Ext).


