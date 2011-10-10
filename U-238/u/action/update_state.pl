%% This file is a part of Uranium, a general-purpose functional test platform.
%% Copyright (C) 2011  Sergei Lodyagin
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
%%
%% e-mail: lodyagin@gmail.com
%% post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6
%% -------------------------------------------------------------------------------
%%

:- module(update_state, [update_state/4]).

:- use_module(u(v)).
:- use_module(u(ur_recorded_db)).

%
% update_state(+Old_State, -New_State)
%

update_state(SAT, Actor, Old_State, New_State) :-

  obj_field(Old_State, update_pred, Update_Pred_List),
  memberchk(SAT = Update_Pred, Update_Pred_List),
  DB1_Key = 'update_state.1',
  DB2_Key = 'update_state.2',
  clear_db(DB1_Key),
  clear_db(DB2_Key),
  get_key(Old_State, Key),
  named_args_unify(Old_State, Key, Key_Value), !,
  ground(Key_Value),
  functor(Old_State, Class, _),
  obj_construct(Class, Key, Key_Value, Key_Object),
  db_recordz(DB1_Key, Key_Object),
  call(Update_Pred, DB1_Key, DB2_Key, Actor, Old_State),
  db_merge(DB1_Key, DB2_Key),
  db_recorded(DB1_Key, New_State), !.
