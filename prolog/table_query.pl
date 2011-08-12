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

:- module(table_query,
          [ select_one_column/4,
            filter_by_column_value/5
          ]).

% like arg but with different order
arg_acb(A, C, B) :- arg(A, B, C).

%% Filter table (-New_Table_Data) by column value
filter_by_column_value(Table_Header, 
                       Table_Data,
                       Col_Name,
                       Col_Value,
                       New_Table_Data
                       ) :-
    
    arg(Col_Num, Table_Header, Col_Name), !, 
    include(arg_acb(Col_Num, Col_Value), 
            Table_Data,
            New_Table_Data
            ).


%% Return value of this column in all rows as a list
select_one_column(Table_Header, Table_Data, Col_Name, Value_List):-

    arg(Col_Num, Table_Header, Col_Name), !, 
    maplist(arg(Col_Num), Table_Data, Value_List).

