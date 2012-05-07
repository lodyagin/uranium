% -*- fill-column: 65; -*-
%
%  This file is a part of Uranium, a general-purpose functional
%  test platform.
%
%  Copyright (C) 2009-2011, Sergei Lodyagin
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
%
%  You should have received a copy of the GNU Lesser General
%  Public License along with this library; if not, write to the
%  Free Software Foundation, Inc., 51 Franklin Street, Fifth
%  Floor, Boston, MA 02110-1301 USA
%
%  e-mail: lodyagin@gmail.com
%  post:   49017 Ukraine, Dnepropetrovsk per. Kamenski, 6

:- module(stream_buffer_v,
          [get_list_at_position/4,
           stream_close/1
           ]).

/** <module> Object for use streams with lazy lists.

  It buffers input packet and give a possibility of returning to
  any previous packet.

  --++ Object
  stream_buffer_v
  
  ---+++ Parent
  object_v

  ---+++ New static fields
  
   * stream_id
   It is autofilled by stream_to_lazy_list/5 with a help of
   v(db_auto_value). stream_to_lazy_list/5 always put the stream
   objects into DB defined as the first argument of the predicate.

  * stream
  Stream pair.

  * packets_db
  Name of DB for storing red packets. It is filled by
  stream_to_lazy_list/5.

  ---+++ Key
  stream

  ---++ Object
  stream_buffer_packet_v

  ---+++ Parent
  db_object_v

  ---+++ New static fields
  * stream_id
  Id of the stream (see stream_buffer_v).

  * packet_num
  1-based incremetal number of the packet.

  * diff_list_head, diff_list_tail
  Difference list with codes red from the stream starting from
  this packets. Usually it is a *|lazy list|*. It is possible to
  read whole stream starting from this place.
    
  @see ../stream_input.pl
*/

:- use_module(u(v)).
:- use_module(u(vd)).

new_class(stream_buffer_v, object_v,
          [stream_id,
           stream,
           packets_db
           %position_packet_map
           ],
          [stream]
         ).

new_class(stream_buffer_packet_v, db_object_v,
          [stream_id,
           packet_num,
           diff_list_head,
           diff_list_tail
          ],
          [stream_id, packet_num]
          ).

get_list_at_position(Buffer, Packet_Num, List0, List) :-

   obj_unify(Buffer,
             [stream_id, stream, packets_db],
             [Stream_Id, Stream, Packet_DB]),

   (  named_args_unify(Packet_DB, _,
                      [stream_id, packet_num,
                       diff_list_head, diff_list_tail],
                       [Stream_Id, Packet_Num,
                        List0, List],
                       Packet),
      obj_same_or_descendant(Packet, stream_buffer_packet_v)
      
   -> true
   ;  get_data(Stream, List0, List),
      obj_construct(stream_buffer_packet_v,
                    [stream_id, packet_num,
                     diff_list_head, diff_list_tail],
                    [Stream_Id, Packet_Num,
                     List0, List], New_Packet),
      store_packet(Packet_DB, New_Packet)
   ).

get_data(Stream, List0, List) :-

   (  at_end_of_stream(Stream)
   -> close(Stream),
      List0 = [], List = List0
   ;
      wait_for_input([Stream], [Stream], infinite),
      % timeout should be set on the stream itself as a property

      (  at_end_of_stream(Stream)
      -> close(Stream),
         List0 = [], List = List0
      ;  read_pending_input(Stream, List0, List)
      )
   ).

store_packet(Packet_DB, Packet) :-

   obj_unify(Packet,
             [stream_id, packet_num, diff_list_head],
             [Stream_Id, Packet_Num, List]),
   
   succ(Pred_Packet_Num, Packet_Num),
   refresh_packets(Packet_DB, Stream_Id, Pred_Packet_Num, List),
   db_put_object(Packet_DB, Packet).
      

refresh_packets(_, _, 0, _) :- !.

refresh_packets(Packet_DB, Stream_Id, Packet_Num, Tail) :-

   % TODO optimize with usage of db_ref instead of packet_num
   named_args_unify(Packet_DB, stream_buffer_packet_v,
                    [stream_id, packet_num,
                     diff_list_head, diff_list_tail],
                    [Stream_Id, Packet_Num,
                     List, Tail],
                    Packet), !,
   db_put_object(Packet_DB, throw, Packet, _, replaced),

   succ(Pred_Packet_Num, Packet_Num),
   refresh_packets(Packet_DB, Stream_Id, Pred_Packet_Num, List).

stream_close(Stream_Obj) :-

   obj_field(Stream_Obj, stream, Stream),
   close(Stream).


