:- module(stream_buffer_v,
          [get_list_at_position/4,
           stream_close/1
           ]).

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
           %stream_position,
           %packet_size,
           diff_list_head,
           diff_list_tail
          ],
          [stream_id, packet_num]
          ).

get_list_at_position(Buffer, Packet_Num, List0, List) :-

   obj_unify(Buffer,
             [stream_id, stream, packets_db],
             [Stream_Id, Stream, Packet_DB]),

   (  db_select(Packet_DB,
                [stream_id, packet_num,
                 diff_list_head, diff_list_tail],
                [Stream_Id, Packet_Num,
                 List0, List])
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


