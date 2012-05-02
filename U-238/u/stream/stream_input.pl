:- module(stream_input,
          [stream_to_lazy_list/3
          ]).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/check_arg)).
:- use_module(v(db_auto_value_v)).
:- use_module(v(stream_buffer_v)).

stream_to_lazy_list(Stream_DB, Stream, List) :-

   init_stream_db(Stream_DB),
   get_buffer(Stream_DB, Stream, Buffer),
   freeze(List, read_input_stream_at(Buffer, 1, List)).

read_input_stream_at(Buffer, Packet_Num, List) :-

   get_list_at_position(Buffer, Packet_Num, List, Tail),
   % now freeze the tail
   succ(Packet_Num, Next_Packet_Num),
   freeze(Tail, read_input_stream_at(Buffer, Next_Packet_Num, Tail)).

get_buffer(Stream_DB, Stream, Buffer) :-

   (  named_args_unify(Stream_DB, stream_buffer_v,
                       [stream], [Stream], Buffer)
   -> true
   ;
      atom_concat(Stream_DB, '.packets', Packets_DB),
      obj_construct(stream_buffer_v,
                   [stream, packets_db],
                   [Stream, Packets_DB],
                   Buffer),
      db_bind_auto(Stream_DB, Buffer),
      db_put_object(Stream_DB, Buffer)
   ).

%read_input_stream_at(

init_stream_db(Stream_DB) :-

   Ctx = context(stream_to_lazy_list/3, _),
   check_db_key(Stream_DB, Ctx),

   (  db_name(Stream_DB) -> true
   ;  init_stream_db2(Stream_DB)
   ).

init_stream_db2(Stream_DB) :-

   db_construct(Stream_DB, db_auto_value_v,
                [class_name, field_name, next_seed_pred, auto_value_seed],
                [stream_buffer_v, stream_id, succ, 0]).


