:- module(stream_input,
          [stream_to_lazy_list/5
          ]).

:- use_module(u(v)).
:- use_module(u(vd)).
:- use_module(u(internal/check_arg)).
:- use_module(v(db_auto_value_v)).
:- use_module(v(stream_buffer_v)).

:- reexport(v(stream_buffer_v), [stream_close/1]).

%% stream_to_lazy_list(+Stream_DB, +Stream, ?Buffer_Class, -Buffer, -List)
stream_to_lazy_list(Stream_DB, Stream, Buffer_Class, Buffer, List) :-

   Ctx = context(stream_to_lazy_list/5, _),
   (  nonvar(Buffer_Class) -> true
   ; Buffer_Class = stream_buffer_v
   ),
   check_existing_class_arg(Buffer_Class, Ctx),
   
   init_stream_db(Stream_DB),
   get_buffer(Stream_DB, Stream, Buffer_Class, Buffer),
   freeze(List, read_input_stream_at(Buffer, 1, List)).

read_input_stream_at(Buffer, Packet_Num, List) :-

   get_list_at_position(Buffer, Packet_Num, List, Tail),
   % now freeze the tail
   succ(Packet_Num, Next_Packet_Num),
   freeze(Tail, read_input_stream_at(Buffer, Next_Packet_Num, Tail)).

get_buffer(Stream_DB, Stream, Buffer_Class, Buffer) :-

   (  named_args_unify(Stream_DB, Buffer_Class,
                       [stream], [Stream], Buffer)
   -> true
   ;
      atom_concat(Stream_DB, '.packets', Packets_DB),
      obj_construct(Buffer_Class,
                   [stream, packets_db],
                   [Stream, Packets_DB],
                   Buffer0),
      db_bind_auto(Stream_DB, Buffer0),
      db_put_object(Stream_DB, Buffer0, Buffer)
   ).

init_stream_db(Stream_DB) :-

   Ctx = context(stream_to_lazy_list/3, _),
   check_db_key(Stream_DB, Ctx),

   Class_Name = stream_buffer_v,
   Field_Name = stream_id,
   (  named_args_unify(Stream_DB, db_auto_value_v,
                       [class_name, field_name],
                       [Class_Name, Field_Name], _)
   -> true
   ;  db_construct(Stream_DB, db_auto_value_v,
                   [class_name, field_name,
                    next_seed_pred, auto_value_seed],
                   [Class_Name, Field_Name,
                    succ, 0])
   ).


   


