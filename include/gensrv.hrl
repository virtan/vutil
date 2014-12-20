-record(gensrv, {
          register, %% {local, LocalName}, {global, GlobalName}
          init, %% /1 init(Opaque) -> {error, Reason} | ignore | NewOpaque
          handle_call, %% /3 handle_call(Message, From, Opaque) -> {error, Reason} | {ignore | Reply, ignore | NewOpaque}
          handle_cast, %% /2 handle_cast(Message, Opaque) -> {error, Reason} | ignore | NewOpaque
          handle_info, %% /2 handle_info(Message, Opaque) -> {error, Reason} | ignore | NewOpaque
          terminate, %% /2 terminate(Reason, Opaque) -> NewReason
          code_change, %% /1 code_change(Opaque) -> {error, Reason} | ignore | NewOpaque
          opaque
         }).
